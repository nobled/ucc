#include "ucl.h"
#include "ast.h"
#include "expr.h"
#include "gen.h"

/**
 * Translates a primary expression.
 */
static Symbol TranslatePrimaryExpression(AstExpression expr)
{
	if (expr->op == OP_CONST)
		return AddConstant(expr->ty, expr->val);

	/// if the expression is adjusted from an array or a function,
	/// returns the address of the symbol for this identifier
	if (expr->op == OP_STR || expr->isarray || expr->isfunc)
		return AddressOf(expr->val.p);

	return expr->val.p;
}

/**
 * Reading a bit field.
 * @param fld  bit field description 
 * @param p    the symbol which represents the whole integer containing the bit field
 */
static Symbol ReadBitField(Field fld, Symbol p)
{
	int size  = 8 * fld->ty->size;
	int lbits = size - (fld->bits + fld->pos);
	int rbits = size - (fld->bits);

	/**
	 * Given the following code snippet:
	 * struct st
	 * {
	 *   int a;
	 *   int b : 3;
	 * } st;    
	 *          |bits(8)|    pos(16)   |        
	 * ---------------------------------
	 * |        |  b    |              |
	 * ---------------------------------
	 * In order to read b's value, at first, we must left shift b to the most significant bit
	 * then right shift b's to the least significant bit.
	 */
	p = Simplify(T(INT), LSH, p, IntConstant(lbits));
	p = Simplify(T(INT), RSH, p, IntConstant(rbits));

	return p;
}

static Symbol WriteBitField(Field fld, Symbol dst, Symbol src)
{
	int fmask = (1 << fld->bits) - 1;
	int mask  = fmask << fld->pos;
	Symbol p;

	if (src->kind == SK_Constant && src->val.i[0] == 0)
	{
		p = Simplify(T(INT), BAND, dst, IntConstant(~mask));
	}
	else if (src->kind == SK_Constant && (src->val.i[0] & fmask) == fmask)
	{
		p  = Simplify(T(INT), BOR, dst, IntConstant(mask));
	}
	else
	{
		if (src->kind == SK_Constant)
		{
			src = IntConstant((src->val.i[0] << fld->pos) & mask);
		}
		else
		{
			src = Simplify(T(INT), LSH, src, IntConstant(fld->pos));
			src = Simplify(T(INT), BAND, src, IntConstant(mask));
		}
		p = Simplify(T(INT), BAND, dst, IntConstant(~mask));
		p = Simplify(T(INT), BOR,  p, src);
	}

	if (dst->kind == SK_Temp && AsVar(dst)->def->op == DEREF)
	{
		Symbol addr = AsVar(dst)->def->src1;

		GenerateIndirectMove(fld->ty, addr, p);
		dst = Deref(fld->ty, addr);
	}
	else
	{
		GenerateMove(fld->ty, dst, p);
	}
	return ReadBitField(fld, dst);
}

static Symbol Offset(Type ty, Symbol addr, Symbol voff, int coff)
{
	if (voff != NULL)
	{
		voff = Simplify(T(POINTER), ADD, voff, IntConstant(coff));
		addr = Simplify(T(POINTER), ADD, addr, voff);
		return Deref(ty, addr);
	}

	if (addr->kind == SK_Temp && AsVar(addr)->def->op == ADDR)
	{
		return CreateOffset(ty, AsVar(addr)->def->src1, coff);
	}

	return Deref(ty, Simplify(T(POINTER), ADD, addr, IntConstant(coff)));
}

/**
 * Generate intermedicate code to calculate a branch expression's value.
 * e.g. int a, b; a = a > b;
 * Introduces a new temporary t to holds the value of a > b.
 *     if a > b goto trueBB
 * falseBB:
 *     t = 0;
 *     goto nextBB;
 * trueBB:
 *     t = 1;
 * nextBB:
 *     ...
 */
static Symbol TranslateBranchExpression(AstExpression expr)
{
	BBlock nextBB, trueBB, falseBB;
	Symbol t;

	t = CreateTemp(expr->ty);
	nextBB = CreateBBlock();
	trueBB = CreateBBlock();
	falseBB = CreateBBlock();

	TranslateBranch(expr, trueBB, falseBB);

	StartBBlock(falseBB);
	GenerateMove(expr->ty, t, IntConstant(0));
	GenerateJump(nextBB);

	StartBBlock(trueBB);
	GenerateMove(expr->ty, t, IntConstant(1));

	StartBBlock(nextBB);

	return t;
}

/**
 * Translates array index access expression.
 */
static Symbol TranslateArrayIndex(AstExpression expr)
{
	AstExpression p;
	Symbol addr, dst, voff = NULL;
	int coff = 0;

	p = expr;
	/// 
	do
	{
		if (p->kids[1]->op == OP_CONST)
		{
			coff += p->kids[1]->val.i[0];
		}
		else if (voff == NULL)
		{
			voff = TranslateExpression(p->kids[1]);
		}
		else
		{
			voff = Simplify(voff->ty, ADD, voff, TranslateExpression(p->kids[1]));
		}
		p = p->kids[0];
	} while (p->op == OP_INDEX && p->kids[0]->isarray);

	addr = TranslateExpression(p);
	dst = Offset(expr->ty, addr, voff, coff);

	return expr->isarray ? AddressOf(dst) : dst;
}

static Symbol TranslateFunctionCall(AstExpression expr)
{
	AstExpression arg;
	Symbol faddr, recv;
	ILArg ilarg;
	Vector args = CreateVector(4);

	expr->kids[0]->isfunc = 0;
	faddr = TranslateExpression(expr->kids[0]);
	arg = expr->kids[1];
	while (arg)
	{
		ALLOC(ilarg);
		ilarg->sym = TranslateExpression(arg);
		ilarg->ty = arg->ty;
		INSERT_ITEM(args, ilarg);
		arg = (AstExpression)arg->next;
	}

	recv = NULL;
	if (expr->ty->categ != VOID)
	{
		recv = CreateTemp(expr->ty);
	}
	GenerateFunctionCall(expr->ty, recv, faddr, args);

	return recv;
}

static Symbol TranslateMemberAccess(AstExpression expr)
{
	AstExpression p;
	Field fld;
	Symbol addr, dst;
	int coff = 0;

	p = expr;
	if (p->op == OP_PTR_MEMBER)
	{
		fld =  p->val.p;
		coff = fld->offset;
		addr = TranslateExpression(expr->kids[0]);
	}
	else
	{
		while (p->op == OP_MEMBER)
		{
			fld = p->val.p;
			coff += fld->offset;
			p = p->kids[0];
		}
		addr = AddressOf(TranslateExpression(p));
	}

	dst = Offset(expr->ty, addr, NULL, coff);
	fld = dst->val.p = expr->val.p;

	if (fld->bits != 0 && expr->lvalue == 0)
		return ReadBitField(fld, dst);

	return expr->isarray ? AddressOf(dst) : dst;
}

static Symbol TranslateIncrement(AstExpression expr)
{
	AstExpression casgn;
	Symbol p;
	Field fld = NULL;

	casgn = expr->kids[0];
	p = TranslateExpression(casgn->kids[0]);
	casgn->kids[0]->op = OP_ID;
	casgn->kids[0]->val.p = p;
	fld = p->val.p;

	if (expr->op == OP_POSTINC || expr->op == OP_POSTDEC)
	{
		Symbol ret;

		ret = p;
		if (casgn->kids[0]->bitfld)
		{
			ret = ReadBitField(fld, p);
		}
		else if (p->kind != SK_Temp)
		{
			ret = CreateTemp(expr->ty);
			GenerateMove(expr->ty, ret, p);
		}
		TranslateExpression(casgn);
		return ret;
	}

	return TranslateExpression(casgn);
}

static Symbol TranslatePostfixExpression(AstExpression expr)
{
	switch (expr->op)
	{
	case OP_INDEX:
		return TranslateArrayIndex(expr);

	case OP_CALL:
		return TranslateFunctionCall(expr);

	case OP_MEMBER:
	case OP_PTR_MEMBER:
		return TranslateMemberAccess(expr);

	case OP_POSTINC:
	case OP_POSTDEC:
		return TranslateIncrement(expr);

	default:
		assert(0);
		return NULL;
	}
}

static Symbol TranslateCast(Type ty, Type sty, Symbol src)
{
	Symbol dst;
	int scode, dcode, opcode;

	dcode = TypeCode(ty);
	scode = TypeCode(sty);

	if (dcode == V)
		return NULL;

	switch (scode)
	{
	case I1:
		opcode = EXTI1; break;

	case I2:
		opcode = EXTI2; break;

	case U1:
		opcode = EXTU1; break;

	case U2:
		opcode = EXTU2; break;

	case I4:
		if (dcode <= U1)
			opcode = TRUI1;
		else if (dcode <= U2)
			opcode = TRUI2;
		else if (dcode == F4)
			opcode = CVTI4F4;
		else if (dcode == F8)
			opcode = CVTI4F8;
		else
			return src;
		break;

	case U4:
		if (dcode == F4)
			opcode = CVTU4F4;
		else if (dcode == F8)
			opcode = CVTU4F8;
		else
			return src;
		break;

	case F4:
		if (dcode == I4)
			opcode = CVTF4I4;
		else if (dcode == U4)
			opcode = CVTF4U4;
		else
			opcode = CVTF4;
		break;

	case F8:
		if (dcode == I4)
			opcode = CVTF8I4;
		else if (dcode == U4)
			opcode = CVTF8U4;
		else
			opcode = CVTF8;
		break;

	default:
		assert(0);
		return NULL;
	}

	dst = CreateTemp(ty);
	GenerateAssign(sty, dst, opcode, src, NULL);
	return dst;
}

static Symbol TranslateUnaryExpression(AstExpression expr)
{
	Symbol src;

	if (expr->op == OP_NOT)
	{
		return TranslateBranchExpression(expr);
	}

	if (expr->op == OP_PREINC || expr->op == OP_PREDEC)
	{
		return TranslateIncrement(expr);
	}

	src = TranslateExpression(expr->kids[0]);
	switch (expr->op)
	{
	case OP_CAST:
		return TranslateCast(expr->ty, expr->kids[0]->ty, src);

	case OP_ADDRESS:
		return AddressOf(src);

	case OP_DEREF:
		return Deref(expr->ty, src);

	case OP_NEG:
	case OP_COMP:
		return Simplify(expr->ty, OPMap[expr->op], src, NULL);

	default:
		assert(0);
		return NULL;
	}
}

static Symbol TranslateBinaryExpression(AstExpression expr)
{
	Symbol src1, src2;

	if (expr->op == OP_OR || expr->op == OP_AND || (expr->op >= OP_EQUAL && expr->op <= OP_LESS_EQ))
	{
		return TranslateBranchExpression(expr);
	}

	src1 = TranslateExpression(expr->kids[0]);
	src2 = TranslateExpression(expr->kids[1]);
	return Simplify(expr->ty, OPMap[expr->op], src1, src2);
}

static Symbol TranslateConditionalExpression(AstExpression expr)
{
	Symbol t, t1, t2;
	BBlock trueBB, falseBB, nextBB;

	t = NULL;
	if (expr->ty->categ != VOID)
	{
		t = CreateTemp(expr->ty);
	}
	trueBB = CreateBBlock();
	falseBB = CreateBBlock();
	nextBB = CreateBBlock();

	TranslateBranch(expr->kids[0], trueBB, falseBB);

	StartBBlock(falseBB);
	t1 = TranslateExpression(expr->kids[1]->kids[1]);
	if (t1 != NULL)
		GenerateMove(expr->ty, t, t1);
	GenerateJump(nextBB);

	StartBBlock(trueBB);
	t2 = TranslateExpression(expr->kids[1]->kids[0]);
	if (t2 != NULL)
		GenerateMove(expr->ty, t, t2);

	StartBBlock(nextBB);
	return t;
}

static Symbol TranslateAssignmentExpression(AstExpression expr)
{
	Symbol dst, src;
	Field fld = NULL;

	dst = TranslateExpression(expr->kids[0]);
	fld = dst->val.p;
	if (expr->op != OP_ASSIGN)
	{
		expr->kids[0]->op = OP_ID;
		expr->kids[0]->val.p = expr->kids[0]->bitfld ? ReadBitField(fld, dst) : dst;
	}
	src = TranslateExpression(expr->kids[1]);

	if (expr->kids[0]->bitfld)
	{
		return WriteBitField(fld, dst, src);
	}
	else if (dst->kind == SK_Temp && AsVar(dst)->def->op == DEREF)
	{
		Symbol addr = AsVar(dst)->def->src1;

		GenerateIndirectMove(expr->ty, addr, src);
		dst = Deref(expr->ty, addr);
	}
	else
	{
		GenerateMove(expr->ty, dst, src);
	}

	return dst;
}

static Symbol TranslateCommaExpression(AstExpression expr)
{
	TranslateExpression(expr->kids[0]);
	return TranslateExpression(expr->kids[1]);
}

static Symbol TranslateErrorExpression(AstExpression expr)
{
	assert(0);
	return NULL;
}

static Symbol (* ExprTrans[])(AstExpression) = 
{
#define OPINFO(op, prec, name, func, opcode) Translate##func##Expression,
#include "opinfo.h"
#undef OPINFO
};

AstExpression Not(AstExpression expr)
{
	static int rops[] = { OP_UNEQUAL, OP_EQUAL, OP_LESS_EQ, OP_GREAT_EQ, OP_LESS, OP_GREAT };
	AstExpression t;

	switch (expr->op)
	{
	case OP_OR:
		expr->op = OP_AND;
		expr->kids[0] = Not(expr->kids[0]);
		expr->kids[1] = Not(expr->kids[1]);
		return expr;

	case OP_AND:
		expr->op = OP_OR;
		expr->kids[0] = Not(expr->kids[0]);
		expr->kids[1] = Not(expr->kids[1]);
		return expr;

	case OP_EQUAL:
	case OP_UNEQUAL:
	case OP_GREAT:
	case OP_LESS:
	case OP_GREAT_EQ:
	case OP_LESS_EQ:
		expr->op = rops[expr->op - OP_EQUAL];
		return expr;

	case OP_NOT:
		return expr->kids[0];

	default:
		CREATE_AST_NODE(t, Expression);
		t->coord = expr->coord;
		t->ty = T(INT);
		t->op = OP_NOT;
		t->kids[0] = expr;
		return FoldConstant(t);
	}
}

void TranslateBranch(AstExpression expr, BBlock trueBB, BBlock falseBB)
{
	BBlock rtestBB;
	Symbol src1, src2;
	Type ty;

	switch (expr->op)
	{
	case OP_AND:
		rtestBB = CreateBBlock();
		TranslateBranch(Not(expr->kids[0]), falseBB, rtestBB);
		StartBBlock(rtestBB);
		TranslateBranch(expr->kids[1], trueBB, falseBB);
		break;

	case OP_OR:
		rtestBB = CreateBBlock();
		TranslateBranch(expr->kids[0], trueBB, rtestBB);
		StartBBlock(rtestBB);
		TranslateBranch(expr->kids[1], trueBB, falseBB);
		break;

	case OP_EQUAL:
	case OP_UNEQUAL:
	case OP_GREAT:
	case OP_LESS:
	case OP_GREAT_EQ:
	case OP_LESS_EQ:
		src1 = TranslateExpression(expr->kids[0]);
		src2 = TranslateExpression(expr->kids[1]);
		GenerateBranch(expr->kids[0]->ty, trueBB, OPMap[expr->op], src1, src2);
		break;

	case OP_NOT:
		src1 = TranslateExpression(expr->kids[0]);
		ty = expr->kids[0]->ty;
		if (ty->categ < INT)
		{
			src1 = TranslateCast(T(INT), ty, src1);
			ty = T(INT);
		}
		GenerateBranch(ty, trueBB, JZ, src1, NULL);
		break;

	case OP_CONST:
		if (! (expr->val.i[0] == 0 && expr->val.i[1] == 0))
		{
			GenerateJump(trueBB);
		}
		break;

	default:
		src1 = TranslateExpression(expr);
		if (src1->kind  == SK_Constant)
		{
			if (! (src1->val.i[0] == 0 && src1->val.i[1] == 0))
			{
				GenerateJump(trueBB);
			}
		}
		else
		{
			ty = expr->ty;
			if (ty->categ < INT)
			{
				src1 = TranslateCast(T(INT), ty, src1);
				ty = T(INT);
			}
			GenerateBranch(ty, trueBB, JNZ, src1, NULL);
		}
		break;
	}
}

Symbol TranslateExpression(AstExpression expr)
{
	return (* ExprTrans[expr->op])(expr);
}

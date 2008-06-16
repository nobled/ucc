#include "ucl.h"
#include "ast.h"
#include "expr.h"
#include "decl.h"

#define SWAP_KIDS(expr)              \
{                                    \
    AstExpression t = expr->kids[0]; \
    expr->kids[0] = expr->kids[1];   \
    expr->kids[1] = t;               \
}

#define PERFORM_ARITH_CONVERSION(expr)                                 \
    expr->ty = CommonRealType(expr->kids[0]->ty, expr->kids[1]->ty);   \
    expr->kids[0] = Cast(expr->ty, expr->kids[0]);                     \
    expr->kids[1] = Cast(expr->ty, expr->kids[1]);


#define REPORT_OP_ERROR                                                 \
    Error(&expr->coord, "Invalid operands to %s", OPNames[expr->op]);   \
    expr->ty = T(INT);                                                  \
    return expr;                   

/**
 * Check if the expression expr is a modifiable-lvalue
 */
static int CanModify(AstExpression expr)
{		
	return (expr->lvalue && ! (expr->ty->qual & CONST) && 
	        (IsRecordType(expr->ty) ? ! ((RecordType)expr->ty)->hasConstFld : 1));
}

/**
 * Check if the expression expr is null constant
 */
static int IsNullConstant(AstExpression expr)
{
	return expr->op == OP_CONST && expr->val.i[0] == 0;
}

/**
 * Construct an expression to multiply offset by scale.
 */
static AstExpression ScalePointerOffset(AstExpression offset, int scale)
{
	AstExpression expr;
	union value val;

	CREATE_AST_NODE(expr, Expression);

	expr->ty = offset->ty;
	expr->op = OP_MUL;
	expr->kids[0] = offset;
	val.i[1] = 0;
	val.i[0] = scale;
	expr->kids[1] = Constant(offset->coord, offset->ty, val);

	return FoldConstant(expr);
}

/**
 * Construct an expression to divide diff by size
 */
static AstExpression PointerDifference(AstExpression diff, int size)
{
	AstExpression expr;
	union value val;

	CREATE_AST_NODE(expr, Expression);

	expr->ty = diff->ty;
	expr->op = OP_DIV;
	expr->kids[0] = diff;
	val.i[1] = 0;
	val.i[0] = size;
	expr->kids[1] = Constant(diff->coord, diff->ty, val);

	return expr;
}

/**
 * Check primary expression
 */
static AstExpression CheckPrimaryExpression(AstExpression expr)
{
	Symbol p;

	if (expr->op == OP_CONST)
		return expr;

	if (expr->op == OP_STR)
	{
		expr->op = OP_ID;
		expr->val.p = AddString(expr->ty, expr->val.p);
		expr->lvalue = 1;
		return expr;
	}

	p = LookupID(expr->val.p);
	if (p == NULL)
	{
		Error(&expr->coord, "Undeclared identifier: %s", expr->val.p);
		p = AddVariable(expr->val.p, T(INT), Level == 0 ? 0 : TK_AUTO);
		expr->ty = T(INT);
		expr->lvalue = 1;
	}
	else if (p->kind == SK_TypedefName)
	{
		Error(&expr->coord, "Typedef name cannot be used as variable");
		expr->ty = T(INT);
	}
	else if (p->kind == SK_EnumConstant)
	{
		expr->op = OP_CONST;
		expr->val = p->val;
		expr->ty = T(INT);
	}
	else
	{
		expr->ty = p->ty;
		expr->val.p = p;
		expr->inreg   = p->sclass == TK_REGISTER;
		expr->lvalue  = expr->ty->categ != FUNCTION;
	}

	return expr;
}

static AstExpression PromoteArgument(AstExpression arg)
{
	Type ty = Promote(arg->ty);

	return Cast(ty, arg);
}

/**
 * Check argument.
 * @param fty function type
 * @param arg argument expression
 * @param argNo the argument's number in function call
 * @param argFull if the function's argument is full
 */
static AstExpression CheckArgument(FunctionType fty, AstExpression arg, int argNo, int *argFull)
{
	Parameter param;
	int parLen = LEN(fty->sig->params);

	arg = Adjust(CheckExpression(arg), 1);

	if (fty->sig->hasProto && parLen == 0)
	{
		*argFull = 1;
		return arg;
	}

	if (argNo == parLen && ! fty->sig->hasEllipse)
		*argFull = 1;
	
	if (! fty->sig->hasProto)
	{
		arg = PromoteArgument(arg);

		if (parLen != 0)
		{
			param = GET_ITEM(fty->sig->params, argNo - 1);
			if (! IsCompatibleType(arg->ty, param->ty))
				goto err;
		}

		return arg;
	}
	else if (argNo <= parLen)
	{
		param = GET_ITEM(fty->sig->params, argNo - 1);
		if (! CanAssign(param->ty, arg))
			goto err;

		if (param->ty->categ < INT)
			arg = Cast(T(INT), arg);
		else
			arg = Cast(param->ty, arg);

		return arg;
	}
	else
	{
		return PromoteArgument(arg);
	}

err:
	Error(&arg->coord, "Incompatible argument");
	return arg;
}

static AstExpression CheckFunctionCall(AstExpression expr)
{
	AstExpression arg;
	Type ty;
	AstNode *tail;
	int argNo, argFull;

	if (expr->kids[0]->op == OP_ID && LookupID(expr->kids[0]->val.p) == NULL)
	{
		expr->kids[0]->ty = DefaultFunctionType;
		expr->kids[0]->val.p = AddFunction(expr->kids[0]->val.p, DefaultFunctionType, TK_EXTERN);
	}
	else
	{
		expr->kids[0] = CheckExpression(expr->kids[0]);
	}
	expr->kids[0] = Adjust(expr->kids[0], 1);
	ty = expr->kids[0]->ty;

	if (! (IsPtrType(ty) && IsFunctionType(ty->bty)))
	{
		Error(&expr->coord, "The left operand must be function or function pointer");
	    ty = DefaultFunctionType;
	}
	else
	{
		ty = ty->bty;
	}

	tail = (AstNode *)&expr->kids[1];
	arg = expr->kids[1];
	argNo = 1;
	argFull = 0;
	while (arg != NULL && ! argFull)
	{
		*tail = (AstNode)CheckArgument((FunctionType)ty, arg, argNo, &argFull);
		tail = &(*tail)->next;
		arg = (AstExpression)arg->next;
		argNo++;
	}
	*tail = NULL;

	if (arg != NULL)
	{
		while (arg != NULL)
		{
			CheckExpression(arg);
			arg = (AstExpression)arg->next;
		}
		Error(&expr->coord, "Too many arguments");
	}
	else if (argNo < LEN(((FunctionType)ty)->sig->params))
	{
		Error(&expr->coord, "Too few arguments");
	}
	expr->ty = ty->bty;

	return expr;
}

static AstExpression CheckMemberAccess(AstExpression expr)
{
	Type ty;
	Field fld;

	expr->kids[0] = CheckExpression(expr->kids[0]);
	if (expr->op == OP_MEMBER)
	{
		expr->kids[0] = Adjust(expr->kids[0], 0);
		ty = expr->kids[0]->ty;
		if (! IsRecordType(ty))
		{
			REPORT_OP_ERROR;
		}
		expr->lvalue = expr->kids[0]->lvalue;
	}
	else
	{
		expr->kids[0] = Adjust(expr->kids[0], 1);
		ty = expr->kids[0]->ty;
		if (! (IsPtrType(ty) && IsRecordType(ty->bty)))
		{
			REPORT_OP_ERROR;
		}
		ty = ty->bty;
		expr->lvalue = 1;
	}
	
	fld = LookupField(Unqual(ty), expr->val.p);
	if (fld == NULL)
	{
		Error(&expr->coord, "struct or union member %s doesn't exsist", expr->val.p);
		expr->ty = T(INT);
		return expr;
	}
	expr->ty = Qualify(ty->qual, fld->ty);
	expr->val.p = fld;
	expr->bitfld = fld->bits != 0;
	return expr;
}

static AstExpression TransformIncrement(AstExpression expr)
{
	AstExpression casgn;
	union value val;
	
	val.i[1] = 0; val.i[0] = 1;
	CREATE_AST_NODE(casgn, Expression);
	casgn->coord = expr->coord;
	casgn->op = (expr->op == OP_POSTINC || expr->op == OP_PREINC) ? OP_ADD_ASSIGN : OP_SUB_ASSIGN;
	casgn->kids[0] = expr->kids[0];
	casgn->kids[1] = Constant(expr->coord, T(INT), val);

	expr->kids[0] = CheckExpression(casgn);
	expr->ty = expr->kids[0]->ty;
	return expr;
}

static AstExpression CheckPostfixExpression(AstExpression expr)
{
	switch (expr->op)
	{
	case OP_INDEX:
		expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);
		expr->kids[1] = Adjust(CheckExpression(expr->kids[1]), 1);
		if (IsIntegType(expr->kids[0]->ty))
		{
			SWAP_KIDS(expr);
		}
		if (IsObjectPtr(expr->kids[0]->ty) && IsIntegType(expr->kids[1]->ty))
		{
			expr->ty = expr->kids[0]->ty->bty;
			expr->lvalue = 1;
			expr->kids[1] = DoIntegerPromotion(expr->kids[1]);
			expr->kids[1] = ScalePointerOffset(expr->kids[1], expr->ty->size);
			return expr;
		}
		REPORT_OP_ERROR;

	case OP_CALL:
		return CheckFunctionCall(expr);

	case OP_MEMBER:
	case OP_PTR_MEMBER:
		return CheckMemberAccess(expr);

	case OP_POSTINC:
	case OP_POSTDEC:
		return TransformIncrement(expr);

	default:
		assert(0);
	}

	REPORT_OP_ERROR;
}


static AstExpression CheckTypeCast(AstExpression expr)
{
	Type ty;

	ty = CheckTypeName((AstTypeName)expr->kids[0]);
	expr->kids[1] = Adjust(CheckExpression(expr->kids[1]), 1);

	if (! (BothScalarType(ty, expr->kids[1]->ty) || ty->categ == VOID))
	{
		Error(&expr->coord, "Illegal type cast");
		return expr->kids[1];
	}

	return Cast(ty, expr->kids[1]);
}



static AstExpression CheckUnaryExpression(AstExpression expr)
{
	Type ty;

	switch (expr->op)
	{
	case OP_PREINC:
	case OP_PREDEC:
		return TransformIncrement(expr);

	case OP_ADDRESS:
		expr->kids[0] = CheckExpression(expr->kids[0]);
		ty = expr->kids[0]->ty;
		if (expr->kids[0]->op == OP_DEREF)
		{
			expr->kids[0]->kids[0]->lvalue = 0;
			return expr->kids[0]->kids[0];
		}
		else if (expr->kids[0]->op == OP_INDEX)
		{
			expr->kids[0]->op = OP_ADD;
			expr->kids[0]->ty = PointerTo(ty);
			expr->kids[0]->lvalue = 0;
			return expr->kids[0];
		}
		else if (IsFunctionType(ty) || 
			     (expr->kids[0]->lvalue && ! expr->kids[0]->bitfld && ! expr->kids[0]->inreg))
		{
			expr->ty = PointerTo(ty);
			return expr;
		}
		break;

	case OP_DEREF:
		expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);
		ty = expr->kids[0]->ty;
		if (expr->kids[0]->op == OP_ADDRESS)
		{
			expr->kids[0]->kids[0]->ty = ty->bty;
			return expr->kids[0]->kids[0];
		}
		else if (expr->kids[0]->op == OP_ADD && expr->kids[0]->kids[0]->isarray)
		{
			expr->kids[0]->op = OP_INDEX;
			expr->kids[0]->ty = ty->bty;
			expr->kids[0]->lvalue = 1;
			return expr->kids[0];
		}
		if (IsPtrType(ty))
		{
			expr->ty = ty->bty;
			if (IsFunctionType(expr->ty))
			{
				return expr->kids[0];
			}
			expr->lvalue = 1;
			return expr;
		}
		break;

	case OP_POS:
	case OP_NEG:
		expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);
		if (IsArithType(expr->kids[0]->ty))
		{
			expr->kids[0] = DoIntegerPromotion(expr->kids[0]);
			expr->ty = expr->kids[0]->ty;
			return expr->op == OP_POS ? expr->kids[0] : FoldConstant(expr);
		}
		break;

	case OP_COMP:
		expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);
		if (IsIntegType(expr->kids[0]->ty))
		{
			expr->kids[0] = DoIntegerPromotion(expr->kids[0]);
			expr->ty = expr->kids[0]->ty;
			return FoldConstant(expr);
		}
		break;

	case OP_NOT:
		expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);
		if (IsScalarType(expr->kids[0]->ty))
		{
			expr->ty = T(INT);
			return FoldConstant(expr);
		}
		break;

	case OP_SIZEOF:
		if (expr->kids[0]->kind == NK_Expression)
		{
			expr->kids[0] = CheckExpression(expr->kids[0]);
			if (expr->kids[0]->bitfld)
				goto err;
			ty = expr->kids[0]->ty;
		}
		else
		{
			ty = CheckTypeName((AstTypeName)expr->kids[0]);
		}
		if (IsFunctionType(ty) || ty->size == 0)
			goto err;

		expr->ty = T(UINT);
		expr->op = OP_CONST;
		expr->val.i[0] = ty->size;
		return expr;

	case OP_CAST:
		return CheckTypeCast(expr);

	default:
		assert(0);
	}

err:
	REPORT_OP_ERROR;

}

static AstExpression CheckMultiplicativeOP(AstExpression expr)
{
	if (expr->op != OP_MOD && BothArithType(expr->kids[0]->ty, expr->kids[1]->ty))
		goto ok;

	if (expr->op == OP_MOD && BothIntegType(expr->kids[0]->ty, expr->kids[1]->ty))
		goto ok;

	REPORT_OP_ERROR;

ok:
	PERFORM_ARITH_CONVERSION(expr);
	return FoldConstant(expr);
}

static AstExpression CheckAddOP(AstExpression expr)
{
	Type ty1, ty2;

	if (expr->kids[0]->op == OP_CONST)
	{
		SWAP_KIDS(expr);
	}
	ty1 = expr->kids[0]->ty;
	ty2 = expr->kids[1]->ty;

	if (BothArithType(ty1, ty2))
	{
		PERFORM_ARITH_CONVERSION(expr);
		return FoldConstant(expr);
	}
	if (IsObjectPtr(ty2) && IsIntegType(ty1))
	{
		SWAP_KIDS(expr);
		ty1 = expr->kids[0]->ty;
		goto left_ptr;
	}

	if (IsObjectPtr(ty1) && IsIntegType(ty2))
	{
left_ptr:
		expr->kids[1] = DoIntegerPromotion(expr->kids[1]);
		expr->kids[1] = ScalePointerOffset(expr->kids[1], ty1->bty->size);
		expr->ty = ty1;
		return expr;
	}

	REPORT_OP_ERROR;
}

static AstExpression CheckSubOP(AstExpression expr)
{
	Type ty1, ty2;

	ty1 = expr->kids[0]->ty;
	ty2 = expr->kids[1]->ty;
	if (BothArithType(ty1, ty2))
	{
		PERFORM_ARITH_CONVERSION(expr);
		return FoldConstant(expr);
	}
	if (IsObjectPtr(ty1) && IsIntegType(ty2))
	{
		expr->kids[1] = DoIntegerPromotion(expr->kids[1]);
		expr->kids[1] = ScalePointerOffset(expr->kids[1], ty1->bty->size);
		expr->ty = ty1;
		return expr;
	}
	if (IsCompatiblePtr(ty1, ty2))
	{
		expr->ty = T(INT);
		expr = PointerDifference(expr, ty1->bty->size);
		return expr;
	}

	REPORT_OP_ERROR;
}

static AstExpression CheckShiftOP(AstExpression expr)
{
	if (BothIntegType(expr->kids[0]->ty, expr->kids[1]->ty))
	{
		expr->kids[0] = DoIntegerPromotion(expr->kids[0]);
		expr->kids[1] = DoIntegerPromotion(expr->kids[1]);
		expr->ty = expr->kids[0]->ty;
		return FoldConstant(expr);
	}

	REPORT_OP_ERROR;
}

static AstExpression CheckRelationalOP(AstExpression expr)
{
	Type ty1, ty2;
	
	expr->ty = T(INT);
	ty1 = expr->kids[0]->ty;
	ty2 = expr->kids[1]->ty;
	if (BothArithType(ty1, ty2))
	{
		PERFORM_ARITH_CONVERSION(expr);
		expr->ty = T(INT);
		return FoldConstant(expr);
	}

	if (IsObjectPtr(ty1) && IsObjectPtr(ty2) && 
		IsCompatibleType(Unqual(ty1->bty), Unqual(ty2->bty)))
	{
		return expr;
	}

	if (IsIncompletePtr(ty1) && IsIncompletePtr(ty2) &&
		IsCompatibleType(Unqual(ty1->bty), Unqual(ty2->bty)))
	{
		return expr;
	}

	REPORT_OP_ERROR;
}

static AstExpression CheckEqualityOP(AstExpression expr)
{
	Type ty1, ty2;

	expr->ty = T(INT);
	ty1 = expr->kids[0]->ty;
	ty2 = expr->kids[1]->ty;
	if (BothArithType(ty1, ty2))
	{
		PERFORM_ARITH_CONVERSION(expr);
		expr->ty = T(INT);
		return FoldConstant(expr);
	}

	if (IsCompatiblePtr(ty1, ty2) ||
		NotFunctionPtr(ty1) && IsVoidPtr(ty2) ||
		NotFunctionPtr(ty2) && IsVoidPtr(ty1) ||
		IsPtrType(ty1) && IsNullConstant(expr->kids[1]) ||
		IsPtrType(ty2) && IsNullConstant(expr->kids[0]))
	{
		return expr;
	}

	REPORT_OP_ERROR;
}

static AstExpression CheckBitwiseOP(AstExpression expr)
{
	if (BothIntegType(expr->kids[0]->ty, expr->kids[1]->ty))
	{
		PERFORM_ARITH_CONVERSION(expr);
		return FoldConstant(expr);
	}

	REPORT_OP_ERROR;
}

static AstExpression CheckLogicalOP(AstExpression expr)
{
	if (BothScalarType(expr->kids[0]->ty, expr->kids[1]->ty))
	{
		expr->ty = T(INT);
		return FoldConstant(expr);
	}

	REPORT_OP_ERROR;		
}

static AstExpression (* BinaryOPCheckers[])(AstExpression) = 
{
	CheckLogicalOP,
	CheckLogicalOP,
	CheckBitwiseOP,
	CheckBitwiseOP,
	CheckBitwiseOP,
	CheckEqualityOP,
	CheckEqualityOP,
	CheckRelationalOP,
	CheckRelationalOP,
	CheckRelationalOP,
	CheckRelationalOP,
	CheckShiftOP,
	CheckShiftOP,
	CheckAddOP,
	CheckSubOP,
	CheckMultiplicativeOP,
	CheckMultiplicativeOP,
	CheckMultiplicativeOP
};

static AstExpression CheckBinaryExpression(AstExpression expr)
{
	expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);;
	expr->kids[1] = Adjust(CheckExpression(expr->kids[1]), 1);

	return (* BinaryOPCheckers[expr->op - OP_OR])(expr);
}

static AstExpression CheckAssignmentExpression(AstExpression expr)
{
	int ops[] = 
	{ 
		OP_BITOR, OP_BITXOR, OP_BITAND, OP_LSHIFT, OP_RSHIFT, 
		OP_ADD,	  OP_SUB,    OP_MUL,    OP_DIV,    OP_MOD 
	};
	Type ty;
	
	expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 0);
	expr->kids[1] = Adjust(CheckExpression(expr->kids[1]), 1);

	if (! CanModify(expr->kids[0]))
	{
		Error(&expr->coord, "The left operand cannot be modified");
	}
	if (expr->op != OP_ASSIGN)
	{
		AstExpression lopr;

		CREATE_AST_NODE(lopr, Expression);
		lopr->coord   = expr->coord;
		lopr->op      = ops[expr->op - OP_BITOR_ASSIGN];
		lopr->kids[0] = expr->kids[0];
		lopr->kids[1] = expr->kids[1];

		expr->kids[1] = (*BinaryOPCheckers[lopr->op - OP_OR])(lopr);
	}

	ty = expr->kids[0]->ty;
	if (! CanAssign(ty, expr->kids[1]))
	{
		Error(&expr->coord, "Wrong assignment");
	}
	else
	{
		expr->kids[1] = Cast(ty, expr->kids[1]);
	}
	expr->ty = ty;
	return expr;
}

static AstExpression CheckConditionalExpression(AstExpression expr)
{
	int qual;
	Type ty1, ty2;

	expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);

	if (! IsScalarType(expr->kids[0]->ty))
	{
		Error(&expr->coord, "The first expression shall be scalar type.");
	}
	expr->kids[1]->kids[0] = Adjust(CheckExpression(expr->kids[1]->kids[0]), 1);
	expr->kids[1]->kids[1] = Adjust(CheckExpression(expr->kids[1]->kids[1]), 1);

	ty1 = expr->kids[1]->kids[0]->ty;
	ty2 = expr->kids[1]->kids[1]->ty;
	if (BothArithType(ty1, ty2))
	{
		expr->ty = CommonRealType(ty1, ty2);
		expr->kids[1]->kids[0] = Cast(expr->ty, expr->kids[1]->kids[0]);
		expr->kids[1]->kids[1] = Cast(expr->ty, expr->kids[1]->kids[1]);

		return FoldConstant(expr);
	}
	else if (IsRecordType(ty1) && ty1 == ty2)
	{
		expr->ty = ty1;
	}
	else if (ty1->categ == VOID && ty2->categ == VOID)
	{
		expr->ty = T(VOID);
	}
	else if (IsCompatiblePtr(ty1, ty2))
	{
		qual = ty1->bty->qual | ty2->bty->qual;
		expr->ty  = PointerTo(Qualify(qual, CompositeType(Unqual(ty1->bty), Unqual(ty2->bty))));
	}
	else if (IsPtrType(ty1) && IsNullConstant(expr->kids[1]->kids[1]))
	{
		expr->ty = ty1;
	}
	else if (IsPtrType(ty2) && IsNullConstant(expr->kids[1]->kids[0]))
	{
		expr->ty = ty2;
	}
	else if (NotFunctionPtr(ty1) && IsVoidPtr(ty2) ||
	         NotFunctionPtr(ty2) && IsVoidPtr(ty1))
	{
		qual = ty1->bty->qual | ty2->bty->qual;
		expr->ty = PointerTo(Qualify(qual, T(VOID)));
	}
	else
	{
		Error(&expr->coord, "invalid operand for ? operator.");
		expr->ty = T(INT);
	}

	return expr;
}

static AstExpression CheckCommaExpression(AstExpression expr)
{
	expr->kids[0] = Adjust(CheckExpression(expr->kids[0]), 1);
	expr->kids[1] = Adjust(CheckExpression(expr->kids[1]), 1);

	expr->ty = expr->kids[1]->ty;

	return expr;
}

static AstExpression CheckErrorExpression(AstExpression expr)
{
	assert(0);
	return NULL;
}

static AstExpression (* ExprCheckers[])(AstExpression) = 
{
#define OPINFO(op, prec, name, func, opcode) Check##func##Expression,
#include "opinfo.h"
#undef OPINFO
};

static AstExpression CastExpression(Type ty, AstExpression expr)
{
	AstExpression cast;

	if (expr->op == OP_CONST && ty->categ != VOID)
		return FoldCast(ty, expr);

	CREATE_AST_NODE(cast, Expression);

	cast->coord = expr->coord;
	cast->op = OP_CAST;
	cast->ty = ty;
	cast->kids[0] = expr;

	return cast;
}

int CanAssign(Type lty, AstExpression expr)
{
	Type rty = expr->ty;

	lty = Unqual(lty);
	rty = Unqual(rty);
	if (lty == rty)
	{
		return 1;
	}
	if (BothArithType(lty, rty))
	{
		return 1;
	}
	if (IsCompatiblePtr(lty, rty) && ((lty->bty->qual & rty->bty->qual) == rty->bty->qual))
	{
		return 1;
	}
	if ((NotFunctionPtr(lty) && IsVoidPtr(rty) || NotFunctionPtr(rty) && IsVoidPtr(lty))&& 
		((lty->bty->qual & rty->bty->qual) == rty->bty->qual))
	{
		return 1;
	}
	if (IsPtrType(lty) && IsNullConstant(expr))
	{
		return 1;
	}

	if (IsPtrType(lty) && IsPtrType(rty))
	{
		//Warning(&expr->coord, "assignment from incompatible pointer type");
		return 1;
	}

	if ((IsPtrType(lty) && IsIntegType(rty) || IsPtrType(rty) && IsIntegType(lty))&&
		(lty->size == rty->size))
	{
		Warning(&expr->coord, "conversion between pointer and integer without a cast");
		return 1;
	}

	return 0;
}

AstExpression Cast(Type ty, AstExpression expr)
{
	int scode = TypeCode(expr->ty);
	int dcode = TypeCode(ty);
	
	if (dcode == V)
	{
		return CastExpression(ty, expr);
	}

	if (scode < F4 && dcode < F4 && scode / 2 == dcode / 2)
	{
		expr->ty = ty;
		return expr;
	}

	if (scode < I4)
	{
		expr = CastExpression(T(INT), expr);
		scode = I4;
	}
	if (scode != dcode)
	{
		if (dcode < I4)
		{
			expr = CastExpression(T(INT), expr);
		}
		expr = CastExpression(ty, expr);
	}
	return expr;
}

AstExpression Adjust(AstExpression expr, int rvalue)
{
	if (rvalue)
	{
		expr->ty = Unqual(expr->ty);
		expr->lvalue = 0;
	}

	if (expr->ty->categ == FUNCTION)
	{
		expr->ty = PointerTo(expr->ty);
		expr->isfunc = 1;
	}
	else if (expr->ty->categ == ARRAY)
	{
		expr->ty = PointerTo(expr->ty->bty);
		expr->lvalue = 0;
		expr->isarray = 1;
	}

	return expr;
}

AstExpression DoIntegerPromotion(AstExpression expr)
{
	return expr->ty->categ < INT ? Cast(T(INT), expr) : expr;
}

AstExpression CheckExpression(AstExpression expr)
{
	return (* ExprCheckers[expr->op])(expr);
}

AstExpression CheckConstantExpression(AstExpression expr)
{
	expr = CheckExpression(expr);
	if (! (expr->op == OP_CONST && IsIntegType(expr->ty)))
	{
		return NULL;
	}
	return expr;
}


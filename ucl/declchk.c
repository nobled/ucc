#include "ucl.h"
#include "ast.h"
#include "decl.h"
#include "expr.h"
#include "stmt.h"

AstFunction CURRENTF;
FunctionSymbol FSYM;

static void CheckDeclarationSpecifiers(AstSpecifiers specs);
static void CheckDeclarator(AstDeclarator dec);

/**
 * Check if the initializer expression is address constant.
 * e.g. 
 * int a;
 * int b = &a;
 */
static AstExpression CheckAddressConstant(AstExpression expr)
{
	AstExpression addr;
	AstExpression p;
	int offset = 0;

	if (! IsPtrType(expr->ty))
		return NULL;

	if (expr->op == OP_ADD || expr->op == OP_SUB)
	{
		addr = CheckAddressConstant(expr->kids[0]);
		if (addr == NULL || expr->kids[1]->op != OP_CONST)
			return NULL;
		
		expr->kids[0] = addr->kids[0];
		expr->kids[1]->val.i[0] += (expr->op == OP_ADD ? 1 : -1) * addr->kids[1]->val.i[0];

		return expr;
	}

	if (expr->op == OP_ADDRESS)
		addr = expr->kids[0];
	else
		addr = expr;

	while (addr->op == OP_INDEX || addr->op == OP_MEMBER)
	{
		if (addr->op == OP_INDEX)
		{
			if (addr->kids[1]->op != OP_CONST)
				return NULL;

			offset += addr->kids[1]->val.i[0] * addr->ty->size;
		}
		else
		{
			Field fld = addr->val.p;

			offset += fld->offset;
		}
		addr = addr->kids[0];
	}

	if (addr->op != OP_ID || (expr->op != OP_ADDRESS && ! addr->isarray && ! addr->isfunc))
		return NULL;


	((Symbol)addr->val.p)->ref++;

	CREATE_AST_NODE(p, Expression);
	p->op = OP_ADD;
	p->ty = expr->ty;
	p->kids[0] = addr;
	p->kids[1] = Constant(addr->coord, T(INT), p->val);

	return p;
}

static void CheckInitConstant(AstInitializer init)
{
	InitData initd = init->idata;

	while (initd)
	{
		if (! (initd->expr->op == OP_CONST || initd->expr->op == OP_STR ||
		       (initd->expr = CheckAddressConstant(initd->expr))))
		{
			Error(&init->coord, "Initializer must be constant");
		}
		initd = initd->next;
	}
	return;
}

static AstExpression BORBitField(AstExpression expr1, AstExpression expr2)
{
	AstExpression bor;

	if (expr1->op == OP_CONST && expr2->op == OP_CONST)
	{
		expr1->val.i[0] |= expr2->val.i[0];
		return expr1;
	}
	
	CREATE_AST_NODE(bor, Expression);

	bor->coord = expr1->coord;
	bor->ty = expr1->ty;
	bor->op = OP_BITOR;
	bor->kids[0] = expr1;
	bor->kids[1] = expr2;

	return bor;
}

static AstExpression PlaceBitField(Field fld, AstExpression expr)
{
	AstExpression lsh;
	union value val;

	if (expr->op == OP_CONST)
	{
		expr->val.i[0] <<= fld->pos;
		return expr;
	}

	CREATE_AST_NODE(lsh, Expression);

	lsh->coord = expr->coord;
	lsh->ty = expr->ty;
	lsh->op = OP_LSHIFT;
	lsh->kids[0] = expr;
	val.i[1] = 0;
	val.i[0] = fld->pos;
	lsh->kids[1] = Constant(expr->coord, T(INT), val);

	return lsh;
}

static AstInitializer CheckInitializerInternal(InitData *tail, AstInitializer init, Type ty, 
                                               int *offset, int *error)
{
	AstInitializer p;
	int size = 0;
	InitData initd;

	if (IsScalarType(ty))
	{
		p = init;
		if (init->lbrace)
		{
			Error(&init->coord, "Can't use brace-enclosed initializer list for scalar");
			*error = 1;
			p = (AstInitializer)init->initials;
		}
		
		p->expr = Adjust(CheckExpression(p->expr), 1);
		if (! CanAssign(ty, p->expr))
		{
			Error(&init->coord, "Wrong initializer");
			*error = 1;
		}
		else
		{
			p->expr = Cast(ty, p->expr);
		}
		ALLOC(initd);
		initd->offset = *offset;
		initd->expr = p->expr;
		initd->next = NULL;
		(*tail)->next = initd;
		*tail = initd;

		return (AstInitializer)init->next;
	}
	else if (ty->categ == UNION)
	{
		p = init->lbrace ? (AstInitializer)init->initials : init;
		ty = ((RecordType)ty)->flds->ty;

		p = CheckInitializerInternal(tail, p, ty, offset, error);

		if (init->lbrace)
		{
			if (p != NULL)
			{
				Error(&init->coord, "too many initializer for union");
			}
			return (AstInitializer)init->next;
		}

		return p;
	}
	else if (ty->categ == ARRAY)
	{
		int start = *offset;
		p = init->lbrace ? (AstInitializer)init->initials : init;

		if (((init->lbrace && ! p->lbrace && p->next == NULL) || ! init->lbrace) &&
		    p->expr->op == OP_STR && ty->categ / 2 == p->expr->ty->categ / 2)
		{
			size = p->expr->ty->size;
			if (ty->size == 0 || ty->size == size)
			{
				ty->size = size;
			}
			else if (ty->size == size - 1)
			{
				p->expr->ty->size = size - 1;
			}
			else if (ty->size < size)
			{
				Error(&init->coord, "string is too long");
				*error = 1;
			}
			ALLOC(initd);
			initd->offset = *offset;
			initd->expr = p->expr;
			initd->next = NULL;
			(*tail)->next = initd;
			*tail = initd;

			return (AstInitializer)init->next;
		}

		while (p != NULL)
		{
			p = CheckInitializerInternal(tail, p, ty->bty, offset, error);
			size += ty->bty->size;
			*offset = start + size;
			if (ty->size == size)
				break;
		}
		
		if (ty->size == 0)
		{
			ty->size = size;
		}
		else if (ty->size < size)
		{
			Error(&init->coord, "too many initializer");
			*error = 1;
		}
	
		if (init->lbrace)
		{
			return (AstInitializer)init->next;
		}
		return p;
	}
	else if (ty->categ == STRUCT)
	{
		int start = *offset;
		Field fld = ((RecordType)ty)->flds;
		p = init->lbrace ? (AstInitializer)init->initials : init;
		
		while (fld && p)
		{
			*offset = start + fld->offset;
			p = CheckInitializerInternal(tail, p, fld->ty, offset, error);
			if (fld->bits != 0)
			{
				(*tail)->expr = PlaceBitField(fld, (*tail)->expr);
			}
			fld = fld->next;
		}

		if (init->lbrace)
		{
			if (p != NULL)
			{
				Error(&init->coord, "too many initializer");
				*error = 1;
			}
			*offset = ty->size;
			return (AstInitializer)init->next;
		}

		return (AstInitializer)p;
	}

	return init;
}

static void CheckInitializer(AstInitializer init, Type ty)
{
	int offset = 0, error = 0;
	struct initData header;
	InitData tail = &header;
	InitData prev, curr;

	header.next = NULL;
	if (IsScalarType(ty) && init->lbrace)
	{
		init = (AstInitializer)init->initials;
	}
	else if (ty->categ == ARRAY && ! (ty->bty->categ == CHAR || ty->bty->categ == UCHAR))
	{
		if (! init->lbrace)
		{
			Error(&init->coord, "Can't initialize array without brace");
			return;
		}
	}
	else if ((ty->categ == STRUCT || ty->categ == UNION) && ! init->lbrace)
	{
		init->expr = Adjust(CheckExpression(init->expr), 1);
		if (! CanAssign(ty, init->expr))
		{
			Error(&init->coord, "Wrong initializer");
		}
		else
		{
			ALLOC(init->idata);
			init->idata->expr = init->expr;
			init->idata->offset = 0;
			init->idata->next = NULL;
		}
		return;
	}

	CheckInitializerInternal(&tail, init, ty, &offset, &error);
	if (error)
		return;

	init->idata = header.next;
	prev = NULL;
	curr = init->idata;
	while (curr)
	{
		if (prev != NULL && prev->offset == curr->offset)
		{
			prev->expr = BORBitField(prev->expr, curr->expr);
			prev->next = curr->next;
			curr = curr->next;
		}
		else
		{
			prev = curr;
			curr = curr->next;
		}
	}
}

static Type DeriveType(TypeDerivList tyDrvList, Type ty)
{
	while (tyDrvList != NULL)
	{
		if (tyDrvList->ctor == POINTER_TO)
		{
			ty = Qualify(tyDrvList->qual, PointerTo(ty));
		}
		else if (tyDrvList->ctor == ARRAY_OF)
		{
			if (ty->categ == FUNCTION || ty->size == 0 || (IsRecordType(ty) && ((RecordType)ty)->hasFlexArray))
				return NULL;
			
			ty = ArrayOf(tyDrvList->len, ty);
		}
		else
		{
			if (ty->categ == ARRAY || ty->categ == FUNCTION)
				return NULL;

			ty = FunctionReturn(ty, tyDrvList->sig);
		}

		tyDrvList = tyDrvList->next;
	}

	return ty;
}

static void AddParameter(Vector params, char *id, Type ty, int reg, Coord coord)
{
	Parameter param;

	FOR_EACH_ITEM(Parameter, param, params)
		if (param->id && param->id == id)
		{
			Error(coord, "Redefine parameter %s", id);
			return;
		}
	ENDFOR

	ALLOC(param);

	param->id = id;
	param->ty = ty;
	param->reg = reg;
	INSERT_ITEM(params, param);
}

static void CheckParameterDeclaration(AstFunctionDeclarator funcDec,
                                      AstParameterDeclaration paramDecl)
{
	char *id = NULL;
	Type ty = NULL;

	CheckDeclarationSpecifiers(paramDecl->specs);
	if (paramDecl->specs->sclass && paramDecl->specs->sclass != TK_REGISTER)
	{
		Error(&paramDecl->coord, "Invalid storage class");
	}
	ty = paramDecl->specs->ty;

	CheckDeclarator(paramDecl->dec);
	if (paramDecl->dec->id == NULL && paramDecl->dec->tyDrvList == NULL &&
	    ty->categ == VOID && LEN(funcDec->sig->params) == 0)
	{
		if (paramDecl->next || ty->qual || paramDecl->specs->sclass)
		{
			Error(&paramDecl->coord, "'void' must be the only parameter");
			paramDecl->next = NULL;
		}
		return;
	}

	ty = DeriveType(paramDecl->dec->tyDrvList, ty);
	if (ty != NULL)
		ty = AdjustParameter(ty);

	if (ty == NULL || ty->size == 0)
	{
		Error(&paramDecl->coord, "Illegal parameter type");
		return;
	}
		
	id = paramDecl->dec->id;
	if (id == NULL && funcDec->partOfDef)
	{
		Error(&paramDecl->coord, "Expect parameter name");
		return;
	}
	
	AddParameter(funcDec->sig->params, id, ty, paramDecl->specs->sclass == TK_REGISTER, &paramDecl->coord);
}

static void CheckParameterTypeList(AstFunctionDeclarator funcDec)
{
	AstParameterTypeList paramTyList = funcDec->paramTyList;
	AstParameterDeclaration paramDecl;
	
	paramDecl = (AstParameterDeclaration)paramTyList->paramDecls;
	while (paramDecl)
	{
		CheckParameterDeclaration(funcDec, paramDecl);
		paramDecl = (AstParameterDeclaration)paramDecl->next;
	}
	funcDec->sig->hasEllipse = paramTyList->ellipse;
}

static void CheckFunctionDeclarator(AstFunctionDeclarator dec)
{
	AstFunctionDeclarator funcDec = (AstFunctionDeclarator)dec;


	CheckDeclarator(funcDec->dec);

	ALLOC(funcDec->sig);
	funcDec->sig->hasProto = funcDec->paramTyList != NULL;
	funcDec->sig->hasEllipse = 0;
	funcDec->sig->params = CreateVector(4);

	if (funcDec->sig->hasProto)
	{
		CheckParameterTypeList(funcDec);
	}
	else if (funcDec->partOfDef)
	{
		char *id;
		FOR_EACH_ITEM(char*, id, funcDec->ids)
			AddParameter(funcDec->sig->params, id, NULL, 0, &funcDec->coord);
		ENDFOR 
	}
	else if (LEN(funcDec->ids))
	{
		Error(&funcDec->coord, "Identifier list should be in definition.");
	}

	ALLOC(funcDec->tyDrvList);
	funcDec->tyDrvList->ctor = FUNCTION_RETURN;
	funcDec->tyDrvList->sig  = funcDec->sig;
	funcDec->tyDrvList->next = funcDec->dec->tyDrvList;
	funcDec->id = funcDec->dec->id;
}

static void CheckArrayDeclarator(AstArrayDeclarator arrDec)
{
	CheckDeclarator(arrDec->dec);
	if (arrDec->expr)
	{
		if ((arrDec->expr = CheckConstantExpression(arrDec->expr)) == NULL)
		{
			Error(&arrDec->coord, "The size of the array must be integer constant.");
		}
	}
	
	ALLOC(arrDec->tyDrvList);
	arrDec->tyDrvList->ctor = ARRAY_OF;
	arrDec->tyDrvList->len  = arrDec->expr ? arrDec->expr->val.i[0] : 0;
	arrDec->tyDrvList->next = arrDec->dec->tyDrvList;
	arrDec->id = arrDec->dec->id;
}

static void CheckPointerDeclarator(AstPointerDeclarator ptrDec)
{
	int qual = 0;
	AstToken tok = (AstToken)ptrDec->tyQuals;

	CheckDeclarator(ptrDec->dec);
	while (tok)
	{
		qual |= tok->token == TK_CONST ? CONST : VOLATILE;
		tok = (AstToken)tok->next;
	}

	ALLOC(ptrDec->tyDrvList);
	ptrDec->tyDrvList->ctor = POINTER_TO;
	ptrDec->tyDrvList->qual = qual;
	ptrDec->tyDrvList->next = ptrDec->dec->tyDrvList;
	ptrDec->id = ptrDec->dec->id;
}

static void CheckDeclarator(AstDeclarator dec)
{
	switch (dec->kind)
	{
	case NK_NameDeclarator:
		break;

	case NK_ArrayDeclarator:
		CheckArrayDeclarator((AstArrayDeclarator)dec);
		break;

	case NK_FunctionDeclarator:
		CheckFunctionDeclarator((AstFunctionDeclarator)dec);
		break;

	case NK_PointerDeclarator:
		CheckPointerDeclarator((AstPointerDeclarator)dec);
		break;

	default:
		assert(0);
	}
}

static void CheckStructDeclarator(Type rty, AstStructDeclarator stDec, Type fty)
{
	char *id = NULL;
	int bits = 0;

	if (stDec->dec != NULL)
	{
		CheckDeclarator(stDec->dec);
		id = stDec->dec->id;
		fty = DeriveType(stDec->dec->tyDrvList, fty);
	}

	if (fty == NULL || fty->categ == FUNCTION || (fty->size == 0 && fty->categ != ARRAY))
	{
		Error(&stDec->coord, "illegal type");
		return;
	}
	if (((RecordType)rty)->hasFlexArray)
	{
		Error(&stDec->coord, "the flexible array must be the last member");
		return;
	}
	if (IsRecordType(fty) && ((RecordType)fty)->hasFlexArray)
	{
		Error(&stDec->coord, "A structure has flexible array shall not be a member");
		return;
	}
	if (id && LookupField(rty, id))
	{
		Error(&stDec->coord, "member redefinition");
		return;
	}

	if (stDec->expr)
	{
		stDec->expr = CheckConstantExpression(stDec->expr);
		if (stDec->expr == NULL)
		{
			Error(&stDec->coord, "The width of bit field should be integer constant.");
		}
		else if (id && stDec->expr->val.i[0] == 0)
		{
			Error(&stDec->coord, "bit field's width should not be 0");
		}
		if (fty->categ != INT && fty->categ != UINT)
		{
			Error(&stDec->coord, "Bit field must be integer type.");
			fty = T(INT);
		}

		bits = stDec->expr ? stDec->expr->val.i[0] : 0;
		if (bits > T(INT)->size * 8)
		{
			Error(&stDec->coord, "Bit field's width exceeds");
			bits = 0;
		}
	}
	AddField(rty, id, fty, bits);
}

static void CheckStructDeclaration(AstStructDeclaration stDecl, Type rty)
{
	AstStructDeclarator stDec;

	CheckDeclarationSpecifiers(stDecl->specs);
	stDec = (AstStructDeclarator)stDecl->stDecs;
	if (stDec == NULL)
	{
		//anonymous struct/union
		AddField(rty, NULL, stDecl->specs->ty, 0);
	}

	while (stDec)
	{
		CheckStructDeclarator(rty, stDec, stDecl->specs->ty);
		stDec = (AstStructDeclarator)stDec->next;
	}  
}

static Type CheckStructOrUnionSpecifier(AstStructSpecifier stSpec)
{
	int categ = (stSpec->kind == NK_StructSpecifier) ? STRUCT : UNION;
	Symbol tag;
	Type ty;
	AstStructDeclaration stDecl;

	if (stSpec->id != NULL && stSpec->stDecls == NULL)
	{
		tag = LookupTag(stSpec->id);
		if (tag == NULL)
		{
			ty = StartRecord(stSpec->id, categ);
			tag = AddTag(stSpec->id, ty);
		}
		else if (tag->ty->categ != categ)
		{
			Error(&stSpec->coord, "Inconsistent tag declaration.");
		}

		return tag->ty;
	}
	else if (stSpec->id == NULL && stSpec->stDecls != NULL)
	{
		ty = StartRecord(NULL, categ);
		goto chk_decls;
	}
	else if (stSpec->id != NULL && stSpec->stDecls != NULL)
	{
		tag = LookupTag(stSpec->id);
		if (tag == NULL || tag->level < Level)
		{
			ty = StartRecord(stSpec->id, categ);
			AddTag(stSpec->id, ty);
		}
		else if (tag->ty->categ == categ && tag->ty->size == 0)
		{
			ty = tag->ty;
		}
		else
		{
			Error(&stSpec->coord, "Tag redefinition.");
			return tag->ty;
		}
		goto chk_decls;
	}
	else
	{
		ty = StartRecord(NULL, categ);
		EndRecord(ty);
		return ty;
	}

chk_decls:
	stDecl = (AstStructDeclaration)stSpec->stDecls;

	while (stDecl)
	{
		CheckStructDeclaration(stDecl, ty);
		stDecl = (AstStructDeclaration)stDecl->next;
	}
	EndRecord(ty);

	return ty;
}

static int CheckEnumerator(AstEnumerator enumer, int last, Type ty)
{
	if (enumer->expr == NULL)
	{
		AddEnumConstant(enumer->id, ty, last + 1);
		return last + 1;
	}
	else
	{
		enumer->expr = CheckConstantExpression(enumer->expr);
		if (enumer->expr == NULL)
		{
			Error(&enumer->coord, "The enumerator value must be integer constant.");
			AddEnumConstant(enumer->id, ty, last + 1);
			return last + 1;
		}
		AddEnumConstant(enumer->id, ty, enumer->expr->val.i[0]);

		return enumer->expr->val.i[0];
	}
}

static Type CheckEnumSpecifier(AstEnumSpecifier enumSpec)
{
	AstEnumerator enumer;
	Symbol tag;
	Type ty;
	int last;

	if (enumSpec->id == NULL && enumSpec->enumers == NULL)
		return T(INT);

	if (enumSpec->id != NULL && enumSpec->enumers == NULL)
	{
		tag = LookupTag(enumSpec->id);
		if (tag == NULL || (tag->level == Level && tag->ty->categ != ENUM))
		{
			Error(&enumSpec->coord, "Undeclared enum type.");
			return T(INT);
		}
		return tag->ty;
	}
	else if (enumSpec->id == NULL && enumSpec->enumers != NULL)
	{
		ty = T(INT);
		goto chk_enumer;
	}
	else
	{
		tag = LookupTag(enumSpec->id);
		if (tag == NULL || tag->level < Level)
		{
			tag = AddTag(enumSpec->id, Enum(enumSpec->id));
		}
		else if (tag->ty->categ != ENUM)
		{
			Error(&enumSpec->coord, "Tag redefinition.");
			return T(INT);
		}

		ty = tag->ty;
		goto chk_enumer;
	}

chk_enumer:
	enumer = (AstEnumerator)enumSpec->enumers;
	last = -1;
	while (enumer)
	{
		last = CheckEnumerator(enumer, last, ty);
		enumer = (AstEnumerator)enumer->next;
	}

	return ty;
}

static void CheckDeclarationSpecifiers(AstSpecifiers specs)
{
	AstToken tok;
	AstNode p;
	Type ty;
	int size = 0, sign = 0;
	int signCnt = 0, sizeCnt = 0, tyCnt = 0;
	int qual = 0;

	tok = (AstToken)specs->stgClasses;
	if (tok)
	{
		if (tok->next)
		{
			Error(&specs->coord, "At most one storage class");
		}
		specs->sclass = tok->token;
	}

	tok = (AstToken)specs->tyQuals;
	while (tok)
	{
		qual |= (tok->token == TK_CONST ? CONST : VOLATILE);
		tok = (AstToken)tok->next;
	}

	p = specs->tySpecs;
	while (p)
	{
		if (p->kind == NK_StructSpecifier || p->kind == NK_UnionSpecifier)
		{
			ty = CheckStructOrUnionSpecifier((AstStructSpecifier)p);
			tyCnt++;
		}
		else if (p->kind == NK_EnumSpecifier)
		{
			ty = CheckEnumSpecifier((AstEnumSpecifier)p);
			tyCnt++;
		}
		else if (p->kind == NK_TypedefName)
		{
			Symbol sym = LookupID(((AstTypedefName)p)->id);

			assert(sym->kind == SK_TypedefName);
			ty = sym->ty;
			tyCnt++;
		}
		else 
		{
			tok = (AstToken)p;

			switch (tok->token)
			{
			case TK_SIGNED:
			case TK_UNSIGNED:
				sign = tok->token;
				signCnt++;
				break;

			case TK_SHORT:
			case TK_LONG:
				if (size == TK_LONG && sizeCnt == 1)
				{
					size = TK_LONG + TK_LONG;
				}
				else
				{
					size = tok->token;
					sizeCnt++;
				}
				break;

			case TK_CHAR:
				ty = T(CHAR);
				tyCnt++;
				break;

			case TK_INT:
				ty = T(INT);
				tyCnt++;
				break;

			case TK_INT64:
				ty = T(INT);
				size = TK_LONG + TK_LONG;
				sizeCnt++;
				break;

			case TK_FLOAT:
				ty = T(FLOAT);
				tyCnt++;
				break;

			case TK_DOUBLE:
				ty = T(DOUBLE);
				tyCnt++;
				break;

			case TK_VOID:
				ty = T(VOID);
				tyCnt++;
				break;
			}
		}
		p = p->next;
	}

	ty = tyCnt == 0 ? T(INT) : ty;

	if (sizeCnt > 1 || signCnt > 1 || tyCnt > 1)
		goto err;

	if (ty == T(DOUBLE) && size == TK_LONG)
	{
		ty = T(LONGDOUBLE);
		size = 0;
	}
	else if (ty == T(CHAR))
	{
		sign = (sign == TK_UNSIGNED);
		ty = T(CHAR + sign);
		sign = 0;
	}

	if (ty == T(INT))
	{
		sign = (sign == TK_UNSIGNED);

		switch (size)
		{
		case TK_SHORT:
			ty = T(SHORT + sign);
			break;

		case TK_LONG:
			ty = T(LONG + sign);
			break;

		case TK_LONG + TK_LONG:
			ty = T(LONGLONG + sign);
			break;

		default:
			assert(size == 0);
			ty = T(INT + sign);
			break;
		}
		
	}
	else if (size != 0 || sign != 0)
	{
		goto err;
	}

	specs->ty = Qualify(qual, ty);
	return;

err:
	Error(&specs->coord, "Illegal type specifier.");
	specs->ty = T(INT);
	return;
}

static void CheckTypedef(AstDeclaration decl)
{
	AstInitDeclarator initDec;
	Type ty;
	Symbol sym;

	initDec = (AstInitDeclarator)decl->initDecs;
	while (initDec)
	{
		CheckDeclarator(initDec->dec);
		if (initDec->dec->id == NULL)
			goto next;

		ty = DeriveType(initDec->dec->tyDrvList, decl->specs->ty);
		if (ty == NULL)
		{
			Error(&initDec->coord, "Illegal type");
			ty = T(INT);
		}

		if (initDec->init)
		{
			Error(&initDec->coord, "Can't initialize typedef name");
		}

		sym = LookupID(initDec->dec->id);
		if (sym && sym->level == Level && (sym->kind != SK_TypedefName || ! IsCompatibleType(ty, sym->ty)))
		{
			Error(&initDec->coord, "Redeclaration of %s", initDec->dec->id);
		}
		else
		{
			AddTypedefName(initDec->dec->id, ty);
		}
next:
		initDec = (AstInitDeclarator)initDec->next;
	}
}

Type CheckTypeName(AstTypeName tname)
{
	Type ty;

	CheckDeclarationSpecifiers(tname->specs);
	CheckDeclarator(tname->dec);
	ty = DeriveType(tname->dec->tyDrvList, tname->specs->ty);
	if (ty == NULL)
	{
		Error(&tname->coord, "Illegal type");
		ty = T(INT);
	}
	return ty;
}

void CheckLocalDeclaration(AstDeclaration decl, Vector v)
{
	AstInitDeclarator initDec;
	Type ty;
	int sclass;
	Symbol sym;

	CheckDeclarationSpecifiers(decl->specs);
	if (decl->specs->sclass == TK_TYPEDEF)
	{
		CheckTypedef(decl);
		return;
	}
	ty = decl->specs->ty;
	sclass = decl->specs->sclass;
	if (sclass == 0)
	{
		sclass = TK_AUTO;
	}

	initDec = (AstInitDeclarator)decl->initDecs;
	while (initDec)
	{
		CheckDeclarator(initDec->dec);
		if (initDec->dec->id == NULL)
			goto next;

		ty = DeriveType(initDec->dec->tyDrvList, decl->specs->ty);
		if (ty == NULL)
		{
			Error(&initDec->coord, "Illegal type");
			ty = T(INT);
		}

		if (initDec->dec->kind == NK_NameDeclarator && IsFunctionType(ty))
		{
			ty = PointerTo(ty);
		}

		if (IsFunctionType(ty))
		{
			if (sclass == TK_STATIC)
			{
				Error(&decl->coord, "can't specify static for block-scope function");
			}
			if (initDec->init != NULL)
			{
				Error(&initDec->coord, "please don't initialize function");
			}

			if ((sym = LookupID(initDec->dec->id)) == NULL)
			{
				sym = AddFunction(initDec->dec->id, ty, TK_EXTERN);
			}
			else if (! IsCompatibleType(sym->ty, ty))
			{
				Error(&decl->coord, "Incompatible with previous declaration");
			}
			else
			{
				sym->ty = CompositeType(sym->ty, ty);
			}

			goto next;
		}

		if (sclass == TK_EXTERN && initDec->init != NULL)
		{
			Error(&initDec->coord, "can't initialize extern variable");
			initDec->init = NULL;
		}

		if ((sym = LookupID(initDec->dec->id)) == NULL || sym->level != Level)
		{
			VariableSymbol vsym;

			vsym = (VariableSymbol)AddVariable(initDec->dec->id, ty, sclass);
			if (initDec->init)
			{
				CheckInitializer(initDec->init, ty);
				if (sclass == TK_STATIC)
				{
					CheckInitConstant(initDec->init);
				}
				else
				{
					INSERT_ITEM(v, vsym);
				}
				vsym->idata = initDec->init->idata;
			}
		}
		else if (! (sym->sclass == TK_EXTERN && sclass == TK_EXTERN && IsCompatibleType(sym->ty, ty)))
		{
			Error(&decl->coord, "Variable redefinition");
		}
next:
		initDec = (AstInitDeclarator)initDec->next;
	}
}

static void CheckGlobalDeclaration(AstDeclaration decl)
{
	AstInitDeclarator initDec;
	Type ty;
	Symbol sym;
	int sclass;

	CheckDeclarationSpecifiers(decl->specs);
	if (decl->specs->sclass == TK_TYPEDEF)
	{
		CheckTypedef(decl);
		return;
	}

	ty = decl->specs->ty;
	sclass = decl->specs->sclass;
	if (sclass == TK_REGISTER || sclass == TK_AUTO)
	{
		Error(&decl->coord, "Invalid storage class");
		sclass = TK_EXTERN;
	}

	initDec = (AstInitDeclarator)decl->initDecs;
	while (initDec)
	{
		CheckDeclarator(initDec->dec);
		if (initDec->dec->id == NULL)
			goto next;

		ty = DeriveType(initDec->dec->tyDrvList, decl->specs->ty);
		if (ty == NULL)
		{
			Error(&initDec->coord, "Illegal type");
			ty = T(INT);
		}
		if (initDec->dec->kind == NK_NameDeclarator && IsFunctionType(ty))
		{
			ty = PointerTo(ty);
		}

		if (IsFunctionType(ty))
		{
			if (initDec->init)
			{
				Error(&initDec->coord, "please don't initalize function");
			}

			if ((sym = LookupID(initDec->dec->id)) == NULL)
			{
				sym = AddFunction(initDec->dec->id, ty, sclass == 0 ? TK_EXTERN : sclass);
			}
			else
			{
				if (sym->sclass == TK_EXTERN && sclass == TK_STATIC)
				{
					Error(&decl->coord, "Conflict linkage of function %s", initDec->dec->id);
				}

				if (! IsCompatibleType(ty, sym->ty))
				{
					Error(&decl->coord, "Incomptabile with previous declaration");
				}
				else
				{
					sym->ty = CompositeType(ty, sym->ty);
				}
			}
			goto next;
		}

		if ((sym = LookupID(initDec->dec->id)) == NULL)
		{
			sym = AddVariable(initDec->dec->id, ty, sclass);
		}

		if (initDec->init)
		{
			CheckInitializer(initDec->init, ty);
			CheckInitConstant(initDec->init);
		}

		if (ty->size == 0 && ! (sclass != TK_STATIC && ty->categ == ARRAY) && 
			! (sclass == TK_EXTERN && IsRecordType(ty)))
		{
			Error(&initDec->coord, "Unknown size of %s", initDec->dec->id);
			ty = T(INT);
		}

		
		sclass = sclass == TK_EXTERN ? sym->sclass : sclass;

		if ((sclass == 0 && sym->sclass == TK_STATIC) || (sclass != 0 && sym->sclass != sclass))
		{
			Error(&decl->coord, "Conflict linkage of %s", initDec->dec->id);
		}
		if (sym->sclass == TK_EXTERN)
			sym->sclass = sclass;

		if (! IsCompatibleType(ty, sym->ty))
		{
			Error(&decl->coord, "Incompatible with previous definition", initDec->dec->id);
			goto next;
		}
		else
		{
			sym->ty = CompositeType(sym->ty, ty);
		}

		if (initDec->init)
		{
			if (sym->defined)
			{
				Error(&initDec->coord, "redefinition of %s", initDec->dec->id);
			}
			else
			{
				AsVar(sym)->idata = initDec->init->idata;
				sym->defined = 1;
			}

		}
next:
		initDec = (AstInitDeclarator)initDec->next;
	}
}

static void CheckIDDeclaration(AstFunctionDeclarator funcDec, AstDeclaration decl)
{
	Type ty, bty;
	int sclass;
	AstInitDeclarator initDec;
	Parameter param;
	Vector params = funcDec->sig->params;

	CheckDeclarationSpecifiers(decl->specs);
	sclass = decl->specs->sclass;
	bty = decl->specs->ty;
	if (! (sclass == 0 || sclass == TK_REGISTER))
	{
		Error(&decl->coord, "Invalid storage class");
		sclass = 0;
	}

	initDec = (AstInitDeclarator)decl->initDecs;
	while (initDec)
	{
		if (initDec->init)
		{
			Error(&initDec->coord, "Parameter can't be initialized");
		}
		CheckDeclarator(initDec->dec);
		if (initDec->dec->id == NULL)
			goto next_dec;

		FOR_EACH_ITEM(Parameter, param, params)
			if (param->id == initDec->dec->id)
				goto ok;
		ENDFOR
		Error(&initDec->coord, "%s is not from the identifier list", initDec->dec->id);
		goto next_dec;

ok:
		ty = DeriveType(initDec->dec->tyDrvList, bty);
		if (ty == NULL)
		{
			Error(&initDec->coord, "Illegal type");
			ty = T(INT);
		}
		if (param->ty == NULL)
		{
			param->ty = ty;
			param->reg = sclass == TK_REGISTER;
		}
		else
		{
			Error(&initDec->coord, "Redefine parameter %s", param->id);
		}
next_dec:
		initDec = (AstInitDeclarator)initDec->next;
	}
}

void CheckFunction(AstFunction func)
{
	Symbol sym;
	Type ty;
	int sclass;
	Label label;
	AstNode p;
	Vector params;
	Parameter param;
	
	func->fdec->partOfDef = 1;

	CheckDeclarationSpecifiers(func->specs);
	if ((sclass = func->specs->sclass) == 0)
	{
		sclass = TK_EXTERN;
	}

	CheckDeclarator(func->dec);

	p = func->decls;
	while (p)
	{
		CheckIDDeclaration(func->fdec, (AstDeclaration)p);
		p = p->next;
	}

	params = func->fdec->sig->params;
	FOR_EACH_ITEM(Parameter, param, params)
		if (param->ty == NULL)
			param->ty = T(INT);
	ENDFOR
		
	ty = DeriveType(func->dec->tyDrvList, func->specs->ty);
	if (ty == NULL)
	{
		Error(&func->coord, "Illegal function type");
		ty = DefaultFunctionType;
	}

	sym = LookupID(func->dec->id);
	if (sym == NULL)
	{
		func->fsym = (FunctionSymbol)AddFunction(func->dec->id, ty, sclass);
	}
	else if (sym->ty->categ != FUNCTION)
	{
		Error(&func->coord, "Redeclaration as a function");
		func->fsym = (FunctionSymbol)AddFunction(func->dec->id, ty, sclass);
	}
	else
	{
		func->fsym = (FunctionSymbol)sym;
		if (sym->sclass == TK_EXTERN && sclass == TK_STATIC)
		{
			Error(&func->coord, "Conflict function linkage");
		}
		if (! IsCompatibleType(ty, sym->ty))
		{
			Error(&func->coord, "Incompatible with previous declaration");
			sym->ty = ty;
		}
		else
		{
			sym->ty = CompositeType(ty, sym->ty);
		}
		if (func->fsym->defined)
		{
			Error(&func->coord, "Function redefinition");
		}
	}
	func->fsym->defined = 1;
	func->loops = CreateVector(4);
	func->breakable = CreateVector(4);
	func->swtches = CreateVector(4);

	CURRENTF = func;
	FSYM = func->fsym;
	EnterScope();
	{
		Vector v= ((FunctionType)ty)->sig->params;
		
		FOR_EACH_ITEM(Parameter, param, v)
			AddVariable(param->id, param->ty, param->reg ? TK_REGISTER : TK_AUTO);
		ENDFOR

		FSYM->locals = NULL;
		FSYM->lastv = &FSYM->locals;
	}
	CheckCompoundStatement(func->stmt);
	ExitScope();

	label = func->labels;
	while (label)
	{
		if (! label->defined)
		{
			Error(&label->coord, "Undefined label");
		}
		label = label->next;
	}
	if (ty->bty != T(VOID) && ! func->hasReturn)
	{
		Warning(&func->coord, "missing return value");
	}
}

void CheckTranslationUnit(AstTranslationUnit transUnit)
{
	AstNode p;
	Symbol f;

	p = transUnit->extDecls;
	while (p)
	{
		if (p->kind == NK_Function)
		{
			CheckFunction((AstFunction)p);
		}
		else
		{
			assert(p->kind == NK_Declaration);
			CheckGlobalDeclaration((AstDeclaration)p);
		}
		p = p->next;
	}

	f = Functions;
	while (f != NULL)
	{
		if (f->sclass == TK_STATIC && ! f->defined)
		{
			Error(NULL, "static function %s not defined", f->name);
		}
		f = f->next;
	}
}



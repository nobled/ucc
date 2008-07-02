#include "ucl.h"
#include "grammer.h"
#include "ast.h"
#include "decl.h"


static int FIRST_StructDeclaration[]   = { FIRST_DECLARATION, 0};
static int FF_StructDeclaration[]      = { FIRST_DECLARATION, TK_RBRACE, 0};
static int FIRST_Function[]            = { FIRST_DECLARATION, TK_LBRACE, 0};
static int FIRST_ExternalDeclaration[] = { FIRST_DECLARATION, TK_MUL, TK_LPAREN, 0};
static Vector TypedefNames, OverloadNames;

int FIRST_Declaration[] = { FIRST_DECLARATION, 0};

static AstDeclarator ParseDeclarator(int abstract);
static AstSpecifiers ParseDeclarationSpecifiers(void);

/**
 * Get the identifier declared by the declarator dec
 */
static char* GetOutermostID(AstDeclarator dec)
{
	if (dec->kind == NK_NameDeclarator)
		return dec->id;

	return GetOutermostID(dec->dec);
}

/**
 * UCC divides the syntax parsing and semantic check into two seperate passes.
 * But during syntax parsing, the parser needs to recognize C's typedef name.
 * In order to handle this, the parser performs minimum semantic check to 
 * process typedef name. For the parser, it is enough to know if an identifier 
 * is a typedef name.
 * 
 * The parser uses struct tdName to manage a typedef name. 
 * id: typedef name id
 * level: if two typedef names' id are same, the level will be the level of the 
 * typedef name who is in the outer scope. e.g.
 * typedef int a;
 * int f(void)
 * {
 *     typedef int a;
 * }
 * the a's level will be 0 instead of 1.
 * overload: A typedef name maybe used as variable in inner scope, e.g.
 * typedef int a;
 * int f(int a)
 * {
 * }
 * In f(), a is used as variable instead of typedef name.
 * For these cases, overload is set. When the scope terminates, overload 
 * is reset.
 *
 * The parser maintains two vectors: TypedefNames and OverloadNames
 * Whenever encountering a declaration, if it is a typedef name, records
 * it in the TypedefNames; else if the declaration redefines a typedef name
 * in outer scope as a variable, records it in the OverloadNames.
 * TypedefNames is for all the scope, while OverloadNames is for current scope,
 * when current scope terminates, reset the overload flags of all the item in
 * OverloadNames.
 */

/**
 * Decide if id is a typedef name
 */
static int IsTypedefName(char *id)
{
	Vector v = TypedefNames;
	TDName tn;

	FOR_EACH_ITEM(TDName, tn, v)
		if (tn->id == id && tn->level <= Level && ! tn->overload)
			return 1;
	ENDFOR

	return 0;
}

/**
 * If sclass is TK_TYPEDEF, records it in TypedefNames.
 * Otherwise, if id redefines a typedef name in outer scope,
 * records the typedef name in OverloadNames.
 */
static void CheckTypedefName(int sclass, char *id)
{
	Vector v;
	TDName tn;

	if (id == NULL)
		return;

	v = TypedefNames;
	if (sclass == TK_TYPEDEF)
	{
		FOR_EACH_ITEM(TDName, tn, v)
			if(tn->id == id)
			{
				if (Level < tn->level)
					tn->level = Level;
				return;
			}
		ENDFOR

		ALLOC(tn);
		tn->id = id;
		tn->level = Level;
		tn->overload = 0;
		INSERT_ITEM(v, tn);
	}
	else
	{
		FOR_EACH_ITEM(TDName, tn, v)
			if (tn->id == id && Level > tn->level)
			{
				tn->overload = 1;
				INSERT_ITEM(OverloadNames, tn);
				return;
			}
		ENDFOR
	}
}

/**
 * Perform minimum semantic check for each declaration 
 */
static void PreCheckTypedef(AstDeclaration decl)
{
	AstNode p;
	int sclass = 0;

	if (decl->specs->stgClasses != NULL)
	{
		sclass = ((AstToken)decl->specs->stgClasses)->token;
	}

	p = decl->initDecs;
	while (p != NULL)
	{
		CheckTypedefName(sclass, GetOutermostID(((AstInitDeclarator)p)->dec));
		p = p->next;
	}
}

/**
 * When current scope except file scope terminates, clear OverloadNames.
 */
void PostCheckTypedef(void)
{
	TDName tn;

	FOR_EACH_ITEM(TDName, tn, OverloadNames)
		tn->overload = 0;
	ENDFOR

	OverloadNames->len = 0;
}

/**
 *  initializer:
 *		assignment-expression
 *		{ initializer-list }
 *		{ initializer-list , }
 *
 *  initializer-list:
 *		initializer
 *		initializer-list, initializer
 */
static AstInitializer ParseInitializer(void)
{
	AstInitializer init;
	AstNode *tail;

	CREATE_AST_NODE(init, Initializer);

	if (CurrentToken == TK_LBRACE)
	{
		init->lbrace = 1;
		NEXT_TOKEN;
		init->initials = (AstNode)ParseInitializer();
		tail = &init->initials->next;
		while (CurrentToken == TK_COMMA)
		{
			NEXT_TOKEN;
			if (CurrentToken == TK_RBRACE)
				break;
			*tail = (AstNode)ParseInitializer();
			tail = &(*tail)->next;
		}
		Expect(TK_RBRACE);
	}
	else
	{
		init->lbrace = 0;
		init->expr = ParseAssignmentExpression();
	}

	return init;
}

/**
 *  init-declarator:
 *		declarator
 *      declarator = initializer
 */
static AstInitDeclarator ParseInitDeclarator(void)
{
	AstInitDeclarator initDec;

	CREATE_AST_NODE(initDec, InitDeclarator);

	initDec->dec = ParseDeclarator(DEC_CONCRETE);
	if (CurrentToken == TK_ASSIGN)
	{
		NEXT_TOKEN;
		initDec->init = ParseInitializer();
	}

	return initDec;
}

/**
 *  direct-declarator:
 *		ID
 *		( declarator )
 *
 *  direct-abstract-declarator:
 *		( abstract-declarator)
 *		nil
 */
static AstDeclarator ParseDirectDeclarator(int kind)
{
	AstDeclarator dec;

	if (CurrentToken == TK_LPAREN)
	{
		NEXT_TOKEN;
		dec = ParseDeclarator(kind);
		Expect(TK_RPAREN);

		return dec;
	}

	CREATE_AST_NODE(dec, NameDeclarator);

	if (CurrentToken == TK_ID)
	{
		if (kind == DEC_ABSTRACT)
		{
			Error(&TokenCoord, "Identifier is not permitted in the abstract declarator");
		}

		dec->id = TokenValue.p;
		NEXT_TOKEN;
	}
	else if (kind == DEC_CONCRETE)
	{
		Error(&TokenCoord, "Expect identifier");
	}

	return dec;
}

/**
 *  parameter-declaration:
 *		declaration-specifiers declarator
 *		declaration-specifiers abstract-declarator
 */
static AstParameterDeclaration ParseParameterDeclaration(void)
{
	AstParameterDeclaration paramDecl;

	CREATE_AST_NODE(paramDecl, ParameterDeclaration);

	paramDecl->specs = ParseDeclarationSpecifiers();
	paramDecl->dec   = ParseDeclarator(DEC_ABSTRACT | DEC_CONCRETE);

	return paramDecl;
}

/**
 *  parameter-type-list:
 *		parameter-list
 *		parameter-list , ...
 *
 *  parameter-list:
 *		parameter-declaration
 *		parameter-list , parameter-declaration
 *
 *  parameter-declaration:
 *		declaration-specifiers declarator
 *		declaration-specifiers abstract-declarator
 */
AstParameterTypeList ParseParameterTypeList(void)
{
	AstParameterTypeList paramTyList;
	AstNode *tail;

	CREATE_AST_NODE(paramTyList, ParameterTypeList);

	paramTyList->paramDecls = (AstNode)ParseParameterDeclaration();
	tail = &paramTyList->paramDecls->next;
	while (CurrentToken == TK_COMMA)
	{
		NEXT_TOKEN;
		if (CurrentToken == TK_ELLIPSE)
		{
			paramTyList->ellipse = 1;
			NEXT_TOKEN;
			break;
		}
		*tail = (AstNode)ParseParameterDeclaration();
		tail = &(*tail)->next;
	}

	return paramTyList;
}

/**
 *  postfix-declarator:
 *		direct-declarator
 *		postfix-declarator [ [constant-expression] ]
 *		postfix-declarator ( parameter-type-list)
 *		postfix-declarator ( [identifier-list] )
 *
 *  postfix-abstract-declarator:
 *		direct-abstract-declarator
 *		postfix-abstract-declarator ( [parameter-type-list] )
 *		postfix-abstract-declarator [ [constant-expression] ]
 */
static AstDeclarator ParsePostfixDeclarator(int kind)
{
	AstDeclarator dec = ParseDirectDeclarator(kind);

	while (1)
	{
		if (CurrentToken == TK_LBRACKET)
		{
			AstArrayDeclarator arrDec;

			CREATE_AST_NODE(arrDec, ArrayDeclarator);
			arrDec->dec = dec;

			NEXT_TOKEN;
			if (CurrentToken != TK_RBRACKET)
			{
				arrDec->expr = ParseConstantExpression();
			}
			Expect(TK_RBRACKET);

			dec = (AstDeclarator)arrDec;
		}
		else if (CurrentToken == TK_LPAREN)
		{
			/// notice: for abstract declarator, identifier list is not permitted,
			/// but we leave this to semantic check
			AstFunctionDeclarator funcDec;

			CREATE_AST_NODE(funcDec, FunctionDeclarator);
			funcDec->dec = dec;
			
			NEXT_TOKEN;
			if (IsTypeName(CurrentToken))
			{
				funcDec->paramTyList = ParseParameterTypeList();
			}
			else
			{
				funcDec->ids = CreateVector(4);
				if (CurrentToken == TK_ID)	
				{
					INSERT_ITEM(funcDec->ids, TokenValue.p);

					NEXT_TOKEN;
					while (CurrentToken == TK_COMMA)
					{
						NEXT_TOKEN;
						if (CurrentToken == TK_ID)
						{
							INSERT_ITEM(funcDec->ids, TokenValue.p);
						}
						Expect(TK_ID);
					}
				}
			}
			Expect(TK_RPAREN);
			dec = (AstDeclarator)funcDec;
		}
		else
		{
			return dec;
		}
	}
}

/**
 *  abstract-declarator:
 *		pointer
 *		[pointer] direct-abstract-declarator
 *
 *  direct-abstract-declarator:
 *		( abstract-declarator )
 *		[direct-abstract-declarator] [ [constant-expression] ]
 *		[direct-abstract-declarator] ( [parameter-type-list] )
 *
 *  declarator:
 *		pointer declarator
 *		direct-declarator
 *
 *  direct-declarator:
 *		ID
 *		( declarator )
 *		direct-declarator [ [constant-expression] ]
 *		direct-declarator ( parameter-type-list )
 *		direct-declarator ( [identifier-list] )
 *
 *  pointer:
 *		* [type-qualifier-list]
 *		* [type-qualifier-list] pointer
 *
 *  We change the above standard grammar into more suitable form defined below.
 *  abstract-declarator:
 *		* [type-qualifer-list] abstract-declarator
 *		postfix-abstract-declarator
 *	
 *  postfix-abstract-declarator:
 *		direct-abstract-declarator
 *		postfix-abstract-declarator [ [constant-expression] ]
 *		postfix-abstrace-declarator( [parameter-type-list] )
 *		
 *  direct-abstract-declarator:
 *		( abstract-declarator )
 *		NULL
 *
 *  declarator:
 *		* [type-qualifier-list] declarator
 *		postfix-declarator
 *
 *  postfix-declarator:
 *		direct-declarator
 *		postfix-declarator [ [constant-expression] ]
 *		postfix-declarator ( parameter-type-list)
 *		postfix-declarator ( [identifier-list] )
 *
 *  direct-declarator:
 *		ID
 *		( declarator )
 *
 *	The declartor is similar as the abstract declarator, we use one function
 *	ParseDeclarator() to parse both of them. kind indicate to parse which kind
 *	of declarator. The possible value can be:
 *	DEC_CONCRETE: parse a declarator
 *	DEC_ABSTRACT: parse an abstract declarator
 *	DEC_CONCRETE | DEC_ABSTRACT: both of them are ok
 */
static AstDeclarator ParseDeclarator(int kind)
{
	if (CurrentToken == TK_MUL)
	{
		AstPointerDeclarator ptrDec;
		AstToken tok;
		AstNode *tail;

		CREATE_AST_NODE(ptrDec, PointerDeclarator);
		tail = &ptrDec->tyQuals;

		NEXT_TOKEN;
		while (CurrentToken == TK_CONST || CurrentToken == TK_VOLATILE)
		{
			CREATE_AST_NODE(tok, Token);
			tok->token = CurrentToken;
			*tail = (AstNode)tok;
			tail = &tok->next;
			NEXT_TOKEN;
		}
		ptrDec->dec = ParseDeclarator(kind);

		return (AstDeclarator)ptrDec;
	}

	return ParsePostfixDeclarator(kind);
}

/**
 *  struct-declarator:
 *		declarator
 *		[declarator] : constant-expression
 */
static AstStructDeclarator ParseStructDeclarator(void)
{
	AstStructDeclarator stDec;

	CREATE_AST_NODE(stDec, StructDeclarator);

	if (CurrentToken != TK_COLON)
	{
		stDec->dec = ParseDeclarator(DEC_CONCRETE);
	}
	if (CurrentToken == TK_COLON)
	{
		NEXT_TOKEN;
		stDec->expr = ParseConstantExpression();
	}

	return stDec;
}

/**
 *  struct-declaration:
 *		specifier-qualifer-list struct-declarator-list ;
 *
 *  specifier-qualifier-list:
 *		type-specifier [specifier-qualifier-list]
 *		type-qualifier [specifier-qualifier-list]
 *
 *  struct-declarator-list:
 *		struct-declarator
 *		struct-declarator-list , struct-declarator
 */
static AstStructDeclaration ParseStructDeclaration(void)
{
	AstStructDeclaration stDecl;
	AstNode *tail;

	CREATE_AST_NODE(stDecl, StructDeclaration);

	/// declaration specifiers is a superset of speicifier qualifier list,
	/// for simplicity, just use ParseDeclarationSpecifiers() and check if
	/// there is storage class
	stDecl->specs = ParseDeclarationSpecifiers();
	if (stDecl->specs->stgClasses != NULL)
	{
		Error(&stDecl->coord, "Struct/union member should not have storage class");
		stDecl->specs->stgClasses = NULL;
	}
	if (stDecl->specs->tyQuals == NULL && stDecl->specs->tySpecs == NULL)
	{
		Error(&stDecl->coord, "Expect type specifier or qualifier");
	}

	// an extension to C89, supports anonymous struct/union member in struct/union
	if (CurrentToken == TK_SEMICOLON)
	{
		NEXT_TOKEN;
		return stDecl;
	}

	stDecl->stDecs = (AstNode)ParseStructDeclarator();
	tail = &stDecl->stDecs->next;
	while (CurrentToken == TK_COMMA)
	{
		NEXT_TOKEN;
		*tail = (AstNode)ParseStructDeclarator();
		tail = &(*tail)->next;
	}
	Expect(TK_SEMICOLON);

	return stDecl;
}

/**
 *  struct-or-union-specifier:
 *		struct-or-union [identifier] { struct-declaration-list }
 *		struct-or-union identifier
 *
 *  struct-or-union:
 *		struct
 *		union
 *
 *  struct-declaration-list:
 *      struct-declaration
 *		struct-declaration-list struct-declaration
 */
static AstStructSpecifier ParseStructOrUnionSpecifier(void)
{
	AstStructSpecifier stSpec;
	AstNode *tail;

	CREATE_AST_NODE(stSpec, StructSpecifier);
	if (CurrentToken == TK_UNION)
	{
		stSpec->kind = NK_UnionSpecifier;
	}

	NEXT_TOKEN;
	switch (CurrentToken)
	{
	case TK_ID:
		stSpec->id = TokenValue.p;
		NEXT_TOKEN;
		if (CurrentToken == TK_LBRACE)
			goto lbrace;

		return stSpec;

	case TK_LBRACE:
lbrace:
		NEXT_TOKEN;
		if (CurrentToken == TK_RBRACE)
		{
			NEXT_TOKEN;
			return stSpec;
		}

		// FIXME: use a better way to handle errors in struct declaration list
		tail = &stSpec->stDecls;
		while (CurrentTokenIn(FIRST_StructDeclaration))
		{
			*tail = (AstNode)ParseStructDeclaration();
			tail = &(*tail)->next;
			SkipTo(FF_StructDeclaration, "the start of struct declaration or }");
		}
		Expect(TK_RBRACE);
		return stSpec;

	default:
		Error(&TokenCoord, "Expect identifier or { after struct/union");
		return stSpec;
	}
}

/**
 *  enumerator:
 *		enumeration-constant
 *		enumeration-constant = constant-expression
 *
 *  enumeration-constant:
 *		identifier
 */
static AstEnumerator ParseEnumerator(void)
{
	AstEnumerator enumer;

	CREATE_AST_NODE(enumer, Enumerator);

	if (CurrentToken != TK_ID)
	{
		Error(&TokenCoord, "The eumeration constant must be identifier");
		return enumer;
	}

	enumer->id = TokenValue.p;
	NEXT_TOKEN;
	if (CurrentToken == TK_ASSIGN)
	{
		NEXT_TOKEN;
		enumer->expr = ParseConstantExpression();
	}

	return enumer;
}

/**
*  enum-specifier
*		enum [identifier] { enumerator-list }
*		enum [identifier] { enumerator-list , }
*		enum identifier
*
*  enumerator-list:
*		enumerator
*		enumerator-list , enumerator
*/
static AstEnumSpecifier ParseEnumSpecifier(void)
{
	AstEnumSpecifier enumSpec;
	AstNode *tail;

	CREATE_AST_NODE(enumSpec, EnumSpecifier);

	NEXT_TOKEN;
	if (CurrentToken == TK_ID)
	{
		enumSpec->id = TokenValue.p;
		NEXT_TOKEN;
		if (CurrentToken == TK_LBRACE)
			goto enumerator_list;
	}
	else if (CurrentToken == TK_LBRACE)
	{
enumerator_list:
		NEXT_TOKEN;
		if (CurrentToken == TK_RBRACE)
			return enumSpec;

		enumSpec->enumers = (AstNode)ParseEnumerator();
		tail = &enumSpec->enumers->next;
		while (CurrentToken == TK_COMMA)
		{
			NEXT_TOKEN;
			if (CurrentToken == TK_RBRACE)
				break;
			*tail = (AstNode)ParseEnumerator();
			tail = &(*tail)->next;
		}
		Expect(TK_RBRACE);
	}
	else
	{
		Error(&TokenCoord, "Expect identifier or { after enum");
	}

	return enumSpec;
}

/**
 *  declaration-specifiers:
 *		storage-class-specifier [declaration-specifiers]
 *		type-specifier [declaration-specifiers]
 *		type-qualifier [declaration-specifiers]
 *
 *  storage-class-specifier:
 *		auto
 *		register
 *		static
 *		extern
 *		typedef
 *
 *  type-qualifier:
 *		const
 *		volatile
 *
 *  type-specifier:
 *		void
 *		char
 *		short
 *		int
 *		long
 *		float
 *		double
 *		signed
 *		unsigned
 *		struct-or-union-specifier
 *		enum-specifier
 *		typedef-name
 */
static AstSpecifiers ParseDeclarationSpecifiers(void)
{
	AstSpecifiers specs;
	AstToken tok;
	AstNode *scTail, *tqTail, *tsTail;
	int seeTy = 0;

	CREATE_AST_NODE(specs, Specifiers);
	scTail = &specs->stgClasses;
	tqTail = &specs->tyQuals;
	tsTail = &specs->tySpecs;

next_specifier:
	switch (CurrentToken)
	{
	case TK_AUTO:
	case TK_REGISTER:
	case TK_STATIC:
	case TK_EXTERN:
	case TK_TYPEDEF:

		CREATE_AST_NODE(tok, Token);
		tok->token = CurrentToken;
		*scTail = (AstNode)tok;
		scTail = &tok->next;
		NEXT_TOKEN;
		break;

	case TK_CONST:
	case TK_VOLATILE:

		CREATE_AST_NODE(tok, Token);
		tok->token = CurrentToken;
		*tqTail = (AstNode)tok;
		tqTail = &tok->next;
		NEXT_TOKEN;
		break;

	case TK_VOID:
	case TK_CHAR:
	case TK_SHORT:
	case TK_INT:
	case TK_INT64:
	case TK_LONG:
	case TK_FLOAT:
	case TK_DOUBLE:
	case TK_SIGNED:
	case TK_UNSIGNED:

		CREATE_AST_NODE(tok, Token);
		tok->token = CurrentToken;
		*tsTail = (AstNode)tok;
		tsTail = &tok->next;
		seeTy = 1;
		NEXT_TOKEN;
		break;

	case TK_ID:

		if (! seeTy && IsTypedefName(TokenValue.p))
		{
			AstTypedefName tname;

			CREATE_AST_NODE(tname, TypedefName);

			tname->id = TokenValue.p;
			*tsTail = (AstNode)tname;
			tsTail = &tname->next;
			NEXT_TOKEN;
			seeTy = 1;
			break;
		} 
		return specs;

	case TK_STRUCT:
	case TK_UNION:

		*tsTail = (AstNode)ParseStructOrUnionSpecifier();
		tsTail = &(*tsTail)->next;
		seeTy = 1;
		break;

	case TK_ENUM:

		*tsTail = (AstNode)ParseEnumSpecifier();
		tsTail = &(*tsTail)->next;
		seeTy = 1;
		break;

	default:
		return specs;
	}
	goto next_specifier;
}

int IsTypeName(int tok)
{
	return tok == TK_ID ? IsTypedefName(TokenValue.p) : (tok >= TK_AUTO && tok <= TK_VOID);
}

/**
 * type-name:
 *     specifier-qualifier-list abstract-declarator
 */
AstTypeName ParseTypeName(void)
{
	AstTypeName tyName;

	CREATE_AST_NODE(tyName, TypeName);

	tyName->specs = ParseDeclarationSpecifiers();
	if (tyName->specs->stgClasses != NULL)
	{
		Error(&tyName->coord, "type name should not have storage class");
		tyName->specs->stgClasses = NULL;
	}
	tyName->dec = ParseDeclarator(DEC_ABSTRACT);

	return tyName;
}

/**
 *  The function defintion and declaration have some common parts:
 *	declaration-specifiers [init-declarator-list]
 *  if we found that the parts followed by a semicolon, then it is a declaration
 *  or if the init-declarator list is a stand-alone declarator, then it may be
 *  a function definition.
 */
static AstDeclaration ParseCommonHeader(void)
{
	AstDeclaration decl;
	AstNode *tail;

	CREATE_AST_NODE(decl, Declaration);

	decl->specs = ParseDeclarationSpecifiers();
	if (CurrentToken != TK_SEMICOLON)
	{
		decl->initDecs = (AstNode)ParseInitDeclarator();
		tail = &decl->initDecs->next;
		while (CurrentToken == TK_COMMA)
		{
			NEXT_TOKEN;
			*tail = (AstNode)ParseInitDeclarator();
			tail = &(*tail)->next;
		}
	}

	return decl;
}

AstDeclaration ParseDeclaration(void)
{
	AstDeclaration decl;

	decl = ParseCommonHeader();
	Expect(TK_SEMICOLON);
	PreCheckTypedef(decl);

	return decl;
}

/**
 * If initDec is a legal function declarator, return it 
 */
static AstFunctionDeclarator GetFunctionDeclarator(AstInitDeclarator initDec)
{
	AstDeclarator dec;

	if (initDec == NULL || initDec->next != NULL || initDec->init != NULL)
		return NULL;

	dec = initDec->dec;
	while (dec && dec->kind != NK_FunctionDeclarator)
		dec = dec->dec;

	if (dec == NULL || dec->dec->kind != NK_NameDeclarator)
		return NULL;

	return (AstFunctionDeclarator)dec;
}

/**
 *  external-declaration:
 *		function-definition
 *		declaration
 *
 *  function-definition:
 *		declaration-specifiers declarator [declaration-list] compound-statement
 *
 *  declaration:
 *		declaration-specifiers [init-declarator-list] ;
 *
 *  declaration-list:
 *		declaration
 *		declaration-list declaration
 */
static AstNode ParseExternalDeclaration(void)
{
	AstDeclaration decl = NULL;
	AstInitDeclarator initDec = NULL;
	AstFunctionDeclarator fdec;

	decl = ParseCommonHeader();
	initDec = (AstInitDeclarator)decl->initDecs;
	if (decl->specs->stgClasses != NULL && ((AstToken)decl->specs->stgClasses)->token == TK_TYPEDEF)
		goto not_func;

	fdec = GetFunctionDeclarator(initDec);
	if (fdec != NULL)
	{
		AstFunction func;
		AstNode *tail;

		if (CurrentToken == TK_SEMICOLON)
		{
			NEXT_TOKEN;
			if (CurrentToken != TK_LBRACE)
				return (AstNode)decl;

			// maybe a common error, function definition follows ;
			Error(&decl->coord, "maybe you accidently add the ;");
		}
		else if (fdec->paramTyList && CurrentToken != TK_LBRACE)
		{
			// a common error, function declaration loses ;
			goto not_func;
		}

		CREATE_AST_NODE(func, Function);

		func->coord = decl->coord;
		func->specs = decl->specs;
		func->dec = initDec->dec;
		func->fdec = fdec;

		Level++;
		if (func->fdec->paramTyList)
		{
			AstNode p = func->fdec->paramTyList->paramDecls;

			while (p)
			{
				CheckTypedefName(0, GetOutermostID(((AstParameterDeclaration)p)->dec));
				p = p->next;
			}
		}
		tail = &func->decls;
		while (CurrentTokenIn(FIRST_Declaration))
		{
			*tail = (AstNode)ParseDeclaration();
			tail = &(*tail)->next;
		}
		Level--;

		func->stmt = ParseCompoundStatement();

		return (AstNode)func;
	}

not_func:
	Expect(TK_SEMICOLON);
	PreCheckTypedef(decl);

	return (AstNode)decl;
}

/**
 *  translation-unit:
 *		external-declaration
 *		translation-unit external-declaration
 */
AstTranslationUnit ParseTranslationUnit(char *filename)
{
	AstTranslationUnit transUnit;
	AstNode *tail;

	ReadSourceFile(filename);

	TokenCoord.filename = filename;
	TokenCoord.line = TokenCoord.col = TokenCoord.ppline = 1;
	TypedefNames = CreateVector(8);
	OverloadNames = CreateVector(8);

	CREATE_AST_NODE(transUnit, TranslationUnit);
	tail = &transUnit->extDecls;

	NEXT_TOKEN;
	while (CurrentToken != TK_END)
	{
		*tail = ParseExternalDeclaration();
		tail = &(*tail)->next;
		SkipTo(FIRST_ExternalDeclaration, "the beginning of external declaration");
	}

	CloseSourceFile();

	return transUnit;
}


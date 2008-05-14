#ifndef __DECL_H_
#define __DECL_H_

enum { DEC_ABSTRACT = 0x01, DEC_CONCRETE = 0x02};
enum { POINTER_TO, ARRAY_OF, FUNCTION_RETURN };

#define AST_DECLARATOR_COMMON   \
    AST_NODE_COMMON             \
    struct astDeclarator *dec;  \
    char *id;                   \
    TypeDerivList tyDrvList;

typedef struct tdname
{
	char *id;
	int level;
	int overload;
} *TDName;

typedef struct typeDerivList
{
	int ctor;
	union
	{
		int len;
		int qual;
		Signature sig;
	};
	struct typeDerivList *next;
} *TypeDerivList;

typedef struct astSpecifiers *AstSpecifiers;

typedef struct astDeclarator
{
	AST_DECLARATOR_COMMON
} *AstDeclarator;

typedef struct astInitializer
{
	AST_NODE_COMMON
	int lbrace;
	union
	{
		AstNode initials;
		AstExpression expr;
	};
	InitData idata;
} *AstInitializer;

typedef struct astInitDeclarator
{
	AST_NODE_COMMON
	AstDeclarator dec;
	AstInitializer init;
} *AstInitDeclarator;

typedef struct astParameterDeclaration
{
	AST_NODE_COMMON
	AstSpecifiers specs;
	AstDeclarator dec;
} *AstParameterDeclaration;

typedef struct astParameterTypeList
{
	AST_NODE_COMMON
	AstNode paramDecls;
	int ellipse;
} *AstParameterTypeList;

typedef struct astFunctionDeclarator
{
	AST_DECLARATOR_COMMON
	Vector ids;
	AstParameterTypeList paramTyList;
	int partOfDef;
	Signature sig;
} *AstFunctionDeclarator;

typedef struct astArrayDeclarator
{
	AST_DECLARATOR_COMMON
	AstExpression expr;
} *AstArrayDeclarator;

typedef struct astPointerDeclarator
{
	AST_DECLARATOR_COMMON
	AstNode tyQuals;
} *AstPointerDeclarator;

typedef struct astStructDeclarator
{
	AST_NODE_COMMON
	AstDeclarator dec;
	AstExpression expr;
} *AstStructDeclarator;

typedef struct astStructDeclaration
{
	AST_NODE_COMMON
	AstSpecifiers specs;
	AstNode stDecs;
} *AstStructDeclaration;

typedef struct astStructSpecifier
{
	AST_NODE_COMMON
	char *id;
	AstNode stDecls;
} *AstStructSpecifier;

typedef struct astEnumerator
{
	AST_NODE_COMMON
	char *id;
	AstExpression expr;
} *AstEnumerator;

typedef struct astEnumSpecifier
{
	AST_NODE_COMMON
	char *id;
	AstNode enumers;
} *AstEnumSpecifier;

typedef struct astTypedefName
{
	AST_NODE_COMMON
	char *id;
	Symbol sym;
} *AstTypedefName;

typedef struct astToken
{
	AST_NODE_COMMON
	int token;
} *AstToken;

struct astSpecifiers
{
	AST_NODE_COMMON
	AstNode stgClasses;
	AstNode tyQuals;
	AstNode tySpecs;
	int sclass;
	Type ty;
};

struct astTypeName
{
	AST_NODE_COMMON
	AstSpecifiers specs;
	AstDeclarator dec;
};

struct astDeclaration
{
	AST_NODE_COMMON
	AstSpecifiers specs;
	AstNode initDecs;
};

typedef struct astFunction
{
	AST_NODE_COMMON
	AstSpecifiers specs;
	AstDeclarator dec;
	AstFunctionDeclarator fdec;
	AstNode decls;
	AstStatement stmt;
	FunctionSymbol fsym;
	Label labels;
	Vector loops;
	Vector swtches;
	Vector breakable;
	int hasReturn;
} *AstFunction;

struct astTranslationUnit
{
	AST_NODE_COMMON
	AstNode extDecls;
};

void CheckLocalDeclaration(AstDeclaration decl, Vector v);
Type CheckTypeName(AstTypeName tname);

extern AstFunction CURRENTF;

#endif


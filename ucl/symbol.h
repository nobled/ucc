#ifndef __SYMBOL_H_
#define __SYMBOL_H_

enum 
{ 
	SK_Tag,    SK_TypedefName, SK_EnumConstant, SK_Constant, SK_Variable, SK_Temp,
	SK_Offset, SK_String,      SK_Label,        SK_Function, SK_Register
};

#define SYM_HASH_MASK 127

#define SYMBOL_COMMON     \
    int kind;             \
    char *name;           \
    char *aname;          \
    Type ty;              \
    int level;            \
    int sclass;           \
    int ref;              \
    int defined   : 1;    \
    int addressed : 1;    \
    int needwb    : 1;    \
    int unused    : 29;   \
    union value val;      \
    struct symbol *reg;   \
    struct symbol *link;  \
    struct symbol *next;

typedef struct bblock *BBlock;
typedef struct initData *InitData;

typedef struct symbol
{
	SYMBOL_COMMON
} *Symbol;

typedef struct valueDef
{
	Symbol dst;
	int op;
	Symbol src1;
	Symbol src2;
	BBlock ownBB;
	struct valueDef *link;
} *ValueDef;

typedef struct valueUse
{
	ValueDef def;
	struct valueUse *next;
} *ValueUse;

typedef struct variableSymbol
{
	SYMBOL_COMMON
	InitData idata;
	ValueDef def;
	ValueUse uses;
	int offset;
} *VariableSymbol;

typedef struct functionSymbol
{
	SYMBOL_COMMON
	Symbol params;
	Symbol locals;
	Symbol *lastv;
	int nbblock;
	BBlock entryBB;
	BBlock exitBB;
	ValueDef valNumTable[16];
} *FunctionSymbol;

typedef struct table
{
	Symbol *buckets;
	int level;
	struct table *outer;
} *Table;

#define AsVar(sym)  ((VariableSymbol)sym)
#define AsFunc(sym) ((FunctionSymbol)sym)

void InitSymbolTable(void);
void EnterScope(void);
void ExitScope(void);

Symbol LookupID(char *id);
Symbol LookupTag(char *id);
Symbol AddConstant(Type ty, union value val);
Symbol AddEnumConstant(char *id, Type ty, int val);
Symbol AddTypedefName(char *id, Type ty);
Symbol AddVariable(char *id, Type ty, int sclass);
Symbol AddFunction(char *id, Type ty, int sclass);
Symbol AddTag(char *id, Type ty);
Symbol IntConstant(int i);
Symbol CreateTemp(Type ty);
Symbol CreateLabel(void);
Symbol AddString(Type ty, String str);
Symbol CreateOffset(Type ty, Symbol base, int offset);

extern int Level;
extern int LabelNum, TempNum;
extern Symbol Functions;
extern Symbol Globals;
extern Symbol Strings;
extern Symbol FloatConstants;
extern FunctionSymbol FSYM;

#endif



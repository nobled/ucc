#include "ucl.h"
#include "output.h"

// number of strings
static int StringNum;
// tags in global scope, tag means struct/union, enumeration name
static struct table GlobalTags;
// normal identifiers in global scope
static struct table GlobalIDs;
// all the constants
static struct table Constants;
// tags in current scope
static Table Tags;
// normal identifiers in current scope
static Table Identifiers;
// used to construct symobl list
static Symbol *FunctionTail, *GlobalTail, *StringTail, *FloatTail;

// all the function symbols
Symbol Functions;
// all the gloabl variables and static variables
Symbol Globals;
// all the strings
Symbol Strings;
// all the floating constants
Symbol FloatConstants;
/// Scope level, file scope will be 0, when entering each nesting level,
/// Level increment; exiting each nesting level, Level decrement
int Level;
// number of temporaries
int TempNum;
// number of labels
int LabelNum;

/**
 * Look up 'name' in current scope and all the enclosing scopes
 */
static Symbol LookupSymbol(Table tbl, char *name)
{
	Symbol p;
	unsigned h = (unsigned long)name & SYM_HASH_MASK;

	do
	{
		if (tbl->buckets != NULL)
		{
			for (p = tbl->buckets[h]; p; p = p->link)
			{
				if (p->name == name)
					return p;
			}
		}
	} while ((tbl = tbl->outer) != NULL);

	return NULL;
}

/**
 * Add a symbol sym to symbol table tbl
 */
static Symbol AddSymbol(Table tbl, Symbol sym)
{
	unsigned int h = (unsigned long)sym->name & SYM_HASH_MASK;

	/// many scope doesn't introduce any new variable, in order to
	/// save memory, only when there is variable, allocate memory
	if (tbl->buckets == NULL)
	{
		int size = sizeof(Symbol) * (SYM_HASH_MASK + 1);

		tbl->buckets = HeapAllocate(CurrentHeap, size);
		memset(tbl->buckets, 0, size);
	}

	sym->link = tbl->buckets[h];
	sym->level = tbl->level;
	tbl->buckets[h] = sym;

	return sym;
}

/**
 * Enter a nesting scope. Increment the nesting level and 
 * create two new symbol table for normal identifiers and tags.
 */
void EnterScope(void)
{
	Table t;

	Level++;

	ALLOC(t);
	t->level = Level;
	t->outer = Identifiers;
	t->buckets = NULL;
	Identifiers = t;

	ALLOC(t);
	t->level = Level;
	t->outer = Tags;
	t->buckets = NULL;
	Tags = t;
}

/**
 * Exit a nesting scope. Decrement the nesting level and 
 * up to the enclosing normal identifiers and tags
 */
void ExitScope(void)
{
	Level--;
	Identifiers = Identifiers->outer;
	Tags = Tags->outer;
}

Symbol LookupID(char *name)
{
	return LookupSymbol(Identifiers, name);
}

Symbol LookupTag(char *name)
{
	return LookupSymbol(Tags, name);
}

Symbol AddTag(char *name, Type ty)
{
	Symbol p;

	CALLOC(p);

	p->kind = SK_Tag;
	p->name = name;
	p->ty = ty;

	return AddSymbol(Tags, p);
}

Symbol AddEnumConstant(char *name, Type ty, int val)
{
	Symbol p;

	CALLOC(p);

	p->kind = SK_EnumConstant;
	p->name = name;
	p->ty   = ty;
	p->val.i[0] = val;

	return AddSymbol(Identifiers, p);
}

Symbol AddTypedefName(char *name, Type ty)
{
	Symbol p;

	CALLOC(p);

	p->kind = SK_TypedefName;
	p->name = name;
	p->ty = ty;

	return AddSymbol(Identifiers, p);
}

Symbol AddVariable(char *name, Type ty, int sclass)
{
	VariableSymbol p;

	CALLOC(p);

	p->kind = SK_Variable;
	p->name = name;
	p->ty = ty;
	p->sclass = sclass;

	if (Level == 0 || sclass == TK_STATIC)
	{
		*GlobalTail = (Symbol)p;
		GlobalTail = &p->next;
	}
	else if (sclass != TK_EXTERN)
	{
		*FSYM->lastv = (Symbol)p;
		FSYM->lastv = &p->next;
	}

	return AddSymbol(Identifiers, (Symbol)p);
}

Symbol AddFunction(char *name, Type ty, int sclass)
{
	FunctionSymbol p;

	CALLOC(p);

	p->kind = SK_Function;
	p->name = name;
	p->ty = ty;
	p->sclass = sclass;
	p->lastv = &p->params;

	*FunctionTail = (Symbol)p;
	FunctionTail = &p->next;

	return AddSymbol(&GlobalIDs, (Symbol)p);
}

Symbol AddConstant(Type ty, union value val)
{
	unsigned h = (unsigned)val.i[0] & SYM_HASH_MASK;
	Symbol p;

	ty = Unqual(ty);
	if (IsIntegType(ty))
	{
		ty = T(INT);
	}
	else if (IsPtrType(ty))
	{
		ty = T(POINTER);
	}
	else if (ty->categ == LONGDOUBLE)
	{
		ty = T(DOUBLE);
	}

	for (p = Constants.buckets[h]; p != NULL; p = p->link)
	{
		if (p->ty == ty && p->val.i[0] == val.i[0] && p->val.i[1] == val.i[1])
			return p;
	}

	CALLOC(p);

	p->kind = SK_Constant;
	switch (ty->categ)
	{
	case INT:
		p->name = FormatName("%d", val.i[0]);
		break;

	case POINTER:
		if (val.i[0] == 0)
		{
			p->name = "0";
		}
		else
		{
			p->name = FormatName("0x%x", val.i[0]);
		}
		break;

	case FLOAT:
		p->name = FormatName("%g", val.f);
		break;

	case DOUBLE:
		p->name = FormatName("%g", val.d);
		break;

	default:
		assert(0);
	}
	p->ty = ty;
	p->sclass = TK_STATIC;
	p->val = val;

	p->link = Constants.buckets[h];
	Constants.buckets[h] = p;

	if (ty->categ == FLOAT || ty->categ == DOUBLE)
	{
		*FloatTail = p;
		FloatTail = &p->next;
	}
	return p;
}

Symbol IntConstant(int i)
{
	union value val;

	val.i[0] = i;
	val.i[1] = 0;

	return AddConstant(T(INT), val);
}

Symbol AddString(Type ty, String str)
{
	Symbol p;

	CALLOC(p);

	p->kind = SK_String;
	p->name = FormatName("str%d", StringNum++);
	p->ty = ty;
	p->sclass = TK_STATIC;
	p->val.p = str;

	*StringTail = p;
	StringTail = &p->next;

	return p;
}

Symbol CreateTemp(Type ty)
{
	VariableSymbol p;

	CALLOC(p);

	p->kind = SK_Temp;
	p->name = FormatName("t%d", TempNum++);
	p->ty = ty;
	p->level = 1;


	*FSYM->lastv = (Symbol)p;
	FSYM->lastv = &p->next;

	return (Symbol)p;
}

Symbol CreateLabel(void)
{
	Symbol p;

	CALLOC(p);

	p->kind = SK_Label;
	p->name = FormatName("BB%d", LabelNum++);

	return p;
}

Symbol CreateOffset(Type ty, Symbol base, int coff)
{
	VariableSymbol p;

	if (coff == 0)
		return base;

	CALLOC(p);
	if (base->kind == SK_Offset)
	{
		coff += AsVar(base)->offset;
		base = base->link;
	}
	p->addressed = 1;
	p->kind = SK_Offset;
	p->ty = ty;
	p->link = base;
	p->offset = coff;
	p->name = FormatName("%s[%d]", base->name, coff);
	base->ref++;

	return (Symbol)p;
}

void InitSymbolTable(void)
{
	int size;

	Level = 0;

	GlobalTags.buckets = GlobalIDs.buckets = NULL;
	GlobalTags.outer = GlobalIDs.outer = NULL;
	GlobalTags.level = GlobalIDs.level = 0;

	size = sizeof(Symbol) * (SYM_HASH_MASK + 1);
	Constants.buckets = HeapAllocate(CurrentHeap, size);
	memset(Constants.buckets, 0, size);
	Constants.outer = NULL;
	Constants.level = 0;

	Functions = Globals = Strings = FloatConstants = NULL;
	FunctionTail = &Functions;
	GlobalTail = &Globals;
	StringTail = &Strings;
	FloatTail = &FloatConstants;

	Tags = &GlobalTags;
	Identifiers = &GlobalIDs;

	TempNum = LabelNum = StringNum = 0;
}


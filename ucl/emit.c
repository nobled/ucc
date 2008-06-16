#include "ucl.h"
#include "ast.h"
#include "decl.h"
#include "expr.h"
#include "gen.h"
#include "output.h"
#include "target.h"

int SwitchTableNum;

/**
 * Emit all the strings to assembly file
 */
static void EmitStrings(void)
{
	Symbol p = Strings;
	String str;
	int len;
	int size;

	while (p)
	{
		DefineGlobal(p);
		str = p->val.p;
		len = strlen(p->aname);
		size = str->len + 1;
		if (p->ty == WCharType)
		{
			int i = 0;
			union value val;
			int *wcp = (int *)str->chs;

			val.i[1] = 0;
			while (i < size)
			{
				val.i[0] = wcp[i];
				DefineValue(WCharType, val);
				LeftAlign(ASMFile, len);
				PutString("\t");
				i++;
			}
			PutString("\n");
		}
		else
		{
			DefineString(str, size);
		}
		p = p->next;
	}
	PutString("\n");
}

void EmitFloatConstants(void)
{
	Symbol p = FloatConstants;

	while (p)
	{
		DefineFloatConstant(p);
		p = p->next;
	}
	PutString("\n");
}

static void EmitGlobals(void)
{
	Symbol p = Globals;
	InitData initd;
	int len, size;

	while (p)
	{
		initd = AsVar(p)->idata;

		if (p->sclass == TK_EXTERN && initd == NULL)
		{
			if (p->ref > 0)
			{
				Import(p);
			}
		}
		else if (initd == NULL)
		{
			DefineCommData(p);
		}
		else 
		{
			DefineGlobal(p);
			len = strlen(p->aname);
			size = 0;
			while (initd)
			{
				if (initd->offset != size)
				{
					LeftAlign(ASMFile, len);
					PutString("\t");
					Space(initd->offset - size);
				}
				if (initd->offset != 0)
				{
					LeftAlign(ASMFile, len);
					PutString("\t");
				}
				if (initd->expr->op == OP_ADD)
				{
					int n = initd->expr->kids[1]->val.i[0];

					DefineAddress(initd->expr->kids[0]->val.p);
					if (n != 0)
					{
						Print("%s%d", n > 0 ? " + " : " ", n);
					}
					PutString("\n");
				}
				else if (initd->expr->op == OP_STR)
				{
					String str = initd->expr->val.p;

					size = initd->expr->ty->size;
					if (initd->expr->ty == WCharType)
					{
						int i = 0;
						union value val;
						int *wcp = (int *)str->chs;

						val.i[1] = 0;
						while (i < size)
						{
							val.i[0] = wcp[i];
							DefineValue(WCharType, val);
							LeftAlign(ASMFile, len);
							PutString("\t");
							i++;
						}
					}
					else
					{
						DefineString(str, size);
					}
				}
				else
				{
					DefineValue(initd->expr->ty, initd->expr->val);
				}
				size = initd->offset + initd->expr->ty->size;
				initd = initd->next;
			}
			if (size < p->ty->size)
			{
				LeftAlign(ASMFile, len);
				PutString("\t");
				Space(p->ty->size - size);
			}
			PutString("\n");
		}
		p = p->next;
	}
	PutString("\n");
}

static void ImportFunctions(void)
{
	Symbol p = Functions;

	while (p)
	{
		if (! p->defined && p->ref)
		{
			Import(p);
		}
		p = p->next;
	}
}

/**
 * Emit all the functions
 */
static void EmitFunctions(AstTranslationUnit transUnit)
{
	AstNode p;
	FunctionSymbol fsym;

	p = transUnit->extDecls;
	while (p != NULL)
	{
		if (p->kind == NK_Function)
		{
			fsym = ((AstFunction)p)->fsym;
			if (fsym->sclass != TK_STATIC || fsym->ref > 0)
			{
				EmitFunction(fsym);
			}
		}
		p = p->next;
	}
}

/**
 * Emit the assembly code for the translation unit
 */
void EmitTranslationUnit(AstTranslationUnit transUnit)
{
	ASMFile = CreateOutput(Input.filename, ExtName);

	SwitchTableNum = 1;

	BeginProgram();

	Segment(DATA);

	EmitStrings();

	EmitFloatConstants();

	EmitGlobals();

	Segment(CODE);

	ImportFunctions();

	EmitFunctions(transUnit);

	EndProgram();

	fclose(ASMFile);
}

#include "ucl.h"
#include "output.h"
#include "ast.h"
#include "decl.h"
#include "gen.h"

#define DST  inst->opds[0]
#define SRC1 inst->opds[1]
#define SRC2 inst->opds[2]

static char *OPCodeNames[] = 
{
#define OPCODE(code, name, func) name,
#include "opcode.h"
#undef OPCODE
};

static void DAssemUIL(IRInst inst)
{
	int op = inst->opcode;

	fprintf(IRFile, "\t");
	switch (op)
	{
	case BOR: 
	case BXOR: 
	case BAND:
	case LSH: 
	case RSH:
	case ADD:
	case SUB:
	case MUL: 
	case DIV: 
	case MOD:
		fprintf(IRFile, "%s = %s %s %s", DST->name, SRC1->name, OPCodeNames[op], SRC2->name);
		break;

	case INC:
	case DEC:
		fprintf(IRFile, "%s%s", OPCodeNames[op], DST->name);
		break;

	case BCOM:
	case NEG:
	case ADDR:
	case DEREF:
		fprintf(IRFile, "%s = %s%s", DST->name, OPCodeNames[op], SRC1->name);
		break;

	case MOV:
		fprintf(IRFile, "%s = %s", DST->name, SRC1->name);
		break;

	case IMOV:
		fprintf(IRFile, "*%s = %s", DST->name, SRC1->name);
		break;

	case JE:
	case JNE:
	case JG:
	case JL:
	case JGE:
	case JLE:
		fprintf(IRFile, "if (%s %s %s) goto %s", SRC1->name, OPCodeNames[op],
			    SRC2->name, ((BBlock)DST)->sym->name);
		break;

	case JZ:
		fprintf(IRFile, "if (! %s) goto %s", SRC1->name, ((BBlock)DST)->sym->name);
		break;

	case JNZ:
		fprintf(IRFile, "if (%s) goto %s", SRC1->name, ((BBlock)DST)->sym->name);
		break;

	case JMP:
		fprintf(IRFile, "goto %s", ((BBlock)DST)->sym->name);
		break;

	case IJMP:
		{
			BBlock *p = (BBlock *)DST;

			fprintf(IRFile, "goto (");
			while (*p != NULL)
			{
				fprintf(IRFile, "%s,",  (*p)->sym->name);
				p++;
			}
			fprintf(IRFile, ")[%s]", SRC1->name);
		}
		break;

	case CALL:
		{
			ILArg arg;
			Vector args = (Vector)SRC2;
			int i;

			if (DST != NULL)
			{
				fprintf(IRFile, "%s = ", DST->name);
			}
			fprintf(IRFile, "%s(", SRC1->name);
			for (i = 0; i < LEN(args); ++i)
			{
				arg = GET_ITEM(args, i);
				if (i != LEN(args) - 1)
					fprintf(IRFile, "%s, ", arg->sym->name);
				else
					fprintf(IRFile, "%s", arg->sym->name);
			}
			fprintf(IRFile, ")");
		}
		break;

	case RET:
		fprintf(IRFile, "return %s", DST->name);
		break;

	default:
		fprintf(IRFile, "%s = %s%s", DST->name, OPCodeNames[op], SRC1->name);
		break;
	}
	fprintf(IRFile, ";\n");
}

void DAssemFunction(AstFunction func)
{
	FunctionSymbol fsym = func->fsym;
	BBlock bb = fsym->entryBB;
	IRInst inst;

	if (! fsym->defined)
		return;

	fprintf(IRFile, "function %s\n", fsym->name);

	while (bb != NULL)
	{
		if (bb->ref != 0)
		{
			fprintf(IRFile, "%s:\n", bb->sym->name);
		}
		inst = bb->insth.next;
		while (inst != &bb->insth)
		{
			DAssemUIL(inst);
			inst = inst->next;
		}
		bb = bb->next;
	}
	if (fsym->exitBB->npred != 0)
		fprintf(IRFile, "\tret\n");

	fprintf(IRFile, "\n\n");
}

void DAssemTranslationUnit(AstTranslationUnit transUnit)
{
	AstNode p = transUnit->extDecls;

	IRFile = CreateOutput(Input.filename, ".uil");

	while (p)
	{
		if (p->kind == NK_Function)
		{
			DAssemFunction((AstFunction)p);
		}
		p = p->next;
	}

	fclose(IRFile);
}

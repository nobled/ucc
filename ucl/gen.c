#include "ucl.h"
#include "gen.h"

#define IsCommute(op) 0

BBlock CurrentBB;
int OPMap[] = 
{
#define OPINFO(tok, prec, name, func, opcode) opcode,
#include "opinfo.h"
#undef  OPINFO
};

static void TrackValueChange(Symbol p)
{
	ValueUse use = AsVar(p)->uses;

	while (use)
	{
		if (use->def->op != ADDR)
			use->def->dst = NULL;
		use = use->next;
	}
}

static void TrackValueUse(Symbol p, ValueDef def)
{
	ValueUse use;

	ALLOC(use);
	use->def = def;
	use->next = AsVar(p)->uses;
	AsVar(p)->uses = use;
}

void DefineTemp(Symbol t, int op, Symbol src1, Symbol src2)
{
	ValueDef def;

	ALLOC(def);
	def->dst = t;
	def->op = op;
	def->src1 = src1;
	def->src2 = src2;
	def->ownBB = CurrentBB;

	if (op == MOV || op == CALL)
	{
		def->link = AsVar(t)->def;
		AsVar(t)->def = def;
		return;
	}

	if (src1->kind == SK_Variable)
	{
		TrackValueUse(src1, def);
	}
	if (src2 && src2->kind == SK_Variable)
	{
		TrackValueUse(src2, def);
	}
	AsVar(t)->def = def;
}

BBlock CreateBBlock(void)
{
	BBlock bb;

	CALLOC(bb);

	bb->insth.opcode = NOP;
	bb->insth.prev = bb->insth.next = &bb->insth;
	return bb;
}

void StartBBlock(BBlock bb)
{
	IRInst lasti;

	CurrentBB->next = bb;
	bb->prev = CurrentBB;
	lasti = CurrentBB->insth.prev;
	if (lasti->opcode != JMP && lasti->opcode != IJMP)
	{
		DrawCFGEdge(CurrentBB, bb);
	}
	CurrentBB = bb;
}

void AppendInst(IRInst inst)
{
	assert(CurrentBB != NULL);

	CurrentBB->insth.prev->next = inst;
	inst->prev = CurrentBB->insth.prev;
	inst->next = &CurrentBB->insth;
	CurrentBB->insth.prev = inst;

	CurrentBB->ninst++;
}

void GenerateMove(Type ty, Symbol dst, Symbol src)
{
	IRInst inst;

	ALLOC(inst);
	dst->ref++;
	src->ref++;
	inst->ty = ty;
	inst->opcode  = MOV;
	inst->opds[0] = dst;
	inst->opds[1] = src;
	inst->opds[2] = NULL;
	AppendInst(inst);

	if (dst->kind == SK_Variable)
	{
		TrackValueChange(dst);
	}
	else if (dst->kind == SK_Temp)
	{
		DefineTemp(dst, MOV, (Symbol)inst, NULL);
	}
}

void GenerateIndirectMove(Type ty, Symbol dst, Symbol src)
{
	IRInst inst;

	ALLOC(inst);
	dst->ref++;
	src->ref++;
	inst->ty = ty;
	inst->opcode  = IMOV;
	inst->opds[0] = dst;
	inst->opds[1] = src;
	inst->opds[2] = NULL;
	AppendInst(inst);
}

void GenerateAssign(Type ty, Symbol dst, int opcode, Symbol src1, Symbol src2)
{
	IRInst inst;

	ALLOC(inst);
	dst->ref++;
	src1->ref++;
	if (src2) src2->ref++;
	inst->ty = ty;
	inst->opcode = opcode;
	inst->opds[0] = dst;
	inst->opds[1] = src1;
	inst->opds[2] = src2;
	AppendInst(inst);

	DefineTemp(dst, opcode, src1, src2);
}

void GenerateBranch(Type ty, BBlock dstBB, int opcode, Symbol src1, Symbol src2)
{
	IRInst inst;

	ALLOC(inst);
	dstBB->ref++;
	src1->ref++;
	if (src2) src2->ref++;
	DrawCFGEdge(CurrentBB, dstBB);
	inst->ty = ty;
	inst->opcode  = opcode;
	inst->opds[0] = (Symbol)dstBB;
	inst->opds[1] = src1;
	inst->opds[2] = src2;
	AppendInst(inst);
}

void GenerateJump(BBlock dstBB)
{
	IRInst inst;

	ALLOC(inst);
	dstBB->ref++;
	DrawCFGEdge(CurrentBB, dstBB);
	inst->ty = T(VOID);
	inst->opcode = JMP;
	inst->opds[0] = (Symbol)dstBB;
	inst->opds[1] = inst->opds[2] = NULL;
	AppendInst(inst);
}

void GenerateIndirectJump(BBlock *dstBBs, int len, Symbol index)
{
	IRInst inst;
	int i;
	
	ALLOC(inst);
	index->ref++;
	for (i = 0; i < len; ++i)
	{
		dstBBs[i]->ref++;
		DrawCFGEdge(CurrentBB, dstBBs[i]);
	}
	inst->ty = T(VOID);
	inst->opcode = IJMP;
	inst->opds[0] = (Symbol)dstBBs;
	inst->opds[1] = index;
	inst->opds[2] = NULL;
	AppendInst(inst);
}

void GenerateReturn(Type ty, Symbol src)
{
	IRInst inst;

	ALLOC(inst);
	src->ref++;
	inst->ty = ty;
	inst->opcode = RET;
	inst->opds[0] = src;
	inst->opds[1] = inst->opds[2] = NULL;
	AppendInst(inst);
}

void GenerateFunctionCall(Type ty, Symbol recv, Symbol faddr, Vector args)
{
	ILArg p;
	IRInst inst;

	ALLOC(inst);
	if (recv) recv->ref++;
	faddr->ref++;
	FOR_EACH_ITEM(ILArg, p, args)
		p->sym->ref++;
	ENDFOR
	inst->ty = ty;
	inst->opcode = CALL;
	inst->opds[0] = recv;
	inst->opds[1] = faddr;
	inst->opds[2] = (Symbol)args;
	AppendInst(inst);

	if (recv != NULL)
		DefineTemp(recv, CALL, (Symbol)inst, NULL);
}

void GenerateClear(Symbol dst, int size)
{
	IRInst inst;

	ALLOC(inst);
	dst->ref++;
	inst->ty = T(UCHAR);
	inst->opcode = CLR;
	inst->opds[0] = dst;
	inst->opds[1] = IntConstant(size);
	inst->opds[1]->ref++;
	inst->opds[2] = NULL;
	AppendInst(inst);
}

Symbol AddressOf(Symbol p)
{
	if (p->kind == SK_Temp && AsVar(p)->def->op == DEREF)
	{
		return AsVar(p)->def->src1;
	}

	p->addressed = 1;
	if (p->kind == SK_Variable)
	{
		TrackValueChange(p);
	}
	return TryAddValue(T(POINTER), ADDR, p, NULL); 
}


Symbol Deref(Type ty, Symbol addr)
{
	Symbol tmp;
	
	if (addr->kind == SK_Temp && AsVar(addr)->def->op == ADDR)
	{
		return AsVar(addr)->def->src1;
	}

	tmp = CreateTemp(ty);
	GenerateAssign(ty, tmp, DEREF, addr, NULL);
	return tmp;
}

Symbol TryAddValue(Type ty, int op, Symbol src1, Symbol src2)
{
	int h = ((unsigned)src1 + (unsigned)src2 + op) & 15;
	ValueDef def = FSYM->valNumTable[h];
	Symbol t;

	if (op != ADDR && (src1->addressed || (src2 && src2->addressed)))
		goto new_temp;

	while (def)
	{
		if (def->op == op && (def->src1 == src1 && def->src2 == src2))
			break;
		def = def->link;
	}

	if (def && def->ownBB == CurrentBB && def->dst != NULL)
		return def->dst;

new_temp:
	t = CreateTemp(ty);
	GenerateAssign(ty, t, op, src1, src2);

	def = AsVar(t)->def;
	def->link = FSYM->valNumTable[h];
	FSYM->valNumTable[h] = def;
	return t;
}

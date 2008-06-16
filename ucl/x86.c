#include "ucl.h"
#include "ast.h"
#include "expr.h"
#include "gen.h"
#include "reg.h"
#include "target.h"
#include "output.h"

extern int SwitchTableNum;
enum ASMCode
{
#define TEMPLATE(code, str) code,
#include "x86win32.tpl"
#undef TEMPLATE
};

#define ASM_CODE(opcode, tcode) ((opcode << 2) + tcode - I4)
#define DST  inst->opds[0]
#define SRC1 inst->opds[1]
#define SRC2 inst->opds[2]
#define IsNormalRecord(rty) (rty->size != 1 && rty->size != 2 && rty->size != 4 && rty->size != 8)
#define PRESERVE_REGS 4
#define SCRATCH_REGS  4
#define STACK_ALIGN_SIZE 4

static Symbol X87Top;
static int X87TCode;

/**
 * Put assembly code to move src into dst
 */
static void Move(int code, Symbol dst, Symbol src)
{
	Symbol opds[2];

	opds[0] = dst;
	opds[1] = src;
	PutASMCode(code, opds);
}

/**
 * Mark the variable is in register
 */
static void AddVarToReg(Symbol reg, Symbol v)
{
	v->link = reg->link;
	reg->link = v;
	v->reg = reg;
}

/**
 * When a variable is modified, if it is not in a register, do nothing;
 * otherwise, spill othere variables in this register, set the variable's
 * needWB flag.(need write back to memory)
 */
static void ModifyVar(Symbol p)
{
	Symbol reg;

	if (p->reg == NULL)
		return;

	p->needwb = 0;
	reg = p->reg;
	SpillReg(reg);
	
	AddVarToReg(reg, p);
	p->needwb = 1;
}

/**
 * Allocate register for instruction operand. index indicates which operand
 */
static void AllocateReg(IRInst inst, int index)
{
	Symbol reg;
	Symbol p;

	p = inst->opds[index];

	// In x86, UCC only allocates register for temporary
	if (p->kind != SK_Temp)
		return;

	// if it is already in a register, mark the register as used
	if (p->reg != NULL)
	{
		UsedRegs |= 1 << p->reg->val.i[0];
		return;
	}

	/// in x86, the first source operand is also destination operand, 
	/// reuse the first source operand's register if the first source operand 
	/// will not be used after this instruction
	if (index == 0 && SRC1->ref == 1 && SRC1->reg != NULL)
	{
		reg = SRC1->reg;
		reg->link = NULL;
		AddVarToReg(reg, p);
		return;
	}

	/// get a register, if the operand is not destination operand, load the 
	/// variable into the register
	reg = GetReg();
	if (index != 0)
	{
		Move(X86_MOVI4, reg, p);
	}
	AddVarToReg(reg, p);
}

/**
 * Before executing an float instruction, UCC ensures that only TOP register of x87 stack 
 * may be used. And whenenver a basic block ends, save the x87 stack top if needed. Thus
 * we can assure that when entering and leaving each basic block, the x87 register stack
 * is in init status.
 */
static void SaveX87Top(void)
{
	if (X87Top == NULL)
		return;

	if (X87Top->needwb && X87Top->ref > 0)
	{
		PutASMCode(X86_STF4 + X87TCode - F4, &X87Top);
	}
	X87Top = NULL;
}

/**
 * Emit floating point branch instruction
 */
static void EmitX87Branch(IRInst inst, int tcode)
{
	// branch instructions modify EAX
	SpillReg(X86Regs[EAX]);
	PutASMCode(X86_LDF4 + tcode - F4, &SRC1);
	PutASMCode(ASM_CODE(inst->opcode, tcode), inst->opds);
}

/**
 * Emit floating point move instruction
 */
static void EmitX87Move(IRInst inst, int tcode)
{
	// let X87 TOP be the first source operand
	if (X87Top != SRC1)
	{
		SaveX87Top();
		PutASMCode(X86_LDF4 + tcode - F4, &SRC1);
	}

	// only put temporary in register
	if (DST->kind != SK_Temp)
	{
		PutASMCode(X86_STF4 + tcode - F4, &DST);
		X87Top = NULL;
	}
	else
	{
		DST->needwb = 1;
		X87Top = DST;
		X87TCode = tcode;
	}
}

/**
 * Emit floating point assign instruction
 */
static void EmitX87Assign(IRInst inst, int tcode)
{
	// let X87 TOP be the first source operand
	if (SRC1 != X87Top)
	{
		SaveX87Top();
		PutASMCode(X86_LDF4 + tcode - F4, &SRC1);
	}

	PutASMCode(ASM_CODE(inst->opcode, tcode), inst->opds);

	// only put temporary in register
	if (DST->kind != SK_Temp)
	{
		PutASMCode(X86_STF4 + tcode - F4, inst->opds);
		X87Top = NULL;
	}
	else
	{
		DST->needwb = 1;
		X87Top = DST;
		X87TCode = tcode;
	}
}

/**
 * Emit assembly code for block move
 */
static void EmitMoveBlock(IRInst inst)
{
	SpillReg(X86Regs[EDI]);
	SpillReg(X86Regs[ESI]);
	SpillReg(X86Regs[ECX]);

	SRC2 = IntConstant(inst->ty->size);
	PutASMCode(X86_MOVB, inst->opds);
}

/**
 * Emit assembly code for move
 */
static void EmitMove(IRInst inst)
{
	int tcode = TypeCode(inst->ty);
	Symbol reg;

	if (tcode == F4 || tcode == F8)
	{
		EmitX87Move(inst, tcode);
		return;
	}

	if (tcode == B)
	{
		EmitMoveBlock(inst);
		return;
	}

	switch (tcode)
	{
	case I1: case U1:
		if (SRC1->kind == SK_Constant)
		{
			Move(X86_MOVI1, DST, SRC1);
		}
		else
		{
			reg = GetByteReg();
			Move(X86_MOVI1, reg, SRC1);
			Move(X86_MOVI1, DST, reg);
		}
		break;

	case I2: case U2:
		if (SRC1->kind == SK_Constant)
		{
			Move(X86_MOVI2, DST, SRC1);
		}
		else
		{
			reg = GetWordReg();
			Move(X86_MOVI2, reg, SRC1);
			Move(X86_MOVI2, DST, reg);
		}
		break;

	case I4: case U4:
		if (SRC1->kind == SK_Constant)
		{
			Move(X86_MOVI4, DST, SRC1);
		}
		else
		{
			AllocateReg(inst, 1);
			AllocateReg(inst, 0);
			if (SRC1->reg == NULL && DST->reg == NULL)
			{
				reg = GetReg();
				Move(X86_MOVI4, reg, SRC1);
				Move(X86_MOVI4, DST, reg);
			}
			else
			{
				Move(X86_MOVI4, DST, SRC1);
			}
		}
		ModifyVar(DST);
		break;

	default:
		assert(0);
	}
}

/**
 * Put the variable in register
 */
static Symbol PutInReg(Symbol p)
{
	Symbol reg;

	if (p->reg != NULL)
	{
		return p->reg;
	}
	reg = GetReg();
	Move(X86_MOVI4, reg, p);

	return reg;
}

/**
 * Emit assembly code for indirect move
 */
static void EmitIndirectMove(IRInst inst)
{
	Symbol reg;

	/// indirect move is the same as move, except using register indirect address
	/// mode for destination operand
	reg = PutInReg(DST);
	inst->opcode = MOV;
	DST = reg->next;
	EmitMove(inst);
}

/**
 * Emit assembly code for assign
 */
static void EmitAssign(IRInst inst)
{
	Symbol dst;
	int code;
	int tcode= TypeCode(inst->ty);

	if (tcode == F4 || tcode == F8)
	{
		EmitX87Assign(inst, tcode);
		return;
	}

	dst = DST;
	code = ASM_CODE(inst->opcode, tcode);
	switch (code)
	{
	case X86_DIVI4:
	case X86_DIVU4:
	case X86_MODI4:
	case X86_MODU4:
	case X86_MULU4:
		if (SRC1->reg == X86Regs[EAX])
		{
			SRC1->needwb = 0;
			SpillReg(X86Regs[EAX]);
		}
		else
		{
			SpillReg(X86Regs[EAX]);
			Move(X86_MOVI4, X86Regs[EAX], SRC1);
		}
		SpillReg(X86Regs[EDX]);
		UsedRegs = 1 << EAX | 1 << EDX;
		if (SRC2->kind == SK_Constant)
		{
			Symbol reg = GetReg();

			Move(X86_MOVI4, reg, SRC2);
			SRC2 = reg;
		}
		else
		{
			AllocateReg(inst, 2);
		}
		DST = X86Regs[EAX];
		PutASMCode(code, inst->opds);
		if (code == X86_MODI4 || code == X86_MODU4)
		{
			DST = X86Regs[EDX];
		}
		break;

	case X86_LSHI4:
	case X86_LSHU4:
	case X86_RSHI4:
	case X86_RSHU4:
		AllocateReg(inst, 1);
		if (SRC2->kind != SK_Constant)
		{
			if (SRC2->reg != X86Regs[ECX])
			{
				SpillReg(X86Regs[ECX]);
				Move(X86_MOVI4, X86Regs[ECX], SRC2);
			}
			SRC2 = X86ByteRegs[ECX];
			UsedRegs = 1 << ECX;
		}
		goto put_code;

	case X86_NEGI4:
	case X86_NEGU4:
	case X86_BCOMI4:
	case X86_BCOMU4:
		AllocateReg(inst, 1);
		goto put_code;

	default:
		AllocateReg(inst, 1);
		AllocateReg(inst, 2);

put_code:
		AllocateReg(inst, 0);
		if (DST->reg == NULL)
		{
			DST = GetReg();
		}
		if (DST->reg != SRC1->reg)
		{
			Move(X86_MOVI4, DST, SRC1);
		}
		PutASMCode(code, inst->opds);
		break;
	}

	if (dst != DST)
	{
		Move(X86_MOVI4, dst, DST);
	}
	ModifyVar(dst);
}

static void EmitCast(IRInst inst)
{
	Symbol dst, reg;
	int code;

	dst = DST;
	reg = NULL;
	code = inst->opcode + X86_EXTI1 - EXTI1;
	switch (code)
	{
	case X86_EXTI1:
	case X86_EXTI2:
	case X86_EXTU1:
	case X86_EXTU2:
		AllocateReg(inst, 0);
		if (DST->reg == NULL)
		{
			DST = GetReg();
		}
		PutASMCode(code, inst->opds);
		if (dst != DST)
		{
			Move(X86_MOVI4, dst, DST);
		}
		break;

	case X86_TRUI1:
		if (SRC1->reg != NULL)
		{
			reg = X86ByteRegs[SRC1->reg->val.i[0]];
		}
		if (reg == NULL)
		{
			reg = GetByteReg();
			Move(X86_MOVI4, X86Regs[reg->val.i[0]], SRC1);
		}
		Move(X86_MOVI1, DST, reg);
		break;

	case X86_TRUI2:
		if (SRC1->reg != NULL)
		{
			reg = X86WordRegs[SRC1->reg->val.i[0]];
		}
		if (reg == NULL)
		{
			reg = GetWordReg();
			Move(X86_MOVI4, X86Regs[reg->val.i[0]], SRC1);
		}
		Move(X86_MOVI2, DST, reg);
		break;

	case X86_CVTI4F4:
	case X86_CVTI4F8:
	case X86_CVTU4F4:
	case X86_CVTU4F8:
		assert(X87Top != DST);
		PutASMCode(code, inst->opds);
		break;

	default:
		if (SRC1 == X87Top)
		{
			SaveX87Top();
		}
		if (code != X86_CVTF4 && code != X86_CVTF8)
		{
			SpillReg(X86Regs[EAX]);
			AllocateReg(inst, 0);
		}
		PutASMCode(code, inst->opds);
		if (DST->reg && DST->reg != X86Regs[EAX])
		{
			Move(X86_MOVI4, DST, X86Regs[EAX]);
		}
		break;
	}
	ModifyVar(dst);
}

static void EmitInc(IRInst inst)
{
	PutASMCode(X86_INCI1 + TypeCode(inst->ty), inst->opds);
}

static void EmitDec(IRInst inst)
{
	PutASMCode(X86_DECI1 + TypeCode(inst->ty), inst->opds);
}

static void EmitBranch(IRInst inst)
{
	int tcode = TypeCode(inst->ty);
	BBlock p = (BBlock)DST;

	DST = p->sym;
	if (tcode == F4 || tcode == F8)
	{
		EmitX87Branch(inst, tcode);
		return;
	}

	assert(tcode >= I4);
	
	if (SRC2)
	{
		if (SRC2->kind != SK_Constant)
		{
			SRC1 = PutInReg(SRC1);
		}
	}

	SRC1->ref--;
	if (SRC1->reg != NULL)
		SRC1 = SRC1->reg;
	if (SRC2)
	{
		SRC2->ref--;
		if (SRC2->reg != NULL)
			SRC2 = SRC2->reg;
	}
	ClearRegs();
	PutASMCode(ASM_CODE(inst->opcode, tcode), inst->opds);
}

static void EmitJump(IRInst inst)
{
	BBlock p = (BBlock)DST;

	DST = p->sym;
	ClearRegs();
	PutASMCode(X86_JMP, inst->opds); 
}

static void EmitIndirectJump(IRInst inst)
{
	BBlock *p;
	Symbol swtch;
	int len;
	Symbol reg;

	
	SRC1->ref--;
	p = (BBlock *)DST;
	reg = PutInReg(SRC1);

	PutString("\n");
	Segment(DATA);

	CALLOC(swtch);
	swtch->kind = SK_Variable;
	swtch->ty = T(POINTER);
	swtch->name = FormatName("swtchTable%d", SwitchTableNum++);
	swtch->sclass = TK_STATIC;
	swtch->level = 0;
	DefineGlobal(swtch);

	DST = swtch;
	len = strlen(DST->aname);
	while (*p != NULL)
	{
		DefineAddress((*p)->sym);
		PutString("\n");
		LeftAlign(ASMFile, len);
		PutString("\t");
		p++;
	}
	PutString("\n");

	Segment(CODE);

	SRC1 = reg;
	ClearRegs();
	PutASMCode(X86_IJMP, inst->opds);
}

static void EmitReturn(IRInst inst)
{
	Type ty = inst->ty;

	if (IsRealType(ty))
	{
		if (X87Top != DST)
		{
			SaveX87Top();
			PutASMCode(X86_LDF4 + X87TCode - F4, inst->opds);
		}
		X87Top = NULL;
		return;
	}

	if (IsRecordType(ty) && IsNormalRecord(ty))
	{
		inst->opcode = IMOV;
		SRC1 = DST;
		DST = FSYM->params;
		EmitIndirectMove(inst);
		return;
	}

	switch (ty->size)
	{
	case 1:
		Move(X86_MOVI1, X86ByteRegs[EAX], DST);
		break;

	case 2:
		Move(X86_MOVI2, X86WordRegs[EAX], DST);
		break;

	case 4:
		if (DST->reg != X86Regs[EAX])
		{
			Move(X86_MOVI4, X86Regs[EAX], DST);
		}
		break;

	case 8:
		Move(X86_MOVI4, X86Regs[EAX], DST);
		Move(X86_MOVI4, X86Regs[EDX], CreateOffset(T(INT), DST, 4));
		break;

	default:
		assert(0);
	}
}

static void PushArgument(Symbol p, Type ty)
{
	int tcode = TypeCode(ty);

	if (tcode == F4)
	{
		PutASMCode(X86_PUSHF4, &p);
	}
	else if (tcode == F8)
	{
		PutASMCode(X86_PUSHF8, &p);
	}
	else if (tcode == B)
	{
		Symbol opds[2];

		SpillReg(X86Regs[ESI]);
		SpillReg(X86Regs[EDI]);
		SpillReg(X86Regs[ECX]);
		opds[0] = p;
		opds[1] = IntConstant(ty->size);
		opds[2] = IntConstant(ALIGN(ty->size, STACK_ALIGN_SIZE));
		PutASMCode(X86_PUSHB, opds);
	}
	else
	{
		PutASMCode(X86_PUSH, &p);
	}
}

static void EmitCall(IRInst inst)
{
	Vector args;
	ILArg arg;
	Type rty;
	int i, stksize;

	args = (Vector)SRC2;
	stksize = 0;
	rty = inst->ty;

	for (i = LEN(args) - 1; i >= 0; --i)
	{
		arg = GET_ITEM(args, i);
		PushArgument(arg->sym, arg->ty);
		if (arg->sym->kind != SK_Function) arg->sym->ref--;
		stksize += ALIGN(arg->ty->size, STACK_ALIGN_SIZE);
	}
	SpillReg(X86Regs[EAX]);
	SpillReg(X86Regs[ECX]);
	SpillReg(X86Regs[EDX]);
	if (IsRecordType(rty) && IsNormalRecord(rty))
	{
		Symbol opds[2];

		opds[0] = GetReg();
		opds[1] = DST;
		PutASMCode(X86_ADDR, opds);
		PutASMCode(X86_PUSH, opds);
		stksize += 4;
		DST = NULL;
	}

	PutASMCode(SRC1->kind == SK_Function ? X86_CALL : X86_ICALL, inst->opds);
	{
		Symbol p;
		p = IntConstant(stksize);
		PutASMCode(X86_REDUCEF, &p);
	}

	if (DST != NULL)
		DST->ref--;
	if (SRC1->kind != SK_Function) SRC1->ref--;

	if (DST == NULL)
		return;

	if (IsRealType(rty))
	{
		PutASMCode(X86_STF4 + (rty->categ != FLOAT), inst->opds);
		return;
	}
	
	switch (rty->size)
	{
	case 1:
		Move(X86_MOVI1, DST, X86ByteRegs[EAX]);
		break;

	case 2:
		Move(X86_MOVI2, DST, X86WordRegs[EAX]);
		break;

	case 4:
		AllocateReg(inst, 0);
		if (DST->reg != X86Regs[EAX])
		{
			Move(X86_MOVI4, DST, X86Regs[EAX]);
		}
		ModifyVar(DST);
		break;

	case 8:
		Move(X86_MOVI4, DST, X86Regs[EAX]);
		Move(X86_MOVI4, CreateOffset(T(INT), DST, 4), X86Regs[EDX]);
		break;

	default:
		assert(0);
	}
}

static void EmitAddress(IRInst inst)
{
	Symbol dst;

	dst = DST;
	AllocateReg(inst, 0);
	if (DST->reg == NULL)
	{
		DST = GetReg();
	}
	PutASMCode(X86_ADDR, inst->opds);
	if (dst != DST)
	{
		Move(X86_MOVI4, dst, DST);
	}
	ModifyVar(dst);
}

static void EmitDeref(IRInst inst)
{
	Symbol reg;

	reg = PutInReg(SRC1);
	inst->opcode = MOV;
	SRC1 = reg->next;
	EmitMove(inst);
	ModifyVar(DST);
	return;
}

static void EmitClear(IRInst inst)
{
	int size = SRC1->val.i[0];
	Symbol p = IntConstant(0);

	switch (size)
	{
	case 1:
		Move(X86_MOVI1, DST, p);
		break;

	case 2:
		Move(X86_MOVI2, DST, p);
		break;

	case 4:
		Move(X86_MOVI4, DST, p);
		break;

	default:
		SpillReg(X86Regs[EAX]);
		PutASMCode(X86_CLEAR, inst->opds);
		break;
	}
}

static void EmitNOP(IRInst inst)
{
	assert(0);
}

static void (* Emitter[])(IRInst inst) = 
{
#define OPCODE(code, name, func) Emit##func, 
#include "opcode.h"
#undef OPCODE
};

static void EmitIRInst(IRInst inst)
{
	struct irinst instc = *inst;

	(* Emitter[inst->opcode])(&instc);
	return;
}

static void EmitBBlock(BBlock bb)
{
	IRInst inst = bb->insth.next;

	while (inst != &bb->insth)
	{
		UsedRegs = 0;
		EmitIRInst(inst);
		if (! (inst->opcode >= JZ && inst->opcode <= IJMP) &&
		    inst->opcode != CALL)
		{
			DST->ref--;
			if (SRC1 && SRC1->kind != SK_Function) SRC1->ref--;
			if (SRC2 && SRC2->kind != SK_Function) SRC2->ref--;
		}
		inst = inst->next;
	}
	ClearRegs();
	SaveX87Top();
}

static int LayoutFrame(FunctionSymbol fsym, int fstParamPos)
{
	Symbol p;
	int offset;

	offset = fstParamPos * STACK_ALIGN_SIZE;
	p = fsym->params;
	while (p)
	{
		AsVar(p)->offset = offset;
		offset += ALIGN(p->ty->size, STACK_ALIGN_SIZE);
		p = p->next;
	}

	offset = 0;
	p = fsym->locals;
	while (p)
	{
		if (p->ref == 0)
			goto next;

		offset += ALIGN(p->ty->size, STACK_ALIGN_SIZE);
		AsVar(p)->offset = -offset;
next:
		p = p->next;
	}

	return offset;
}

static void EmitPrologue(int stksize)
{
	/**
	 * regardless of the actual register usage, always save the preserve registers.
	 * on x86 platform, they are ebp, ebx, esi and edi
	 */
	PutASMCode(X86_PROLOGUE, NULL);
	if (stksize != 0)
	{
		Symbol sym = IntConstant(stksize);
		PutASMCode(X86_EXPANDF, &sym);
	}
}

static void EmitEpilogue(int stksize)
{
	PutASMCode(X86_EPILOGUE, NULL);
}

void EmitFunction(FunctionSymbol fsym)
{
	BBlock bb;
	Type rty;
	int stksize;

	FSYM = fsym;
	if (fsym->sclass != TK_STATIC)
	{
		Export((Symbol)fsym);
	}
	DefineLabel((Symbol)fsym);

	rty = fsym->ty->bty;
	if (IsRecordType(rty) && IsNormalRecord(rty))
	{
		VariableSymbol p;

		CALLOC(p);
		p->kind = SK_Variable;
		p->name = "recvaddr";
		p->ty = T(POINTER);
		p->level = 1;
		p->sclass = TK_AUTO;
		
		p->next = fsym->params;
		fsym->params = (Symbol)p;
	}

	stksize = LayoutFrame(fsym, PRESERVE_REGS + 1);

	EmitPrologue(stksize);

	bb = fsym->entryBB;
	while (bb != NULL)
	{
		if (bb->ref != 0)
		{
			DefineLabel(bb->sym);
		}
		EmitBBlock(bb);
		bb = bb->next;
	}

	EmitEpilogue(stksize);
	PutString("\n");
}

void StoreVar(Symbol reg, Symbol v)
{
	Move(X86_MOVI4, v, reg);
}


#include "ucl.h"
#include "gen.h"
#include "reg.h"
#include "target.h"
#include "output.h"

Symbol X86Regs[EDI + 1];
Symbol X86ByteRegs[EDI + 1];
Symbol X86WordRegs[EDI + 1];
int UsedRegs;

static int FindEmptyReg(int endr)
{
	int i;
	
	for (i = EAX; i <= endr; ++i)
	{
		if (X86Regs[i] != NULL && X86Regs[i]->link == NULL && ! (1 << i & UsedRegs))
			return i;
	}

	return NO_REG;
}

static int SelectSpillReg(int endr)
{
	Symbol p;
	int i;
	int reg = NO_REG;
	int ref, mref = INT_MAX;

	for (i = EAX; i <= endr; ++i)
	{
		if (X86Regs[i] == NULL || (1 << i & UsedRegs))
			continue;

		p = X86Regs[i]->link;
		ref = 0;
		while (p)
		{
			if (p->needwb && p->ref > 0)
			{
				ref += p->ref;
			}
			p = p->link;
		}
		if (ref < mref)
		{
			mref = ref;
			reg = i;
		}
	}

	assert(reg != NO_REG);
	return reg;
}

static Symbol GetRegInternal(int width)
{
	int i, endr;
	Symbol *regs;

	switch (width)
	{
	case 1:
		endr = EDX;
		regs = X86ByteRegs;
		break;

	case 2:
		endr = EDI;
		regs = X86WordRegs;
		break;

	case 4:
		endr = EDI;
		regs = X86Regs;
		break;
	}

	i = FindEmptyReg(endr);
	if (i == NO_REG)
	{
		i = SelectSpillReg(endr);
		SpillReg(X86Regs[i]);
	}
	UsedRegs |= 1 << i;

	return regs[i];
}

void SpillReg(Symbol reg)
{
	Symbol p;

	p = reg->link;
	while (p)
	{
		p->reg = NULL;
		if (p->needwb && p->ref > 0)
		{
			p->needwb = 0;
			StoreVar(reg, p);
		}
		p = p->link;
	}
	reg->link = NULL;
}

void ClearRegs(void)
{
	int i;

	for (i = EAX; i <= EDI; ++i)
	{
		if (X86Regs[i])
			SpillReg(X86Regs[i]);
	}
}

Symbol GetByteReg(void)
{
	return GetRegInternal(1);
}

Symbol GetWordReg(void)
{
	return GetRegInternal(2);
}

Symbol GetReg(void)
{
	return GetRegInternal(4);
}

Symbol CreateReg(char *name, char *iname, int no)
{
	Symbol reg;

	CALLOC(reg);

	reg->kind = SK_Register;
	reg->name = reg->aname = name;
	reg->val.i[0] = no;
	reg->reg = reg;

	if (iname != NULL)
	{
		CALLOC(reg->next);
		reg->next->kind = SK_IRegister;
		reg->next->name = reg->next->aname = iname;
	}

	return reg;
}


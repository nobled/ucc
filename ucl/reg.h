#ifndef __REG_H_
#define __REG_H_

enum { EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI };

#define SK_IRegister (SK_Register + 1)
#define NO_REG -1

void StoreVar(Symbol reg, Symbol v);
void SpillReg(Symbol reg);
void ClearRegs(void);
Symbol CreateReg(char *name, char *iname, int no);
Symbol GetByteReg(void);
Symbol GetWordReg(void);
Symbol GetReg(void);

extern Symbol X86Regs[];
extern Symbol X86ByteRegs[];
extern Symbol X86WordRegs[];
extern int UsedRegs;

#endif

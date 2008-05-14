#ifndef __UCC_H_
#define __UCC_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#include "input.h"
#include "error.h"
#include "alloc.h"
#include "vector.h"
#include "str.h"
#include "lex.h"
#include "type.h"
#include "symbol.h"


#define ALIGN(size, align) ((size + align - 1) & (~(align - 1)))

extern Vector ExtraWhiteSpace;
extern Vector ExtraKeywords;
extern FILE  *ASTFile;
extern FILE  *IRFile;
extern FILE  *ASMFile;
extern char  *ExtName;
extern Heap CurrentHeap;
extern struct heap ProgramHeap;
extern struct heap FileHeap;
extern struct heap StringHeap;
extern int WarningCount;
extern int ErrorCount;

#endif



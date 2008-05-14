#ifndef __UCC_H_
#define __UCC_H_


enum { C_FILE, PP_FILE, ASM_FILE, OBJ_FILE, LIB_FILE, EXE_FILE };

typedef struct list
{
	char *str;
	struct list *next;
} *List;

struct option
{
	List pflags;
	List cflags;
	List aflags;
	List lflags;
	List cfiles;
	List pfiles;
	List afiles;
	List ofiles;
	List lfiles;
	List linput;
	int verbose;
	int oftype;
	char *out;
};

void* Alloc(int len);
char* FileName(char *name, char *ext);
List  ListAppend(List list, char *str);
List  ListCombine(List to, List from);
List  ParseOption(char *opt);
char** BuildCommand(char *cmd[], List flags, List infiles, List outfiles);

void SetupToolChain(void);
int  InvokeProgram(int oftype);

extern char* ExtNames[];
extern List PPFiles;
extern List ASMFiles;
extern List OBJFiles;
extern struct option Option;

#endif

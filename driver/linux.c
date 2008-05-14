#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "ucc.h"

#define _P_WAIT 0
#define UCCDIR "/usr/local/lib/ucc/"

static char *CPPProg[] = 
{ 
	"/usr/bin/gcc", "-U__GNUC__", "-D_POSIX_SOURCE", "-D__STRICT_ANSI__",
	"-Dunix", "-Di386", "-Dlinux", "-D__unix__", "-D__i386__", "-D__linux__", 
	"-D__signed__=signed", "-D_UCC", "-I" UCCDIR "include", "$1", "-E", "$2", "-o", "$3", 0 
};
static char *CCProg[] = 
{
	UCCDIR "ucl", "-ext:.s", "$1", "$2", 0 
};
static char *ASProg[] = 
{ 
	"/usr/bin/as", "-o", "$3", "$1", "$2", 0 
};
static char *LDProg[] = 
{
	"/usr/bin/gcc", "-o", "$3", "$1", "$2", UCCDIR "assert.o", "-lc", "-lm", 0 
};
char *ExtNames[] = { ".c", ".i", ".s", ".o", ".a;.so", 0 };

static int Execute(char **cmd)
{
	int pid, n, status;

	pid = fork();
	if (pid == -1)
	{
		fprintf(stderr, "no more processes\n");
		return 100;
	}
	else if (pid == 0)
	{
		execv(cmd[0], cmd);
		perror(cmd[0]);
		fflush(stdout);
		exit(100);
	}
	while ((n = wait(&status)) != pid && n != -1)
		;
	if (n == -1)
		status = -1;
	if (status & 0xff)
	{
		fprintf(stderr, "fatal error in %s\n", cmd[0]);
		status |= 0x100;
	}

	return (status >> 8) & 0xff;
}

void SetupToolChain(void)
{
}

int InvokeProgram(int oftype)
{
	List p, il, ol;
	char *ofname;
	char **cmd;
	int status = 0;

	switch (oftype)
	{
	case PP_FILE:
		for (p = Option.cfiles; p != NULL; p = p->next)
		{
			ofname = FileName(p->str, ".i");
			PPFiles = ListAppend(PPFiles, ofname);
			il = ListAppend(NULL, p->str);
			ol = ListAppend(NULL, ofname);
			cmd = BuildCommand(CPPProg, Option.pflags, il, ol);
			status = Execute(cmd);
		}
		Option.pfiles = ListCombine(Option.pfiles, PPFiles);
		break;

	case ASM_FILE:
		if (Option.pfiles == NULL)
			return 0;

		for (p = Option.aflags, Option.aflags = NULL; p != NULL; p = p->next)
		{
			Option.aflags = ListCombine(Option.aflags, ParseOption(p->str + 4));
		}
		for (p = Option.pfiles; p != NULL; p = p->next)
		{
			ASMFiles = ListAppend(ASMFiles, FileName(p->str, ".s"));
		}
		Option.afiles = ListCombine(Option.afiles, ASMFiles);
		cmd = BuildCommand(CCProg, Option.cflags, Option.pfiles, ASMFiles);
		status = Execute(cmd);
		break;

	case OBJ_FILE:
		for (p = Option.afiles; p != NULL; p = p->next)
		{
			ofname = FileName(p->str, ".o");
			OBJFiles = ListAppend(OBJFiles, ofname);
			il = ListAppend(NULL, p->str);
			ol = ListAppend(NULL, ofname);
			cmd = BuildCommand(ASProg, Option.aflags, il, ol);
			status = Execute(cmd);
		}
		Option.ofiles = ListCombine(Option.ofiles, OBJFiles);
		break;

	case LIB_FILE:
		return 0;

	case EXE_FILE:
		if (Option.ofiles == NULL)
			return 0;

		if (Option.out == NULL)
			Option.out = "a.out";
		cmd = BuildCommand(LDProg, Option.lflags, Option.linput, ListAppend(NULL, Option.out));
		status = Execute(cmd);
		break;
	}

	return status;
}


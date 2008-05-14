#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>
#include "ucc.h"

static char *CPPProg[] = 
{ 
	"cl", "-nologo", "-u", "-D_WIN32", "-D_M_IX86", "-D_UCC", "$1", "-P", "$2", 0 
};
static char *CCProg[] = 
{
	"ucl", /*1*/"", /*2*/"", "-ext:.asm", "$1", "$2", 0 
};
static char *ASProg[] = 
{ 
	"ml", "-nologo", "-c", "-Cp", "-coff", "-Fo$3", "$1", "$2", 0 
};
static char *LDProg[] = 
{
	"link", "-nologo", "-subsystem:console", "-entry:mainCRTStartup", "$1", /*6*/"-OUT:$3", 
	"$2",   "oldnames.lib", /*8*/"libc.lib", "kernel32.lib", 0
};

char *ExtNames[] = { ".c;.C,", ".i;.I", ".asm;.ASM;.s;.S", ".obj;.OBJ", ".lib", 0 };

void SetupToolChain(void)
{
	char *dir;

	if ((dir = getenv("VS90COMNTOOLS")) != NULL)
	{
		LDProg[8] = "libcmt.lib";
		goto ok;
	}

	if ((dir = getenv("VS80COMNTOOLS")) != NULL)
	{
		LDProg[8] = "libcmt.lib";
		goto ok;
	}

	if ((dir = getenv("VS71COMNTOOLS")) != NULL)
		goto ok;

	return;

ok:
	CCProg[1] = "-ignore __declspec(deprecated),__declspec(noreturn),__inline,__fastcall";
	CCProg[2] = "-keyword __int64";
}

int InvokeProgram(int oftype)
{
	List p = NULL;
	char **cmd;
	char *file;
	int status = 0;

	switch (oftype)
	{
	case PP_FILE:
		if (Option.cfiles == NULL)
			return 0;

		for (p = Option.cfiles; p != NULL; p = p->next)
		{
			PPFiles = ListAppend(PPFiles, FileName(p->str, ".i"));	
		}

		Option.pfiles = ListCombine(Option.pfiles, PPFiles);
		cmd = BuildCommand(CPPProg, Option.pflags, Option.cfiles, PPFiles);
		status = _spawnvp(_P_WAIT, cmd[0], cmd);

		for (p = PPFiles; p != NULL; p = p->next)
		{
			if ((file = strrchr(p->str, '\\')) || (file = strrchr(p->str, '/')))
			{
				rename(file + 1, p->str);
			}
		}
		break;

	case ASM_FILE:
		if (Option.pfiles == NULL)
			return 0;

		for (p = Option.pfiles; p != NULL; p = p->next)
		{
			ASMFiles = ListAppend(ASMFiles, FileName(p->str, ".asm"));
		}

		Option.afiles = ListCombine(Option.afiles, ASMFiles);
		cmd = BuildCommand(CCProg, Option.cflags, Option.pfiles, ASMFiles);
		status = _spawnvp(_P_WAIT, cmd[0], cmd);
		break;

	case OBJ_FILE:
		if (Option.afiles == NULL)
			return 0;

		for (p = Option.aflags, Option.aflags = NULL; p != NULL; p = p->next)
		{
			Option.aflags = ListCombine(Option.aflags, ParseOption(p->str + 4));
		}
		for (p = Option.afiles; p != NULL; p = p->next)
		{
			file = FileName(p->str, ".obj");
			OBJFiles = ListAppend(OBJFiles, file);
			cmd = BuildCommand(ASProg, Option.aflags, ListAppend(NULL, p->str), ListAppend(NULL, file));
			status = _spawnvp(_P_WAIT, cmd[0], cmd);
		}
		Option.ofiles = ListCombine(Option.ofiles, OBJFiles);
		break;

	case LIB_FILE:
		break;

	case EXE_FILE:
		if (Option.ofiles == NULL)
			return 0;

		if (Option.out == NULL)
		{
			Option.out = Option.ofiles->str;
		}
		Option.out = FileName(Option.out, ".exe");
		for (p = Option.lflags, Option.lflags = NULL; p != NULL; p = p->next)
		{
			Option.lflags = ListCombine(Option.lflags, ParseOption(p->str + 4));
		}
		cmd = BuildCommand(LDProg, Option.lflags, Option.linput, ListAppend(NULL, Option.out));
		status = _spawnvp(_P_WAIT, cmd[0], cmd);
		break;
	}

	return status;
}

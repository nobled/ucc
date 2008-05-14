#include "ucl.h"
#include "ast.h"
#include "target.h"

static int DumpAST;
static int DumpIR;

Vector ExtraWhiteSpace;
Vector ExtraKeywords;
FILE *ASTFile;
FILE *IRFile;
FILE *ASMFile;
char *ExtName = ".s";
Heap CurrentHeap;
HEAP(ProgramHeap);
HEAP(FileHeap);
HEAP(StringHeap);
int WarningCount;
int ErrorCount;

static void Initialize(void)
{
	CurrentHeap = &FileHeap;
	ErrorCount = WarningCount = 0;
	InitSymbolTable();
	ASTFile = IRFile = ASMFile = NULL;
}

static void Finalize(void)
{
	FreeHeap(&FileHeap);
}

static void Compile(char *file)
{
	AstTranslationUnit transUnit;

	Initialize();

	transUnit = ParseTranslationUnit(file);

	CheckTranslationUnit(transUnit);

	if (ErrorCount != 0)
		goto exit;

	if (DumpAST)
	{
		DumpTranslationUnit(transUnit);
	}

	Translate(transUnit);

	if (DumpIR)
	{
		DAssemTranslationUnit(transUnit);
	}

	EmitTranslationUnit(transUnit);

exit:
	Finalize();
}

static void AddWhiteSpace(char *str)
{
	char *p, *q;

	ExtraWhiteSpace = CreateVector(1);
	p = str;
	while ((q = strchr(p, ',')) != NULL)
	{
		*q = 0;
		INSERT_ITEM(ExtraWhiteSpace, p);
		p = q + 1;
	}
	INSERT_ITEM(ExtraWhiteSpace, p);
}

static void AddKeyword(char *str)
{
	char *p, *q;

	ExtraKeywords = CreateVector(1);
	p = str;
	while ((q = strchr(p, ',')) != NULL)
	{
		*q = 0;
		INSERT_ITEM(ExtraKeywords, p);
		p = q + 1;
	}
	INSERT_ITEM(ExtraKeywords, p);
}

static int ParseCommandLine(int argc, char *argv[])
{
	int i;

	for (i = 0; i < argc; ++i)
	{
		if (strncmp(argv[i], "-ext:", 5) == 0)
		{
			ExtName = argv[i] + 5;
		}
		else if (strcmp(argv[i], "-ignore") == 0)
		{
			i++;
			AddWhiteSpace(argv[i]);
		}
		else if (strcmp(argv[i], "-keyword") == 0)
		{
			i++;
			AddKeyword(argv[i]);
		}
		else if (strcmp(argv[i], "--dump-ast") == 0)
		{
			DumpAST = 1;
		}
		else if (strcmp(argv[i], "--dump-IR") == 0)
		{
			DumpIR = 1;
		} 
		else
			return i;
	}
	return i;
}

int main(int argc, char *argv[])
{
	int i;

	CurrentHeap = &ProgramHeap;
	argc--; argv++;
	i = ParseCommandLine(argc, argv);

	SetupRegisters();
	SetupLexer();
	SetupTypeSystem();
	for (; i < argc; ++i)
	{
		Compile(argv[i]);
	}

	return (ErrorCount != 0);
}



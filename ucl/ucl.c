#include "ucl.h"
#include "ast.h"
#include "target.h"

// flag to control if dump abstract syntax tree
static int DumpAST;
// flag to control if dump intermediate code
static int DumpIR;
// file to hold abstract synatx tree
FILE *ASTFile;
// file to hold intermediate code
FILE *IRFile;
// file to hold assembly code
FILE *ASMFile;
// assembly file's extension name
char *ExtName = ".s";
// please see alloc.h, defaultly, ALLOC() allocates memory from CurrentHeap
Heap CurrentHeap;
// hold memory whose lifetime is the whole program
HEAP(ProgramHeap);
// hold memory whose lifetime is the whole file
HEAP(FileHeap);
// all the strings and identifiers are hold in StringHeap, the lifetime is the whole program 
HEAP(StringHeap);
// number of warnings in a file
int WarningCount;
// number of errors in a file 
int ErrorCount;
Vector ExtraWhiteSpace;
Vector ExtraKeywords;

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

	// parse preprocessed C file, generate an abstract syntax tree
	transUnit = ParseTranslationUnit(file);

	// perform semantic check on abstract synatx tree
	CheckTranslationUnit(transUnit);

	if (ErrorCount != 0)
		goto exit;

	if (DumpAST)
	{
		DumpTranslationUnit(transUnit);
	}

	// translate the abstract synatx tree into intermediate code
	Translate(transUnit);

	if (DumpIR)
	{
		DAssemTranslationUnit(transUnit);
	}

	// emit assembly code from intermediate code
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


/**
 * The compiler's main entry point. 
 * The compiler handles C files one by one.
 */
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



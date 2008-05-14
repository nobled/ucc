#include "ucl.h"
#include "ast.h"

int CurrentToken;

void Expect(int tok)
{
	if (CurrentToken == tok)
	{
		NEXT_TOKEN;
		return;
	}

	if (tok == TK_SEMICOLON && TokenCoord.line - PrevCoord.line == 1)
	{
		Error(&PrevCoord, "Expect ;");
	}
	else
	{
		Error(&TokenCoord, "Expect %s", TokenStrings[tok - 1]);
	}
}

int CurrentTokenIn(int toks[])
{
	int *p = toks;

	while (*p)
	{
		if (CurrentToken == *p)
			return 1;
		p++;
	}

	return 0;
}

void SkipTo(int toks[], char *einfo)
{
	int *p = toks;
	struct coord cord;

	if (CurrentTokenIn(toks) || CurrentToken == TK_END)
		return;

	cord = TokenCoord;
	while (CurrentToken != TK_END)
	{
		p = toks;
		while (*p)
		{
			if (CurrentToken == *p)
				goto sync;
			p++;
		}
		NEXT_TOKEN;
	}

sync:
	Error(&cord, "skip to %s\n", einfo);
}

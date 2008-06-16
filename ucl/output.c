#include "ucl.h"
#include "output.h"
#include "target.h"

#define BUF_LEN 4096

#define PUT_CHAR(c)            \
if (BufferSize + 1 > BUF_LEN)  \
{                              \
    Flush();                   \
}                              \
OutBuffer[BufferSize++] = c;

char OutBuffer[BUF_LEN];
int BufferSize;

void LeftAlign(FILE *file, int pos)
{
	char spaces[256];
	
	pos = pos >= 256 ? 2 : pos;
	memset(spaces, ' ', pos);
	spaces[pos] = '\0';
	if (file != ASMFile)
	{
		fprintf(file, "\n%s", spaces);
	}
	else
	{
		PutString(spaces);
	}
}

/**
 * Open a file for writing. The file's name is 
 * formed by filename's base name and ext
 */
FILE* CreateOutput(char *filename, char *ext)
{
	char tmp[256];
	char *p = tmp;

	while (*filename && *filename != '.')
		*p++ = *filename++;
	strcpy(p, ext);
	
	return fopen(tmp, "w");
}

/**
 * Format a name
 */
char* FormatName(const char *fmt, ...)
{
	char buf[256];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	va_end(ap);

	return InternName(buf, strlen(buf));
}

/**
 * Formated print to output
 */
void Print(const char *fmt, ...)
{
	char tmp[1024];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(tmp, fmt, ap);
	va_end(ap);
	PutString(tmp);
}

void PutString(char *s)
{
	int len = strlen(s);
	int i;
	char *p;

	if (len > BUF_LEN)
	{
		fwrite(s, 1, len, ASMFile);
		return;
	}
	
	if (len > BUF_LEN - BufferSize)
	{
		Flush();
	}

	p = OutBuffer + BufferSize;
	for (i = 0; i < len; ++i)
	{
		p[i] = s[i];
	}
	BufferSize += len;
}

void PutChar(int ch)
{
	PUT_CHAR(ch);
}

void Flush(void)
{
	if (BufferSize != 0)
	{
		fwrite(OutBuffer, 1, BufferSize, ASMFile);
	}
	BufferSize = 0;
}


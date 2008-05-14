#include "ucl.h"

void Error(Coord coord, const char *format, ...)
{
	va_list ap;

	ErrorCount++;
	if (coord)
	{
		fprintf(stderr, "(%s,%d):", coord->filename, coord->ppline);
	}
	fprintf(stderr, "error:");
	va_start(ap, format);
	vfprintf(stderr, format, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

void Fatal(const char *format, ...)
{
	va_list ap;

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	exit(-1);
}

void Warning(Coord coord, const char *format, ...)
{
	va_list ap;

	WarningCount++;
	if (coord)
	{
		fprintf(stderr, "(%s,%d):", coord->filename, coord->ppline);
	}

	fprintf(stderr, "warning:");
	va_start(ap, format);
	vfprintf(stderr, format, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	return;
}




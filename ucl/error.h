#ifndef __ERROR_H_
#define __ERROR_H_

void Error(Coord coord, const char *format, ...);
void Fatal(const char *format, ...);
void Warning(Coord coord, const char *format, ...);

#endif


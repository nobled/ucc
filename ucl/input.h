#ifndef __INPUT_H_
#define __INPUT_H_

typedef struct coord
{
	char *filename;
	int ppline;
	int line;
	int col;
} *Coord;

struct input
{
	char *filename;
	unsigned char *base;
	unsigned char *cursor;
	unsigned char *lineHead;
	int line;
	void* file;
	void* fileMapping;
	unsigned long size;
};

extern unsigned char END_OF_FILE;
extern struct input Input;

void ReadSourceFile(char *fileName);
void CloseSourceFile(void);

#endif


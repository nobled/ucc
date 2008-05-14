#include "ucl.h"
#include "lex.h"

static NameBucket NameBuckets[NAME_HASH_MASK + 1];

static unsigned int ELFHash(char *str, int len)
{
	unsigned int h = 0;
	unsigned int x = 0;
	int i;

	for (i = 0; i < len; ++i)
	{
		h = (h << 4) + *str++;
		if ((x = h & 0xF0000000) != 0)
		{
			h ^= x >> 24;
			h &= ~x;
		}
	}

	return h;
}

char* InternName(char *id, int len)
{
	int i;
	int h;
	NameBucket p;

	h = ELFHash(id, len) & NAME_HASH_MASK;
	for (p = NameBuckets[h]; p != NULL; p = p->link)
	{
		if (len == p->len && strncmp(id, p->name, len) == 0)
			return p->name;
	}
	
	p = HeapAllocate(&StringHeap, sizeof(*p));
	p->name = HeapAllocate(&StringHeap, len + 1);
	for (i = 0; i < len; ++i)
	{
		p->name[i] = id[i];
	}
	p->name[len] = 0;
	p->len = len;

	p->link = NameBuckets[h];
	NameBuckets[h] = p;

	return p->name;
}

void AppendSTR(String str, char *tmp, int len, int wide)
{
	int i, size;
	char *p;
	int times = 1;

	size = str->len + len + 1;
	if (wide) times = 4;

	p = HeapAllocate(&StringHeap, size * times);
	for (i = 0; i < str->len * times; ++i)
	{
		p[i] = str->chs[i];
	}
	for (i = 0; i < len * times; ++i)
	{
		p[i] = tmp[i];
	}
	str->chs = p;
	str->len = size - 1;
	if (! wide)
	{
		str->chs[size - 1] = 0;
	}
	else
	{
		int *wcp = (int *)p + (size - 1);
		*wcp = 0;
	}
}

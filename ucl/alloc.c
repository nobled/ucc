#include "ucl.h"

static struct mblock *FreeBlocks;

void InitHeap(Heap hp)
{
	hp->head.next = NULL;
	hp->head.begin = hp->head.end = hp->head.avail = NULL;
	hp->last = &hp->head;	
}

void* HeapAllocate(Heap hp, int size)
{
	struct mblock *blk = NULL;

	size = ALIGN(size, sizeof(union align));

	blk = hp->last;
	while (size > blk->end - blk->avail)
	{
		if ((blk->next = FreeBlocks) != NULL)
		{
			FreeBlocks = FreeBlocks->next;
			blk = blk->next;
		}
		else
		{
			int m = size + MBLOCK_SIZE + sizeof(struct mblock);

			blk->next = (struct mblock *)malloc(m);
			blk = blk->next;
			if (blk == NULL)
			{
				Fatal("Memory exhausted");
			}
			blk->end = (char *)blk + m;
		}

		blk->avail = blk->begin = (char *)(blk + 1);
		blk->next = NULL;
		hp->last = blk;
	}

	blk->avail += size;

	return blk->avail - size;
}

void FreeHeap(Heap hp)
{
	hp->last->next = FreeBlocks;
	FreeBlocks = hp->head.next;
	InitHeap(hp);
}

#include "ucl.h"
#include "gen.h"

CFGEdge PreOrder;

void AppendBBlock(CFGEdge *pprev, BBlock bb)
{
	CFGEdge e;

	while (*pprev != NULL)
		pprev = &(*pprev)->next;
	
	ALLOC(e);
	e->bb = bb;
	e->next = NULL;
	*pprev = e;	
}

void DFS(BBlock bb)
{
	CFGEdge succ = bb->succs;

	AppendBBlock(&PreOrder, bb);
	bb->accessed = 1;
	while (succ != NULL)
	{
		if (! succ->bb->accessed )
			DFS(succ->bb);
		succ = succ->next;
	}
}

void Dominator(int nbbs)
{
	CFGEdge e = PreOrder;
	CFGEdge pred;
	int update = 1;
	int n = (nbbs + 31) / 32;
	int i;

	while (e != NULL)
	{
		e->bb->dom = HeapAllocate(CurrentHeap, n * sizeof(int));
		for (i = 0; i < n; ++i)
			e->bb->dom[i] = -1;
		e = e->next;
	}
	for (i = 0; i < n; ++i)
		PreOrder->bb->dom[i] = 0;
	PreOrder->bb->dom[0] = 1;

	while (update)
	{
		update = 0;
		e = PreOrder;
		i = 0;
		while (e != NULL)
		{
			pred = e->bb->preds;
			while (pred != NULL)
			{
				if (pred->bb->dom)
					update |= Join(e->bb->dom, pred->bb->dom, n, i);
				pred = pred->next;
			}
			e = e->next;
			i++;
		}
	}
}

#define SET_BIT(bits, no)   bits[no / 32] | (1 << (no % 32))
#define CLEAR_BIT(bits, no) bits[no / 32] & ~ (1 << (no % 32))

int Join(int bv1[], int bv2[], int n, int pos)
{
	int update = 0;
	int i;
	int v;

	bv1[pos / 32] &= ~ (1 << (pos % 32));
	for (i = 0; i < n; ++i)
	{
		v = bv1[i] & bv2[i];
		if (v != bv1[i])
		{
			update = 1;
			bv1[i] = v;
		}
	}
	bv1[pos / 32] |= (1 << (pos % 32));
	return update;
}


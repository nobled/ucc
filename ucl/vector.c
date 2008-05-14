#include "ucl.h"

Vector CreateVector(int size)
{
	Vector v = NULL;

	ALLOC(v);
	v->data = (void **)HeapAllocate(CurrentHeap, size * sizeof(void *));
	v->size = size;
	v->len = 0;
	return v;
}

void ExpandVector(Vector v)
{
	void *orig;

	orig = v->data;
	v->data = HeapAllocate(CurrentHeap, v->size * 2 * sizeof(void *));
	memcpy(v->data, orig, v->size * sizeof(void *));
	v->size *= 2;
}


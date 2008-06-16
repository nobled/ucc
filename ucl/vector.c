#include "ucl.h"

/**
 * Create a vector with the given size
 * @param size vector's size
 */
Vector CreateVector(int size)
{
	Vector v = NULL;

	ALLOC(v);
	v->data = (void **)HeapAllocate(CurrentHeap, size * sizeof(void *));
	v->size = size;
	v->len = 0;
	return v;
}

/**
 * Double the vector's size to hold more data
 */
void ExpandVector(Vector v)
{
	void *orig;

	orig = v->data;
	v->data = HeapAllocate(CurrentHeap, v->size * 2 * sizeof(void *));
	memcpy(v->data, orig, v->size * sizeof(void *));
	v->size *= 2;
}


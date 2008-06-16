#include "ucl.h"
#include "output.h"
#include "type.h"
#include "config.h"

struct type Types[VOID - CHAR + 1];
Type DefaultFunctionType;
Type WCharType;

/**
 * Apply default argument promotion to a type ty
 */
Type Promote(Type ty)
{
	return ty->categ < INT ? T(INT) : (ty->categ == FLOAT ? T(DOUBLE) : ty);
}

/**
 * Check if two functions are compatible.
 * If compatible, return 1; otherwise, return 0
 */
static int IsCompatibleFunction(FunctionType fty1, FunctionType fty2)
{
	Signature sig1 = fty1->sig;
	Signature sig2 = fty2->sig;
	Parameter p1, p2;
	int parLen1, parLen2;
	int i;

	// return types shall be compatible
	if (! IsCompatibleType(fty1->bty, fty2->bty))
		return 0;

	if (! sig1->hasProto && ! sig2->hasProto)
	{
		return 1;
	}
	else if (sig1->hasProto && sig2->hasProto)
	{
		parLen1 = LEN(sig1->params);
		parLen2 = LEN(sig2->params);

		if ((sig1->hasEllipse ^ sig2->hasEllipse) || (parLen1 != parLen2))
			return 0;

		for (i = 0; i < parLen1; ++i)
		{
			p1 = (Parameter)GET_ITEM(sig1->params, i);
			p2 = (Parameter)GET_ITEM(sig2->params, i);

			if (! IsCompatibleType(p1->ty, p2->ty))
				return 0;
		}

		return 1;
	}
	else if (! sig1->hasProto && sig2->hasProto)
	{
		sig1 = fty2->sig;
		sig2 = fty1->sig;
		goto mix_proto;
	}
	else
	{
mix_proto:
		parLen1 = LEN(sig1->params);
		parLen2 = LEN(sig2->params);

		if (sig1->hasEllipse)
			return 0;

		if (parLen2 == 0)
		{
			FOR_EACH_ITEM(Parameter, p1, sig1->params)
				if (! IsCompatibleType(Promote(p1->ty), p1->ty))
					return 0;
			ENDFOR

			return 1;
		}
		else if (parLen1 != parLen2)
		{
			return 0;
		}
		else
		{
			for (i = 0; i < parLen1; ++i)
			{
				p1 = (Parameter)GET_ITEM(sig1->params, i);
				p2 = (Parameter)GET_ITEM(sig2->params, i);

				if (! IsCompatibleType(p1->ty, Promote(p2->ty)))
					return 0;
			}		
			return 1;
		}
	}				
}

/**
 * Check if two types are compatible
 * If compatible, return 1; otherwise, return 0
 */
int IsCompatibleType(Type ty1, Type ty2)
{
	if (ty1 == ty2)
		return 1;

	if (ty1->qual != ty2->qual)
		return 0;

	ty1 = Unqual(ty1);
	ty2 = Unqual(ty2);

	if (ty1->categ == ENUM && ty2 == ty1->bty ||
		ty2->categ == ENUM && ty1 == ty2->bty)
		return 1;

	if (ty1->categ != ty2->categ)
		return 0;

	switch (ty1->categ)
	{
	case POINTER:
		return IsCompatibleType(ty1->bty, ty2->bty);

	case ARRAY:
		return IsCompatibleType(ty1->bty, ty2->bty) && 
		       (ty1->size == ty2->size || ty1->size == 0 || ty2->size == 0);

	case FUNCTION:
		return IsCompatibleFunction((FunctionType)ty1, (FunctionType)ty2);

	default:
		return ty1 == ty2;
	}
}

/**
 * A very important principle: for those type constructing functions, they must keep
 * the integrity of the base type.
 */

/**
 * Construct an enumeration type whose name is id. id can be null
 */
Type Enum(char *id)
{
	EnumType ety;

	ALLOC(ety);

	ety->categ = ENUM;
	ety->id = id;

	// enumeration type is compatilbe with int
	ety->bty = T(INT);
	ety->size = ety->bty->size;
	ety->align = ety->bty->align;
	ety->qual = 0;

	return (Type)ety;
}

/**
 * Contruct a qualified type
 */
Type Qualify(int qual, Type ty)
{
	Type qty;

	if (qual == 0 || qual == ty->qual)
		return ty;

	ALLOC(qty);
	*qty = *ty;
	qty->qual |= qual;
	if (ty->qual != 0)
	{
		qty->bty = ty->bty;
	}
	else
	{
		qty->bty = ty;
	}
	return qty;
}

/**
 * Get a type's unqualified version
 */
Type Unqual(Type ty)
{
	if (ty->qual)
		ty = ty->bty;
	return ty;
}

/**
 * Construct an array type, len is the array length.
 */
Type ArrayOf(int len, Type ty)
{
	Type aty;

	ALLOC(aty);

	aty->categ = ARRAY;
	aty->qual = 0;
	aty->size = len * ty->size;
	aty->align = ty->align;
	aty->bty = ty;

	return (Type)aty;
}

/**
 * Construct a type pointing to ty.
 */
Type PointerTo(Type ty)
{
	Type pty;

	ALLOC(pty);

	pty->categ  = POINTER;
	pty->qual = 0;
	pty->align = T(POINTER)->align;
	pty->size = T(POINTER)->size;
	pty->bty = ty;

	return pty;
}

/**
 * Construct a function type, the return type is ty. 
 */
Type FunctionReturn(Type ty, Signature sig)
{
	FunctionType fty;

	ALLOC(fty);

	fty->categ = FUNCTION;
	fty->qual = 0;
	fty->size = T(POINTER)->size;
	fty->align = T(POINTER)->align;
	fty->sig = sig;
	fty->bty = ty;

	return (Type)fty;
}

/**
 * Construct a struct/union type whose name is id, id can be NULL.
 */
Type StartRecord(char *id, int categ)
{
	RecordType rty;

	ALLOC(rty);
	memset(rty, 0, sizeof(*rty));

	rty->categ = categ;
	rty->id = id;
	rty->tail= &rty->flds;

	return (Type)rty;
}

/**
 * Add a field to struct/union type ty. fty is the type of the field.
 * id is the name of the field. If the field is a bit-field, bits is its
 * bit width, for non bit-field, bits will be 0.
 */
Field AddField(Type ty, char *id, Type fty, int bits)
{
	RecordType rty = (RecordType)ty;
	Field fld;

	if (fty->size == 0)
	{
		assert(fty->categ == ARRAY);
		rty->hasFlexArray = 1;
	}
	if (fty->qual & CONST)
	{
		rty->hasConstFld = 1;
	}

	ALLOC(fld);

	fld->id = id;
	fld->ty = fty;
	fld->bits = bits;
	fld->pos = fld->offset = 0;
	fld->next = NULL;

	*rty->tail = fld;
	rty->tail  = &(fld->next);

	return fld;
}

/**
 * Lookup if there is a field named id in struct/union type
 */
Field LookupField(Type ty, char *id)
{
	RecordType rty = (RecordType)ty;
	Field fld = rty->flds;

	while (fld != NULL)
	{
		// unnamed struct/union field in a struct/union
		if (fld->id == NULL && IsRecordType(fld->ty))
		{
			Field p;

			p = LookupField(fld->ty, id);
			if (p)
				return p;
		}
		else if (fld->id == id)
			return fld;

		fld = fld->next;
	}

	return NULL;
}

/**
 * For unamed struct/union field in a struct/union, the offset of fields in the 
 * unnamed struct/union should be fix up.
 */
void AddOffset(RecordType rty, int offset)
{
	Field fld = rty->flds;


	while (fld)
	{
		fld->offset += offset;
		if (fld->id == NULL && IsRecordType(fld->ty))
		{
			AddOffset((RecordType)fld->ty, fld->offset);
		}
		fld = fld->next;
	}

}

/**
 * When a struct/union type's delcaration is finished, layout the struct/union.
 * Calculate each field's offset from the beginning of struct/union and the size
 * and alignment of struct/union.
 */
void EndRecord(Type ty)
{
	RecordType rty = (RecordType)ty;
	Field fld = rty->flds;
	int bits = 0;
	int intBits = T(INT)->size * 8;

	if (rty->categ == STRUCT)
	{
		while (fld)
		{
			// align each field
			fld->offset = rty->size = ALIGN(rty->size, fld->ty->align);
			if (fld->id == NULL && IsRecordType(fld->ty))
			{
				AddOffset((RecordType)fld->ty, fld->offset);
			}
			if (fld->bits == 0)
			{
				/// if current field is not a bit-field, whenever last field is bit-field or not, 
				/// it will occupy a chunk of memory of its size.
				if (bits != 0)
				{
					fld->offset = rty->size = ALIGN(rty->size + T(INT)->size, fld->ty->align);
				}
				bits = 0;
				rty->size = rty->size + fld->ty->size;
			}
			else if (bits + fld->bits <= intBits)
			{
				// current bit-field with previous bit-fields can be placed together
				fld->pos = LITTLE_ENDIAN ? bits : intBits - bits;
				bits = bits + fld->bits;
				if (bits == intBits)
				{
					rty->size += T(INT)->size;
					bits = 0;
				}
			}
			else
			{
				/// current bit-field can't be placed together with previous bit-fields,
				/// must start a new chunk of memory
				rty->size += T(INT)->size;
				fld->offset += T(INT)->size;
				fld->pos = LITTLE_ENDIAN ? 0 : intBits - fld->bits;
				bits = fld->bits;
			}
			if (fld->ty->align > rty->align)
			{
				rty->align = fld->ty->align;
			}

			fld = fld->next;
		}
		if (bits != 0)
		{
			rty->size += T(INT)->size;
		}
		rty->size = ALIGN(rty->size, rty->align);
	}
	else
	{
		while (fld)
		{
			if (fld->ty->align > rty->align)
			{
				rty->align = fld->ty->align;
			}
			if (fld->ty->size > rty->size)
			{
				rty->size = fld->ty->size;
			}
			fld = fld->next;
		}
	}
}

/**
 * Return the composite type of ty1 and ty2.
 */
Type CompositeType(Type ty1, Type ty2)
{
	// ty1 and ty2 must be compatible when calling this function
	assert(IsCompatibleType(ty1, ty2));

	if (ty1->categ == ENUM)
		return ty1;

	if (ty2->categ == ENUM)
		return ty2;

	switch (ty1->categ)
	{
	case POINTER:
		return Qualify(ty1->qual, PointerTo(CompositeType(ty1->bty, ty2->bty)));

	case ARRAY:
		return ty1->size != 0 ? ty1 : ty2;

	case FUNCTION:
		{
			FunctionType fty1 = (FunctionType)ty1;
			FunctionType fty2 = (FunctionType)ty2;

			fty1->bty = CompositeType(fty1->bty, fty2->bty);
			if (fty1->sig->hasProto && fty2->sig->hasProto)
			{
				Parameter p1, p2;
				int i, len = LEN(fty1->sig->params);

				for (i = 0; i < len; ++i)
				{
					p1 = (Parameter)GET_ITEM(fty1->sig->params, i);
					p2 = (Parameter)GET_ITEM(fty2->sig->params, i);
					p1->ty = CompositeType(p1->ty, p2->ty);
				}

				return ty1;
			}

			return fty1->sig->hasProto ? ty1 : ty2;
		}

	default:
		return ty1;
	}
}

/**
 * Return the common real type for ty1 and ty2
 */
Type CommonRealType(Type ty1, Type ty2)
{
	if (ty1->categ == LONGDOUBLE || ty2->categ == LONGDOUBLE)
		return T(LONGDOUBLE);

	if (ty1->categ == DOUBLE || ty2->categ == DOUBLE)
		return T(DOUBLE);

	if (ty1->categ == FLOAT || ty2->categ == FLOAT)
		return T(FLOAT);

	ty1 = ty1->categ < INT ? T(INT) : ty1;
	ty2 = ty2->categ < INT ? T(INT) : ty2;

	if (ty1->categ == ty2->categ)
		return ty1;

	if ((IsUnsigned(ty1) ^ IsUnsigned(ty2)) == 0)
		return ty1->categ > ty2->categ ? ty1 : ty2;

	if (IsUnsigned(ty2))
	{
		Type ty;

		ty = ty1;
		ty1 = ty2;
		ty2 = ty;
	}

	if (ty1->categ  >= ty2->categ)
		return ty1;

	if (ty2->size > ty1->size)
		return ty2;

	return T(ty2->categ + 1);

}

/**
 * Adjust paramter type
 */
Type AdjustParameter(Type ty)
{
	ty = Unqual(ty);

	if (ty->categ == ARRAY)
		return PointerTo(ty->bty);

	if (ty->categ == FUNCTION)
		return PointerTo(ty);

	return ty;
}

/**
 * Although int and long are different types from C language, but from some underlying implementations,
 * the size and representation of them maybe identical. Actually, the function should be provided by 
 * the target. UCC defines a series of type code: 
 * I1: signed 1 byte       U1: unsigned 1 byte
 * I2: signed 2 byte       U2: unsigned 2 byte
 * I4: signed 4 byte       U4: unsigned 4 byte
 * F4: 4 byte floating     F8: 8 byte floating
 * V: no type              B: memory block, used for struct/union and array.
 * The target should provide TypeCode to define the map from type category to type code. And UCC can
 * add more type codes on demand of different targets.
 */
int TypeCode(Type ty)
{
	static int optypes[] = {I1, U1, I2, U2, I4, U4, I4, U4, I4, U4, I4, F4, F8, F8, U4, V, B, B, B};

	assert(ty->categ != FUNCTION);
	return optypes[ty->categ];
}

/**
 * Get the type's user-readable string.
 */
char* TypeToString(Type ty)
{
	int qual;
	char *str;
	char *names[] = 
	{
		"char", "unsigned char", "short", "unsigned short", "int", "unsigned int",
		"long", "unsigned long", "long long", "unsigned long long", "enum", "float", "double",
		"long double"
	};

	if (ty->qual != 0)
	{
		qual = ty->qual;
		ty = Unqual(ty);

		if (qual == CONST)
			str = "const";
		else if (qual == VOLATILE)
			str = "volatile";
		else 
			str = "const volatile";

		return FormatName("%s %s", str, TypeToString(ty));
	}

	if (ty->categ >= CHAR && ty->categ <= LONGDOUBLE && ty->categ != ENUM)
		return names[ty->categ];

	switch (ty->categ)
	{
	case ENUM:
		return FormatName("enum %s", ((EnumType)ty)->id);

	case POINTER:
		return FormatName("%s *", TypeToString(ty->bty));

	case UNION:
		return FormatName("union %s", ((RecordType)ty)->id);

	case STRUCT:
		return FormatName("struct %s", ((RecordType)ty)->id);

	case ARRAY:
		return FormatName("%s[%d]", TypeToString(ty->bty), ty->size / ty->bty->size);

	case VOID:
		return "void";

	case FUNCTION:
		{
			FunctionType fty = (FunctionType)ty;

			return FormatName("%s ()", TypeToString(fty->bty));
		}

	default:
		assert(0);
		return NULL;
	}
}

/**
 * Setup the type system according to the target configuration.
 */
void SetupTypeSystem(void)
{
	int i;
	FunctionType fty;

	T(CHAR)->size = T(UCHAR)->size = CHAR_SIZE;
	T(SHORT)->size = T(USHORT)->size = SHORT_SIZE;
	T(INT)->size = T(UINT)->size = INT_SIZE;
	T(LONG)->size = T(ULONG)->size = LONG_SIZE;
	T(LONGLONG)->size = T(ULONGLONG)->size = LONG_LONG_SIZE;
	T(FLOAT)->size = FLOAT_SIZE;
	T(DOUBLE)->size = DOUBLE_SIZE;
	T(LONGDOUBLE)->size = LONG_DOUBLE_SIZE;
	T(POINTER)->size = INT_SIZE;

	for (i = CHAR; i <= VOID; ++i)
	{
		T(i)->categ = i;
		T(i)->align = T(i)->size;
	}

	ALLOC(fty);
	fty->categ = FUNCTION;
	fty->qual = 0;
	fty->align = fty->size = T(POINTER)->size;
	fty->bty = T(INT);
	ALLOC(fty->sig);
	CALLOC(fty->sig->params);
	fty->sig->hasProto = 0;
	fty->sig->hasEllipse = 0;

	DefaultFunctionType = (Type)fty;
	WCharType = T(WCHAR);
}


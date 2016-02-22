#pragma once

#include "SWI-cpp-m.h"
#include "Brush.h"

class PlBrush
{
	Brush * b;
public:
	PlBrush(Brush * b) : b(b) {}
	PlBrush(PlTerm t) { if (!(t = Functor())) throw PlTypeError("brush", t); b = Cast(t[1]); }
	static PlFunctor Functor() { static PlFunctor brush("brush", 1); return brush; }
	static Brush * Cast(PlTerm t) { return reinterpret_cast<Brush *>(static_cast<void *>(t)); }

	bool operator = (PlTerm t) { if (!(t = Functor())) return false; return t[1] = b; }
	operator Brush *() { return b; }
	Brush * operator ->() { return b; }
};


#define PREDICATE_CONT(Module, Prop, Cont) \
PREDICATE_NONDET_M(Module, Prop, 1)\
{\
	auto call = PL_foreign_control(handle);\
	if (call == PL_PRUNED)\
		return true;\
	PlTerm t = A1;\
	if (!(t = PlBrush::Functor()))\
		return false;\
	PlTerm br = t[1];\
	if (br.type() != PL_VARIABLE)\
		return std::find(Cont.begin(), Cont.end(), PlBrush::Cast(br)) != Cont.end();\
	size_t idx = call == PL_FIRST_CALL ? 0 : PL_foreign_context(handle);\
	if (idx < g_map.objects.size() && (br = Cont[idx]))\
		if (++idx == g_map.objects.size())\
			return true;\
		else\
			PL_retry(idx);\
	return false;\
}
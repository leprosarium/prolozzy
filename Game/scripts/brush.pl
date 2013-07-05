:- module(brush, [var/3,
		  varDef/3,
		  new/1]).


var(BrushIdx, Var, Val) :-
	varDef(Var, VarIdx, _),
	map:brushVar(BrushIdx, VarIdx, Val).

varDef(type,	16, 0).
varDef(layer,	0, 0).
varDef(x,	1, 0).
varDef(y,	2, 0).
varDef(w,	3, 0).
varDef(h,	4, 0).
varDef(tile,	5, -1).
varDef(frame,	6, 0).
varDef(x1,	7, 0).
varDef(y1,	8, 0).
varDef(x2,	9, 0).
varDef(y2,	10, 0).
varDef(flip,	11, 0).
varDef(color,	12, 0xffffffff).
varDef(shader,	13, 0).
varDef(scale,	14, 0).
varDef(id,	17, 0).
varDef(material, 18, 0).
varDef(draw,	19, 0).
varDef(disable, 20, 0).
varDef(delay,	21, 0).
varDef(anim,	22, 0).
varDef(collider, 23, 0).
varDef(class,	24, 0).
varDef(status, 25, 0).
varDef(target,  26, 0).
varDef(death,	27, 0).
varDef(collision,	31, 0).
varDef(user(N),	ID, 0) :-
	between(0, 15, N),
	ID is 32 + N.

setBrush(ObjIdx, Var, Val) :-
	varDef(Var, VarIdx, _),
	map:brushSet(ObjIdx, VarIdx, Val).

setObj(ObjIdx, Var, Val) :-
	varDef(Var, VarIdx, _),
	map:objSet(ObjIdx, VarIdx, Val).


setBrush(_, []).
setBrush(ObjIdx, [Var=Val|Props]) :-
	setBrush(ObjIdx, Var, Val),
	setBrush(ObjIdx, Props).

setObj(_, []).
setObj(ObjIdx, [Var=Val|Props]) :-
	setObj(ObjIdx, Var, Val),
	setObj(ObjIdx, Props).


new([type=1|Ps]):- !, newObj(Ps).
new(Ps) :- !, newBrush(Ps).


newBrush(Ps) :-
	map:brushNew(Idx),
	setBrush(Idx, Ps).

newObj(Ps) :-
	map:objNew(Idx),
	setObj(Idx, Ps).









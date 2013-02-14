:-module(brush, [varDef/3, set/2, set/3, get/3, get/2]).

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
varDef(type,	16, 0).
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
varDef(user(0),	32, 0).
varDef(user(1),	33, 0).
varDef(user(2),	34, 0).
varDef(user(3),	35, 0).
varDef(user(4),	36, 0).
varDef(user(5),	37, 0).
varDef(user(6),	38, 0).
varDef(user(7),	39, 0).
varDef(user(8),	40, 0).
varDef(user(9),	41, 0).
varDef(user(10),42, 0).
varDef(user(11),43, 0).
varDef(user(12),44, 0).
varDef(user(13),45, 0).
varDef(user(14),46, 0).
varDef(user(15),47, 0).
%varDef('O_MAX',	48).


set(ObjIdx, Var, Val) :-
	varDef(Var, VarIdx, _),
	map:brushSet(ObjIdx, VarIdx, Val).

get(ObjIdx, Var, Val) :-
	varDef(Var, VarIdx, _),
	map:brushGet(ObjIdx, VarIdx, Val).


set(_, []).
set(ObjIdx, [Var=Val|Props]) :-
	set(ObjIdx, Var, Val),
	set(ObjIdx, Props).

get(ObjIdx, Props) :-
	findall(Var=Val, (get(ObjIdx, Var, Val), varDef(Var, _, Def), Val =\= Def), Props).








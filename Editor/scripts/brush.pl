:-module(brush, [varDef/3, set/2, set/3, get/3,
		 new/1,
		 new/2,
		 getProps/2,
		 getNonDefProps/2,
		 delete/1,
		 eraseAll/0,
		 getEx/2,
		 getEx/3,
		 setEx/2,
		 setEx/3,
		 clone/2,
		 assign/2]).

%varDef(type,	16, 0).
varDef(id,	17, 0).
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
varDef(material, 18, 0).
varDef(draw,	19, 0).
varDef(disable, 20, 0).
varDef(delay,	21, 0).
varDef(anim,	22, 0).
varDef(collider, 23, 0).
%varDef(class,	24, 0).
%varDef(status, 25, 0).
%varDef(target,  26, 0).
%varDef(death,	27, 0).
varDef(collision,	31, 0).
%varDef(user(0),	32, 0).
%varDef(user(1),	33, 0).
%varDef(user(2),	34, 0).
%varDef(user(3),	35, 0).
%varDef(user(4),	36, 0).
%varDef(user(5),	37, 0).
%varDef(user(6),	38, 0).
%varDef(user(7),	39, 0).
%varDef(user(8),	40, 0).
%varDef(user(9),	41, 0).
%varDef(user(10),42, 0).
%varDef(user(11),43, 0).
%varDef(user(12),44, 0).
%varDef(user(13),45, 0).
%varDef(user(14),46, 0).
%varDef(user(15),47, 0).
%varDef('O_MAX',	48).


set(Br, Var, Val) :-
	varDef(Var, VarIdx, _),
	brush:setProp(Br, VarIdx, Val).

get(B, Var, Val) :-
	varDef(Var, VarIdx, _),
	brush:getProp(B, VarIdx, Val).


set(Br, Props) :-
	forall(member(Var=Val, Props), (set(Br, Var, Val);setEx(Br, Var, Val))).

getNonDefProps(Br, Props) :-
	findall(Var=Val, (get(Br, Var, Val), varDef(Var, _, Def), Val =\= Def;getEx(Br, Var, Val)), Props).

getProps(Br, Props) :-
	findall(Var=Val, (get(Br, Var, Val);getEx(Br, Var, Val)), Props).


new(Props):-
	new(Props, _).

new(Props, Br) :-
	map:brushNew(Br),
	set(Br, Props).


delete(Br) :-
	recorded(brushProps, brush(Br, _), Ref) -> erase(Ref) ; true.

eraseAll :-
	forall(recorded(brushProps, _, Ref), erase(Ref)).

getEx(Br, Prop, Val) :-
	recorded(brushProps, brush(Br, Props)),
	gen_assoc(Prop, Props, Val).

getEx(Br, List) :-
	(   recorded(brushProps, brush(Br, Props))
	->  assoc_to_list(Props, List)
	;   List = []).

setEx(Br, Prop, Val) :-
	(   recorded(brushProps, brush(Br, Props), Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(brushProps, brush(Br, NewProps)).

setEx(Br, List) :-
	(   recorded(brushProps, brush(Br, _), Ref)
	->  erase(Ref)
	;   true),
	list_to_assoc(List, Props),
	recordz(brushProps, brush(Br, Props)).

clone(Org, New) :-
	getNonDefProps(Org, Props),
	new(Props, New),
	core:dl(cloned(Props)).

assign(To, From) :-
	getProps(From, Props),
	delete(To),
	set(To, Props).











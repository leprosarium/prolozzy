:- module(brush, [new/1]).

sysProp(layer, setLayer).
sysProp(x, setX).
sysProp(y, setY).
sysProp(w, setW).
sysProp(h, setH).
sysProp(tile, setTile).
sysProp(frame, setFrame).
sysProp(x1, setX1).
sysProp(y1, setY1).
sysProp(x2, setX2).
sysProp(y2, setY2).
sysProp(flip, setFlip).
sysProp(color, setColor).
sysProp(shader, setShader).
sysProp(scale, setScale).
sysProp(id, setId).
sysProp(material, setMaterial).
sysProp(draw, setDraw).
sysProp(disable, setDisabled).
sysProp(delay, setDelay).
sysProp(anim, setAnim).
sysProp(collider, setCollider).
sysProp(collision, setCollision).

get(Idx, Prop, Val) :-
	sysProp(Prop, _), !,
	Cl =.. [Prop, Idx, Val],
	call(map:Cl).

get(Idx, Prop, Val) :- !,
	recorded(brushProps, brush(Idx, Props)),
	gen_assoc(Prop, Props, Val).

setBrush(Idx, Prop, Val) :-
	sysProp(Prop, SetProp), !,
	Cl =.. [SetProp, Idx, Val],
	call(map:Cl).

setBrush(Idx, Prop, Val) :- !,
	(   recorded(brushProps, brush(Idx, Props), Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(brushProps, brush(Idx, NewProps)).


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









:-module(brush, [varDef/2, set/2, set/3, get/2, get/3,
		 new/1,
		 new/2,
		 getProps/2,
		 setProps/2,
		 getNonDefProps/2,
		 delete/1,
		 eraseAll/0,
		 clone/2,
		 assign/2,
		 paste/1]).


varDef(id,	'').
varDef(layer,	0).
varDef(x,	0).
varDef(y,	0).
varDef(w,	0).
varDef(h,	0).
varDef(tile,	-1).
varDef(frame,	0).
varDef(x1,	0).
varDef(y1,	0).
varDef(x2,	0).
varDef(y2,	0).
varDef(flip,	0).
varDef(color,	0xffffffff).
varDef(shader,	0).
varDef(scale,	0).
varDef(material,0).
varDef(draw,	0).
varDef(disable, 0).
varDef(delay,   0).
varDef(anim,	0).
varDef(collider,0).
varDef(collision,0).


setProps(Br, Props) :-
	forall(member(Var=Val, Props), set(Br, Var, Val)).

getNonDefProps(Br, Props) :-
	findall(Var=Val, (get(Br, Var, Val), (varDef(Var, Def) -> Val \= Def;true)), Props).

getProps(Br, Props) :-
	findall(Var=Val, get(Br, Var, Val), Props).


new(Props):-
	new(Props, _).

new(Props, Br) :-
	map:brushNew(Br),
	setProps(Br, Props).


delete(Br) :-
	recorded(brushProps, brush(Br, _), Ref) -> erase(Ref) ; true.

eraseAll :-
	forall(recorded(brushProps, _, Ref), erase(Ref)).

get(Br, id, Val) :- brush:getID(Br, Val).
get(Br, layer, Val) :- brush:getLayer(Br, Val).
get(Br, x, Val) :- brush:getX(Br, Val).
get(Br, y, Val) :- brush:getY(Br, Val).
get(Br, w, Val) :- brush:getW(Br, Val).
get(Br, h, Val) :- brush:getH(Br, Val).
get(Br, tile, Val) :- brush:getTile(Br, Val).
get(Br, frame, Val) :- brush:getFrame(Br, Val).
get(Br, x1, Val) :- brush:getMapX1(Br, Val).
get(Br, y1, Val) :- brush:getMapY1(Br, Val).
get(Br, x2, Val) :- brush:getMapX2(Br, Val).
get(Br, y2, Val) :- brush:getMapY2(Br, Val).
get(Br, flip, Val) :- brush:getFlip(Br, Val).
get(Br, color, Val) :- brush:getColor(Br, Val).
get(Br, shader, Val) :- brush:getShader(Br, Val).
get(Br, scale, Val) :- brush:getScale(Br, Val).
get(Br, material, Val) :- brush:getMaterial(Br, Val).
get(Br, draw, Val) :- brush:getDraw(Br, Val).
get(Br, disable, Val) :- brush:getDisable(Br, Val).
get(Br, delay, Val) :- brush:getDelay(Br, Val).
get(Br, anim, Val) :- brush:getAnim(Br, Val).
get(Br, collider, Val) :- brush:getCollider(Br, Val).
get(Br, collision, Val) :- brush:getCollision(Br, Val).
get(Br, Prop, Val) :-
	recorded(brushProps, brush(Br, Props)),
	gen_assoc(Prop, Props, Val).

get(Br, List) :-
	(   recorded(brushProps, brush(Br, Props))
	->  assoc_to_list(Props, List)
	;   List = []).

set(Br, id, Val) :- !, brush:setID(Br, Val).
set(Br, layer, Val) :- !, brush:setLayer(Br, Val).
set(Br, x, Val) :- !, brush:setX(Br, Val).
set(Br, y, Val) :- !, brush:setY(Br, Val).
set(Br, w, Val) :- !, brush:setW(Br, Val).
set(Br, h, Val) :- !, brush:setH(Br, Val).
set(Br, tile, Val) :- !, brush:setTile(Br, Val).
set(Br, frame, Val) :- !, brush:setFrame(Br, Val).
set(Br, x1, Val) :- !, brush:setMapX1(Br, Val).
set(Br, y1, Val) :- !, brush:setMapY1(Br, Val).
set(Br, x2, Val) :- !, brush:setMapX2(Br, Val).
set(Br, y2, Val) :- !, brush:setMapY2(Br, Val).
set(Br, flip, Val) :- !, brush:setFlip(Br, Val).
set(Br, color, Val) :- !, brush:setColor(Br, Val).
set(Br, shader, Val) :- !, brush:setShader(Br, Val).
set(Br, scale, Val) :- !, brush:setScale(Br, Val).
set(Br, material, Val) :- !, brush:setMaterial(Br, Val).
set(Br, draw, Val) :- !, brush:setDraw(Br, Val).
set(Br, disable, Val) :- !, brush:setDisable(Br, Val).
set(Br, delay, Val) :- !, brush:setDelay(Br, Val).
set(Br, anim, Val) :- !, brush:setAnim(Br, Val).
set(Br, collider, Val) :- !, brush:setCollider(Br, Val).
set(Br, collision, Val) :- !, brush:setCollision(Br, Val).
set(Br, Prop, Val) :-
	(   recorded(brushProps, brush(Br, Props), Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(brushProps, brush(Br, NewProps)).

set(Br, List) :-
	(   recorded(brushProps, brush(Br, _), Ref)
	->  erase(Ref)
	;   true),
	list_to_assoc(List, Props),
	recordz(brushProps, brush(Br, Props)).

clone(Org, New) :-
	getNonDefProps(Org, Props),
	new(Props, New).

assign(To, From) :-
	getProps(From, Props),
	delete(To),
	setProps(To, Props).

paste(Props) :-
	new(Props, B),
	edi:toolBrush(TB),
	brush:getLayer(TB, L),
	brush:setLayer(B, L).

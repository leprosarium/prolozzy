:-module(brush, [varDef/2, setEx/2, setEx/3, getEx/2, getEx/3,
		 new/1,
		 new/2,
		 getProps/2,
		 setProps/2,
		 getNonDefProps/2,
		 delete/1,
		 eraseAll/0,
		 clone/2,
		 assign/2]).


varDef(id,	0).
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
	forall(member(Var=Val, Props), setEx(Br, Var, Val)).

getNonDefProps(Br, Props) :-
	findall(Var=Val, (getEx(Br, Var, Val), (varDef(Var, Def) -> Val \= Def;true)), Props).

getProps(Br, Props) :-
	findall(Var=Val, getEx(Br, Var, Val), Props).


new(Props):-
	new(Props, _).

new(Props, Br) :-
	map:brushNew(Br),
	setProps(Br, Props).


delete(Br) :-
	recorded(brushProps, brush(Br, _), Ref) -> erase(Ref) ; true.

eraseAll :-
	forall(recorded(brushProps, _, Ref), erase(Ref)).

getEx(Br, id, Val) :- brush:getID(Br, Val).
getEx(Br, layer, Val) :- brush:getLayer(Br, Val).
getEx(Br, x, Val) :- brush:getX(Br, Val).
getEx(Br, y, Val) :- brush:getY(Br, Val).
getEx(Br, w, Val) :- brush:getW(Br, Val).
getEx(Br, h, Val) :- brush:getH(Br, Val).
getEx(Br, tile, Val) :- brush:getTile(Br, Val).
getEx(Br, frame, Val) :- brush:getFrame(Br, Val).
getEx(Br, x1, Val) :- brush:getMapX1(Br, Val).
getEx(Br, y1, Val) :- brush:getMapY1(Br, Val).
getEx(Br, x2, Val) :- brush:getMapX2(Br, Val).
getEx(Br, y2, Val) :- brush:getMapY2(Br, Val).
getEx(Br, flip, Val) :- brush:getFlip(Br, Val).
getEx(Br, color, Val) :- brush:getColor(Br, Val).
getEx(Br, shader, Val) :- brush:getShader(Br, Val).
getEx(Br, scale, Val) :- brush:getScale(Br, Val).
getEx(Br, material, Val) :- brush:getMaterial(Br, Val).
getEx(Br, draw, Val) :- brush:getDraw(Br, Val).
getEx(Br, disable, Val) :- brush:getDisable(Br, Val).
getEx(Br, delay, Val) :- brush:getDelay(Br, Val).
getEx(Br, anim, Val) :- brush:getAnim(Br, Val).
getEx(Br, collider, Val) :- brush:getCollider(Br, Val).
getEx(Br, collision, Val) :- brush:getCollision(Br, Val).
getEx(Br, Prop, Val) :-
	recorded(brushProps, brush(Br, Props)),
	gen_assoc(Prop, Props, Val).

getEx(Br, List) :-
	(   recorded(brushProps, brush(Br, Props))
	->  assoc_to_list(Props, List)
	;   List = []).

setEx(Br, id, Val) :- !, brush:setID(Br, Val).
setEx(Br, layer, Val) :- !, brush:setLayer(Br, Val).
setEx(Br, x, Val) :- !, brush:setX(Br, Val).
setEx(Br, y, Val) :- !, brush:setY(Br, Val).
setEx(Br, w, Val) :- !, brush:setW(Br, Val).
setEx(Br, h, Val) :- !, brush:setH(Br, Val).
setEx(Br, tile, Val) :- !, brush:setTile(Br, Val).
setEx(Br, frame, Val) :- !, brush:setFrame(Br, Val).
setEx(Br, x1, Val) :- !, brush:setMapX1(Br, Val).
setEx(Br, y1, Val) :- !, brush:setMapY1(Br, Val).
setEx(Br, x2, Val) :- !, brush:setMapX2(Br, Val).
setEx(Br, y2, Val) :- !, brush:setMapY2(Br, Val).
setEx(Br, flip, Val) :- !, brush:setFlip(Br, Val).
setEx(Br, color, Val) :- !, brush:setColor(Br, Val).
setEx(Br, shader, Val) :- !, brush:setShader(Br, Val).
setEx(Br, scale, Val) :- !, brush:setScale(Br, Val).
setEx(Br, material, Val) :- !, brush:setMaterial(Br, Val).
setEx(Br, draw, Val) :- !, brush:setDraw(Br, Val).
setEx(Br, disable, Val) :- !, brush:setDisable(Br, Val).
setEx(Br, delay, Val) :- !, brush:setDelay(Br, Val).
setEx(Br, anim, Val) :- !, brush:setAnim(Br, Val).
setEx(Br, collider, Val) :- !, brush:setCollider(Br, Val).
setEx(Br, collision, Val) :- !, brush:setCollision(Br, Val).
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











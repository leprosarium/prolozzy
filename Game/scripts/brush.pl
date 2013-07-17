:- module(brush, [new/1,
		  get/2,
		  get/3,
		  set/2,
		  set/3,
		 disabled/1,
		 disable/1,
		 enable/1]).

disabled(Br) :- brush:getDisable(Br, 1).
disable(Br) :- brush:setDisable(Br, 1).
enable(Br) :- brush:setDisable(Br, 0).

set(brush(Br), Props) :- !, forall(member(Var=Val, Props), set(brush(Br), Var, Val)).
set(Id, Props) :- !, brush:find(Id, Br), set(Br, Props).

get(Br, Props) :-
	findall(Var=Val, get(Br, Var, Val), Props).

new(Props) :-
	(   member(id=_, Props)
	->  brush:create(Br)
	;   brush:createStatic(Br)),
	set(Br, Props).

get(Br, layer, Val) :- brush:getLayer(Br, Val).
get(Br, x, Val) :- brush:getX(Br, Val).
get(Br, y, Val) :- brush:getY(Br, Val).
get(Br, w, Val) :- brush:getW(Br, Val).
get(Br, h, Val) :- brush:getH(Br, Val).
get(Br, tile, Val) :- brush:getTile(Br, Val).
get(Br, x1, Val) :- brush:getMapX1(Br, Val).
get(Br, y1, Val) :- brush:getMapY1(Br, Val).
get(Br, x2, Val) :- brush:getMapX2(Br, Val).
get(Br, y2, Val) :- brush:getMapY2(Br, Val).
get(Br, flip, Val) :- brush:getFlip(Br, Val).
get(Br, color, Val) :- brush:getColor(Br, Val).
get(Br, shader, Val) :- brush:getShader(Br, Val).
get(Br, scale, Val) :- brush:getScale(Br, Val).
get(Br, id, Val) :- brush:getID(Br, Val).
get(Br, material, Val) :- brush:getMaterial(Br, Val).
get(Br, draw, Val) :- brush:getDraw(Br, Val).
get(Br, disable, Val) :- brush:getDisable(Br, Val).
get(Br, delay, Val) :- brush:getDelay(Br, Val).
get(Br, anim, Val) :- brush:getAnim(Br, Val).
get(Br, collider, Val) :- brush:getCollider(Br, Val).
get(Br, collision, Val) :- brush:getCollision(Br, Val).


get(brush(Br), Prop, Val) :-
	recorded(brushProps, brush(Br, Props)),
	gen_assoc(Prop, Props, Val).

set(Br, layer, Val) :- !, brush:setLayer(Br, Val).
set(Br, x, Val) :- !, brush:setX(Br, Val).
set(Br, y, Val) :- !, brush:setY(Br, Val).
set(Br, w, Val) :- !, brush:setW(Br, Val).
set(Br, h, Val) :- !, brush:setH(Br, Val).
set(Br, tile, Val) :- !, brush:setTile(Br, Val).
set(Br, x1, Val) :- !, brush:setMapX1(Br, Val).
set(Br, y1, Val) :- !, brush:setMapY1(Br, Val).
set(Br, x2, Val) :- !, brush:setMapX2(Br, Val).
set(Br, y2, Val) :- !, brush:setMapY2(Br, Val).
set(Br, flip, Val) :- !, brush:setFlip(Br, Val).
set(Br, color, Val) :- !, brush:setColor(Br, Val).
set(Br, shader, Val) :- !, brush:setShader(Br, Val).
set(Br, scale, Val) :- !, brush:setScale(Br, Val).
set(Br, id, Val) :- !, brush:setID(Br, Val).
set(Br, material, Val) :- !, brush:setMaterial(Br, Val).
set(Br, draw, Val) :- !, brush:setDraw(Br, Val).
set(Br, disable, Val) :- !, brush:setDisable(Br, Val).
set(Br, delay, Val) :- !, brush:setDelay(Br, Val).
set(Br, anim, Val) :- !, brush:setAnim(Br, Val).
set(Br, collider, Val) :- !, brush:setCollider(Br, Val).
set(Br, collision, Val) :- !, brush:setCollision(Br, Val).

set(brush(Br), Prop, Val) :-
	(   recorded(brushProps, brush(Br, Props), Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(brushProps, brush(Br, NewProps)).


















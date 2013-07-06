:- module(brush, [new/1]).

setObj(brush(Br), Props) :- !, forall(member(Var=Val, Props), setEx(brush(Br), Var, Val)).
setObj(Id, Props) :- !, brush:find(Id, Br), setObj(Br, Props).

getObj(Br, Props) :-
	findall(Var=Val, getEx(Br, Var, Val), Props).

new(Props) :-
	(   member(id=_, Props)
	->  brush:create(Br)
	;   brush:createStatic(Br)),
	setObj(Br, Props).

getEx(Br, layer, Val) :- brush:getLayer(Br, Val).
getEx(Br, x, Val) :- brush:getX(Br, Val).
getEx(Br, y, Val) :- brush:getY(Br, Val).
getEx(Br, w, Val) :- brush:getW(Br, Val).
getEx(Br, h, Val) :- brush:getH(Br, Val).
getEx(Br, tile, Val) :- brush:getTile(Br, Val).
getEx(Br, x1, Val) :- brush:getMapX1(Br, Val).
getEx(Br, y1, Val) :- brush:getMapY1(Br, Val).
getEx(Br, x2, Val) :- brush:getMapX2(Br, Val).
getEx(Br, y2, Val) :- brush:getMapY2(Br, Val).
getEx(Br, flip, Val) :- brush:getFlip(Br, Val).
getEx(Br, color, Val) :- brush:getColor(Br, Val).
getEx(Br, shader, Val) :- brush:getShader(Br, Val).
getEx(Br, scale, Val) :- brush:getScale(Br, Val).
getEx(Br, id, Val) :- brush:getID(Br, Val).
getEx(Br, material, Val) :- brush:getMaterial(Br, Val).
getEx(Br, draw, Val) :- brush:getDraw(Br, Val).
getEx(Br, disable, Val) :- brush:getDisable(Br, Val).
getEx(Br, delay, Val) :- brush:getDelay(Br, Val).
getEx(Br, anim, Val) :- brush:getAnim(Br, Val).
getEx(Br, collider, Val) :- brush:getCollider(Br, Val).
getEx(Br, collision, Val) :- brush:getCollision(Br, Val).


getEx(brush(Br), Prop, Val) :-
	recorded(brushProps, brush(Br, Props)),
	gen_assoc(Prop, Props, Val).

setEx(Br, layer, Val) :- !, brush:setLayer(Br, Val).
setEx(Br, x, Val) :- !, brush:setX(Br, Val).
setEx(Br, y, Val) :- !, brush:setY(Br, Val).
setEx(Br, w, Val) :- !, brush:setW(Br, Val).
setEx(Br, h, Val) :- !, brush:setH(Br, Val).
setEx(Br, tile, Val) :- !, brush:setTile(Br, Val).
setEx(Br, x1, Val) :- !, brush:setMapX1(Br, Val).
setEx(Br, y1, Val) :- !, brush:setMapY1(Br, Val).
setEx(Br, x2, Val) :- !, brush:setMapX2(Br, Val).
setEx(Br, y2, Val) :- !, brush:setMapY2(Br, Val).
setEx(Br, flip, Val) :- !, brush:setFlip(Br, Val).
setEx(Br, color, Val) :- !, brush:setColor(Br, Val).
setEx(Br, shader, Val) :- !, brush:setShader(Br, Val).
setEx(Br, scale, Val) :- !, brush:setScale(Br, Val).
setEx(Br, id, Val) :- !, brush:setID(Br, Val).
setEx(Br, material, Val) :- !, brush:setMaterial(Br, Val).
setEx(Br, draw, Val) :- !, brush:setDraw(Br, Val).
setEx(Br, disable, Val) :- !, brush:setDisable(Br, Val).
setEx(Br, delay, Val) :- !, brush:setDelay(Br, Val).
setEx(Br, anim, Val) :- !, brush:setAnim(Br, Val).
setEx(Br, collider, Val) :- !, brush:setCollider(Br, Val).
setEx(Br, collision, Val) :- !, brush:setCollision(Br, Val).


setEx(brush(Br), Prop, Val) :-
	(   recorded(brushProps, brush(Br, Props), Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(brushProps, brush(Br, NewProps)).







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


getEx(Br, layer, Val) :- !, brush:getLayer(Br, Val).
getEx(Br, x, Val) :- !, brush:getX(Br, Val).
getEx(Br, y, Val) :- !, brush:getY(Br, Val).
getEx(Br, w, Val) :- !, brush:getW(Br, Val).
getEx(Br, h, Val) :- !, brush:getH(Br, Val).
getEx(Br, tile, Val) :- !, brush:getTile(Br, Val).
getEx(Br, mapX1, Val) :- !, brush:getMapX1(Br, Val).
getEx(Br, mapY1, Val) :- !, brush:getMapY1(Br, Val).
getEx(Br, mapX2, Val) :- !, brush:getMapX2(Br, Val).
getEx(Br, mapY2, Val) :- !, brush:getMapY2(Br, Val).
getEx(Br, flip, Val) :- !, brush:getFlip(Br, Val).
getEx(Br, color, Val) :- !, brush:getColor(Br, Val).
getEx(Br, shader, Val) :- !, brush:getShader(Br, Val).
getEx(Br, scale, Val) :- !, brush:getScale(Br, Val).
getEx(Br, id, Val) :- !, brush:getID(Br, Val).
getEx(Br, material, Val) :- !, brush:getMaterial(Br, Val).
getEx(Br, draw, Val) :- !, brush:getDraw(Br, Val).
getEx(Br, disable, Val) :- !, brush:getDisable(Br, Val).
getEx(Br, delay, Val) :- !, brush:getDelay(Br, Val).
getEx(Br, anim, Val) :- !, brush:getAnim(Br, Val).
getEx(Br, collider, Val) :- !, brush:getCollader(Br, Val).
getEx(Br, collision, Val) :- !, brush:getCollision(Br, Val).

getEx(Br, Prop, Val) :-
	recorded(Br, Props),
	gen_assoc(Prop, Props, Val).


setEx(Br, layer, Val) :- !, brush:setLayer(Br, Val).
setEx(Br, x, Val) :- !, brush:setX(Br, Val).
setEx(Br, y, Val) :- !, brush:setY(Br, Val).
setEx(Br, w, Val) :- !, brush:setW(Br, Val).
setEx(Br, h, Val) :- !, brush:setH(Br, Val).
setEx(Br, tile, Val) :- !, brush:setTile(Br, Val).
setEx(Br, mapX1, Val) :- !, brush:setMapX1(Br, Val).
setEx(Br, mapY1, Val) :- !, brush:setMapY1(Br, Val).
setEx(Br, mapX2, Val) :- !, brush:setMapX2(Br, Val).
setEx(Br, mapY2, Val) :- !, brush:setMapY2(Br, Val).
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
setEx(Br, collider, Val) :- !, brush:setCollader(Br, Val).
setEx(Br, collision, Val) :- !, brush:setCollision(Br, Val).


setEx(Br, Prop, Val) :-
	(   recorded(Br, Props, Ref)
	->  erase(Ref)
	;   empty_assoc(Props)),
	put_assoc(Prop, Props, Val, NewProps),
	recordz(Br, NewProps).





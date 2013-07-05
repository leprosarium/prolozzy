:- module(obj, [layer/2,
		x/2,
		y/2,
		w/2,
		h/2,
		tile/2,
		frame/2,
		map/3,
		flip/2,
		color/2,
		shader/2,
		scale/2,
		id/2,
		material/2,
		draw/2,
		disable/2,
		disable/1,
		delay/2,
		anim/2,
		collider/2,
		class/2,
		status/2,
		target/2,
		death/2,
		collision/2,
		user/3,
	       var/3,
	       varDef/2]).

varDef(layer,	0).
varDef(x,	1).
varDef(y,	2).
varDef(w,	3).
varDef(h,	4).
varDef(tile,	5).
varDef(frame,	6).
varDef(map(0),	7).
varDef(map(1),	8).
varDef(map(2),	9).
varDef(map(3),	10).
varDef(flip,	11).
varDef(color,	12).
varDef(shader,	13).
varDef(scale,	14).
varDef(id,	17).
varDef(material, 18).
varDef(draw,	19).
varDef(disable, 20).
varDef(delay,	21).
varDef(anim,	22).
varDef(collider, 23).
varDef(class,	24).
varDef(status, 25).
varDef(target,  26).
varDef(death,	27).
varDef(collision,	31).
varDef(user(N),	ID) :-
	between(0, 15, N),
	ID is 32 + N.

var(ObjIdx, Var, Val) :-
	varDef(Var, VarIdx),
	map:objVar(ObjIdx, VarIdx, Val).

layer(Idx, Layer) :- var(Idx, layer, Layer).
x(Idx, X) :- var(Idx, x, X).
y(Idx, Y) :- var(Idx, y, Y).
w(Idx, W) :- var(Idx, w, W).
h(Idx, H) :- var(Idx, h, H).
tile(Idx, Tile) :- var(Idx, tile,	Tile).
frame(Idx, Frame) :- var(Idx, frame, Frame).
map(Idx, N, Map) :- var(Idx, map(N), Map).
flip(Idx, Flip) :- var(Idx, flip,	Flip).
color(Idx, Color) :- var(Idx, color, Color).
shader(Idx, Shader) :- var(Idx, shader, Shader).
scale(Idx, Scale) :- var(Idx, scale, Scale).
id(Idx, Id) :- var(Idx, id, Id).
material(Idx, Material) :- var(Idx, material, Material).
draw(Idx, Draw) :- var(Idx, draw, Draw).
disable(Idx, Disable) :- var(Idx, disable, Disable).
disable(Idx) :- disable(Idx, D), D =:= 1.
delay(Idx, Delay) :- var(Idx, delay, Delay).
anim(Idx, Anim) :- var(Idx, anim, Anim).
collider(Idx, Collider) :- var(Idx, collider, Collider).
class(Idx, Class) :- var(Idx, class, Class).
status(Idx, Status) :- var(Idx, status, Status).
target(Idx, Target) :- var(Idx, target, Target).
death(Idx, Death) :- var(Idx, death, Death).
collision(Idx, Collision) :- var(Idx, collision, Collision).
user(Idx, N, Val) :- var(Idx, user(N), Val).










:-module(mod, [shader/2,
	       density/2,
	       view/1,
	       setView/1,
	       viewMode/2,
	       roomInfo/1,
	       setRoomInfo/1,
	       brushNew/1,
	       brushProp/2,
	       brushProp/3,
	       brushProp/4]).

brushProp(user, 32).
brushProp(max, 48).

brushProp(layer, "layer",	"layer").
brushProp(x, "x*",		"x readonly").
brushProp(y, "y*",		"y readonly").
brushProp(w, "w*",		"w readonly").
brushProp(h, "h*",		"h readonly").
brushProp(title, "tile",	"tile id").
brushProp(frame, "frame",	"tile frame").
brushProp(maxX1, "map x1",	"map left").
brushProp(maxY1, "map y1",	"map top").
brushProp(maxX2, "map x2",	"map right").
brushProp(maxY2, "map y2",	"map bottom").
brushProp(flip,  "flip",	"flip", select(Flips)):- findall(Id-Name, def:flip(Id, _, Name), Flips).
brushProp(color, "color",	"color", color).
brushProp(shader, "shader",	"shader", select(ShaderNames)) :- findall(Id-Name, def:shader(Id, _, Name), ShaderNames).
brushProp(scale, "scale",	"scale").
brushProp(select, "select*",	"select").

		% 16
brushProp(type, "type",		"brush type", select(Types)):- findall(Id-Name, def:brushType(Id, _, Name), Types).
brushProp(id, "id",		"object id").
brushProp(material, "material",	"material", select(MatNames)):- findall(Name-Name, def:material(Name, _, _, _), MatNames).
brushProp(draw, "draw",		"draw mode", select(DrawNames)):- findall(Id-Name, def:drawMode(Id, _, Name), DrawNames).
brushProp(disable, "disable",	"no update no draw", select(["no","yes"])).
brushProp(delay, "delay",	"frame delay").
brushProp(anim, "anim",		"animation mode", select(["stop","play","loop"])).
brushProp(collider, "collider",	"collider mode", select(["none","call handler","hard collision"])).
brushProp(class, "class",	"generic class", select(ClassNames)) :- findall(Id-Name, def:class(Id, _, Name), ClassNames).

brushProp(status, "status",	"generic status").
brushProp(target, "target",	"target id").
brushProp(death, "death",	"death cause").
brushProp(reserved, "reserved",	"reserved").
brushProp(reserved, "reserved",	"reserved").
brushProp(reserved, "reserved",	"reserved").
brushProp(reserved, "reserved",	"reserved").% collision

		% 32
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).
brushProp(user, "user",		"user", custom).
brushProp(user, "user",		"user",	custom).







shader(opaque,	 0). % opaque
shader(blend,    1). % alpha blend
shader(add,      2). % additive color - used for light
shader(mod,      3). % modulate color - used for darken
shader(mod2,     4). % modulate 2 color - used for detail
shader(alpharep, 5). % internal use for material view
shader(max,      6). % max shaders

density(void, 0xFF000000).
density(soft, 0xff606060).
density(hard, 0xffa0a0a0).
density(jump, 0xffffffff).



propGet(Key, Val) :-
	recorded(Key, Val).
propSet(Key, Val) :-
	recorded(Key, _, Ref),
	erase(Ref),
	recorda(view, Val).

viewMode(default, "default view").
viewMode(select, "select view").
viewMode(game, "game view").
viewMode(matrial, "matrial view").
viewMode(density, "density view").

property(view, Def) :- viewMode(Def, _), !.
property(roomInfo, 0).


:- forall(property(Prop, Def), recorda(Prop, Def)).


view(V) :- propGet(view, V).
setView(V) :- viewMode(V, _), propSet(view, V).
roomInfo(I):- propGet(roomInfo, I).
setRoomInfo(I) :- propSet(roomInfo, I).

% Initialize props of the current tool brush, specific to the current type
% Called from Brush Type button in the menu bar

brushNew(Type) :-
	shader(blend, SHADER_BLEND),
	edi:toolBrushSetShader(SHADER_BLEND),
	edi:toolBrushSetScale(100),
	edi:toolBrushSetID(0),
	edi:toolBrushSetMaterial(0),
	edi:toolBrushSetDisable(0),
	edi:toolBrushSetDelay(3),
	edi:toolBrushSetAnim(2),
	edi:toolBrushSetCollider(0),
	edi:toolBrushSetClass(0),
	edi:toolBrushSetStatus(0),
	edi:toolBrushSetTarget(0),
	edi:toolBrushSetDeath(0),
	(   Type = static
	->  Draw = 3   % draw in img+mat
	;   Draw = 1), % visible
	edi:toolBrushSetDraw(Draw),
	brushProp(user, USER),
	brushProp(max, MX),
	UserMax is MX - USER - 1,
	forall(between(0, UserMax, Idx), edi:toolBrushSetUser(Idx,0)).











:-module(mod, [shader/2,
	       density/2,
	       material/4,
	       class/1,
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
brushProp(flip,  "flip",	"flip", select([none-"none",x-"flip x",y-"flip y",xy-"flip xy",r-"flip r",xr-"flip xr",yr-"flip yr",xyr-"flip xyr"])).
brushProp(color, "color",	"color", color).
brushProp(shader, "shader",	"shader", select(ShaderNames)) :-
	ShaderNames = [opaque, blend, add, mod, mod2]. % only those shaders are available for users
brushProp(scale, "scale",	"scale").
brushProp(select, "select*",	"select").

		% 16
brushProp(type, "type",		"brush type (0=static,1=dynamic)", select(["static","dynamic"])).
brushProp(id, "id",		"object id").
brushProp(material, "material",	"material", select(MatNames)):-
	mod:matNames(MatNames).
brushProp(draw, "draw",		"draw mode (1=img,2=mat,3=both)", select(["none","img","mat","img+mat"])).
brushProp(disable, "disable",	"no update no draw", select(["no","yes"])).
brushProp(delay, "delay",	"frame delay").
brushProp(anim, "anim",		"animation mode", select(["stop","play","loop"])).
brushProp(collider, "collider",	"collider mode", select(["none","call handler","hard collision"])).
brushProp(class, "class",	"generic class", select(ClassNames)) :-
	mod:classNames(ClassNames).

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

material(air,	 0, void, 0xFF000000).	% void
material(water,	 1, void, 0xFF0060FF).	% water (void); player can drawn in water
material(hurt,	 2, void, 0xFFFF8000).	% hurt (void); player gets hurt
material(kill,	 3, void, 0xFFD00000).	% kill (void); player gets killed
material(cloud,	 4, soft, 0xFFC0C0C0).	% clouds (medium); player sinks on clouds
material(climb,	 5, soft, 0xFF909090).	% stairs (medium); player stands on
material(wind,	 6, soft, 0xFF707070).	% winds (medium); player is pushed up
material(block,	 7, hard, 0xFF006000).	% ground, walls (hard); blocks the player
material(jumpFix, 8, jump, 0xFF008000).	% jumper fix (hard)
material(jumpPro, 9, jump, 0xFF00B000).	% jumper progressive (hard)

class(none).
class(action).
class(hurt).
class(kill).
class(item).
class(coin).
class(food).
class(life).
class(waypoint).

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

classNames(Names):-
	findall(Name, class(Name), Names).
matNames(Names) :-
	findall(Name, material(Name, _, _, _), Names).

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
	(   Type == 0
	->  Draw = 3   % draw in img+mat
	;   Draw = 1), % visible
	edi:toolBrushSetDraw(Draw),
	brushProp(user, USER),
	brushProp(max, MX),
	UserMax is MX - USER - 1,
	forall(between(0, UserMax, Idx), edi:toolBrushSetUser(Idx,0)).











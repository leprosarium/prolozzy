:-module(mod, [init/0,
	       density/2,
	       material/4,
	       class/1,
	       view/1,
	       setView/1,
	       viewMode/2,
	       roomInfo/1,
	       setRoomInfo/1]).

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
setView(V) :- propSet(view, V).
roomInfo(I):- propGet(roomInfo, I).
setRoomInfo(I) :- propSet(roomInfo, I).

init :-
	% shaders
	ShaderNames = [opaque, blend, add, mod, mod2], % only those shaders are available for users
	mod:classNames(ClassNames),
	mod:matNames(MatNames),
	BrushProps = [
		prop( "layer",		"layer"),
		prop( "x*",		"x readonly"),
		prop( "y*",		"y readonly"),
		prop( "w*",		"w readonly"),
		prop( "h*",		"h readonly"),
		prop( "tile",		"tile id"),
		prop( "frame",		"tile frame"),
		prop( "map x1",		"map left"),
		prop( "map y1",		"map top"),
		prop( "map x2",		"map right"),
		prop( "map y2",		"map bottom"),
		prop( "flip",		"flip", select(["none","flip x","flip y","flip xy","flip r","flip xr","flip yr","flip xyr"])),
		prop( "color",		"color", color),
		prop( "shader",		"shader", select(ShaderNames)),
		prop( "scale",		"scale"),
		prop( "select*",	"select"),

		% 16
		prop( "type",		"brush type (0=static,1=dynamic)", select(["static","dynamic"])),
		prop( "id",		"object id"),
		prop( "material",	"material", select(MatNames)),
		prop( "draw",		"draw mode (1=img,2=mat,3=both)", select(["none","img","mat","img+mat"])),
		prop( "disable",	"no update no draw", select(["no","yes"])),
		prop( "delay",		"frame delay"),
		prop( "anim",		"animation mode", select(["stop","play","loop"])),
		prop( "collider",	"collider mode", select(["none","call handler","hard collision"])),
		prop( "class",		"generic class", select(ClassNames)),
		prop( "status",		"generic status"),
		prop( "target",		"target id"),
		prop( "death",		"death cause"),
		prop( "reserved",	"reserved"),
		prop( "reserved",	"reserved"),
		prop( "reserved",	"reserved"),
		prop( "reserved",	"reserved"),% collision

		% 32
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom),
		prop( "user",		"user", custom),
		prop( "user",		"user",	custom)],
	core:dl(props(BrushProps)).

classNames(Names):-
	findall(Name, class(Name), Names).
matNames(Names) :-
	findall(Name, material(Name, _, _, _), Names).









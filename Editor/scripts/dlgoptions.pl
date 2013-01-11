:- module(dlgoptions, [load/0,
		      colorTheme/0]).

getOpt(Opt, _, Val) :-
	format(string(Key), 'options_~a', [Opt]),
	core:ini('editor.ini', 'editor', Key, Val), !.
getOpt(_Key, Def, Def).



load :-
	core:dl(load),
	getOpt(axes, 0, Axes ),
	getOpt(grid, 1, Grid ),
	getOpt(snap, 1, Snap ),
	getOpt(roomgrid, 0, Roomgrid),
	getOpt(brushrect, 0, Brushrect),
	def:roomW(DefRoomW),
	getOpt(roomw, DefRoomW, Roomw),
	def:roomH(DefRoomH),
	getOpt(roomh, DefRoomH, Roomh),
	def:color(map, DefColor),
	getOpt(colormap, DefColor, Color),

	core:dl(axes(Axes)),
	edi:setAxes(Axes),
	edi:setGrid(Grid),
	edi:setSnap(Snap),
	edi:setRoomGrid(Roomgrid),
	edi:setBrushRect(Brushrect),
	edi:setRoomW(Roomw),
	edi:setRoomH(Roomh),
	edi:setColorMap(Color),

	core:dl(grid(Grid)),
	core:dl(snap(Snap)),
	core:dl(roomgrid(Roomgrid)),
	core:dl(brushrect(Brushrect)),
	core:dl(roomw(Roomw)),
	core:dl(roomh(Roomh)),
	core:dl(coloMap(Color)).

colorTheme.


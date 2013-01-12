:- module(dlgoptions, [load/0,
		      getOpt/3]).

getOpt(Opt, _, Val) :-
	format(string(Key), 'options_~a', [Opt]),
	core:ini('editor.ini', 'editor', Key, Val), !.
getOpt(_Key, Def, Def).



load :-
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
	getOpt(colortheme, 0, Theme),

	edi:setAxes(Axes),
	edi:setGrid(Grid),
	edi:setSnap(Snap),
	edi:setRoomGrid(Roomgrid),
	edi:setBrushRect(Brushrect),
	edi:setRoomW(Roomw),
	edi:setRoomH(Roomh),
	edi:setColorMap(Color),
	def:setColorTheme(Theme).








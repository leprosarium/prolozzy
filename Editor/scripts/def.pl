:- module(def, [roomW/1, roomH/1,
		color/2,
		colorTheme/3,
		setColorTheme/1,
		dlg/2,
		mb/3,
		mb/2,
		layerMax/1,
	        flip/3,
		shader/3,
		internalShader/2,
		brushType/3,
		drawMode/3,
		material/4,
		density/2,
		class/3]).

roomW(240).	% default game room width
roomH(136).	% default game room height


layerMax(8).

flip(none, 0, "none").
flip(x, 1, "flip x").
flip(y, 2, "flip y").
flip(xy, 3, "flip xy").
flip(r, 4, "flip r").
flip(xr, 5, "flip xr").
flip(yr, 6, "flip yr").
flip(xyr, 7, "flip xyr").

shader(opaque, 0, "opaque").
shader(blend, 1, "blend").
shader(add, 2, "add").
shader(mod, 3, "mod").
shader(mod2, 4, "mod2").

internalShader(alpharep, 5).

brushType(static, 0, "static").
brushType(dynamic, 1, "dynamic").

drawMode(none, 0, "none").
drawMode(img, 1, "img").
drawMode(mat, 2, "mat").
drawMode(imgmat, 3, "img+mat").

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

density(void, 0xff000000).
density(soft, 0xff606060).
density(hard, 0xffa0a0a0).
density(jump, 0xffffffff).

class(none, 0, "none").
class(action, 1, "action").
class(hurt, 2, "hurt").
class(kill, 3, "kill").
class(item, 4, "item").
class(coin, 5, "coin").
class(food, 6, "food").
class(life, 7, "life").
class(waypoint, 8, "waypoint").



% Colors



color(black, 0xff000000). % black
color(white, 0xffffffff). % white

color(tilebkgr, 0xffff00ff). % tile backgr color
color(back1, 0xff202020). % editor background
color(back2,     0xff808080). % editor background silders
color(backtext, 0xffdbffa7). % editor background text
color(grid1, 0x40ffffff). % editor grid
color(grid2, 0x80ffffff). % editor room grid
color(grid3, 0x80ffff00). % editor axes
color(map, 0xff000000). % map background

color(Color, Value) :-
	recorded(theme, Theme),
	colorTheme(Theme, Color, Value).


setColorTheme(Theme) :-
	(   recorded(theme, _, Ref)
	->  erase(Ref)
	;   true),
	recorda(theme, Theme).


colorTheme(0, gui, 0xff4d4833).
colorTheme(0, gui1, 0xff605a42).
colorTheme(0, gui2, 0xff3c3a29).
colorTheme(0, title, 0xffefa845).
colorTheme(0, title1, 0xfffbbe55).
colorTheme(0, title2, 0xffe39237).
colorTheme(0, modal, 0xffff3000).
colorTheme(0, modal1,0xffff6000).
colorTheme(0, modal2,0xffff0000).
colorTheme(0, edit, 0xff5e6d8b).
colorTheme(0, editsel, 0xfffbbe55).
colorTheme(0, layer0, 0xff4d4833).
colorTheme(0, layer1, 0xffbd540e).
colorTheme(0, layer2, 0xffffff00).
colorTheme(0, sample, 0xff4d4833).


colorTheme(1, gui, 0xff006B46).
colorTheme(1, gui1, 0xff007751).
colorTheme(1, gui2, 0xff00643F).
colorTheme(1, title, 0xff009445).
colorTheme(1, title1, 0xff009F50).
colorTheme(1, title2, 0xff006A3B).
colorTheme(1, modal, 0xff00C040).
colorTheme(1, modal1, 0xff00E05D).
colorTheme(1, modal2, 0xff00AB2C).
colorTheme(1, edit, 0xff31B582).
colorTheme(1, editsel, 0xfffbbe55).%0xff009F50).
colorTheme(1, layer0, 0xff006B46).
colorTheme(1, layer1, 0xff00BD73).
colorTheme(1, layer2, 0xff00FF9C).
colorTheme(1, sample, 0xff006B46).


colorTheme(2, gui, 0xff7A848B).
colorTheme(2, gui1, 0xff8A969E).
colorTheme(2, gui2, 0xff6E777D).
colorTheme(2, title, 0xff5D83bC).
colorTheme(2, title1, 0xff638Ac6).
colorTheme(2, title2, 0xff5172a4).
colorTheme(2, modal, 0xff7DA3DC).
colorTheme(2, modal1, 0xff83AAE6).
colorTheme(2, modal2, 0xff7192C4).
colorTheme(2, edit, 0xff90C0DF).
colorTheme(2, editsel, 0xffffffff).
colorTheme(2, layer0, 0xff7A848B).
colorTheme(2, layer1, 0xff90C0DF).
colorTheme(2, layer2, 0xffffffff).
colorTheme(2, sample, 0xff7A848B).


% IDS

dlg(back, 10).
dlg(title, 11).
dlg(item, 20).
dlg(item1, 21).
dlg(item(N), NN) :- NN is 20 + N.

% user defined ids should start from here (1000+)
dlg(menuBar, 1000).
dlg(statusBar1, 1001).
dlg(statusBar2, 1002).

dlg(tileMap, 1015).
dlg(tileBrowse, 1018).
dlg(props, 1020).
dlg(color, 1021).
dlg(options, 1022).
dlg(info, 1023).
dlg(roomProps, 1024).

%mb(first, 1100).


% menubar buttons
mb(t0, menu, 1100).
mb(t0, view, 1101).
mb(t0, tool, 1102).

mb(t1, props, 1103).
mb(t1, tile, 1104).
mb(t1, mapping, 1105).
mb(t1, flip, 1106).
mb(t1, color, 1107).
mb(t1, shader, 1108).
mb(t1, type, 1109).
mb(t1, draw, 1110).
mb(t1, material, 1111).
mb(t1, class, 1112).

mb(t2, search, 1113).
mb(t2, change, 1114).
mb(t2, script, 1115).
mb(t2, script2, 1116).
mb(t2, script3, 1117).

%mb(last, 1118).

mb(layer, 1200). %LAYER_MAX, consecutive, ids
%, 1220












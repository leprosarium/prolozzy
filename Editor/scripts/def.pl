:- module(def, [roomW/1, roomH/1,
		color/2,
		colorTheme/3,
		setColorTheme/1]).

roomW(240).	% default game room width
roomH(136).	% default game room height

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








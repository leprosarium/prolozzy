:- module(def, [roomW/1, roomH/1,
	       color/2]).

roomW(240).	% default game room width
roomH(136).	% default game room height

% Colors

color(black, 0xff000000). % black
color(white, 0xffffffff). % white

color(gui, 0xff4d4833). % gui main color (ff6f7d99)
color(gui1, 0xff605a42). % gui main color gradiant hi (ff808da8)
color(gui2, 0xff3c3a29). % gui main color gradiant lo (ff5e6d8b)
color(title, 0xffefa845). % title color
color(title1, 0xfffbbe55). % title color gradiant hi
color(title2, 0xffe39237). % title color gradiant lo
color(modal, 0xffff3000). % title color
color(modal1, 0xffff6000). % title color gradiant hi
color(modal2, 0xffff0000). % title color gradiant lo
color(edit, 0xff5e6d8b  ). % edit boxes
color(editsel, 0xffbd540e  ). % edit boxes selection
color(layer0, 0xff4d4833). % layer invisible
color(layer1, 0xffbd540e). % layer visible
color(layer2, 0xffffff00). % layer active

color(tilebkgr, 0xffff00ff). % tile backgr color
color(back1, 0xff202020). % editor background
color(back2,     0xff808080). % editor background silders
color(backtext, 0xffdbffa7). % editor background text
color(grid1, 0x40ffffff). % editor grid
color(grid2, 0x80ffffff). % editor room grid
color(grid3, 0x80ffff00). % editor axes
color(map, 0xff000000). % map background

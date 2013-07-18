:-module(message, [msg/5,
		  next/3,
		  next/1,
		  pop/0,
		  qmsg/4,
		  qmsg/5]).

:-use_module(def).

% Utility functions for displaying conversation message boxes.
% Users can define more functions, with specific color settings for
% their most common game characters.

% IN: int; x; horizontal coordinate in characters, multiple of 8 pixels [0,30)
% IN: int; y; vertical coordinate in characters, multiple of 8 pixels [0,17)
% IN: str; text; message text
% IN: int; colorink; the text color
% IN: int; colorborer; the border color
% Stores x, y, and colors, for use with the MessageNext() function.
% This does not closes the dialog. Needs DialogPop(), DialogPopAll(), or usually MessagePop().

msg(X, Y, Text, ColorInk, ColorBorder) :-
	flag(msglast_X, _, X),
	flag(msglast_Y, _, Y),
	flag(msglast_ColorInk, _, ColorInk),
	flag(msglast_ColorBorder, _, ColorBorder),
	X1 is X * 8,
	Y1 is Y * 8,
	game:pause,
	format(atom(Msg), '{c:~16r}~a', [ColorInk, Text]),
	gamedef:fontDefault(Font),
	dialog:fitAt(dialog(_, _, style(Font, ColorBorder), Msg), pos(X1, Y1), Dialog),
	dialog:push(Dialog),
	update:register(ui, game:unpause),
	dialog:run.

% IN: str; text; message text
% [IN]: int; stepx=1; horizontal offset from the last dialog's position, in characters, multiple of 8 pixels
% [IN]: int; stepy=1; vertical offset from the last dialog's position, in characters, multiple of 8 pixels
% Opens another dialog message in cascade, using the position and the colors of the last message.
% This does not closes the dialog. Needs DialogPop(), DialogPopAll(), or usually MessagePop(). See Message().


next(Text) :-
	next(Text, 1, 1).
next(Text, Stepx, Stepy) :-
	flag(msglast_X, X, X),
	flag(msglast_Y, Y, Y),
	flag(msglast_ColorInk, Ink, Ink),
	flag(msglast_ColorBorder, Border, Border),
	X1 is X + Stepx,
	Y1 is Y + Stepy,
	msg(X1, Y1, Text, Ink, Border).


% Closes all opened messages, same as DialogPopAll().
pop :-
	dialog:popAll.

% IN: int; x; horizontal coordinate in characters, multiple of 8 pixels
% IN: int; y; vertical coordinate in characters, multiple of 8 pixels
% IN: str; text; message text
% quick call for Message() with story teller color settings
qmsg(0, X, Y, Text) :-
	color(white, Ink),
	color(red, Border),
	msg(X, Y, Text, Ink, Border).


% IN: int; x; horizontal coordinate in characters, multiple of 8 pixels
% IN: int; y; vertical coordinate in characters, multiple of 8 pixels
% IN: str; text; message text
% quick call for Message() with player color settings
qmsg(1, X, Y, Text) :-
	color(magenta, Ink),
	color(green, Border),
	msg(X, Y, Text, Ink, Border).

% IN: int; x; horizontal coordinate in characters, multiple of 8 pixels
% IN: int; y; vertical coordinate in characters, multiple of 8 pixels
% IN: str; text; message text
% [IN]: int; color=COLOR_WHITE; text color
% quick call for Message() with other characters color settings
qmsg(2, X, Y, Text) :-
	color(white, Ink),
	qmsg(2, X, Y, Text, Ink).

qmsg(2, X, Y, Text, Color) :-
	color(green, Border),
	msg(X, Y, Text, Color, Border).














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

% +X; horizontal coordinate in characters, multiple of 8 pixels [0,30)
% +Y; vertical coordinate in characters, multiple of 8 pixels [0,17)
% +Text; message text
% +ColorInk; the text color
% +ColorBorer; the border color
% Stores x, y, and colors, for use with the next/1.
% This does not closes the dialog. Needs dialog:pop/0, dialog:popAll/0,
% or usually message:pop/0.

msg(X, Y, Text, ColorInk, ColorBorder) :-
	(recorded(last, _, Ref) ->  erase(Ref); true),
	recorda(last, msg(X, Y, ColorInk, ColorBorder)),
	X1 is X * 8,
	Y1 is Y * 8,
	game:pause,
	format(atom(Msg), '{c:~16r}~a', [ColorInk, Text]),
	gamedef:fontDefault(Font),
	dialog:fitAt(dialog(_, _, style(Font, ColorBorder), Msg), pos(X1, Y1), Dialog),
	dialog:push(Dialog),
	update:register(ui, game:unpause),
	dialog:run.

% +Text; message text
% +Stepx; horizontal offset from the last dialog's position, in characters, multiple of 8 pixels
% +Stepy; vertical offset from the last dialog's position, in characters, multiple of 8 pixels
% Opens another dialog message in cascade, using the position and the
% colors of the last message. This does not closes the dialog.

next(Text) :-
	next(Text, 1, 1).
next(Text, Stepx, Stepy) :-
	recorded(last, msg(X, Y, Ink, Border)),
	X1 is X + Stepx,
	Y1 is Y + Stepy,
	msg(X1, Y1, Text, Ink, Border).


% Closes all opened messages.
pop :- dialog:popAll.

% +X; horizontal coordinate in characters, multiple of 8 pixels
% +Y; vertical coordinate in characters, multiple of 8 pixels
% +Text; message text
% quick call for msg/4 with story teller color settings

tellerColors(0, white, red).
tellerColors(1, magenta, green).
tellerColors(2, white, green).

qmsg(Teller, X, Y, Text) :-
	tellerColors(Teller, Color1, Color2),
	color(Color1, Ink),
	color(Color2, Border),
	msg(X, Y, Text, Ink, Border).

qmsg(Teller, X, Y, Text, Ink) :-
	tellerColors(Teller, _, Color2),
	color(Color2, Border),
	msg(X, Y, Text, Ink, Border).













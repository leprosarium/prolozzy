:- module(editor, [init/0]).

:- use_module(gui, []).
:- use_module(mod, []).

init :-
	core:dl("editor init."),
	gui:loadResources,
	mod:init.

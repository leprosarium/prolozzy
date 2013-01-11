:- module(editor, [init/0]).

:- use_module(gui, []).
:- use_module(mod, []).
:- use_module(def, []).
:- use_module(dlgoptions, []).

init :-
	core:dl("editor init."),
	gui:loadResources,
	mod:init,
	dlgoptions:load,
	dlgoptions:colorTheme.

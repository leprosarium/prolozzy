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


	% editor colors
	def:color(back1, BACK1), edi:setColorBack1(BACK1),
	def:color(back2, BACK2), edi:setColorBack2(BACK2),
	def:color(grid1, GRID1), edi:setColorGrid1(GRID1),
	def:color(grid2, GRID2), edi:setColorGrid2(GRID2),
	def:color(grid3, GRID3), edi:setColorGrid3(GRID3).

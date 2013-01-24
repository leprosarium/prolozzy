:- module(editor, [init/0,
		  close/0]).

:- use_module(gui, []).
:- use_module(mod, []).
:- use_module(def, []).
:- use_module(dlgoptions, []).
:- use_module(dlgMenuBar, []).
:- use_module(dlgStatusBar, []).
:- use_module(dlgColor, []).
:- use_module(keys, []).
:- use_module(actions, []).
:- use_module(dlgInfo, []).
:- use_module(roomNames, []).
:- use_module(fileio, []).

init :-
	core:dl("editor init."),
	gui:loadResources,
	dlgOptions:load,


	% editor colors
	def:color(back1, BACK1), edi:setColorBack1(BACK1),
	def:color(back2, BACK2), edi:setColorBack2(BACK2),
	def:color(grid1, GRID1), edi:setColorGrid1(GRID1),
	def:color(grid2, GRID2), edi:setColorGrid2(GRID2),
	def:color(grid3, GRID3), edi:setColorGrid3(GRID3),

	% MenuBar
	dlgMenuBar:create,

	dlgStatusBar:create,

	dlgColor:init,
	(   edi:tileReload
	->  (   edi:tileCount(0)
	    ->	gui:msgBoxOk("Warning","No tiles loaded.\nCheck the path and the tiles folder.", icon_warning)
	    ;	true)
	;   actions:options), %failed to load tiles, check the tiles folder

	roomNames:reset(false),


	% initialize default static brush
	mod:brushNew(static).

% also called on Alt+F4
close :-
	gui:msgBox("Question", "Do you want to exit the editor?\n(current map will be lost if not saved)", icon_question, [btn("EXIT", edi:exit), btn("CANCEL",true)]),
	gui:dlgAddKeys([return > (gui:dlgClose, edi:exit),
			escape > gui:dlgClose]).

:- module(editor, [init/0,
		   close/0,
		   done/0,
		   param/3,
		   param/2]).
:- abolish(system:get/2).
:- use_module(gui, []).
:- use_module(mod, []).
:- use_module(def, []).
:- use_module(dlgOptions, []).
:- use_module(dlgMenuBar, []).
:- use_module(dlgStatusBar, []).
:- use_module(dlgTileBrowse, []).
:- use_module(dlgColor, []).
:- use_module(dlgTileMap, []).
%:- use_module(dlgProps, []).
:- use_module(dlgProps2, []).
:- use_module(dlgRoomProps, []).
:- use_module(dlgBrushProps, []).
:- use_module(keys, []).
:- use_module(actions, []).
:- use_module(dlgInfo, []).
:- use_module(roomNames, []).
:- use_module(fileio, []).
:- use_module(brush, []).
:- use_module(scripts, []).
:- use_module(scripts2, []).


param(Key, Value) :-
	core:ini('editor.ini', 'editor', Key, Value).
param(Key, _, Value) :-
	param(Key, Value).
param(_, Def, Def).


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

	dlgMenuBar:create,
	dlgStatusBar:create,
	dlgColor:init,
	(   edi:tileReload
	->  (   edi:tileCount(0)
	    ->	gui:msgBoxOk("Warning","No tiles loaded.\nCheck the path and the tiles folder.", icon_warning)
	    ;	true)
	;   actions:options), %failed to load tiles, check the tiles folder

	roomNames:reset(false),
	mod:brushNew(static).   % initialize default static brush

% also called on Alt+F4
close :-
	gui:msgBox('Question', 'Do you want to exit the editor?\n(current map will be lost if not saved)', icon_question, [btn('EXIT', edi:exit), btn('CANCEL', true)]),
	gui:dlgAddKeys([return > (gui:dlgClose, edi:exit),
			escape > gui:dlgClose]).
done :-
	dlgOptions:save,
	core:dl('editor done.').




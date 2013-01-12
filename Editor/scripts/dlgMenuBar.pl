:- module(dlgMenuBar, [create/0]).




create :-
	edi:getScrW(BarW),
	BarH = 32,
	Style = [backgr, border3d],
	gui:createDlg(0, 0, BarW, BarH, Style).

%func DlgMenuBar_Create()
%{
%	// dialog
%	barw = EdiGet(EDI_SCRW);
%	barh = 32;
%	style = GUISTYLE_BACKGR | GUISTYLE_BORDER3D;
%	CreateDlg( 0, 0, barw, barh, style);
%	DlgSetInt( DV_ID, ID_DLGMENUBAR );
%
%	// shortcuts
%	DlgSetInt( DV_TESTKEY,	1 ); // always test keys
%	DlgAddKey( KEY_GRAVE,	0,					"Act_Menu();" );
%	DlgAddKey( KEY_1,		0,					"Act_Menu();" );
%	DlgAddKey( KEY_2,		0,					"Act_View();" );
%	DlgAddKey( KEY_SPACE,	0,					"Act_Tool();" );
%	DlgAddKey( KEY_P,		0,					"Act_Props();" );
%	DlgAddKey( KEY_B,	0,					"Act_Tile();" );
%	DlgAddKey( KEY_M,	0,					"Act_Mapping();" );
%	DlgAddKey( KEY_F,	0,					"Act_JustFlip();" );
%	DlgAddKey( KEY_R,	0,					"Act_JustRotate();" );
%	DlgAddKey( KEY_C,	0,					"Act_Color();" );
%	DlgAddKey( KEY_C,	GUIKEYFLAG_SHIFT,	"Act_ColorWin();" );
%
%	DlgAddKey( KEY_EQUALS,	0,					"Act_ZoomSet(1);" );
%	DlgAddKey( KEY_MINUS,	0,					"Act_ZoomSet(-1);" );
%	DlgAddKey( KEY_ADD,	0,					"Act_ZoomSet(1);" );	// on numpad
%	DlgAddKey( KEY_SUBTRACT,0,					"Act_ZoomSet(-1);" );	// on numpad
%	DlgAddKey( KEY_O,		GUIKEYFLAG_CTRL,	"Act_FileOpen();" );
%	DlgAddKey( KEY_S,		GUIKEYFLAG_CTRL,	"Act_FileSave(1);" );
%	DlgAddKey( KEY_F1,		0,					"Act_Help();" );
%	DlgAddKey( KEY_F2,		0,					"MarkerGoto(1);" );
%	DlgAddKey( KEY_F2,		GUIKEYFLAG_SHIFT,	"MarkerGoto(-1);" );
%	DlgAddKey( KEY_F2,		GUIKEYFLAG_CTRL,	"MarkerToggle( EdiGet(EDI_CAMX), EdiGet(EDI_CAMY) );" );
%	DlgAddKey( KEY_F3,		0,					"SelectionGoto(1);" );
%	DlgAddKey( KEY_F3,		GUIKEYFLAG_SHIFT,	"SelectionGoto(-1);" );
%	DlgAddKey( KEY_F,		GUIKEYFLAG_CTRL,	"Act_Search();" );
%	DlgAddKey( KEY_R,		GUIKEYFLAG_CTRL,	"Act_RoomProps();" );
%	DlgAddKey( KEY_U,		0,					"Act_Script3();" );
%
%	// buttons
%	DlgMenuBar_AddButton(	ID_MB_MENU,		"menu",		"system menu [`]/[1]",				"Act_Menu();"		);
%	DlgMenuBar_AddButton(	ID_MB_VIEW,		"view",		"view mode [2]",						"Act_View();"		);
%	DlgMenuBar_AddButton(	ID_MB_TOOL,		"tool",		"switch tool [SPACE]",				"Act_Tool();"		);
%
%	DlgMenuBar_AddButton(	ID_MB_PROPS,	"props",	"brush properties [P]",					"Act_Props();"	);
%	DlgMenuBar_AddButton(	ID_MB_TILE,		"tile",		"brush tile [B]",					"Act_Tile();"		);
%	DlgMenuBar_AddButton(	ID_MB_MAPPING,	"mapping",	"brush mapping [M]",					"Act_Mapping();"	);
%	DlgMenuBar_AddButton(	ID_MB_FLIP,		"flip",		"brush flip [F]",					"Act_Flip();"		);
%	DlgMenuBar_AddButton(	ID_MB_COLOR,	"color",	"brush color [C]",					"Act_Color();"		);
%	DlgMenuBar_AddButton(	ID_MB_SHADER,	"shader",	"brush shader",						"Act_Shader();"		);
%	DlgMenuBar_AddButton(	ID_MB_TYPE,		"type",		"brush type",							"Act_Type();"		);
%	DlgMenuBar_AddButton(	ID_MB_DRAW,		"draw",		"brush draw mode",						"Act_Draw();"		);
%	DlgMenuBar_AddButton(	ID_MB_MATERIAL,	"material",	"brush material",						"Act_Material();"	);
%	DlgMenuBar_AddButton(	ID_MB_CLASS,	"class",	"brush class",						"Act_Class();"		);
%
%	DlgMenuBar_AddButton(	ID_MB_SEARCH,	"search",	"search brushes and select",			"Act_Search();"		);
%	DlgMenuBar_AddButton(	ID_MB_CHANGE,	"change",	"change selected brushes",			"Act_Change();"		);
%	DlgMenuBar_AddButton(	ID_MB_SCRIPT,	"script",	"select scripts",				"Act_Script();"		);
%	DlgMenuBar_AddButton(	ID_MB_SCRIPT2,	"script2",	"debug scripts",				"Act_Script2();"	);
%	DlgMenuBar_AddButton(	ID_MB_SCRIPT3,	"script3",	"user scripts",					"Act_Script3();"	);
%
%	DlgMenuBar_Refresh();
%
%	// layers
%	size = 16;
%	x = barw-2-size*LAYER_MAX;
%	for(i=0;i<LAYER_MAX;i++)
%	{
%		LayerCreateButton(i,x,2);
%		x+=size;
%	}
%}

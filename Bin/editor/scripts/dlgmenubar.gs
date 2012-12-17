//////////////////////////////////////////////////////////////////////////////////
// DlgMenuBar Dialog
//////////////////////////////////////////////////////////////////////////////////
func DlgMenuBar_Create()
{
	// dialog
	barw = EdiGet(EDI_SCRW);
	barh = 32;
	style = GUISTYLE_BACKGR | GUISTYLE_BORDER3D;
	CreateDlg( 0, 0, barw, barh, style);
	DlgSetInt( DV_ID, ID_DLGMENUBAR );

	// shortcuts
	DlgSetInt( DV_TESTKEY, 	1 ); // always test keys
	DlgAddKey( KEY_GRAVE,	0,					"Act_Menu();" );
	DlgAddKey( KEY_1,		0,					"Act_Menu();" );
	DlgAddKey( KEY_2,		0,					"Act_View();" );
	DlgAddKey( KEY_SPACE,  	0, 					"Act_Tool();" );
	DlgAddKey( KEY_P,	  	0, 					"Act_Props();" );
	DlgAddKey( KEY_B,   	0, 					"Act_Tile();" );
	DlgAddKey( KEY_M,   	0, 					"Act_Mapping();" );
	DlgAddKey( KEY_F,   	0, 					"Act_JustFlip();" );
	DlgAddKey( KEY_R,   	0, 					"Act_JustRotate();" );
	DlgAddKey( KEY_C,   	0, 					"Act_Color();" );
	DlgAddKey( KEY_C,   	GUIKEYFLAG_SHIFT,	"Act_ColorWin();" );

	DlgAddKey( KEY_EQUALS, 	0, 					"Act_ZoomSet(1);" );
	DlgAddKey( KEY_MINUS,	0, 					"Act_ZoomSet(-1);" );
	DlgAddKey( KEY_ADD,   	0, 					"Act_ZoomSet(1);" );	// on numpad
	DlgAddKey( KEY_SUBTRACT,0, 					"Act_ZoomSet(-1);" );	// on numpad
	DlgAddKey( KEY_O,		GUIKEYFLAG_CTRL, 	"Act_FileOpen();" );
	DlgAddKey( KEY_S,		GUIKEYFLAG_CTRL, 	"Act_FileSave(1);" );
	DlgAddKey( KEY_F1,		0, 					"Act_Help();" );
	DlgAddKey( KEY_F2,		0, 					"MarkerGoto(1);" );
	DlgAddKey( KEY_F2,		GUIKEYFLAG_SHIFT, 	"MarkerGoto(-1);" );
	DlgAddKey( KEY_F2,		GUIKEYFLAG_CTRL,	"MarkerToggle( EdiGet(EDI_CAMX), EdiGet(EDI_CAMY) );" );
	DlgAddKey( KEY_F3,		0, 					"SelectionGoto(1);" );
	DlgAddKey( KEY_F3,		GUIKEYFLAG_SHIFT, 	"SelectionGoto(-1);" );
	DlgAddKey( KEY_F,		GUIKEYFLAG_CTRL,	"Act_Search();" );
	DlgAddKey( KEY_R,		GUIKEYFLAG_CTRL,	"Act_RoomProps();" );
	DlgAddKey( KEY_U,		0,					"Act_Script3();" );

	// buttons
	DlgMenuBar_AddButton(	ID_MB_MENU,		"menu",		"system menu [`]/[1]",      			"Act_Menu();" 		);
	DlgMenuBar_AddButton(	ID_MB_VIEW,		"view",		"view mode [2]",   						"Act_View();" 		);
	DlgMenuBar_AddButton(	ID_MB_TOOL,		"tool",		"switch tool [SPACE]",      			"Act_Tool();" 		);

	DlgMenuBar_AddButton(	ID_MB_PROPS,	"props",	"brush properties [P]",    				"Act_Props();" 	);
	DlgMenuBar_AddButton(	ID_MB_TILE,		"tile",		"brush tile [B]",    					"Act_Tile();" 		);
	DlgMenuBar_AddButton(	ID_MB_MAPPING,	"mapping",	"brush mapping [M]",		 			"Act_Mapping();" 	);
	DlgMenuBar_AddButton(	ID_MB_FLIP,		"flip",		"brush flip [F]",    					"Act_Flip();" 		);
	DlgMenuBar_AddButton(	ID_MB_COLOR,	"color",	"brush color [C]",   					"Act_Color();" 		);
	DlgMenuBar_AddButton(	ID_MB_SHADER,	"shader",	"brush shader",       					"Act_Shader();" 	);
	DlgMenuBar_AddButton(	ID_MB_TYPE,		"type",		"brush type",							"Act_Type();" 		);
	DlgMenuBar_AddButton(	ID_MB_DRAW,		"draw",		"brush draw mode",						"Act_Draw();" 		);
	DlgMenuBar_AddButton(	ID_MB_MATERIAL,	"material",	"brush material",						"Act_Material();"	);
	DlgMenuBar_AddButton(	ID_MB_CLASS,	"class",	"brush class",       					"Act_Class();" 		);

	DlgMenuBar_AddButton(	ID_MB_SEARCH,	"search",	"search brushes and select",   			"Act_Search();" 	);
	DlgMenuBar_AddButton(	ID_MB_CHANGE,	"change",	"change selected brushes",     			"Act_Change();" 	);
	DlgMenuBar_AddButton(	ID_MB_SCRIPT,	"script",	"select scripts",             			"Act_Script();" 	);
	DlgMenuBar_AddButton(	ID_MB_SCRIPT2,	"script2",	"debug scripts",             			"Act_Script2();" 	);
	DlgMenuBar_AddButton(	ID_MB_SCRIPT3,	"script3",	"user scripts",             			"Act_Script3();" 	);

	DlgMenuBar_Refresh();
	
	// layers
	size = 16;
	x = barw-2-size*LAYER_MAX;
	for(i=0;i<LAYER_MAX;i++)
	{
		LayerCreateButton(i,x,2);
		x+=size;
	}
}

// reposition the visible buttons
func DlgMenuBar_Reposition()
{
	x=0; barh=32;
	for(i=ID_MB_FIRST;i<=ID_MB_LAST;i++)
	{
		idx = ItemFind(i); 
		if(idx==-1) continue;
		ItemSelect(idx);
		if(ItemGetInt(IV_HIDDEN)) continue;
		ItemSetInt(IV_RECT,x,0,x+barh,barh);
		x+=barh;
	}
}

// visibility dependences and repositioning
func DlgMenuBar_Refresh()
{
	tool = EdiGet(EDI_TOOL);
	for(i=ID_MB_PROPS;i<=ID_MB_CLASS;i++)
		DlgMenuBar_ShowButton(i,tool==0);
	for(i=ID_MB_SEARCH;i<=ID_MB_SCRIPT3;i++)
		DlgMenuBar_ShowButton(i,tool==1);
}


//////////////////////////////////////////////////////////////////////////////////
// utils
//////////////////////////////////////////////////////////////////////////////////
func DlgMenuBar_AddButton( id, img, tooltip, cmd )
{
	DlgSelect(DlgFind(ID_DLGMENUBAR));
	barh = 32;
	img0 = ImgLoad( "Editor\\Graphics\\mb1_" + img + ".tga" );
	img1 = ImgLoad( "Editor\\Graphics\\mb2_" + img + ".tga" );
	CreateButtonImg( 0, 0, barh, barh, img0, img1, cmd );
	ItemSetInt(IV_ID, id); 
	ItemSetInt(IV_STYLE, 0); 
	ItemSetTxt(IV_TOOLTIP, tooltip);
}

func DlgMenuBar_AddKey( key, keyflag, keycmd )
{
	DlgSelect(DlgFind(ID_DLGMENUBAR));
	DlgAddKey(key, keyflag, keycmd);
}

func DlgMenuBar_ShowButton( id, show )
{
	DlgSelect(DlgFind(ID_DLGMENUBAR));
	idx = ItemFind(id); 
	if(idx==-1) return;
	ItemSelect(idx);
	ItemSetInt(IV_HIDDEN, !show);
	ItemSetInt(IV_DISABLE, !show);
	DlgMenuBar_Reposition();
}

func LayerCreateButton( layer, x, y )
{
	if(layer<0 || layer>=LAYER_MAX) return;
	ItemNew("cGUICheck");
	ItemSetInt(IV_ID, ID_MB_LAYER+layer);
	ItemSetInt(IV_RECT, x,y,x+16,y+28);
	ItemSetTxt(IV_TXT, (str)(layer));
	ItemSetInt(IV_TXTALIGN, GUIALIGN_CENTERXY);
	style = GUISTYLE_BACKGR | GUISTYLE_BORDER3D;
	ItemSetInt(IV_STYLE, style);
	ItemSetInt(IV_COLOR, COLOR_LAYER1, COLOR_LAYER1, COLOR_GUI);
	ItemSetInt(IV_VALUE, 1);	
	ItemSetTxt(IV_CMDACTION,"Act_Layer("+(str)layer+");");
}

func LayerSetButton( layer, status )
{
	DlgSelect(DlgFind(ID_DLGMENUBAR));
	if(layer<0 || layer>=LAYER_MAX) return;
	ItemSelect(ItemFind(ID_MB_LAYER+layer));
	ItemSetInt(IV_VALUE, status);	
	if(status==0) color = COLOR_LAYER0; else 
	if(status==1) color = COLOR_LAYER1; else 
	if(status==2) color = COLOR_LAYER2;
	ItemSetInt(IV_COLOR, color);
	LayerSet(layer,status);
}

//////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// Editor main file
//////////////////////////////////////////////////////////////////////////////////

#include "Editor\\Scripts\\keys.gs"
#include "Editor\\Scripts\\def.gs"
#include "Editor\\Scripts\\gui.gs"

// mod
#include "Editor\\Scripts\\gamemod.gs"

// editor
#include "Editor\\Scripts\\dlgmenubar.gs"
#include "Editor\\Scripts\\dlgstatusbar.gs"
#include "Editor\\Scripts\\dlgoptions.gs"
#include "Editor\\Scripts\\dlginfo.gs"
#include "Editor\\Scripts\\dlgcolor.gs"
#include "Editor\\Scripts\\dlgtilemap.gs"
#include "Editor\\Scripts\\dlgtilebrowse.gs"
#include "Editor\\Scripts\\dlgprops.gs"
#include "Editor\\Scripts\\dlghelp.gs"
#include "Editor\\Scripts\\scripts.gs"
#include "Editor\\Scripts\\scripts2.gs"
#include "Editor\\Scripts\\fileio.gs"
#include "Editor\\Scripts\\actions.gs"
#include "Editor\\Scripts\\roomnames.gs"
#include "Editor\\Scripts\\roomtexts.gs"
#include "Editor\\Scripts\\roomprops.gs"
#include "Editor\\Scripts\\dlgroomprops.gs"

// user scripts
#include "editor.gs"

//////////////////////////////////////////////////////////////////////////////////
func EDI_Init()
{
	println("editor init.");
	GuiLoadResources(); // load gui resources
	
	// initialize game module
	MOD_Init();
	if( (!?g_brushprop) ) error("No brush props defined!");

	if( (!?g_view) || (!?g_viewcount) || (!?g_viewname) ) error("No view modes defined!");
	if( g_viewcount==0 ) error("No view modes defined!");
	
	// Options
//	DlgOptions_Load();
	DlgOptions_ColorTheme();

	// editor colors
//	EdiSet( EDI_COLORBACK1, 	COLOR_BACK1 ); 
//	EdiSet( EDI_COLORBACK2, 	COLOR_BACK2 ); 
//	EdiSet( EDI_COLORGRID1,		COLOR_GRID1 ); 
//	EdiSet( EDI_COLORGRID2,		COLOR_GRID2 ); 
//	EdiSet( EDI_COLORGRID3,		COLOR_GRID3 ); 
//	EdiSet( EDI_COLORMAP,		COLOR_MAP ); 
	
	// MenuBar
//	DlgMenuBar_Create();
//	LayerSetButton(0,2);
	
	// Status Bar
//	DlgStatusBar_Create();
	
	// DlgColor
	DlgColor_Init();

	// Load tiles
	ok = TileReload();
	if(!ok) // failed to load tiles, check the tiles folder
		Act_Options();
	else
	if(TileCount()==0)
		MsgBoxOk("Warning","No tiles loaded.\nCheck the path and the tiles folder.",ICON_WARNING );

	g_mapfile = "noname.map";
	RoomNamesReset(0);
	RoomTextsReset(0);
	RoomPropsReset(0);
	
	// initialize default static brush
	MOD_BrushNew(0);
}

func EDI_Done()
{
	DlgOptions_Save();
	MOD_Done();
	println("editor done.");
}

func EDI_Close()
{
	// also called on Alt+F4
	MsgBox("Question", "Do you want to exit the editor?\n(current map will be lost if not saved)", ICON_QUESTION, {{"EXIT","EdiExit();"}, {"CANCEL",""}} );
	DlgAddKey( KEY_RETURN,0,"DlgClose();EdiExit();" );
	DlgAddKey( KEY_ESCAPE,0,"DlgClose();" ); 
}

func EDI_Load( filename )
{
	g_mapfile = strtrim(filename,"\"");
	println("loading map ",g_mapfile," ...");
	ToolReset(); // safe
	WaitCursor(1);
	ret = MAPLoad(g_mapfile);
	WaitCursor(0);
	if(!ret) MsgBoxOk("Error", "File open failed.\nFile might be incorrect or damaged.", ICON_ERROR );
}

//////////////////////////////////////////////////////////////////////////////////

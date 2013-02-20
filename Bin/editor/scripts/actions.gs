//////////////////////////////////////////////////////////////////////////////////
// Actions
//////////////////////////////////////////////////////////////////////////////////

func Act_Script3()
{
	if(EdiGet(EDI_TOOL)!=1) return;
	datatab = MOD_GetUserScripts();
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse(); DlgDockUp();
}

func Act_FileExport()
{
	datatab = { 
		{"Export",""},
		{" image map", 	"DlgClose();MAPExportImage();",	-1, 	"export image map"},
		{" text map", 	"DlgClose();MAPExportText();",	-1, 	"export text map"},
		{" brushes ids","DlgClose();ExportBrushIds(1);",	-1,		"export ids from all selected brushes"}
	};
	CreatePullDown( 0,0, &datatab );
	DlgMoveToMouse();// DlgDockUp();	
}

func Act_Help()
{
	DlgHelp_Create();
}

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

func Act_Help()
{
	DlgHelp_Create();
}

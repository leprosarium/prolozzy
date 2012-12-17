//////////////////////////////////////////////////////////////////////////////////
// DlgStatusBar Dialog
// Those are some text controls that are updated per frame from
// the MOD_UserUpdate callback to show status info
// Users use SetStatusBar1, SetStatusBar2, SetStatusBar3 and SetStatusBar4 to set their texts
//////////////////////////////////////////////////////////////////////////////////
func DlgStatusBar_Create()
{
	barw = EdiGet(EDI_SCRW);

	// dialog top
	DlgNew(); DlgSetInt(DV_RECT, 0, 30, barw, 30+16); DlgSetInt( DV_ID, ID_DLGSTATUSBAR1 );
	CreateText( 0, 0, barw, "", GUIALIGN_LEFT|GUIALIGN_CENTERY ); 
	ItemSetInt( IV_TXTCOLOR, COLOR_BACKTEXT );
	ItemSetInt( IV_ID, ID_DLGITEM+0 );
	CreateText( 0, 0, barw, "", GUIALIGN_RIGHT|GUIALIGN_CENTERY );
	ItemSetInt( IV_TXTCOLOR, COLOR_BACKTEXT );
	ItemSetInt( IV_ID, ID_DLGITEM+1 );

	// dialog bottom
	DlgNew(); DlgSetInt(DV_RECT, 0, EdiGet(EDI_SCRH)-17, barw, EdiGet(EDI_SCRW)); DlgSetInt( DV_ID, ID_DLGSTATUSBAR2 );
	CreateItem( 0, 0, barw, 17, GUISTYLE_BACKGR, NUL, COLOR_GUI, COLOR_GUI, COLOR_GUI ); ItemSetInt(IV_ID,ID_DLGBACK);
	CreateText( 0, -1, barw, "", GUIALIGN_LEFT|GUIALIGN_CENTERY ); 
	ItemSetInt( IV_TXTCOLOR, COLOR_BLACK );
	ItemSetInt( IV_ID, ID_DLGITEM+0 );
	CreateText( 0, -1, barw, "", GUIALIGN_RIGHT|GUIALIGN_CENTERY );
	ItemSetInt( IV_TXTCOLOR, COLOR_BLACK );
	ItemSetInt( IV_ID, ID_DLGITEM+1 );

}

func SetStatusBar1( text )
{
	DlgSelect(DlgFind(ID_DLGSTATUSBAR1));
	ItemSelect(ItemFind(ID_DLGITEM+0));
	ItemSetTxt(IV_TXT,text);
}

func SetStatusBar2( text )
{
	DlgSelect(DlgFind(ID_DLGSTATUSBAR1));
	ItemSelect(ItemFind(ID_DLGITEM+1));
	ItemSetTxt(IV_TXT,text);
}

func SetStatusBar3( text )
{
	DlgSelect(DlgFind(ID_DLGSTATUSBAR2));
	ItemSelect(ItemFind(ID_DLGITEM+0));
	ItemSetTxt(IV_TXT,text);
}

func SetStatusBar4( text )
{
	DlgSelect(DlgFind(ID_DLGSTATUSBAR2));
	ItemSelect(ItemFind(ID_DLGITEM+1));
	ItemSetTxt(IV_TXT,text);
}

//////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// Room Props 
// dialog with room properties: name, texts, numeric
//////////////////////////////////////////////////////////////////////////////////
func DlgRoomProps_Create( rx, ry )
{
	dlgh=DLGTITLEH+8+22*(1+1+1+4+1+2)+8;
	CreateDlgTitle( 0, 0, 200+16, dlgh, "ROOM_"+(str)rx+"_"+(str)ry, 1 );
	DlgSetInt( DV_ID, ID_DLGROOMPROPS );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgClose(0);");
	DlgSetInt( DV_CLOSEOUT, 1 );
	DlgSetTxt( DV_CLOSECMD, "DlgRoomProps_Close("+(str)rx+","+(str)ry+");" );
	
	x = 8; y = DLGTITLEH+8; h=22;

	// create name
	CreateText( x, y, 32, "Room name");
	y+=h;
	CreateEdit( x, y, 200, RoomNamesGet(rx,ry) );
	ItemSetInt(IV_ID,ID_DLGITEM+0); 
	ItemSetInt(IV_USER+1,128); ItemBuild(); // resize editbox
	y+=h;

	// create custom texts
	CreateText( x, y, 32, "Room custom texts");
	y+=h;
	for(i=0;i<MAX_ROOMTEXTS;i++)
	{
		CreateEdit( x, y, 200, RoomTextsGet(rx,ry,i) );
		ItemSetInt(IV_ID,ID_DLGITEM+1+i); 
		ItemSetInt(IV_USER+1,255); ItemBuild(); // resize editbox
		ItemSetTxt(IV_CMDACTION,"DlgRoomPropsBrowseText("+(str)i+");");
		y+=h;
	}
	
	// create numeric props
	CreateText( x, y, 32, "Room numeric properties");
	y+=h;
	col=4;
	for(i=0;i<MAX_ROOMPROPS/col;i++)
	{
		x = 8;
		for(j=0;j<col;j++)
		{
			idx = i*col+j;
			CreateEdit( x, y, 40, (str)RoomPropsGet(rx,ry,idx) );
			ItemSetInt(IV_ID,ID_DLGITEM+1+MAX_ROOMTEXTS+idx); 
			ItemSetTxt(IV_CMDACTION,"DlgRoomPropsBrowseProp("+(str)idx+");");
			x+=50;
		}
		y+=h;
	
	}
	
	DlgMoveToMouse(); DlgDockUp();
}

// refresh map when close dialog
func DlgRoomProps_Close(rx,ry)
{
	// set name
	ItemSelect(ItemFind(ID_DLGITEM+0));
	name = strupr(ItemGetTxt(IV_TXT));
	RoomNamesSet(rx,ry,name);
	
	// set texts
	for(i=0;i<MAX_ROOMTEXTS;i++)
	{
		ItemSelect(ItemFind(ID_DLGITEM+1+i));
		RoomTextsSet(rx,ry,i,ItemGetTxt(IV_TXT));
	}

	// set props
	for(i=0;i<MAX_ROOMPROPS;i++)
	{
		ItemSelect(ItemFind(ID_DLGITEM+1+MAX_ROOMTEXTS+i));
		RoomPropsSet(rx,ry,i,(int)ItemGetTxt(IV_TXT));
	}
	
	MapRefresh();
}

func DlgRoomPropsBrowseText(idx)
{
	if(ItemGetInt(IV_CMDACTIONPARAM)==2) MOD_RoomTextsBrowser(idx);
}

func DlgRoomPropsBrowseProp(idx)
{
	if(ItemGetInt(IV_CMDACTIONPARAM)==2) MOD_RoomPropsBrowser(idx);
}


// for use in custom browser
func DlgRoomProps_GetProp(idx)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGROOMPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+1+MAX_ROOMTEXTS+idx));
	val = (int)ItemGetTxt(IV_TXT);
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
	return val;
}

// for use in custom browser
func DlgRoomProps_SetProp(idx,val)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGROOMPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+1+MAX_ROOMTEXTS+idx));
	ItemSetTxt(IV_TXT,(str)val);
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
}

// for use in custom browser
func DlgRoomProps_GetText(idx)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGROOMPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+1+idx));
	txt = ItemGetTxt(IV_TXT);
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
	return txt;
}

// for use in custom browser
func DlgRoomProps_SetText(idx,txt)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGROOMPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+1+idx));
	ItemSetTxt(IV_TXT,txt);
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
}

//////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// Brush Props 
// generic dialog with brush properties
// mode: 0=normal, 1=with check boxes for search, 2=with checkboxes for change
// brushidx: map brush index to set props to, or -1 for the current toolbrush
// can't change position, size and selection for the brush (would repartition and stuff)
//////////////////////////////////////////////////////////////////////////////////
#def DLGPROPS_COUNT	16 // props per page
int g_dlgprop_brushidx;
int g_dlgprop_mode;

func DlgProp_IsRestricted( idx ) // tells if a brush prop is restricted from edit
{
	return (idx==BRUSH_X)||(idx==BRUSH_Y)||(idx==BRUSH_W)||(idx==BRUSH_H)||(idx==BRUSH_SELECT);
}

func DlgProps_Create( mode, brushidx )
{
	if(!?mode) mode=0;
	if(!?brushidx) brushidx=-1;
	g_dlgprop_brushidx = brushidx;
	g_dlgprop_mode = mode;
	
	dlgh=DLGTITLEH+8+24+22*DLGPROPS_COUNT+8;
	if(brushidx>=0)	title = "#"+(str)brushidx+" brush props";
	else			title = "Tool brush props";
	CreateDlgTitle( 0, 0, 200, dlgh, title, 1 );
	DlgSetInt( DV_ID, ID_DLGPROPS );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgClose(0);");
	DlgSetInt( DV_CLOSEOUT, 1 );
	DlgSetTxt( DV_CLOSECMD, "DlgProps_Close();" );
	
	// create props
	wmax = 100;
	x = 0; y = DLGTITLEH+8+24; 
	for(idx=0; idx<BRUSH_MAX; idx++)
	{
		w = 8;
		
		// index
		CreateText( x+w, y, 32, "#"+(str)idx, GUIALIGN_LEFT|GUIALIGN_CENTERY );
		ItemSetInt(IV_ID, ID_DLGITEM+idx*10+0);
		ItemSetInt(IV_STYLE, GUISTYLE_BACKGR|GUISTYLE_BORDER);
		ItemSetInt(IV_COLOR, COLOR_GUI1, COLOR_GUI1, COLOR_BLACK);
		w+=32+8;
		
		// multi-checks
		if(mode!=0)
		{
			CreateImage( x+w, y, 20, 20, img_check1 );
			ItemSetInt(IV_IMGCOLOR, COLOR_EDIT);
			ItemSetInt(IV_VALUE, 0);
			ItemSetTxt(IV_CMDACTION,"DlgProps_MultiCheck();");

			//CreateCheck( x+w, y, 0 ); // old version with simple check
			ItemSetInt(IV_ID,ID_DLGITEM+idx*10+1);
			if(DlgProp_IsRestricted(idx))
			{
				ItemSetInt(IV_DISABLE,1);
				ItemSetInt(IV_IMGCOLOR, COLOR_GUI1);
			}
			w+=20+8;
		}

		// value edit
		if(brushidx>=0)	value = MapBrushGet(brushidx,idx);
		else			value = ToolBrushGet(idx);
		szvalue = (idx==BRUSH_COLOR) ? ((str "%x")value) : ((str)value);
		CreateEdit( x+w, y, 80, szvalue );
		ItemSetInt(IV_ID, ID_DLGITEM+idx*10+2);
		ItemSetTxt(IV_CMDACTION, "DlgProps_InputSet("+(str)idx+");");
		if(DlgProp_IsRestricted(idx)) ItemSetInt(IV_DISABLE,1);
		w+=80+8;

		// name + browse
		tw = 140;
		name = g_brushprop[idx][BRUSHPROP_NAME] + DlgProps_GetPropValueName(idx);
		CreateButton( x+w, y, tw, name );
		ItemSetInt(IV_ID, ID_DLGITEM+idx*10+3);
		ItemSetInt(IV_TXTALIGN, GUIALIGN_LEFT|GUIALIGN_CENTERY);
		ItemSetTxt(IV_TOOLTIP, g_brushprop[idx][BRUSHPROP_TOOLTIP]);
		ItemSetInt(IV_STYLE,GUISTYLE_BACKGR|GUISTYLE_BORDER3D);
		if( g_brushprop[idx][BRUSHPROP_BROWSE]!=0 )
			ItemSetTxt(IV_CMDACTION,"DlgProps_Browse("+(str)idx+");");
		if(DlgProp_IsRestricted(idx)) ItemSetInt(IV_DISABLE,1);
		w+=tw+8;
		
		if(w>wmax) wmax = w;
		y+=22;
	}

	// create page tabs
	page = 0;
	gs_inigetint( INIFILE, "editor", "props_page", &page );

	x=8; y=DLGTITLEH+8; w=(wmax-16)/3;
	CreateRadioButton(x,y,w, "BRUSH",  (page==0),1,"DlgProps_SetPage(0);"); x+=w;
	CreateRadioButton(x,y,w, "OBJECT", (page==1),1,"DlgProps_SetPage(1);"); x+=w;
	CreateRadioButton(x,y,w, "USER" ,  (page==2),1,"DlgProps_SetPage(2);"); x+=w;
	
	DlgProps_SetPage(page);
	
	DlgResize(0,0,wmax,dlgh );
	DlgMoveToMouse(); DlgDockUp();
}

// return the value name (if suitable) that is written on the browse button
func DlgProps_GetPropValueName( idx )
{
	if(g_dlgprop_brushidx>=0)	value = MapBrushGet(g_dlgprop_brushidx,idx);
	else						value = ToolBrushGet(idx);

	if( g_brushprop[idx][BRUSHPROP_BROWSE]==USERBROWSE_SELECT )
	{
		valuename = &(g_brushprop[idx][BRUSHPROP_BROWSESEL]);
		if( !?valuename || value<0 || value>=sizeof(*valuename) ) return " = unknown";
		return " = " + (*valuename)[value];
	}
	else
	if( g_brushprop[idx][BRUSHPROP_BROWSE]==USERBROWSE_COLOR )
	{
		return " = " + (str "%08X")value;
	}
	else return "";	
}

// updates value names on the browse buttons
func DlgProps_Update()
{
	for(idx=0; idx<BRUSH_MAX; idx++)
	{
		ItemSelect(ItemFind(ID_DLGITEM+idx*10+3));
		name = g_brushprop[idx][BRUSHPROP_NAME] + DlgProps_GetPropValueName(idx);
		ItemSetTxt(IV_TXT,name);
	}
}

// browse command for the idx prop (store prop idx in DV_USER)
func DlgProps_Browse( idx )
{
	DlgSetInt(DV_USER,idx);
	if( g_brushprop[idx][BRUSHPROP_BROWSE]==USERBROWSE_SELECT )
	{
		valuename = &(g_brushprop[idx][BRUSHPROP_BROWSESEL]);
		if( !?valuename ) return;	
		x=DlgGetInt(DV_X)+ItemGetInt(IV_X);
		y=DlgGetInt(DV_Y)+ItemGetInt(IV_Y)+20;
		CreatePullDownSelect( x, y, "DlgProps_BrowseSet", *valuename );
		DlgMoveInBound();
	}
	else
	if( g_brushprop[idx][BRUSHPROP_BROWSE]==USERBROWSE_COLOR )
	{
		if(g_dlgprop_brushidx>=0)	color = MapBrushGet(g_dlgprop_brushidx,idx);
		else						color = ToolBrushGet(idx);
		x=DlgGetInt(DV_X)+ItemGetInt(IV_X);
		y=DlgGetInt(DV_Y)+ItemGetInt(IV_Y)+20;
		DlgColor_Create( x, y, "DlgProps_BrowseSet", color );
		DlgMoveInBound();
	}
	else
	if( g_brushprop[idx][BRUSHPROP_BROWSE]==USERBROWSE_CUSTOM )
	{
		MOD_BrushPropsBrowser(idx);
	}
}

// apply the browsed value
func DlgProps_BrowseSet( value )
{
	DlgSelect(DlgFind(ID_DLGPROPS));
	idx = DlgGetInt(DV_USER);
	
	if(g_dlgprop_brushidx>=0)	MapBrushSet(g_dlgprop_brushidx,idx,value);
	else						ToolBrushSet(idx,value);
	
	ItemSelect(ItemFind(ID_DLGITEM+idx*10+2));
	szvalue = (idx==BRUSH_COLOR) ? ((str "%x")value) : ((str)value);
	ItemSetTxt(IV_TXT,szvalue);
	DlgProps_Update();
}

// apply the edited value
func DlgProps_InputSet( idx )
{
	szvalue = ItemGetTxt(IV_TXT);
	if(idx==BRUSH_COLOR)
		value = (int "%x")szvalue;
	else
		value = (int)szvalue;
	if(g_dlgprop_brushidx>=0)	MapBrushSet(g_dlgprop_brushidx,idx,value);
	else						ToolBrushSet(idx,value);
	DlgProps_Update();
}

// change prop page
func DlgProps_SetPage( page )
{
	mode = g_dlgprop_mode;
	h = 22;
	for(idx=0; idx<BRUSH_MAX; idx++)
	{
		itempage = idx / DLGPROPS_COUNT;
		y = (DLGTITLEH+8+24) + idx*h - (page*DLGPROPS_COUNT*h);
		
		ItemSelect(ItemFind(ID_DLGITEM+idx*10+0)); 
		ItemSetInt(IV_Y,y); ItemSetInt(IV_Y2,y+20);
		ItemSetInt(IV_HIDDEN,itempage!=page);
		ItemSetInt(IV_DISABLE,itempage!=page);
		
		if(mode!=0)
		{
			ItemSelect(ItemFind(ID_DLGITEM+idx*10+1)); 
			ItemSetInt(IV_Y,y); ItemSetInt(IV_Y2,y+20);
			ItemSetInt(IV_HIDDEN,itempage!=page);
			ItemSetInt(IV_DISABLE,itempage!=page);
			if(DlgProp_IsRestricted(idx)) ItemSetInt(IV_DISABLE,1);
		}
		
		ItemSelect(ItemFind(ID_DLGITEM+idx*10+2)); 
		ItemSetInt(IV_Y,y); ItemSetInt(IV_Y2,y+20);
		ItemSetInt(IV_HIDDEN,itempage!=page);
		ItemSetInt(IV_DISABLE,itempage!=page);
		if(DlgProp_IsRestricted(idx)) ItemSetInt(IV_DISABLE,1);
		
		ItemSelect(ItemFind(ID_DLGITEM+idx*10+3)); 
		ItemSetInt(IV_Y,y); ItemSetInt(IV_Y2,y+20);
		ItemSetInt(IV_HIDDEN,itempage!=page);
		ItemSetInt(IV_DISABLE,itempage!=page);
		if(DlgProp_IsRestricted(idx)) ItemSetInt(IV_DISABLE,1);
	}	
	gs_inisetint( INIFILE, "editor", "props_page", page );
}

// refresh map when close dialog
func DlgProps_Close()
{
	MapRefresh();
}

func DlgProps_MultiCheck()
{
	if(ItemGetInt(IV_DISABLE)) return;
	val = ItemGetInt(IV_VALUE);
	val++; 
	if(g_dlgprop_mode==1) // search
	{
		if(val==3) val=0;
		if(val==0)	ItemSetInt(IV_IMG,img_check1);
		if(val==1)	ItemSetInt(IV_IMG,img_check3);
		if(val==2)	ItemSetInt(IV_IMG,img_check4);
	}
	else
	{
		if(val==2) val=0;
		if(val==0)	ItemSetInt(IV_IMG,img_check1);
		if(val==1)	ItemSetInt(IV_IMG,img_check2);
	}
	ItemSetInt(IV_VALUE,val);
}

// for use in custom browser
func DlgProps_GetProp(idx)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+idx*10+2));
	val = (int)ItemGetTxt(IV_TXT);
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
	return val;
}

// for use in custom browser
func DlgProps_SetProp(idx,val)
{
	dlg = DlgGetSelect();
	item = ItemGetSelect();
	DlgSelect(DlgFind(ID_DLGPROPS));
	ItemSelect(ItemFind(ID_DLGITEM+idx*10+2));
	ItemSetTxt(IV_TXT,(str)val);
	if(g_dlgprop_brushidx>=0)	MapBrushSet(g_dlgprop_brushidx,idx,val);
	else						ToolBrushSet(idx,val);
	DlgProps_Update();
	if(dlg!=-1) DlgSelect(dlg);
	if(item!=-1) ItemSelect(item);
}

//////////////////////////////////////////////////////////////////////////////////

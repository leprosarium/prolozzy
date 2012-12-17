//////////////////////////////////////////////////////////////////////////////////
// BRUSH SEARCH
// Opens a Props dialog for the current tool brush and allow user to select a
// search filter by cheking and editing the values for the props to match
// Then it searches all brushes that match the selected props and either
// select or unselect them.
//////////////////////////////////////////////////////////////////////////////////
func ScrBrushSearch()
{
	DlgProps_Create(1);
	DlgSetTitle("Brush Search");

	x=8; y=DlgGetInt(DV_Y2)-DlgGetInt(DV_Y)+8;
	CreateButton( x,y,80,"SEARCH","ScrBrushSearch_Apply();" );
	ItemSetTxt(IV_TOOLTIP, "Select brushes that match checked properties\nusing the specified selection operation.");

	x=24+80;
	CreateRadio( x, y, 1, 2 ); ItemSetInt(IV_ID, ID_DLGITEM+1000); ItemSetTxt(IV_TOOLTIP, "select brushes"); 
	CreateText( x+20, y, 32, "SET" ); x+=60;
	CreateRadio( x, y, 0, 2 ); ItemSetInt(IV_ID, ID_DLGITEM+1001); ItemSetTxt(IV_TOOLTIP, "add to current selection"); 
	CreateText( x+20, y, 32, "ADD" ); x+=60;
	CreateRadio( x, y, 0, 2 ); ItemSetInt(IV_ID, ID_DLGITEM+1002); ItemSetTxt(IV_TOOLTIP, "remove from current selection"); 
	CreateText( x+20, y, 32, "SUB" );
	y+=22+8;
	
	DlgResize(0,0,DlgGetInt(DV_X2)-DlgGetInt(DV_X), y);
}

func ScrBrushSearch_Apply()
{
	mode = 1; // set
	ItemSelect(ItemFind(ID_DLGITEM+1001));
	if(ItemGetInt(IV_VALUE)) mode=2; // add
	ItemSelect(ItemFind(ID_DLGITEM+1002));
	if(ItemGetInt(IV_VALUE)) mode=3; // del
	
	// get checked props
	apply = 0;
	checks = tab(BRUSH_MAX);
	values = tab(BRUSH_MAX);
	for(j=0;j<BRUSH_MAX;j++)
	{
		ItemSelect(ItemFind(ID_DLGITEM+j*10+1));
		checks[j] = ItemGetInt(IV_VALUE);
		values[j] = ToolBrushGet(j);
		if(checks[j]) apply=1;
	}
	if(!apply) { MsgBoxOk("Message", "no props checked.", ICON_INFO ); return;	}
	
	count = 0;
	WaitCursor(1);
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		match = 1; // if brush match
		props = 0; // checked props count
		for(j=0;j<BRUSH_MAX;j++)
		{
			if(!checks[j]) continue;
			props++;
			val = MapBrushGet(i,j);
			if(checks[j]==1) // values shoud be the same
			{
				if(val!=values[j]) { match=0; break; }
			}
			else // negation (values should be different)
			{
				if(val==values[j]) { match=0; break; }
			}
		}
	
		if( props>0 ) // if something was checked
		{
			if( match ) // if brush matched
			{
				if(mode==1) 
				{ 
					count++;
					MapBrushSet(i,BRUSH_SELECT,1); 
				}
				else
				if(mode==2) 
				{
					if(MapBrushGet(i,BRUSH_SELECT)==0) count++;
					MapBrushSet(i,BRUSH_SELECT,1); 
				}
				else
				if(mode==3) 
				{
					if(MapBrushGet(i,BRUSH_SELECT)!=0) count++;
					MapBrushSet(i,BRUSH_SELECT,0); 	
				}
			}
			else // no match
				if(mode==1) MapBrushSet(i,BRUSH_SELECT,0);
		}
	}
	WaitCursor(0);
	DlgClose(); // close search dlg
	if(mode==1)	szop = "selected";
	if(mode==2)	szop = "selected";
	if(mode==3)	szop = "deselected";
	MsgBoxOk("Message", (str)count+" brushes "+szop+".", ICON_INFO ); 
	SelectionRefresh();
	MapRefresh();
}

//////////////////////////////////////////////////////////////////////////////////
// BRUSH CHANGE
// Opens a Props dialog for the current tool brush and allow user to select a
// change filter by cheking and editing the values for the props to change
// Then it changes selected props for all the brushes in the current selection.
//////////////////////////////////////////////////////////////////////////////////
func ScrBrushChange()
{
	DlgProps_Create(2);
	DlgSetTitle("Brush Change");
	
	x=8; y=DlgGetInt(DV_Y2)-DlgGetInt(DV_Y)+8;
	CreateButton( x,y,80,"CHANGE","ScrBrushChange_Apply();" );
	ItemSetTxt(IV_TOOLTIP, "Set checked properties of the selected brushes\nwith values from the current brush.");
	y+=22+8;
	
	DlgResize(0,0,DlgGetInt(DV_X2)-DlgGetInt(DV_X), y);
}

func ScrBrushChange_Apply()
{
	// get checked props
	apply=0;
	checks = tab(BRUSH_MAX);
	values = tab(BRUSH_MAX);
	for(j=0;j<BRUSH_MAX;j++)
	{
		ItemSelect(ItemFind(ID_DLGITEM+j*10+1));
		checks[j] = ItemGetInt(IV_VALUE);
		values[j] = ToolBrushGet(j);
		if(checks[j]) apply=1;
	}
	if(!apply) { MsgBoxOk("Message", "no props checked.", ICON_INFO ); return;	}
	
	count = 0;
	WaitCursor(1);
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		if(!MapBrushGet(i,BRUSH_SELECT)) continue;
		for(j=0;j<BRUSH_MAX;j++)
		{
			if(!checks[j]) continue;
			MapBrushSet(i,j,values[j]);
		}
		count++;
	}
	
	WaitCursor(0);
	DlgClose(); // close change dlg
	MsgBoxOk("Message", "changes applied on "+(str)count+" brushes.", ICON_INFO ); 
	MapRefresh();
}

//////////////////////////////////////////////////////////////////////////////////
// BRUSH INVERT
// Invert the curent selection (deselect current selected brushes and select all others)
//////////////////////////////////////////////////////////////////////////////////
func ScrBrushInvert()
{
	count = 0;
	WaitCursor(1);
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		sel = !MapBrushGet(i,BRUSH_SELECT);
		MapBrushSet(i,BRUSH_SELECT,sel);
		if(sel) count++;
	}
	WaitCursor(0);
	
	if(count==0) text = "No"; 
	else
	if(count==MapBrushCount()) text = "All"; 
	else
	text = (str)count;
	
	MsgBoxOk("Message", text+" brushes selected.", ICON_INFO ); 
	SelectionRefresh();
	MapRefresh();
}


//////////////////////////////////////////////////////////////////////////////////
// BRUSH MOVE
// Move current selected brushes by an offset
// Useful when resizing map to make more rooms at the left or top side
//////////////////////////////////////////////////////////////////////////////////
func ScrBrushMove()
{
	CreateDlgTitle( 0, 0, 48*3+16, DLGTITLEH+8+20, "Brush Move by offset",1);
	DlgSetInt( DV_CLOSEOUT, 1 );
	x = 4; y=DLGTITLEH+4;
	CreateEdit( x, y, 48, "0" ); ItemSetTxt(IV_TOOLTIP,"X offset"); ItemSetInt(IV_ID,ID_DLGITEM+0); x+=48+4;
	CreateEdit( x, y, 48, "0" ); ItemSetTxt(IV_TOOLTIP,"Y offset"); ItemSetInt(IV_ID,ID_DLGITEM+1); x+=48+4;
	CreateButton( x,y,48, "MOVE", "ScrBrushMove_Apply();" ); ItemSetTxt(IV_TOOLTIP,"Move selected brushes"); 
	DlgMoveToMouse(); DlgDockUp();
}
func ScrBrushMove_Apply()
{
	count = 0;
	ItemSelect(ItemFind(ID_DLGITEM+0));
	ofsx = (int)ItemGetTxt(IV_TXT);
	ItemSelect(ItemFind(ID_DLGITEM+1));
	ofsy = (int)ItemGetTxt(IV_TXT);
	WaitCursor(1);
	
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		if(!MapBrushGet(i,BRUSH_SELECT)) continue;
		MapBrushSet(i,BRUSH_X,MapBrushGet(i,BRUSH_X)+ofsx);
		MapBrushSet(i,BRUSH_Y,MapBrushGet(i,BRUSH_Y)+ofsy);
		count++;
	}
	WaitCursor(0);

	DlgClose();
	MsgBoxOk("Message", (str)count+" brushes moved.", ICON_INFO ); 
	MapRefresh();
}

//////////////////////////////////////////////////////////////////////////////////
// BRUSH SELECT BY IDX
// Select the brush with the specified index
// Please note that a brush index is not fixed. 
// It may change when previous brushes are deleted or when brush is moved in
// the front-back order.
//////////////////////////////////////////////////////////////////////////////////
func ScrSelectByIdx()
{
	CreateDlgTitle( 0, 0, 8+64+4+48, DLGTITLEH+8+20+20*3+4, "Brush Select by idx",1 );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgClose(0);");
	DlgSetInt( DV_CLOSEOUT, 1 );
	x=4; y=DLGTITLEH+4;
	CreateRadio( x, y, 1, 1 ); ItemSetInt(IV_ID, ID_DLGITEM+1); ItemSetTxt(IV_TOOLTIP, "count all brushes"); 
	CreateText( x+20, y, 32, "ALL" ); y+=20;
	CreateRadio( x, y, 0, 1 ); ItemSetInt(IV_ID, ID_DLGITEM+2); ItemSetTxt(IV_TOOLTIP, "count static brushes only"); 
	CreateText( x+20, y, 32, "STATIC" ); y+=20;
	CreateRadio( x, y, 0, 1 ); ItemSetInt(IV_ID, ID_DLGITEM+3); ItemSetTxt(IV_TOOLTIP, "count dynamic brushes only"); 
	CreateText( x+20, y, 32, "DYNAMIC" ); y+=20;
	y+=2;
	CreateEdit( x, y, 64, "0" ); ItemSetInt(IV_ID,ID_DLGITEM); x+=64+4;
	CreateButton( x, y, 48, "Select", "ScrSelectByIdx_Apply();" );
	DlgMoveToMouse(); DlgDockUp();
}
func ScrSelectByIdx_Apply()
{
	if(MapBrushCount()==0) { DlgClose(); MsgBoxOk("Message", "nothing to select.", ICON_INFO); return; }
	mode=0;
	ItemSelect(ItemFind(ID_DLGITEM+2)); if(ItemGetInt(IV_VALUE)) mode=1; // static
	ItemSelect(ItemFind(ID_DLGITEM+3)); if(ItemGetInt(IV_VALUE)) mode=2; // dynamic
	ItemSelect(ItemFind(ID_DLGITEM));
	idx = (int)ItemGetTxt(IV_TXT);
	brushcount=MapBrushCount();
	WaitCursor(1);

	if(mode==0) // all brushes
	{	
		if(idx<0) idx=0;
		if(idx>MapBrushCount()-1) idx=MapBrushCount()-1;
	}
	else // static or dynamic only
	{
		// search the real index of the requested brush
		cnt=0; // counter typed brushes
		for(i=0;i<brushcount;i++)
		{
			if(mode==1)	{ if(MapBrushGet(i,BRUSH_TYPE)==0) cnt++; } else
			if(mode==2)	{ if(MapBrushGet(i,BRUSH_TYPE)==1) cnt++; }
			if(cnt==idx+1) { idx = i; break; }
		}
	}
	
	// clear current selection
	for(i=0;i<brushcount;i++) MapBrushSet(i,BRUSH_SELECT,0);
	WaitCursor(0);
	// select
	MapBrushSet(idx,BRUSH_SELECT,1);
	SelectionGoto(1);
	EdiSet(EDI_SELECT,1);
	MapRefresh();
	DlgClose();
	MsgBoxOk("Message", "brush #"+(str)idx+" selected.", ICON_INFO);
}

//////////////////////////////////////////////////////////////////////////////////
// BRUSH KEEP TOPMOST
// keep selected only the topmost brush from the current selection (deselect the others)
//////////////////////////////////////////////////////////////////////////////////
func ScrBrushKeepTopmost()
{
	WaitCursor(1);
	brushcount=MapBrushCount();
	topmost = -1;
	for(i=0;i<brushcount;i++)
	{
		if(MapBrushGet(i,BRUSH_SELECT)) topmost=i;
		MapBrushSet(i,BRUSH_SELECT,0);
	}
	WaitCursor(0);
	if(topmost!=-1)
	{
		MapBrushSet(topmost,BRUSH_SELECT,1);
		MsgBoxOk("Message", "Topmost brush selected.", ICON_INFO ); 
	}
	SelectionRefresh();
	MapRefresh();
}

//////////////////////////////////////////////////////////////////////////////////
// BRUSH GROUP IDS
// Select the brush with the specified index
// Please note that a brush index is not fixed. 
// It may change when previous brushes are deleted or when brush is moved in
// the front-back order.
//////////////////////////////////////////////////////////////////////////////////
func ScrBrushGroupIds()
{
	if(EdiGet(EDI_SELECT)==0) { MsgBoxOk("Message", "No selected brushes.", ICON_INFO); return; }
	CreateDlgTitle( 0, 0, 8+132, DLGTITLEH+4+14*6+10+20+4, "Brush Group Ids",1 );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgClose(0);");
	DlgSetInt( DV_CLOSEOUT, 1 );
	x=4; y=DLGTITLEH+4;
	CreateBar(x,y,132,14*6+6, COLOR_GUI1);
	CreateText( x, y, 132, "Choose the first id and"); y+=14;
	CreateText( x, y, 132, "the selected brushes"); y+=14;
	CreateText( x, y, 132, "will have incrementing"); y+=14;
	CreateText( x, y, 132, "ids starting from it." ); y+=14;
	CreateText( x, y, 132, "Make sure not to"); y+=14;
	CreateText( x, y, 132, "generate duplicated ids!" ); y+=14;
	y+=10;
	CreateEdit( x, y, 64, "0" ); ItemSetInt(IV_ID,ID_DLGITEM);
	x = 8+132-48-4;
	CreateButton( x, y, 48, "Apply", "ScrBrushGroupIds_Apply();" );
	DlgMoveToMouse(); DlgDockUp();
}
func ScrBrushGroupIds_Apply()
{
	ItemSelect(ItemFind(ID_DLGITEM));
	id = (int)ItemGetTxt(IV_TXT);
	if(id==0) { MsgBoxOk("Message", "Choose a valid starting id (non zero).", ICON_INFO); return; }
	brushcount=MapBrushCount();
	WaitCursor(1);
	id0=id;
	for(i=0;i<brushcount;i++)
	{
		if(MapBrushGet(i,BRUSH_SELECT)==1)
		{
			MapBrushSet(i,BRUSH_ID,id);
			id++;
		}
	}
	WaitCursor(0);
	MapRefresh();
	DlgClose();
	MsgBoxOk("Message", "Done.\nIds set from "+(str)id0+" to "+(str)(id-1)+".", ICON_INFO);
}

//////////////////////////////////////////////////////////////////////////////////
// CUSTOM BLOCKING
// set material and draw mode to img/air (0) or to img+mat/block (1)
//////////////////////////////////////////////////////////////////////////////////
func ScrCustomBlocking( mode )
{
	if(EdiGet(EDI_SELECT)==0) { MsgBoxOk("Message", "No selected brushes.", ICON_INFO); return; }
	WaitCursor(1);
	count = 0;
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		if(MapBrushGet(i,BRUSH_SELECT))
		{
			if(mode)
			{
				MapBrushSet(i,BRUSH_DRAW,3);
				MapBrushSet(i,BRUSH_MATERIAL,7);
			}
			else
			{
				MapBrushSet(i,BRUSH_DRAW,1);
				MapBrushSet(i,BRUSH_MATERIAL,0);
			}
			count++;
		}
	}
	WaitCursor(0);
	MsgBoxOk("Message", (str)count+" brushes set to "+(mode?"block":"unblock"), ICON_INFO ); 
	MapRefresh();
}

//////////////////////////////////////////////////////////////////////////////////

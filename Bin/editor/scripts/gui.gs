//////////////////////////////////////////////////////////////////////////////////
// Gui helpers
//////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// GUI Resource
//////////////////////////////////////////////////////////////////////////////////
#def ICON_INFO			0
#def ICON_QUESTION		1
#def ICON_WARNING		2
#def ICON_ERROR			3
#def ICON_MAX			4

// globals with gui resources for easy access
int img_check1, img_check2, img_check3, img_check4;
int img_radio1, img_radio2;
tab img_icon;

func GuiLoadResources()
{
	img_check1 = ImgLoad("Editor\\Graphics\\check1.tga");
	img_check2 = ImgLoad("Editor\\Graphics\\check2.tga");
	img_check3 = ImgLoad("Editor\\Graphics\\check3.tga");
	img_check4 = ImgLoad("Editor\\Graphics\\check4.tga");
	img_radio1 = ImgLoad("Editor\\Graphics\\radio1.tga");
	img_radio2 = ImgLoad("Editor\\Graphics\\radio2.tga");
	
	img_icon = tab(ICON_MAX);
	img_icon[ICON_INFO] 	= ImgLoad("Editor\\Graphics\\icon_info.tga");
	img_icon[ICON_QUESTION] = ImgLoad("Editor\\Graphics\\icon_question.tga");
	img_icon[ICON_WARNING] 	= ImgLoad("Editor\\Graphics\\icon_warning.tga");
	img_icon[ICON_ERROR] 	= ImgLoad("Editor\\Graphics\\icon_error.tga");
}


//////////////////////////////////////////////////////////////////////////////////
// Controls
//////////////////////////////////////////////////////////////////////////////////

func CreateItem( x, y, w, h, style, text, color0, color1, color2 )
{
	ItemNew("cGUIItem");
	ItemSetInt(IV_RECT, x,y,x+w,y+h);
	if(?text) 	ItemSetTxt(IV_TXT, text );
	if(?color0)	ItemSetInt(IV_COLOR, color0);
	if(?color1)	ItemSetInt(IV_COLOR+1, color1);
	if(?color2)	ItemSetInt(IV_COLOR+2, color2);
	ItemSetInt(IV_STYLE,style);
	ItemSetInt(IV_TXTALIGN, GUIALIGN_CENTERXY);
}


func CreateBar( x, y, w, h, color0, color1 )
{
	ItemNew("cGUIItem");
	ItemSetInt(IV_RECT, x,y,x+w,y+h);
	style = GUISTYLE_BACKGR;
	ItemSetInt(IV_COLOR, color0);
	if(?color1)
	{
		ItemSetInt(IV_COLOR+1, color1);
		style = GUISTYLE_GRADIENT;
	}
	ItemSetInt(IV_STYLE,style);
}

func CreateRect( x, y, w, h, color, is3d, ispressed )
{
	ItemNew("cGUIItem");
	ItemSetInt(IV_RECT, x,y,x+w,y+h);
	style = GUISTYLE_BORDER;
	ItemSetInt(IV_COLOR+2, color);
	if(?is3d)
	{
		style = GUISTYLE_BORDER3D;
		if(?ispressed) style |= GUISTYLE_PRESSED;
	}
	ItemSetInt(IV_STYLE,style);
}

func CreateText( x, y, w, text, align )
{
	ItemNew("cGUIItem");
	ItemSetInt(IV_RECT, x,y,x+w,y+20);
	ItemSetTxt(IV_TXT, text);
	if(?align) 	ItemSetInt(IV_TXTALIGN, align);
	else		ItemSetInt(IV_TXTALIGN, GUIALIGN_LEFT|GUIALIGN_CENTERY);
}

func CreateButton( x, y, w, text, cmd )
{
	ItemNew("cGUIButton");
	ItemSetInt(IV_RECT, x,y,x+w,y+20);
	ItemSetTxt(IV_TXT, text);
	ItemSetInt(IV_TXTALIGN, GUIALIGN_CENTERXY);
	style = GUISTYLE_GRADIENT | GUISTYLE_BORDER3D;
	ItemSetInt(IV_STYLE, style);
	ItemSetInt(IV_COLOR, COLOR_GUI1, COLOR_GUI2, COLOR_GUI);
	if(?cmd) ItemSetTxt(IV_CMDACTION,cmd);
}

func CreateButtonImg( x, y, w, h, img0, img1, cmd )
{
	ItemNew("cGUIButton");
	ItemSetInt(IV_RECT, x,y,x+w,y+h);
	ItemSetInt(IV_IMG, img0, img1);
	style = GUISTYLE_GRADIENT | GUISTYLE_BORDER3D;
	ItemSetInt(IV_STYLE, style);
	ItemSetInt(IV_COLOR, COLOR_GUI1, COLOR_GUI2, COLOR_GUI);
	if(?cmd) ItemSetTxt(IV_CMDACTION,cmd);
}

func CreateCheck( x, y, value, cmd )
{
	ItemNew("cGUICheck");
	ItemSetInt(IV_RECT, x,y,x+20,y+20);
	ItemSetInt(IV_IMG, img_check1, img_check2);
	ItemSetInt(IV_IMGCOLOR, COLOR_EDIT);
	ItemSetInt(IV_VALUE, value);
	if(?cmd) ItemSetTxt(IV_CMDACTION,cmd);
}

func CreateRadio( x, y, value, group, cmd )
{
	ItemNew("cGUIRadio");
	ItemSetInt(IV_RECT, x,y,x+20,y+20);
	ItemSetInt(IV_IMG, img_radio1, img_radio2);
	ItemSetInt(IV_IMGCOLOR, COLOR_EDIT);
	ItemSetInt(IV_VALUE, value);
	ItemSetInt(IV_GROUP, group);
	if(?cmd) ItemSetTxt(IV_CMDACTION,cmd);
}

func CreateRadioButton( x, y, w, text, value, group, cmd )
{
	ItemNew("cGUIRadio");
	ItemSetInt(IV_RECT, x,y,x+w,y+20);
	ItemSetTxt(IV_TXT, text);
	ItemSetInt(IV_TXTALIGN, GUIALIGN_CENTERXY);
	ItemSetInt(IV_STYLE, GUISTYLE_BORDER3D); // GUISTYLE_GRADIENT
	ItemSetInt(IV_COLOR, COLOR_GUI1, COLOR_GUI2, COLOR_GUI);
	ItemSetInt(IV_VALUE, value);
	ItemSetInt(IV_GROUP, group);
	if(?cmd) ItemSetTxt(IV_CMDACTION,cmd);
}

func CreateEdit( x, y, w, text, cmd )
{
	ItemNew("cGUIEdit");
	ItemSetInt(IV_RECT, x,y,x+w,y+20);
	ItemSetTxt(IV_TXT, text);
	if(?cmd) ItemSetTxt(IV_CMDACTION,cmd);
	style = GUISTYLE_BACKGR | GUISTYLE_BORDER;
	ItemSetInt(IV_STYLE, style);
	ItemSetInt(IV_COLOR, COLOR_EDIT, COLOR_GUI1, COLOR_BLACK, COLOR_EDITSEL);
}

func CreateImage( x, y, w, h, img )
{
	ItemNew("cGUIItem");
	ItemSetInt(IV_RECT, x,y,x+w,y+h);
	ItemSetInt(IV_STYLE,GUISTYLE_NONE);
	ItemSetInt(IV_IMG, img);
	ItemSetInt(IV_IMGALIGN, GUIALIGN_LEFT|GUIALIGN_TOP);
}

//////////////////////////////////////////////////////////////////////////////////
// Dialogs
//////////////////////////////////////////////////////////////////////////////////
#def DLGTITLEH			20

func CreateDlg( x, y, w, h, style )
{
	DlgNew();
	DlgSetInt(DV_RECT, x, y, x+w, y+h);
	CreateItem( 0, 0, w, h, style, NUL, COLOR_GUI, COLOR_GUI, COLOR_GUI );
	ItemSetInt(IV_ID,ID_DLGBACK);
}

func CreateDlgTitle( x, y, w, h, text, modal )
{
	style = GUISTYLE_BACKGR | GUISTYLE_BORDER3D;
	CreateDlg( x, y, w, h, style );
	style = GUISTYLE_GRADIENT | GUISTYLE_BORDER3D | GUISTYLE_PRESSED;

	ItemNew("cGUITitle");
	ItemSetInt(IV_ID,		ID_DLGTITLE);
	ItemSetInt(IV_RECT,		2,2,w-2,2+DLGTITLEH);
	ItemSetTxt(IV_TXT,		text );
	ItemSetInt(IV_COLOR,	COLOR_TITLE1);
	ItemSetInt(IV_COLOR+1,	COLOR_TITLE2);
	ItemSetInt(IV_COLOR+2,	COLOR_GUI);
	ItemSetInt(IV_STYLE,	style);
	ItemSetInt(IV_TXTALIGN, GUIALIGN_CENTERXY);

	if(?modal)
	{
		DlgSetInt(DV_MODAL,modal);
		ItemSetInt(IV_COLOR, COLOR_MODAL1);  // title item is still selected
		ItemSetInt(IV_COLOR+1, COLOR_MODAL2);
	}
}

// prevent dialog to get outside the screen
func DlgMoveInBound()
{
	x = DlgGetInt(DV_X);	
	y = DlgGetInt(DV_Y);
	w = DlgGetInt(DV_X2)-x;	
	h = DlgGetInt(DV_Y2)-y;	
	if(x<0) x=0;
	if(x>ScrW()-w) x = ScrW()-w;
	if(y<0) y=0;
	if(y>ScrH()-h) y = ScrH()-h;
	DlgSetInt(DV_X,x);
	DlgSetInt(DV_Y,y);
	DlgSetInt(DV_X2,x+w);
	DlgSetInt(DV_Y2,y+h);	
}

// move dialog to mouse position and also prevent it to get ouside the screen
func DlgMoveToMouse()
{
	x = DlgGetInt(DV_X);	
	y = DlgGetInt(DV_Y);
	w = DlgGetInt(DV_X2)-x;	
	h = DlgGetInt(DV_Y2)-y;	
	x = MouseX()-w/2;
	y = MouseY()-h/2;
	if(x<0) x=0;
	if(x>ScrW()-w) x = ScrW()-w;
	if(y<0) y=0;
	if(y>ScrH()-h) y = ScrH()-h;
	DlgSetInt(DV_X,x);
	DlgSetInt(DV_Y,y);
	DlgSetInt(DV_X2,x+w);
	DlgSetInt(DV_Y2,y+h);
}

// just move dialog
func DlgMove( x, y )
{
	w=DlgGetInt(DV_X2)-DlgGetInt(DV_X);
	h=DlgGetInt(DV_Y2)-DlgGetInt(DV_Y);
	DlgSetInt(DV_RECT,x,y,x+w,y+h);
}

// reposition and resize dialog and it's background and title bar
func DlgResize( x, y, w, h )
{
	DlgSetInt(DV_X,x);		
	DlgSetInt(DV_Y,y);
	DlgSetInt(DV_X2,x+w);	
	DlgSetInt(DV_Y2,y+h);

	i = ItemFind(ID_DLGBACK);
	if(i!=-1)
	{
		ItemSelect(i);
		ItemSetInt(IV_RECT, 0,0,w,h);
	}

	i = ItemFind(ID_DLGTITLE);
	if(i!=-1)
	{
		ItemSelect(i);
		ItemSetInt(IV_RECT,	2,2,w-2,2+DLGTITLEH);
	}
}

// move dialog to the top menubar
func DlgDockUp()
{
	if(DlgGetInt(DV_Y)<32) DlgMove(DlgGetInt(DV_X),32);
}

// change dialog title
func DlgSetTitle( text )
{
	i = ItemFind(ID_DLGTITLE);	
	if(i==-1) return;
	ItemSelect(i);
	ItemSetTxt(IV_TXT, text );
}

//////////////////////////////////////////////////////////////////////////////////
// PullDown Menu
// menu is reference to tab of entries {name,cmd,keycode,tooltip}
// if cmd=NIL then title item
// if keycode==-1 no key shortcut
// if valid sel is the index of the option that will be highlighted
//////////////////////////////////////////////////////////////////////////////////
func CreatePullDown( x, y, menu, sel )
{
	if(typeof(menu)!=5) error("reference to tab expected");
	if(sizeof(*menu)==0) return;
	if(!?sel) sel=-1;
	
	// get dialog sizes
	itemh = 20; // button height
	menuh = sizeof(*menu)*itemh;
	menuw = 0;
	for(i=0;i<sizeof(*menu);i++)
	{ 
		tw = TextW( (*menu)[i][0] );
		if(tw>menuw) menuw=tw;
	}
	menuw += 20;
	
	// create dialog
	CreateDlg( x, y, menuw, menuh, GUISTYLE_NONE );
	DlgSetInt(DV_MODAL,1);
	DlgSetInt(DV_CLOSEOUT,1);
	DlgAddKey( KEY_ESCAPE, 		0, "DlgClose();" );
	
	by = 0;
	for(i=0;i<sizeof(*menu);i++)
	{
		name = (*menu)[i][0];
		cmd = (*menu)[i][1];
		if(sizeof((*menu)[i])>2) keycode = (*menu)[i][2];
		if(sizeof((*menu)[i])>3) tooltip = (*menu)[i][3];
		
		CreateButton( 0, by, menuw, name, cmd );
		style = GUISTYLE_BACKGR | GUISTYLE_BORDER3D;
		ItemSetInt(IV_STYLE, style);
		ItemSetInt(IV_TXTALIGN, GUIALIGN_LEFT|GUIALIGN_CENTERY);
		color = COLOR_GUI;
		if(cmd=="")
		{
			if(name!="") color = COLOR_GUI1;
			ItemSetInt(IV_DISABLE,1);
		}
		if(i==sel) color = COLOR_LAYER1;
		
		ItemSetInt(IV_COLOR, color, color, color);
		
		if(?keycode) if(keycode!=-1)	DlgAddKey( keycode, 0, cmd );
		if(?tooltip) if(tooltip!="")	ItemSetTxt(IV_TOOLTIP, tooltip);
			
		by+=itemh;
	}
}

func CreatePullDownSelect( x, y, callbackname, list, sel )
{
	count = sizeof(list); if(count==0) return;
	menu = tab(count);
	for(i=0;i<count;i++)
	{
		menu[i] = { list[i], "DlgClose();"+callbackname+"("+(str)i+");" };
	}
	CreatePullDown( x, y, &menu, sel );
}

//////////////////////////////////////////////////////////////////////////////////
// Message Boxes
// icon = -1 no icon
// buttoninfo = button tab info (if missing or empty then no buttons) { {b1_name, b1_cmd}, ... }
//////////////////////////////////////////////////////////////////////////////////

func MsgBox( title, text, icon, buttoninfo )
{
	if(!?icon) icon=-1;
	if(!?buttoninfo) buttoninfo={};
	if(icon<0 || icon>sizeof(img_icon)) icon=-1;

	// button sizes
	buttons = sizeof(buttoninfo);
	buttonw = 64;
	for(i=0;i<buttons;i++)
	{
		bw = TextW(buttoninfo[i][0])+32;
		if(bw>buttonw) buttonw=bw;
	}
	if(buttonw>200) buttonw=200; // some decent limit
	
	// dialog and text sizes
	titlew=TextW(title);
	textw=TextW(text);
	texth=TextH(text);
	dlgw = textw;
	dlgh = texth+DLGTITLEH+16;
	if(icon!=-1) dlgw+=8+32; // images are 32x32
	if(dlgw<titlew) dlgw=titlew;
	if(buttons>0)
	{
		dlgh+=8+20;
		if(dlgw<buttons*(buttonw+8)) dlgw=buttons*(buttonw+8);
	}
	dlgw+=16; // side borders
		
	// dialog	
	CreateDlgTitle( 0,0,dlgw,dlgh,title,1);
	DlgSetInt(DV_CLOSEOUT,1);
	
	// text box
	textx=(icon==-1)?8:40;
	texty=DLGTITLEH+8;
	CreateText( textx, texty, textw, text, GUIALIGN_LEFT|GUIALIGN_TOP );
	
	// icon
	if(icon!=-1) CreateImage( 8, texty, 32, 32, img_icon[icon] );
	
	// buttons
	if(buttons>0)
	{
		buts = (dlgw-16)/buttons;
		butx = 8;
		buty = texty+texth+8;
		for(i=0;i<buttons;i++)
		{
			CreateButton( butx+buts/2-buttonw/2, buty, buttonw, buttoninfo[i][0], "DlgClose();"+buttoninfo[i][1] );
			butx += buts;
		}
	}
		
	DlgMoveToMouse(); DlgDockUp();
}

func MsgBoxOk(title, text, icon)
{
	MsgBox(title,text,icon,{{"OK",""}});
	DlgAddKey( KEY_RETURN,0,"DlgClose();" ); 
	DlgAddKey( KEY_ESCAPE,0,"DlgClose();" ); 
}

//////////////////////////////////////////////////////////////////////////////////

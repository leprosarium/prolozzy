//////////////////////////////////////////////////////////////////////////////////
// Color Dialog
// Browse color using first mouse button and alpha using second mouse button
// when closed, it calls the callback with color value ( ex: callbackname(color) )
// uses DV_USER to store current color
// More: functionality for last selected colors
//////////////////////////////////////////////////////////////////////////////////
#def COLOR_USEDMAX	14

int g_color_pal;
tab g_color_used;
str g_color_callbackname;

func DlgColor_Create( x, y, callbackbame, color )
{
	if(!?color) color=0xffffffff;
	g_color_callbackname = callbackbame;
	
	space = 8;
	dlgw = 128+space*2;
	dlgh = DLGTITLEH+128+space*2;
	CreateDlgTitle( x, y, dlgw, dlgh, "Color", 1);
	DlgSetInt( DV_ID, ID_DLGCOLOR );
	DlgSetInt( DV_CLOSEOUT,	 	1 );
	DlgSetTxt( DV_CLOSECMD, 	"DlgColor_Close();" );
	DlgAddKey( KEY_ESCAPE, 		0, "DlgClose(0);");
	DlgAddKey( KEY_PGUP, 		0, "DlgColor_SetPalette(g_color_pal-1);");
	DlgAddKey( KEY_PGDN, 		0, "DlgColor_SetPalette(g_color_pal+1);");
	DlgAddKey( MOUSE_WHEELUP, 	0, "DlgColor_SetPalette(g_color_pal-1);");
	DlgAddKey( MOUSE_WHEELDN, 	0, "DlgColor_SetPalette(g_color_pal+1);");
	ItemSetInt(IV_TXTALIGN, GUIALIGN_LEFT|GUIALIGN_CENTERY);

	DlgSetInt(DV_USER,color);
	
	// color palette
	y = DLGTITLEH+8;
	CreateRect( 7, y-1 , 128+2, 128+2, COLOR_GUI, 1, 1 );
	CreateImage( 8, y, 128, 128, -1 );
	ItemSetInt(IV_ID,ID_DLGITEM+0);
	
	// color picker
	ItemNew("cGUIColorPick");
	ItemSetInt(IV_ID,ID_DLGITEM+1);
	ItemSetInt(IV_RECT, 8, y, 8+128, y+128);
	ItemSetTxt(IV_CMDACTION, "DlgColor_Pick();");

	// set palette	
	g_color_pal=0;
	gs_inigetint( INIFILE, "editor", "color_pal", &g_color_pal );
	if(g_color_pal<0) g_color_pal=0;
	ret = DlgColor_SetPalette(g_color_pal); // last used
	if(!ret) ret = DlgColor_SetPalette(0); // first
	if(!ret) DlgClose(0); // no palettes
}

func DlgColor_Close()
{
	gs_inisetint( INIFILE, "editor", "color_pal", g_color_pal );
	fid = gs_fid(g_color_callbackname);
	if(fid!=-1) call(DlgGetInt(DV_USER), fid);
}

// initialize dlg color (call only once, on editor start)
func DlgColor_Init()
{
	// last used colors init
	g_color_used = tab(COLOR_USEDMAX);
	for(i=0;i<COLOR_USEDMAX;i++) g_color_used[i]=0xffffffff;
}

func DlgColor_SetPalette( pal )
{
	if(pal<0) pal=0;
	imgname = "Editor\\Graphics\\pal"+(str)pal+".tga";
	img = ImgFind(imgname); // check if already loaded
	if(img==-1) img = ImgLoad(imgname); // load
	if(img==-1) return false; // not found

	g_color_pal = pal;
	
	ItemSelect(ItemFind(ID_DLGITEM+0));
	ItemSetInt(IV_IMG,img);

	ItemSelect(ItemFind(ID_DLGITEM+1));
	ItemSetTxt(IV_TXT, imgname);

	ItemSelect(ItemFind(ID_DLGTITLE));
	ItemSetTxt(IV_TXT,"Color palette #"+(str)(g_color_pal+1));
	
	return true;
}

func DlgColor_Pick()
{
	argb 	= ItemGetInt(IV_COLOR);
	rgb 	= argb & 0x00ffffff;
	alpha 	= (argb & 0x000000ff)<<24; // use alpha from blue channel
	param = ItemGetInt(IV_CMDACTIONPARAM);

	color = DlgGetInt(DV_USER);
	if(param==1) color = (color & 0xff000000) | rgb; // B1
	if(param==2) color = (color & 0x00ffffff) | alpha; // B2

	DlgSetInt(DV_USER,color);
	DlgClose(1);
}

func PushUsedColor( color )
{
	// find
	for(i=COLOR_USEDMAX-1;i>=0;i--)
		if(g_color_used[i]==color) break;
	if(i<0) i=0; // if not found, remove oldest
	// shift left
	for(j=i;j<COLOR_USEDMAX-1;j++)
		g_color_used[j]=g_color_used[j+1];
	// add color
	g_color_used[COLOR_USEDMAX-1]=color;
}

//////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// Help dialog
//////////////////////////////////////////////////////////////////////////////////
#def DLGHELP_COUNT 16
tab g_helptab;

func DlgHelp_Create()
{
	wkey = 88;
	wcmd = 192;
	dlgw = 8+wkey+8+wcmd+8;
	dlgh = DLGTITLEH+8+24+ 22*DLGHELP_COUNT +8;
	CreateDlgTitle( 0, 0, dlgw, dlgh, "Quick Help", 1 );
	DlgAddKey( KEY_ESCAPE, 	0, "DlgClose(0);");
	DlgSetInt( DV_CLOSEOUT, 1 );

	g_helptab = 
	{	
		{"",				"GENERAL"},
		{"~",				"system menu"},
		{"SPACE",			"switch tool mode"},
		{"S/G/A",			"snap/grid/axes"},
		{"+/-",				"zoom in/out"},
		{"F1",				"help"},
		{"F2",				"go to next marker"},
		{"shift+F2",		"go to previous marker"},
		{"ctrl+F2",			"add/remove marker"},
		{"F3",				"go to next selected brush"},
		{"shift+F3",		"go to previous selected brush"},
		{"ctrl+O",			"open map"},
		{"ctrl+S",			"save map"},
		{"arrows",			"scroll map"},
		{"mouse wheel",		"scroll map (on sliders)"},
		{"",""},

		{"",				"PAINT TOOL MODE"},
		{"mouse B1",		"paint brush"},
		{"mouse B2",		"open brush menu (on brush)"},
		{"alt, mouse B3", 	"pick brush (on brush)"},
		{"shift",			"snap to tile size (while painting)"},
		{"P",				"brush properties"},
		{"B",				"brush browse tile"},
		{"M",				"brush mapping"},
		{"F",				"brush flip"},
		{"R",				"brush rotate"},
		{"C",				"brush color"},
		{"shift+C",			"brush color system"},
		{"ctrl+Z",			"undo last brush add or delete"},
		{"ctrl+R",			"set room's properties"},
		{"",""},
		{"",""},
		
		{"",				"EDIT TOOL MODE "},
		{"ctrl+C",			"copy selection"},
		{"ctrl+V",			"paste selection"},
		{"ctrl+X",			"cut selection"},
		{"DEL",				"delete selection"},
		{"shift",			"add to selection"},
		{"alt, mouse B3",	"substract from selection"},
		{"mouse B1",		"make selection"},
		{"mouse B2",		"move selection"},
		{"ctrl+F",			"search dialog"},
		{"U",				"user scripts"},
		{"",""},
		
		{"",				"COLOR DIALOG"},
		{"mouse B1",		"pick RGB"},
		{"mouse B2",		"pick ALPHA"},
		{"page up/down",	"change palette"},
				
		{"",				"TILE BROWSE DIALOG"},
		{"up/down",			"up/down one row"},
		{"page up/down",	"up/down one page"},
		{"home/end",		"first/last page"},
		{"",""},
		{"",				"MAPPING DIALOG"},
		{"+/-",				"zoom in/out"},
		{"page up/down",	"previous/next tile"},
		{"home/end",		"first/last tile"},
		{"mouse B1",		"make selection"},
		{"mouse B2",		"move selection"},
		{"arrows",			"move selection"},
		{"shift+arrows",	"resize selection"},
		{"S/G/A",			"snap/grid/axes"},
		{"",""}
	};
		
	// create entries
	x = 0; y = DLGTITLEH+8+24; 
	for(i=0; i<sizeof(g_helptab); i++)
	{
		w = 8;

		// key
		CreateText( x+w, y, wkey, g_helptab[i][0], GUIALIGN_LEFT|GUIALIGN_CENTERY );
		ItemSetInt(IV_ID, ID_DLGITEM+i*2+0);
		ItemSetInt(IV_STYLE, GUISTYLE_BACKGR);
		ItemSetInt(IV_COLOR, COLOR_GUI1, COLOR_GUI1, COLOR_BLACK);
		if(g_helptab[i][0]!="") w+=wkey+8;
		
		// command
		CreateText( x+w, y, dlgw-8-x-w, g_helptab[i][1], GUIALIGN_LEFT|GUIALIGN_CENTERY );
		ItemSetInt(IV_ID, ID_DLGITEM+i*2+1);
		ItemSetInt(IV_STYLE, GUISTYLE_BACKGR);
		if(g_helptab[i][1]=="")	ItemSetInt(IV_COLOR, COLOR_GUI); else
		if(g_helptab[i][0]!="")	ItemSetInt(IV_COLOR, COLOR_GUI1); else
		{
			ItemSetInt(IV_COLOR, COLOR_GUI1);
		}
		y+=22;
	}

	// create page tabs
	pages = (sizeof(g_helptab)+DLGHELP_COUNT-1) / DLGHELP_COUNT; 
	x=8; y=DLGTITLEH+8; w=(dlgw-16)/pages;
	for(i=0;i<pages;i++)
	{
		CreateRadioButton(x,y,w, "PAGE "+(str)(i+1), (i==0),1,"DlgHelp_SetPage("+(str)i+");"); 
		x+=w;
	}
	DlgHelp_SetPage(0);
	
	DlgMoveToMouse(); DlgDockUp();
}

func DlgHelp_SetPage( page )
{
	h = 22;
	for(i=0; i<sizeof(g_helptab); i++)
	{
		itempage = i / DLGHELP_COUNT;
		y = (DLGTITLEH+8+24) + i*h - (page*DLGHELP_COUNT*h);
		
		ItemSelect(ItemFind(ID_DLGITEM+i*2+0)); 
		ItemSetInt(IV_Y,y); ItemSetInt(IV_Y2,y+20);
		ItemSetInt(IV_HIDDEN,itempage!=page);
		ItemSetInt(IV_DISABLE,itempage!=page);

		ItemSelect(ItemFind(ID_DLGITEM+i*2+1)); 
		ItemSetInt(IV_Y,y); ItemSetInt(IV_Y2,y+20);
		ItemSetInt(IV_HIDDEN,itempage!=page);
		ItemSetInt(IV_DISABLE,itempage!=page);
	}
}

//////////////////////////////////////////////////////////////////////////////////

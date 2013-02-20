//////////////////////////////////////////////////////////////////////////////////
// FileIO
//////////////////////////////////////////////////////////////////////////////////

func MAPExportText()
{
	if(g_mapfile=="") g_mapfile = "noname.map";
	expfile = g_mapfile;
	dot = strchr(expfile,".");
	if(?dot) expfile[[dot]]=0; // cut extention
	ToolReset();
	ret = WinDlgOpenFile( &expfile, "txt", 1 );
	if(!ret) return;

	// save
	WaitCursor(1);
	file = gs_fileopen(expfile,1); if(!file) goto fileerror2;
	if(!gs_filewritetext("DIZZY AGE EDITOR EXPORT", file)) goto fileerror1;
	if(!gs_filewritetext("MAP SIZE = "+(str)EdiGet(EDI_MAPW)+" x "+(str)EdiGet(EDI_MAPH), file)) goto fileerror1;
	if(!gs_filewritetext("BRUSHES_COUNT = "+(str)MapBrushCount(), file)) goto fileerror1;
	if(!gs_filewritetext("BRUSHES", file)) goto fileerror1;
	brushcount = MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		text = "";
		for(j=0;j<BRUSH_MAX;j++)
			text += (str "%10i")MapBrushGet(i,j);
		if(!gs_filewritetext(text, file)) goto fileerror1;
	}
	if(!gs_filewritetext("EOF", file)) goto fileerror1;

	gs_fileclose(file);	
	WaitCursor(0);
	MsgBoxOk("Message", "File export successful.", ICON_INFO );
	return; // ok
	
fileerror1:
	gs_fileclose(file);
fileerror2:
	WaitCursor(0);
	MsgBoxOk("Error", "File export failed. ", ICON_ERROR );
}

//////////////////////////////////////////////////////////////////////////////////
// EXPORT image
//////////////////////////////////////////////////////////////////////////////////
func MAPExportImage()
{
	MsgBox("Question", "Do you want to export a huge image with all the map ?\nIt may take a while. Make sure you have your map saved.", ICON_QUESTION, {{"YES","MAPExportImageDo();"}, {"NO",""}} );
}

func MAPExportImageDo()
{
	if(g_mapfile=="") g_mapfile = "noname.map";
	expfile = g_mapfile;
	dot = strchr(expfile,".");
	if(?dot) expfile[[dot]]=0; // cut extention
	ToolReset();
	ret = WinDlgOpenFile( &expfile, "png", 1 );
	if(!ret) return;

	// save
	WaitCursor(1);
	ret = SaveMapImage( expfile );
	WaitCursor(0);
	if(!ret) 	MsgBoxOk("Message", "Export map image failed.", ICON_ERROR);
	else		MsgBoxOk("Message", "Export map image successful.", ICON_INFO);
}


//////////////////////////////////////////////////////////////////////////////////
// EXPORT brushes ids
//////////////////////////////////////////////////////////////////////////////////
str g_brushesfile; // last selected file
func ExportBrushIds( mode, brushesfile, silent )
{
	if(EdiGet(EDI_SELECT)==0) { MsgBoxOk("Message", "No selected brushes.", ICON_INFO); return 0; }
	
	if(g_brushesfile=="") g_brushesfile = "brushes.txt";
	ToolReset();
	ret = WinDlgOpenFile( &g_brushesfile, "txt", 1 );
	if(!ret) return 0;
	
	// save
	println("Saving brushes ids ...");
	f = gs_fileopen( g_brushesfile, 1 );
	if(!f) { println("ERROR: can't open file."); return 0; }

	WaitCursor(1);
	count = 0;
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		if(!MapBrushGet(i,BRUSH_SELECT)) continue;
		id = MapBrushGet(i,BRUSH_ID);
		if(id) 
		{
			gs_filewritetext( ((str)id), f );
			count++;
		}
	}
	WaitCursor(0);
	gs_fileclose( f );
	
	MsgBoxOk("Message", ((str)count)+" brushes ids have been exported.", ICON_INFO);
	return count;
}

// save all static brushes with ids, used by map save
func MapStaticBrushIdsSave( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return;
	filename[[szlen-3]]='b';
	filename[[szlen-2]]='r';
	filename[[szlen-1]]='s';
	
	// save
	println("Saving brushes ids ...");
	f = gs_fileopen( filename, 1 );
	if(!f) { println("ERROR: can't open file."); return 0; }

	WaitCursor(1);
	count = 0;
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		if(MapBrushGet(i,BRUSH_TYPE)==0) // if static
		{
			id = MapBrushGet(i,BRUSH_ID);
			if(id) 
			{
				gs_filewritetext( ((str)id), f );
				count++;
			}
		}
	}
	WaitCursor(0);
	gs_fileclose( f );
	return count;
}

//////////////////////////////////////////////////////////////////////////////////

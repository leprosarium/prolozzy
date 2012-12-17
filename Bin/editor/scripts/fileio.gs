//////////////////////////////////////////////////////////////////////////////////
// FileIO
//////////////////////////////////////////////////////////////////////////////////
#def MAP_ID				"dizzymap"
#def MAP_CHUNKID		0x11111111
#def MAP_CHUNKINFO	 	0x22222222	// obsolete
#def MAP_CHUNKINFO2	 	0x22222223
#def MAP_CHUNKMARKERS 	0x33333333	// obsolete
#def MAP_CHUNKMARKERS2 	0x33333334
#def MAP_CHUNKBRUSHES 	0x88888888	// obsolede
#def MAP_CHUNKBRUSHES2 	0x88888889	

//////////////////////////////////////////////////////////////////////////////////
// SAVE
//////////////////////////////////////////////////////////////////////////////////
func MAPSave( filename )
{
	println("Saving map ...");
	f = gs_fileopen( filename, 1 );
	if(!f) { println("ERROR: can't open file."); return false; }
	
	ok=1;
	chunkid=0;
	chunksize=0;
	
	// ID
	chunkid = MAP_CHUNKID;
	chunksize = strlen(MAP_ID);
	ok &= gs_filewriteint(chunkid,f);
	ok &= gs_filewriteint(chunksize,f);
	ok &= gs_filewritedata(&MAP_ID,chunksize,f);
	
	// INFO2
	chunkid = MAP_CHUNKINFO2;
	chunksize = 6*4;
	ok &= gs_filewriteint(chunkid,f);
	ok &= gs_filewriteint(chunksize,f);
	ok &= gs_filewriteint(EdiGet(EDI_MAPW),f);
	ok &= gs_filewriteint(EdiGet(EDI_MAPH),f);
	ok &= gs_filewriteint(EdiGet(EDI_ROOMW),f);
	ok &= gs_filewriteint(EdiGet(EDI_ROOMH),f);
	ok &= gs_filewriteint(EdiGet(EDI_CAMX),f);
	ok &= gs_filewriteint(EdiGet(EDI_CAMY),f);
	
	// MARKERS2
	chunkid = MAP_CHUNKMARKERS2;
	chunksize = MarkerCount()*3*4;
	ok &= gs_filewriteint(chunkid,f);
	ok &= gs_filewriteint(chunksize,f);
	for(i=0;i<MarkerCount();i++)
	{
		ok &= gs_filewriteint(MarkerX(i),f);
		ok &= gs_filewriteint(MarkerY(i),f);
		ok &= gs_filewriteint(MarkerZ(i),f);
	}
	
	// BRUSHES2
	chunkid = MAP_CHUNKBRUSHES2;
	chunksize = MapBrushCount() * BRUSH_MAX * 4;
	ok &= gs_filewriteint(chunkid,f);
	ok &= gs_filewriteint(chunksize,f);
	brushcount = MapBrushCount();
	for(i=0;i<brushcount;i++)
		for(j=0;j<BRUSH_MAX;j++)
			ok &= gs_filewriteint(MapBrushGet(i,j),f);
	
	gs_fileclose(f);
	
	// ROOMS
	RoomNamesSave(filename);
	RoomTextsSave(filename);
	RoomPropsSave(filename);

	// BRUSHIDS
	MapStaticBrushIdsSave(filename);
	
	if(ok) 	println("Saving map SUCCESSFUL!");
	else 	println("Saving map FAILED!");
	return ok;
}


//////////////////////////////////////////////////////////////////////////////////
// LOAD
//////////////////////////////////////////////////////////////////////////////////
func MAPLoad( filename )
{
	// reset current map
	MapReset();
	RoomNamesReset(0);
	RoomTextsReset(0);
	RoomPropsReset(0);
	
	println("Loading map ...");
	f = gs_fileopen( filename, 0 );
	if(!f) { println("ERROR: can't open file."); return false; }
	
	gs_fileseek(f,0,2); 
	filesize = gs_filetell(f);
	gs_fileseek(f,0,0);

	mapw=0;	maph=0;
	camx=0; camy=0;
	roomw=ROOMW; roomh=ROOMH;
	chunkid=0;
	chunksize=0;

	while(true) // read chunks
	{
		chunkid = 0;
		if(!gs_filereadint(&chunkid,f)) goto error_chunk;
		if(!gs_filereadint(&chunksize,f)) goto error_chunk;
		if(chunksize<0) goto error_chunk;
	
		if(chunkid==MAP_CHUNKID)
		{
			if(chunksize!=strlen(MAP_ID)) goto error_chunk;
			sz = str(chunksize+1); sz[[chunksize]]=0;
			if(!gs_filereaddata(&sz,chunksize,f)) goto error_chunk;
			for(j=0;j<chunksize;j++)
				if(sz[[j]]!=MAP_ID[[j]]) goto error_chunk;
		}
		else
		if(chunkid==MAP_CHUNKINFO)
		{
			if(chunksize!=4*4) goto error_chunk;
			if(!gs_filereadint(&mapw,f)) goto error_chunk;
			if(!gs_filereadint(&maph,f)) goto error_chunk;
			if(!gs_filereadint(&camx,f)) goto error_chunk;
			if(!gs_filereadint(&camy,f)) goto error_chunk;
		}
		else
		if(chunkid==MAP_CHUNKINFO2)
		{
			if(chunksize!=6*4) goto error_chunk;
			if(!gs_filereadint(&mapw,f)) goto error_chunk;
			if(!gs_filereadint(&maph,f)) goto error_chunk;
			if(!gs_filereadint(&roomw,f)) goto error_chunk;
			if(!gs_filereadint(&roomh,f)) goto error_chunk;
			if(!gs_filereadint(&camx,f)) goto error_chunk;
			if(!gs_filereadint(&camy,f)) goto error_chunk;
		}
		else
		if(chunkid==MAP_CHUNKMARKERS) // obsolete, keept for compatibility
		{
			x=0; y=0;
			if((chunksize%(2*4))!=0) goto error_chunk;
			markers = chunksize/(2*4);
			for(i=0;i<markers;i++)
			{
				if(!gs_filereadint(&x,f)) goto error_chunk;
				if(!gs_filereadint(&y,f)) goto error_chunk;
				MarkerAdd(x,y,1);
			}
		}
		else
		if(chunkid==MAP_CHUNKMARKERS2)
		{
			x=0; y=0; z=1;
			if((chunksize%(3*4))!=0) goto error_chunk;
			markers = chunksize/(3*4);
			for(i=0;i<markers;i++)
			{
				if(!gs_filereadint(&x,f)) goto error_chunk;
				if(!gs_filereadint(&y,f)) goto error_chunk;
				if(!gs_filereadint(&z,f)) goto error_chunk;
				MarkerAdd(x,y,z);
			}
		}
		else
		if(chunkid==MAP_CHUNKBRUSHES)
		{
			brushmax = 40; // in v2.0 it's 48
			if((chunksize%(brushmax*4))!=0) goto error_chunk;
			brushcount = chunksize/(brushmax*4);
			for(i=0;i<brushcount;i++)
			{
				if(i!=MapBrushNew()) goto error_chunk; // stange if happend
				for(j=0;j<brushmax;j++)
				{
					val=0;
					if(!gs_filereadint(&val,f)) goto error_chunk;
					MapBrushSet(i,j,val);
					
					// users can hack (adjust) loaded brushes here, if needed
				}
	
				// upgrade: move select prop on 15 to convert to v2.0 !!!
				MapBrushSet(i,BRUSH_SELECT,MapBrushGet(i,14));

				// upgrate: reverse shader opaque with blend to convert to v2.0 !!!
				if(MapBrushGet(i,BRUSH_SHADER)==0) MapBrushSet(i,BRUSH_SHADER,SHADER_BLEND);
				else
				if(MapBrushGet(i,BRUSH_SHADER)==1) MapBrushSet(i,BRUSH_SHADER,SHADER_OPAQUE);
			}
		}
		else
		if(chunkid==MAP_CHUNKBRUSHES2)
		{
			if((chunksize%(BRUSH_MAX*4))!=0) goto error_chunk;
			brushcount = chunksize/(BRUSH_MAX*4);
			for(i=0;i<brushcount;i++)
			{
				if(i!=MapBrushNew()) goto error_chunk; // stange if happend
				for(j=0;j<BRUSH_MAX;j++)
				{
					val=0;
					if(!gs_filereadint(&val,f)) goto error_chunk;
					MapBrushSet(i,j,val);
					
					// users can hack (adjust) loaded brushes here, if needed
				}
			}
		}
		else
		{
			println( "WARNING: unknown chunk: id=",(str "%x")chunkid, " size=", (str)chunksize );
			if(0!=gs_fileseek(f,chunksize,1)) goto error_chunk; // step over
		}
		
		if( gs_filetell(f)>=filesize) break;
	}
	
	gs_fileclose(f);

	println("Loading map SUCCESSFUL!");
	
	// resize map (includes size checks, repartition, etc)
	MapResize(mapw,maph);
	RoomNamesReset(0);
	RoomTextsReset(0);
	RoomPropsReset(0);
	
	// ROOMS
	RoomNamesLoad(filename);
	RoomTextsLoad(filename);
	RoomPropsLoad(filename);

	EdiSet(EDI_CAMX, camx);
	EdiSet(EDI_CAMY, camy);
	EdiSet(EDI_ROOMW, roomw);
	EdiSet(EDI_ROOMH, roomh);

	return true;
	
error_chunk:
	println("ERROR: chunk broken.", (str "%x")chunkid); 
	gs_fileclose(f); 
	return false;
}


//////////////////////////////////////////////////////////////////////////////////
// EXPORT text
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

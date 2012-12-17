//////////////////////////////////////////////////////////////////////////////////
// RoomTexts.gs
//////////////////////////////////////////////////////////////////////////////////

#def MAX_ROOMTEXTS	4 	// Obs: when adjusted, change the default texts too (search for @ADJUST)

tab g_roomtexts; // matrix with room's custom texts lists

func RoomTextsReset( keeptexts )
{
	mapw = EdiGet(EDI_MAPW) / EdiGet(EDI_ROOMW);
	maph = EdiGet(EDI_MAPH) / EdiGet(EDI_ROOMH);
	roomtexts = tab(maph);
	for(ry=0;ry<maph;ry++)
	{
		roomtexts[ry] = tab(mapw);
		for(rx=0;rx<mapw;rx++)
		{
			if(keeptexts)
			{
				if(ry>=0 && ry<sizeof(g_roomtexts))
					if(rx>=0 && rx<sizeof(g_roomtexts[ry]))
						roomtexts[ry][rx] = g_roomtexts[ry][rx];
			}
			
			if(!?(roomtexts[ry][rx])) roomtexts[ry][rx] = {"","","",""}; // @ADJUST: default texts
		}
	}
	g_roomtexts = roomtexts;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int(roomx), int(roomy), int(idx)
// OUT: str(roomtext/"")
// Get one of the room's texts
/////////////////////////////////////////////////////////////////////////////////
func RoomTextsGet( rx, ry, idx )
{
	if(ry<0||ry>=sizeof(g_roomtexts)) return "";
	if(rx<0||rx>=sizeof(g_roomtexts[ry])) return "";
	if(idx<0||idx>=MAX_ROOMTEXTS) return "";
	return g_roomtexts[ry][rx][idx];
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int(roomx), int(roomy), int(idx), str(text)
// Set one of the room's texts
/////////////////////////////////////////////////////////////////////////////////
func RoomTextsSet( rx, ry, idx, text )
{
	if(ry<0||ry>=sizeof(g_roomtexts)) return;
	if(rx<0||rx>=sizeof(g_roomtexts[ry])) return;
	if(idx<0||idx>=MAX_ROOMTEXTS) return;
	g_roomtexts[ry][rx][idx] = text;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: str(filename)
// OUT: int(0/1)
// Called by map load to load room texts if available
// line format: rx;ry;txt1;txt2;txt3;txt4
/////////////////////////////////////////////////////////////////////////////////
func RoomTextsLoad( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return false;
	filename[[szlen-3]]='r';
	filename[[szlen-2]]='t';
	filename[[szlen-1]]=0;

	f = gs_fileopen(filename,0);
	if(!f) return false;
	while(true)
	{
		entry = "";
		eof = (gs_filereadtext(&entry,f)==0);
		if(eof) break;
		t = strexplode(entry,';');
		if(sizeof(t)!=MAX_ROOMTEXTS+2) continue;
		rx = (int)t[0];
		ry = (int)t[1];
		for(i=0;i<MAX_ROOMTEXTS;i++)
			RoomTextsSet(rx,ry,i,t[2+i]);
	}
	gs_fileclose(f);	
	return true;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: str(filename)
// Called by map save to save map's texts
/////////////////////////////////////////////////////////////////////////////////
func RoomTextsSave( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return;
	filename[[szlen-3]]='r';
	filename[[szlen-2]]='t';
	filename[[szlen-1]]=0;
	
	f = gs_fileopen(filename,1);
	if(!f) return 0;
	mapw = EdiGet(EDI_MAPW) / EdiGet(EDI_ROOMW);
	maph = EdiGet(EDI_MAPH) / EdiGet(EDI_ROOMH);
	for(ry=0;ry<maph;ry++)
	{
		for(rx=0;rx<mapw;rx++)
		{
			entry = "";
			for(i=0;i<MAX_ROOMTEXTS;i++)
				entry += ";"+RoomTextsGet(rx,ry,i);
			if(entry==";;;;") continue; // @ADJUST
			gs_filewritetext((str)rx+";"+(str)ry+entry,f);
		}
	}	
	gs_fileclose(f);
}

//////////////////////////////////////////////////////////////////////////////////
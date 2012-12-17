//////////////////////////////////////////////////////////////////////////////////
// RoomNames
//////////////////////////////////////////////////////////////////////////////////
tab g_roomnames; // table with room names

/////////////////////////////////////////////////////////////////////////////////
// IN: int(keep names 0/1)
// Call it after map resize or init
/////////////////////////////////////////////////////////////////////////////////
func RoomNamesReset( keepnames )
{
	mapw = EdiGet(EDI_MAPW) / EdiGet(EDI_ROOMW);
	maph = EdiGet(EDI_MAPH) / EdiGet(EDI_ROOMH);
	roomnames = tab(maph);
	for(ry=0;ry<maph;ry++)
	{
		roomnames[ry] = tab(mapw);
		for(rx=0;rx<mapw;rx++)
		{
			roomnames[ry][rx] = keepnames ? RoomNamesGet(rx,ry) : "";
		}
	}
	g_roomnames = roomnames;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int(roomx), int(roomy)
// OUT: str(roomname/"")
// Get room's name
/////////////////////////////////////////////////////////////////////////////////
func RoomNamesGet( rx, ry )
{
	if(ry<0||ry>=sizeof(g_roomnames)) return "";
	if(rx<0||rx>=sizeof(g_roomnames[ry])) return "";
	return g_roomnames[ry][rx];
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int(roomx), int(roomy), str(roomname)
// Set room's name
/////////////////////////////////////////////////////////////////////////////////
func RoomNamesSet( rx, ry, name )
{
	if(ry<0||ry>=sizeof(g_roomnames)) return;
	if(rx<0||rx>=sizeof(g_roomnames[ry])) return;
	g_roomnames[ry][rx] = name;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: str(filename)
// OUT: int(0/1)
// Called by map load to load room names if available
/////////////////////////////////////////////////////////////////////////////////
func RoomNamesLoad( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return false;
	filename[[szlen-3]]='n';
	filename[[szlen-2]]='a';
	filename[[szlen-1]]='m';

	f = gs_fileopen(filename,0);
	if(!f) return false;
	while(true)
	{
		entry = "";
		eof = (gs_filereadtext(&entry,f)==0);
		if(eof) break;
		t = strexplode(entry,';');
		if(sizeof(t)!=3) continue;
		RoomNamesSet((int)t[0],(int)t[1],t[2]);
	}
	gs_fileclose(f);	
	return true;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: str(filename)
// Called by map save to save map's names
/////////////////////////////////////////////////////////////////////////////////
func RoomNamesSave( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return;
	filename[[szlen-3]]='n';
	filename[[szlen-2]]='a';
	filename[[szlen-1]]='m';
	
	f = gs_fileopen(filename,1);
	if(!f) return 0;
	mapw = EdiGet(EDI_MAPW) / EdiGet(EDI_ROOMW);
	maph = EdiGet(EDI_MAPH) / EdiGet(EDI_ROOMH);
	for(ry=0;ry<maph;ry++)
	{
		for(rx=0;rx<mapw;rx++)
		{
			if(RoomNamesGet(rx,ry)=="") continue;
			entry = (str)rx+";"+(str)ry+";"+RoomNamesGet(rx,ry);
			gs_filewritetext(entry,f);
		}
	}	
	gs_fileclose(f);
}

//////////////////////////////////////////////////////////////////////////////////

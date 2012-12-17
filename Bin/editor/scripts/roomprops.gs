//////////////////////////////////////////////////////////////////////////////////
// RoomProps.gs
//////////////////////////////////////////////////////////////////////////////////

#def MAX_ROOMPROPS	8 	// Obs: when adjusted, change the default props too (search for @ADJUST)

tab g_roomprops; // matrix with room's props lists

func RoomPropsReset( keepprops )
{
	mapw = EdiGet(EDI_MAPW) / EdiGet(EDI_ROOMW);
	maph = EdiGet(EDI_MAPH) / EdiGet(EDI_ROOMH);
	roomprops = tab(maph);
	for(ry=0;ry<maph;ry++)
	{
		roomprops[ry] = tab(mapw);
		for(rx=0;rx<mapw;rx++)
		{
			if(keepprops)
			{
				if(ry>=0 && ry<sizeof(g_roomprops))
					if(rx>=0 && rx<sizeof(g_roomprops[ry]))
						roomprops[ry][rx] = g_roomprops[ry][rx];
			}
			
			if(!?(roomprops[ry][rx])) roomprops[ry][rx] = {0,0,0,0, 0,0,0,0}; // @ADJUST: default props
		}
	}
	g_roomprops = roomprops;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int(roomx), int(roomy), int(idx)
// OUT: int(roomprop/"")
// Get one of the room's props
/////////////////////////////////////////////////////////////////////////////////
func RoomPropsGet( rx, ry, idx )
{
	if(ry<0||ry>=sizeof(g_roomprops)) return 0;
	if(rx<0||rx>=sizeof(g_roomprops[ry])) return 0;
	if(idx<0||idx>=MAX_ROOMPROPS) return 0;
	return g_roomprops[ry][rx][idx];
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int(roomx), int(roomy), int(idx), int(value)
// Set one of the room's props
/////////////////////////////////////////////////////////////////////////////////
func RoomPropsSet( rx, ry, idx, value )
{
	if(ry<0||ry>=sizeof(g_roomprops)) return;
	if(rx<0||rx>=sizeof(g_roomprops[ry])) return;
	if(idx<0||idx>=MAX_ROOMPROPS) return;
	g_roomprops[ry][rx][idx] = value;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: str(filename)
// OUT: int(0/1)
// Called by map load to load room props if available
// line format: rx;ry;int1;int2;int3;int4
/////////////////////////////////////////////////////////////////////////////////
func RoomPropsLoad( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return false;
	filename[[szlen-3]]='r';
	filename[[szlen-2]]='p';
	filename[[szlen-1]]=0;

	f = gs_fileopen(filename,0);
	if(!f) return false;
	while(true)
	{
		entry = "";
		eof = (gs_filereadtext(&entry,f)==0);
		if(eof) break;
		t = strexplode(entry,';');
		if(sizeof(t)!=MAX_ROOMPROPS+2) continue;
		rx = (int)t[0];
		ry = (int)t[1];
		for(i=0;i<MAX_ROOMPROPS;i++)
		{
			RoomPropsSet(rx,ry,i,(int)t[2+i]);
		}
	}
	gs_fileclose(f);	
	return true;
}

/////////////////////////////////////////////////////////////////////////////////
// IN: str(filename)
// Called by map save to save map's props
/////////////////////////////////////////////////////////////////////////////////
func RoomPropsSave( filename )
{
	szlen = strlen(filename);
	if(szlen<5) return;
	filename[[szlen-3]]='r';
	filename[[szlen-2]]='p';
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
			for(i=0;i<MAX_ROOMPROPS;i++)
				entry += ";"+(str)RoomPropsGet(rx,ry,i);
			if(entry==";0;0;0;0;0;0;0;0") continue; // @ADJUST
			gs_filewritetext((str)rx+";"+(str)ry+entry,f);
		}
	}	
	gs_fileclose(f);
}

//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////
// CHECK BRUSH TILE
// Select all the brushes that have invalid tile ids.
// An invalid tile id means that there is no tile loaded having that id.
// It may occure when loading a map without having all the used tiles in the tile folder.
// Used for debuging purposes. 
// It also list their indexes and invalid tile ids in the log.
//////////////////////////////////////////////////////////////////////////////////
func ScrCheckTile()
{
	select = 0;
	println("Check Brush Tile:");
	WaitCursor(1);
	brushcount=MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		tile = MapBrushGet(i,BRUSH_TILE);
		if(TileFind(tile)==-1) // not found
		{
			select++;
			MapBrushSet(i,BRUSH_SELECT,1);
			println("missing tile ",tile," on brush #",i);
		}
		else
			MapBrushSet(i,BRUSH_SELECT,0);
	}
	WaitCursor(0);
	EdiSet(EDI_SELECT,select);
	if(select==0)
		MsgBoxOk("Message", "All brush tiles are valid.", ICON_INFO);
	else
		MsgBoxOk("Warning", "There are "+(str)select+" missing tiles.", ICON_WARNING);
}


//////////////////////////////////////////////////////////////////////////////////
// CHECK BRUSH ID DUPLICATES
// Search all brushes with ids (a valid id is different from 0) and select all
// those that have duplicate ids (there is another brush with the same id)
// Usually brush ids should be unique.
//////////////////////////////////////////////////////////////////////////////////
func ScrCheckId()
{
	println("Check Brush Id:");
	brushcount=MapBrushCount();
	
	// make id list
	WaitCursor(1);
	idxlist = tab(0); // collect idx of brushes with id
	for(i=0;i<brushcount;i++)
	{
		id = MapBrushGet(i,BRUSH_ID);
		MapBrushSet(i,BRUSH_SELECT,0);
		if(id==0) continue;
		tabadd(&idxlist,1);
		idxlist[sizeof(idxlist)-1] = i;
	}
	WaitCursor(0);
	EdiSet(EDI_SELECT,0);
	
	if(sizeof(idxlist)==0) { MsgBoxOk("Message", "no brushes with ids found.", ICON_INFO); return; }
	
	// check duplicates
	duplicates=0;
	WaitCursor(1);
	for(i=0;i<sizeof(idxlist);i++)
	{
		for(j=0;j<i;j++)
		{
			id1=MapBrushGet(idxlist[i],BRUSH_ID);
			id2=MapBrushGet(idxlist[j],BRUSH_ID);
			if(id1==id2)
			{
				if(!MapBrushGet(idxlist[i],BRUSH_SELECT))
				{
					MapBrushSet(idxlist[i],BRUSH_SELECT,1);
					println("bursh #",i," duplicate id=",id1);
					duplicates++;
				}
				if(!MapBrushGet(idxlist[j],BRUSH_SELECT))
				{
					MapBrushSet(idxlist[j],BRUSH_SELECT,1);
					println("bursh #",j," duplicate id=",id2);
					duplicates++;
				}
			}
		}
	}
	WaitCursor(0);
	EdiSet(EDI_SELECT,duplicates);
	if(duplicates)
		MsgBoxOk("Warning",	(str)sizeof(idxlist)+" brushes with ids\n"+(str)duplicates+" duplicates ids", ICON_WARNING );
	else
		MsgBoxOk("Message",	(str)sizeof(idxlist)+" brushes with ids\nno duplicates ids", ICON_INFO );
}


//////////////////////////////////////////////////////////////////////////////////
// CHECK BRUSH OVERLAPPING
// Select and list in log the brushes that are overlapping.
// Two brushes are considered to be overlapping when they have the same
// position, size, tile and mapping. 
// This may occure when placing a brush twice in the same place, by mistake.
// To repare this, you may select those brushes by their indexes (listed in log).
//////////////////////////////////////////////////////////////////////////////////
func ScrCheckOverlapping()
{
	println("Check Brush Overlapping:");
	WaitCursor(1);
	overlapping = 0;
	brushcount = MapBrushCount();
	for(i=0;i<brushcount;i++)
	{
		MapBrushSet(i,BRUSH_SELECT,0);

		x 		= MapBrushGet(i,BRUSH_X);
		y 		= MapBrushGet(i,BRUSH_Y);
		w 		= MapBrushGet(i,BRUSH_W);
		h 		= MapBrushGet(i,BRUSH_H);
		tile	= MapBrushGet(i,BRUSH_TILE);
		map0 	= MapBrushGet(i,BRUSH_MAP+0);
		map1 	= MapBrushGet(i,BRUSH_MAP+1);
		map2 	= MapBrushGet(i,BRUSH_MAP+2);
		map3 	= MapBrushGet(i,BRUSH_MAP+3);
	
		for(j=0;j<i;j++)
		{
			if( x		!= MapBrushGet(j,BRUSH_X) ) continue;
			if( y		!= MapBrushGet(j,BRUSH_Y) ) continue;
			if( w		!= MapBrushGet(j,BRUSH_W) ) continue;
			if( h		!= MapBrushGet(j,BRUSH_H) ) continue;
			if( tile	!= MapBrushGet(j,BRUSH_TILE) ) continue;
			if( map0	!= MapBrushGet(j,BRUSH_MAP+0) ) continue;
			if( map1	!= MapBrushGet(j,BRUSH_MAP+1) ) continue;
			if( map2	!= MapBrushGet(j,BRUSH_MAP+2) ) continue;
			if( map3	!= MapBrushGet(j,BRUSH_MAP+3) ) continue;
				
			// overlapping	
			if(!MapBrushGet(i,BRUSH_SELECT))
			{
				MapBrushSet(i,BRUSH_SELECT,1);
				println("bursh #",i," overlapping");
				overlapping++;
			}
			if(!MapBrushGet(j,BRUSH_SELECT))
			{
				MapBrushSet(j,BRUSH_SELECT,1);
				println("bursh #",j," overlapping");
				overlapping++;
			}
		}
	}

	WaitCursor(0);
	EdiSet(EDI_SELECT,overlapping);
	if(overlapping)	MsgBoxOk("Warning",	(str)overlapping+" overlapping brushes found.", ICON_WARNING);
	else			MsgBoxOk("Message",	"no overlapping brushes found.", ICON_INFO);
}

//////////////////////////////////////////////////////////////////////////////////
// CHECK DYNAMIC BRUSHES WITHOUT ID
// Search and select all dynamic brushes without ids, those are inaccesible
//////////////////////////////////////////////////////////////////////////////////
func ScrCheckDynamicBrushId()
{
	println("Check Dynamic Brush Id:");
	count = 0;
	brushcount=MapBrushCount();
	
	// search
	WaitCursor(1);
	for(i=0;i<brushcount;i++)
	{
		id = MapBrushGet(i,BRUSH_ID);
		MapBrushSet(i,BRUSH_SELECT,0);
		if(MapBrushGet(i,BRUSH_TYPE)==0) continue;
		if(id!=0) continue;
		MapBrushSet(i,BRUSH_SELECT,1);
		count++;
	}
	WaitCursor(0);
	EdiSet(EDI_SELECT,count);
	if(count)
		MsgBoxOk("Warning",	"There are "+(str)count+" dynamic brushes without ids.\nThey will not be accessible in script.\nSet them ids or make them static brushes.", ICON_WARNING );
	else
		MsgBoxOk("Info",	"There are no dynamic brushes without ids.\nThat's good.", ICON_INFO );
}

//////////////////////////////////////////////////////////////////////////////////
// CHECK STATIC BRUSHES WITH ID
// Search and select all static brushes with ids
// Users must make sure those are indeed the brushes that must have ids
//////////////////////////////////////////////////////////////////////////////////
func ScrCheckStaticBrushId()
{
	println("Check Static Brush Id:");
	count = 0;
	brushcount=MapBrushCount();
	
	// search
	WaitCursor(1);
	for(i=0;i<brushcount;i++)
	{
		id = MapBrushGet(i,BRUSH_ID);
		MapBrushSet(i,BRUSH_SELECT,0);
		if(id==0) continue;
		if(MapBrushGet(i,BRUSH_TYPE)==1) continue;
		MapBrushSet(i,BRUSH_SELECT,1);
		count++;
	}
	WaitCursor(0);
	EdiSet(EDI_SELECT,count);
	if(count)
		MsgBoxOk("Info",	"There are "+(str)count+" static brushes with ids.\nMake sure they need ids indeed.", ICON_INFO );
	else
		MsgBoxOk("Info",	"There are no static brushes with ids.", ICON_INFO );
}

//////////////////////////////////////////////////////////////////////////////////
// COUNT ROOMS WITH BRUSH CONTENT
// Counts rooms that have at least a brush inside.
// Top-left or bottom-right brush corner must be at lest 32 inside the room area.
// Guide green brushes are inserted in each counted room, to help you verify the counting.
// Because of that the map is altered after the operation so make sure you don't save those guide brushes.
// Save your map BEFORE counting and reopen it after.
//////////////////////////////////////////////////////////////////////////////////
func ScrCountRooms()
{
	MsgBox("Question", "Count rooms with brush content.\nPlease be patient, it may take a while, \ndepending on the map size and brushes count.\n\nWARNING\nSave your map BEFORE this operation!\nGuide green brushes are going to be inserted\nto help you verify the counting.\nProceed?\n", ICON_QUESTION, {{"YES","ScrCountRooms_Apply();"}, {"NO",""}} );
}

func ScrCountRooms_Apply()
{
	count = 0;
	WaitCursor(1);

	roomw = EdiGet(EDI_ROOMW);
	roomh = EdiGet(EDI_ROOMH);
	roomx = EdiGet(EDI_MAPW) / roomw;
	roomy = EdiGet(EDI_MAPH) / roomh;
	
	brushcount=MapBrushCount();
	
	for(y=0;y<roomy;y++)
	{
		for(x=0;x<roomx;x++)
		{
			for(i=0;i<brushcount;i++)
			{
				rx = x * roomw;
				ry = y * roomh;
				bx = MapBrushGet(i,BRUSH_X);
				by = MapBrushGet(i,BRUSH_Y);
				bw = MapBrushGet(i,BRUSH_W);
				bh = MapBrushGet(i,BRUSH_H);
				if( ( (bx>rx+32 && bx<rx+roomw-32) && (by>ry+32 && by<ry+roomh-32) ) || 
					( (bx+bw>rx+32 && bx+bw<rx+roomw-32) && (by+bh>ry+32 && by+bh<ry+roomh-32) ) )
					{
						bn = MapBrushNew();
						MapBrushSet(bn,BRUSH_LAYER,7);
						MapBrushSet(bn,BRUSH_X,rx);
						MapBrushSet(bn,BRUSH_Y,ry);
						MapBrushSet(bn,BRUSH_W,roomw);
						MapBrushSet(bn,BRUSH_H,roomh);
						MapBrushSet(bn,BRUSH_TILE,0);
						MapBrushSet(bn,BRUSH_MAP+2,8);
						MapBrushSet(bn,BRUSH_MAP+3,8);
						MapBrushSet(bn,BRUSH_COLOR,0x8000ff00);
						MapBrushSet(bn,BRUSH_SHADER,SHADER_BLEND);
						MapBrushSet(bn,BRUSH_ID,1234567);  // funky id for easy selection
						// MapBrushSet(bn,BRUSH_SELECT,1);
						count++;
						break;
					}
			}
		}
	}
	WaitCursor(0);
	MsgBoxOk("Message", (str)count+" rooms found with content.\nMap has been altered with guide green brushes.\nCheck the counting, but DON'T save the map.", ICON_INFO ); 
	MapRepartition();
	MapRefresh();	
}

//////////////////////////////////////////////////////////////////////////////////

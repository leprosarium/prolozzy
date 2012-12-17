/////////////////////////////////////////////////////////////////////////////////
// IN: int; idx; object index
// Object plays all frames from the current tile's animation.
/////////////////////////////////////////////////////////////////////////////////
func ObjPlayAnim( idx )
{
	ObjSet( idx, O_FRAME, 0 );
	ObjSet( idx, O_ANIM, 1 );
	tileidx = TileFind( ObjGet(idx, O_TILE) );
	frames = TileGet( tileidx, TILE_FRAMES ) * ObjGet( idx, O_DELAY );
	WaitFrames(frames);
}

/////////////////////////////////////////////////////////////////////////////////
// IN: int; idx; object index
// IN: tab; frames; list with frames to play
// Object plays all the frames specified in the table from the current tile.
// Waits object's O_DELAY game cycles between each frame. 
/////////////////////////////////////////////////////////////////////////////////
func ObjPlayAnimFrames( idx, frames )
{
	ObjSet( idx, O_ANIM, 0 );
	delay = ObjGet( idx, O_DELAY );
	for(i=0;i<sizeof(frames);i++)
	{
		ObjSet( idx, O_FRAME, frames[i] );
		WaitFrames(delay);
	}
}


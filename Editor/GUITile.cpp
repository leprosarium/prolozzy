//////////////////////////////////////////////////////////////////////////////////////////////////
// GUITile.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUITile.h"
#include "GUI.h"
#include "EdiPaint.h"
#include "EdiApp.h"
#include "EdiMap.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUITile
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUITile::cGUITile(): scale(), shrink()
{
	imgAlign = 0;
}

void cGUITile::OnDraw()
{
	iRect rc = scrRect();

	// tile
	int idx = g_paint.TileFind(value);
	cTile* tile = g_paint.TileGet(idx); 
	if(tile==NULL) return;
	int x = rc.p1.x;
	int y = rc.p1.y;
	int w = tile->GetWidth();
	int h = tile->GetHeight();

	// sprite
	fRect src(0,0,w,h);

	// shrink or clip
	float scale = static_cast<float>(this->scale);
	if( w*scale > rc.Width())
	{
		if(shrink)
			scale = (float)(rc.Width()) / w;
		else
			src.p2.x = (rc.Width()) / scale;
	}
	if( h*scale > rc.Height())
	{
		if(shrink)
			scale = (float)(rc.Height()) / h;
		else
			src.p2.y = (rc.Height()) / scale;
	}

	// frame anim (1 game frame = 25ms); don't know brush delay !
	int frame = GetTickCount() / (25*3);
	frame = frame % tile->m_frames;
	int fx = tile->GetFx(frame);
	int fy = tile->GetFy(frame);
	fV2 ss(fx * w, fy * h);
	src.p1 += ss;
	src.p2 += ss;

	// align
	int align = imgAlign;
	if((align & GUIALIGN_CENTERX) == GUIALIGN_CENTERX)	x = (rc.p1.x + rc.p2.x - w)/2;	else	
	if(align & GUIALIGN_LEFT)		x = rc.p1.x;				else
	if(align & GUIALIGN_RIGHT)		x = rc.p2.x-w;
	if((align & GUIALIGN_CENTERY) == GUIALIGN_CENTERY)	y = (rc.p1.y + rc.p2.y-h)/2;	else	
	if(align & GUIALIGN_TOP)		y = rc.p1.y;				else
	if(align & GUIALIGN_BOTTOM)		y = rc.p2.y-h;

	// clipping on
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(fRect(rc));

	// background
	iRect rect(x, y, 
		std::min(x+static_cast<int>(w*scale), static_cast<int>(rc.p2.x)), 
		std::min(y+static_cast<int>(h*scale), static_cast<int>(rc.p2.y)));
	GUIDrawBar(rect, color[1]); 
		
	// sprite
	R9_DrawSprite( fV2(x,y), src, tile->m_tex, 0xffffffff, 0, scale );
	
	// clipping off
	R9_SetClipping(oldclip);

}


//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUITileMap
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUITileMap::cGUITileMap() : scale(), snap(), grid(), axes(), m_mode()
{
}

void cGUITileMap::OnUpdate()
{
	iV2 sel = map.p1;
	iV2 sels = map.Size();
	if(sels.x < 0) sels.x = 0;
	if(sels.y < 0) sels.y = 0;

	// tile info
	int tileid = value;
	int idx = g_paint.TileFind(tileid);
	cTile* tile = g_paint.TileGet(idx); 
	if(tile==NULL) return;
	int tilew = tile->GetWidth();
	int tileh = tile->GetHeight();

	iRect rc = scrRect(); // control rect in screen
	m_mousein = rc.IsInside(g_gui->m_mouse);
	iV2 m = g_gui->m_mouse - rc.p1;	// mouse relative to client
	m = fV2(m) / scale; // to tile space

	iRect rctile(0, 0, tilew, tileh);
	BOOL mouseintile = rctile.IsInside(m);

	iRect rcsel(sel, sel + sels);
	BOOL mouseinsel = rcsel.IsInside(m);

	// additional keys for snap and grid
	BOOL shift	= (I9_GetKeyValue(I9K_LSHIFT)) || (I9_GetKeyValue(I9K_RSHIFT));
	if(I9_GetKeyDown(I9K_S))	snap = !snap;
	if(I9_GetKeyDown(I9K_G))	grid = !grid;
	if(I9_GetKeyDown(I9K_A))	axes = !axes;
	if(I9_GetKeyDown(I9K_LEFT))	if(shift) sels.x--; else sel.x--;
	if(I9_GetKeyDown(I9K_RIGHT))	if(shift) sels.x++; else sel.x++;
	if(I9_GetKeyDown(I9K_UP))		if(shift) sels.y--; else sel.y--;
	if(I9_GetKeyDown(I9K_DOWN))	if(shift) sels.y++; else sel.y++;

	// mouse down
	if(m_mode==0)
	{
		if(m_mousein && I9_GetKeyDown(I9_MOUSE_B1))
		{ 
			// start new selection
			m_mode = 1;
			sel.x = Snap(m.x);
			sel.y = Snap(m.y);
			sels = 0;
			Capture(true);
		}
		if(mouseinsel && I9_GetKeyDown(I9_MOUSE_B2))
		{ 
			// start moving selection
			m_mode = 2;
			m_move = m - sel;
			Capture(true);
		}
	}
	else
	if(m_mode==1)	// selecting
	{
		sels = iV2(Snap(m.x), Snap(m.y)) - sel;

		if(sels.x<0) sels.x=0;
		if(sels.x>tilew-sel.x)	sels.x=tilew-sel.x;
		if(sels.y<0) sels.y=0;
		if(sels.y>tileh-sel.y)	sels.y=tileh-sel.y;
	}
	else
	if(m_mode==2)  // move selection
	{
		sel =  m - m_move;
		sel.x = Snap(sel.x);
		sel.y = Snap(sel.y);

		if(sel.x<0) sel.x=0;
		if(sel.y<0) sel.y=0;
		if(sel.x+sels.x>tilew) sel.x=tilew-sels.x;
		if(sel.y+sels.y>tileh) sel.y=tileh-sels.y;
	}

	// loosing captures
	if(m_mode==1 && !I9_GetKeyValue(I9_MOUSE_B1))
	{
		m_mode = 0;
		Capture(false);
	}
	if(m_mode==2 && !I9_GetKeyValue(I9_MOUSE_B2))
	{
		m_mode = 0;
		Capture(false);
	}

	
	// bound
	if(sel.x<0) sel.x=0;
	if(sel.y<0) sel.y=0;
	if(sel.x>tilew-1) sel.x=tilew-1;
	if(sel.y>tileh-1) sel.y=tileh-1;
	if(sel.x+sels.x>tilew) sels.x=tilew-sel.x;
	if(sel.y+sels.y>tileh) sels.y=tileh-sel.y;

	// set back
	map = iRect(sel, sel+sels);
}

void cGUITileMap::OnDraw()
{
	iRect rect = scrRect();

	iV2 sel = map.p1;
	iV2 sels = map.Size();
	if(sels.x<0) sels.x=0;
	if(sels.y<0) sels.y=0;
	iV2 m = g_gui->m_mouse - rect.p1; // mouse relative to client
	m = fV2(m) / scale; // to tile space

	// draw grid
	if(grid)
	{
		iRect rc = scrRect();
		dword color = 0xA0ffffff;
		int st = 8 * scale;
		for(int i=rc.p1.y; i<rc.p2.y; i+=st)
			R9_DrawLine( fV2(rc.p1.x,i), fV2(rc.p2.x,i), color );

		for(int i=rc.p1.x; i<rc.p2.x; i+=st)
			R9_DrawLine( fV2(i,rc.p1.y), fV2(i,rc.p2.y), color );
	}

	// draw selection
	if( sels.x!=0 && sels.y!=0 )
	{
		iRect r(sel * scale,  (sel + sels) * scale);
		r.Offset(rect.p1);
		if( r.p1.x != r.p2.x && r.p1.y != r.p2.y ) 
			GUIDrawRectDot(r, 0xffffffff);
	}

	// draw axes
	if(axes)
	{
		iRect rc = scrRect();
		dword color = 0xff00ff00;
		
		int x = rect.p1.x + Snap(m.x)*scale;
		int y = rect.p1.y + Snap(m.y)*scale;
		if(m_mousein)
		{
			R9_DrawLine( fV2(rc.p1.x,y), fV2(rc.p2.x,y), color );
			R9_DrawLine( fV2(x,rc.p1.y), fV2(x,rc.p2.y), color );
		}
	}

	// tooltip bar info
	m.x=Snap(m.x); 
	m.y=Snap(m.y);

	if(m_mousein)
	{
		std::stringstream o;
		if(m_mode==0)	o << m.x << " " << m.y;
		else if(m_mode==1)	o << m.x << " " << m.y << std::endl << sels.x << " x " << sels.y;
		else if(m_mode==2)	o << m.x << " " << m.y;
		g_gui->ToolTip = o.str();
	}

}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

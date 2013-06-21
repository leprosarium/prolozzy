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


cGUITile::~cGUITile()
{
}

void cGUITile::Draw()
{
	RECT rc;
	GetScrRect(rc);

	// tile
	int idx = g_paint.TileFind(value);
	cTile* tile = g_paint.TileGet(idx); 
	if(tile==NULL) return;
	int x = rc.left;
	int y = rc.top;
	int w = tile->GetWidth();
	int h = tile->GetHeight();

	// sprite
	fRect src(0,0,w,h);

	// shrink or clip
	float scale = static_cast<float>(this->scale);
	if( w*scale > rc.right-rc.left )
	{
		if(shrink)
			scale = (float)(rc.right-rc.left) / w;
		else
			src.p2.x = (rc.right-rc.left) / scale;
	}
	if( h*scale > rc.bottom-rc.top )
	{
		if(shrink)
			scale = (float)(rc.bottom-rc.top) / h;
		else
			src.p2.y = (rc.bottom-rc.top) / scale;
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
	if((align & GUIALIGN_CENTERX) == GUIALIGN_CENTERX)	x = (rc.left+rc.right-w)/2;	else	
	if(align & GUIALIGN_LEFT)		x = rc.left;				else
	if(align & GUIALIGN_RIGHT)		x = rc.right-w;
	if((align & GUIALIGN_CENTERY) == GUIALIGN_CENTERY)	y = (rc.top+rc.bottom-h)/2;	else	
	if(align & GUIALIGN_TOP)		y = rc.top;				else
	if(align & GUIALIGN_BOTTOM)		y = rc.bottom-h;

	// clipping on
	fRect oldclip = R9_GetClipping();
	R9_SetClipping( fRect(rc.left,rc.top,rc.right,rc.bottom) );

	// background
	iRect rect(x, y, 
		std::min(x+static_cast<int>(w*scale), static_cast<int>(rc.right)), 
		std::min(y+static_cast<int>(h*scale), static_cast<int>(rc.bottom)));
	GUIDrawBar(rect.p1.x, rect.p1.y, rect.p2.x, rect.p2.y, color[1]); 
		
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

cGUITileMap::~cGUITileMap()
{
}

void cGUITileMap::Update()
{
	int selx = map.p1.x;
	int sely = map.p1.y;
	int selw = map.Width();
	int selh = map.Height();
	if(selw<0) selw=0;
	if(selh<0) selh=0;

	// tile info
	int tileid = value;
	int idx = g_paint.TileFind(tileid);
	cTile* tile = g_paint.TileGet(idx); 
	if(tile==NULL) return;
	int tilew = tile->GetWidth();
	int tileh = tile->GetHeight();

	RECT rc;
	GetScrRect(rc); // control rect in screen
	m_mousein = INRECT( g_gui->m_mousex, g_gui->m_mousey, rc);
	int mx = g_gui->m_mousex - rc.left;	// mousex relative to client
	int my = g_gui->m_mousey - rc.top;	// mousey relative to client
	mx = static_cast<int>(static_cast<float>(mx) / scale); // to tile space
	my = static_cast<int>(static_cast<float>(my) / scale); // to tile space

	iRect rctile(0, 0, tilew, tileh);
	BOOL mouseintile = rctile.IsInside(iV2(mx, my));

	iRect rcsel(selx, sely, selx+selw, sely+selh);
	BOOL mouseinsel = rcsel.IsInside(iV2(mx, my));

	// additional keys for snap and grid
	BOOL shift	= (I9_GetKeyValue(I9K_LSHIFT)) || (I9_GetKeyValue(I9K_RSHIFT));
	if(I9_GetKeyDown(I9K_S))	snap = !snap;
	if(I9_GetKeyDown(I9K_G))	grid = !grid;
	if(I9_GetKeyDown(I9K_A))	axes = !axes;
	if(I9_GetKeyDown(I9K_LEFT))	if(shift) selw--; else selx--;
	if(I9_GetKeyDown(I9K_RIGHT))	if(shift) selw++; else selx++;
	if(I9_GetKeyDown(I9K_UP))		if(shift) selh--; else sely--;
	if(I9_GetKeyDown(I9K_DOWN))	if(shift) selh++; else sely++;

	// mouse down
	if(m_mode==0)
	{
		if(m_mousein && I9_GetKeyDown(I9_MOUSE_B1))
		{ 
			// start new selection
			m_mode = 1;
			selx = Snap(mx);
			sely = Snap(my);
			selw = 0;
			selh = 0;
			Capture(true);
		}
		if(mouseinsel && I9_GetKeyDown(I9_MOUSE_B2))
		{ 
			// start moving selection
			m_mode = 2;
			m_move = iV2(mx-selx, my-sely);
			Capture(true);
		}
	}
	else
	if(m_mode==1)	// selecting
	{
		selw = Snap(mx) - selx;
		selh = Snap(my) - sely;

		if(selw<0) selw=0;
		if(selw>tilew-selx)	selw=tilew-selx;
		if(selh<0) selh=0;
		if(selh>tileh-sely)	selh=tileh-sely;
	}
	else
	if(m_mode==2)  // move selection
	{
		selx =  mx - m_move.x;
		sely =  my - m_move.y;
		selx = Snap(selx);
		sely = Snap(sely);

		if(selx<0) selx=0;
		if(sely<0) sely=0;
		if(selx+selw>tilew) selx=tilew-selw;
		if(sely+selh>tileh) sely=tileh-selh;
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
	if(selx<0) selx=0;
	if(sely<0) sely=0;
	if(selx>tilew-1) selx=tilew-1;
	if(sely>tileh-1) sely=tileh-1;
	if(selx+selw>tilew) selw=tilew-selx;
	if(sely+selh>tileh) selh=tileh-sely;

	// set back
	map = iRect(selx, sely, selx+selw, sely+selh);
}

void cGUITileMap::Draw()
{

	RECT rect;
	GetScrRect(rect);

	int selx = map.p1.x;
	int sely = map.p1.y;
	int selw = map.Width();
	int selh = map.Height();
	if(selw<0) selw=0;
	if(selh<0) selh=0;
	int mx = g_gui->m_mousex - rect.left; // mousex relative to client
	int my = g_gui->m_mousey - rect.top; // mousey relative to client
	mx = static_cast<int>(static_cast<float>(mx) / scale); // to tile space
	my = static_cast<int>(static_cast<float>(my) / scale); // to tile space

	// draw grid
	if(grid)
	{
		int i;
		RECT rc; 
		GetScrRect(rc);
		int step=8;
		dword color = 0xA0ffffff;

		for(i=rc.top; i<rc.bottom; i+=step*scale)
			R9_DrawLine( fV2(rc.left,i), fV2(rc.right,i), color );

		for(i=rc.left; i<rc.right; i+=step*scale)
			R9_DrawLine( fV2(i,rc.top), fV2(i,rc.bottom), color );
	}

	// draw selection
	if( selw!=0 && selh!=0 )
	{
		int x1,x2,y1,y2;
		x1 = rect.left + selx * scale;
		y1 = rect.top + sely * scale;
		x2 = rect.left + (selx+selw) * scale;
		y2 = rect.top + (sely+selh) * scale;
		if( x1!=x2 && y1!=y2 ) 
			GUIDrawRectDot(x1,y1,x2,y2,0xffffffff);
	}

	// draw axes
	if(axes)
	{
		RECT rc; 
		GetScrRect(rc);
		dword color = 0xff00ff00;
		
		int x = rect.left+Snap(mx)*scale;
		int y = rect.top+Snap(my)*scale;
		if(m_mousein)
		{
			R9_DrawLine( fV2(rc.left,y), fV2(rc.right,y), color );
			R9_DrawLine( fV2(x,rc.top), fV2(x,rc.bottom), color );
		}
	}

	// tooltip bar info
	mx=Snap(mx); 
	my=Snap(my);

	if(m_mousein)
	{
		std::stringstream o;
		if(m_mode==0)	o << mx << " " << my;
		else if(m_mode==1)	o << mx << " " << my << std::endl << selw << " x " << selh;
		else if(m_mode==2)	o << mx << " " << my;
		g_gui->ToolTip = o.str();
	}

}

int cGUITileMap::Snap( int x )
{ 
	if(!snap) return x;
	return (x/8)*8 + (x%8>=4)*8; // snap closer
	// return x = (x/8)*8; // snap under
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

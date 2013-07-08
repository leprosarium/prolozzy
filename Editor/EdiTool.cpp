//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiTool.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "EdiTool.h"
#include "EdiApp.h"
#include "EdiMap.h"
#include "GUI.h"

#include <algorithm>


#define SNAP(x,grid)	( ((x)/(grid))*(grid) + ((x)%(grid)>=(grid)/2)*(grid) )

#define SNAP2GRID(x,y)						\
{											\
	if(EdiApp()->m_snap)					\
	{										\
		x=SNAP(x,EdiApp()->m_gridsize);		\
		y=SNAP(y,EdiApp()->m_gridsize);		\
	}										\
}

cEdiTool::cEdiTool(const std::string & name) : m_name(name), m_mode(), m_ax(), m_ay(), m_isbusy()
{
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolPaint
// mode: 0=none, 1=paint, 2=alt pick/del
//////////////////////////////////////////////////////////////////////////////////////////////////
cEdiToolPaint::cEdiToolPaint() : cEdiTool("PAINT"), m_brushidx(-1)
{

}

void cEdiToolPaint::Switch( BOOL on )
{
	Reset();
}

void cEdiToolPaint::Reset()
{
	if( m_mode==2 )	return;
	m_mode=-1; // draw trick
}

void cEdiToolPaint::Update( float dtime )
{

	int mx = EdiApp()->GetMouseX() - VIEWX;
	int my = EdiApp()->GetMouseY() - VIEWY;
	BOOL inview = INVIEW(mx,my);
	if(!inview && !m_isbusy) return;

	BOOL alt = (I9_GetKeyValue(I9K_LALT)) || (I9_GetKeyValue(I9K_RALT));
	BOOL ctrl	= (I9_GetKeyValue(I9K_LCONTROL)) || (I9_GetKeyValue(I9K_RCONTROL));

	m_brushidx = -1; // clear picked brush
	tBrush& brush = EdiApp()->m_brush;

	if( m_mode==-1 ) m_mode=0; // draw trick

	if( m_mode==0 )
	{
		int bw = static_cast<int>(brush.mapWith()); // brush.m_data[BRUSH_MAP+2]-brush.m_data[BRUSH_MAP+0];
		int bh = static_cast<int>(brush.mapHeight()); // brush.m_data[BRUSH_MAP+3]-brush.m_data[BRUSH_MAP+1];
		VIEW2CAM(mx,my);
		if(mx<CAMX1+bw)	mx=CAMX1+bw;
		if(my<CAMY1+bh)	my=CAMY1+bh;
		if(mx>CAMX2)	mx=CAMX2;
		if(my>CAMY2)	my=CAMY2;
		mx = mx-bw;
		my = my-bh;
		SNAP2GRID(mx,my); // grid snap

		brush.m_data[BRUSH_X] = mx;
		brush.m_data[BRUSH_Y] = my;
		brush.m_data[BRUSH_W] = bw;
		brush.m_data[BRUSH_H] = bh;

		if(I9_GetKeyDown(I9_MOUSE_B1)) m_mode=1;
		else
		if(I9_GetKeyValue(I9_MOUSE_B2)) m_mode=2;
		else
		if(I9_GetKeyValue(I9_MOUSE_B3) || alt) m_mode=3;
		else
		if(ctrl && I9_GetKeyDown(I9K_Z)) EdiApp()->Undo();

		// axes
		m_ax = mx;
		m_ay = my;
	}
	else
	if( m_mode==1 )
	{
		VIEW2CAM(mx,my);
		if(mx>CAMX2) mx=CAMX2;
		if(my>CAMY2) my=CAMY2;

		int bw = mx - brush.m_data[BRUSH_X];
		int bh = my - brush.m_data[BRUSH_Y];
		if(bw<0) bw=0;
		if(bh<0) bh=0;
		
		if(I9_GetKeyValue(I9K_LSHIFT))
		{
			int mw = static_cast<int>(brush.mapWith()); // brush.m_data[BRUSH_MAP+2]-brush.m_data[BRUSH_MAP+0];
			int mh = static_cast<int>(brush.mapHeight()); // brush.m_data[BRUSH_MAP+3]-brush.m_data[BRUSH_MAP+1];
			if(mw<1) bw=0; else	bw = (bw / mw) * mw;
			if(mh<1) bh=0; else	bh = (bh / mh) * mh;
		}
		else
			SNAP2GRID(bw,bh); // grid snap

		brush.m_data[BRUSH_W] = bw;
		brush.m_data[BRUSH_H] = bh;

		// axes
		m_ax = brush.m_data[BRUSH_X] + bw;
		m_ay = brush.m_data[BRUSH_Y] + bh;

		if(!I9_GetKeyValue(I9_MOUSE_B1)) 
		{
			// add brush !
			if( brush.m_data[BRUSH_W]>0 && brush.m_data[BRUSH_H]>0 && inview)
			{
				int idx = g_map.BrushNew();
				tBrush *b = g_map.m_brush[idx];
				*b = brush;
				b->m_data[BRUSH_SELECT] = 0;
				b->m_data[BRUSH_LAYER] = EdiApp()->LayerActive();
				g_map.m_refresh = TRUE;
				g_map.PartitionAdd(b);
				EdiApp()->UndoSet(UNDOOP_DEL,idx);
			}
			m_mode=0;
		}
	}
	else
	if( m_mode==2 || m_mode==3 ) // pick mode
	{
		VIEW2CAM(mx,my);
		if(mx<CAMX1) mx=CAMX1;
		if(my<CAMY1) my=CAMY1;
		if(mx>CAMX2) mx=CAMX2;
		if(my>CAMY2) my=CAMY2;
		SNAP2GRID(mx,my); // grid snap

		//axes
		m_ax = mx;
		m_ay = my;

		m_brushidx = g_map.BrushPick(mx,my);
	
		if(m_mode==2 && !I9_GetKeyValue(I9_MOUSE_B2))
		{
			if( m_brushidx!=-1 ) g_gui->ScriptPrologDo( sprint("actions:toolPickMenu(%i)",m_brushidx) );
			m_mode=-1; // draw trick
		}
		else
		if(m_mode==3 && !I9_GetKeyValue(I9_MOUSE_B3) && !alt)
		{
			if( m_brushidx!=-1 ) brush = *g_map.m_brush[m_brushidx];
			m_mode=-1; // draw trick
		}
		else
		{
			// tooltip
			std::ostringstream o;
			o << (m_mode==2 ? "menu" : "pick");
			if(m_brushidx!=-1)
				o << "#" << m_brushidx;
			g_gui->ToolTip = o.str();
		}
	}

	m_isbusy = (m_mode==1 || m_mode==2 || m_mode==3);
}

void cEdiToolPaint::Draw()
{
	int mx = EdiApp()->GetMouseX() - VIEWX;
	int my = EdiApp()->GetMouseY() - VIEWY;
	if(!INVIEW(mx,my)) return; // && !m_isbusy

	tBrush& brush = EdiApp()->m_brush;
	tBrush brushtemp = brush;

	// axes
	g_map.DrawAxes(m_ax,m_ay);

	if(m_mode==0||m_mode==1)
	{
		g_gui->ScriptPrologDo("mod:brushToolDraw");	
		
		int x = CAMZ*(brush.m_data[BRUSH_X] - CAMX1) + VIEWX;
		int y = CAMZ*(brush.m_data[BRUSH_Y] - CAMY1) + VIEWY;
		
		g_paint.DrawBrushAt( &brush, x, y, (float)CAMZ, TRUE ); // animated
	}
	else
	if( (m_mode==2 || m_mode==3) && m_brushidx!=-1 )
	{
		tBrush * brushpick = g_map.m_brush[m_brushidx];
		int x = CAMZ*(brushpick->m_data[BRUSH_X] - CAMX1) + VIEWX;
		int y = CAMZ*(brushpick->m_data[BRUSH_Y] - CAMY1) + VIEWY;
		g_paint.DrawBrushFlashAt( brushpick, x, y, (float)CAMZ ); // not animated
	}

	brush = brushtemp;

}


void cEdiToolPaint::Command( int cmd )
{
	if(!g_map.validBrushIdx(m_brushidx)) return;
	
	if(cmd==TOOLCMD_PICKBRUSH)
	{
		EdiApp()->m_brush = * g_map.m_brush[m_brushidx];
	}
	else
	if(cmd==TOOLCMD_PICKCOLOR)
	{
		EdiApp()->m_brush.m_data[BRUSH_COLOR] = g_map.m_brush[m_brushidx]->m_data[BRUSH_COLOR];
	}
	else
	if(cmd==TOOLCMD_TOFRONT)
	{
		g_map.BrushToFront(g_map.m_brush[m_brushidx]);
		g_map.m_refresh = TRUE;
		EdiApp()->UndoReset();
	}
	else
	if(cmd==TOOLCMD_TOBACK)
	{
		g_map.BrushToBack(g_map.m_brush[m_brushidx]);
		g_map.m_refresh = TRUE;
		EdiApp()->UndoReset();
	}
	else
	if(cmd==TOOLCMD_DELETE)
	{
		tBrush * brush = g_map.m_brush[m_brushidx];
		EdiApp()->UndoSet(UNDOOP_ADD, m_brushidx, brush);
		g_map.PartitionDel(brush);
		g_map.BrushDel(m_brushidx);
		m_brushidx=-1;
		g_map.m_refresh = TRUE;
	}
}

void cEdiToolPaint::BeginUserUpdate()
{
	if( (m_mode==2 || m_mode==3) && m_brushidx!=-1 )
	{
		m_brushtemp = EdiApp()->m_brush;
		EdiApp()->m_brush = * g_map.m_brush[m_brushidx];
	}
}

void cEdiToolPaint::EndUserUpdate()
{
	if( (m_mode==2 || m_mode==3) && m_brushidx!=-1 )
	{
		EdiApp()->m_brush = m_brushtemp;
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolEdit
// mode: 0=none, 1=select, 2=move
//////////////////////////////////////////////////////////////////////////////////////////////////
cEdiToolEdit::cEdiToolEdit() : cEdiTool("EDIT"), m_selop()
{
}

void cEdiToolEdit::Done()
{
	m_drag.clear();
}

void cEdiToolEdit::Switch( BOOL on )
{
	Reset();
}

void cEdiToolEdit::Reset()
{
	if(m_mode==2)
	{
		m_drag.clear();
		g_map.m_hideselected = FALSE;
		g_map.m_refresh = TRUE;
		App.SetCursor(Cursor::Arrow);
	}
	m_mode = 0;
	m_selop = 0;
	m_rect = iRect(0,0,0,0);
}

void cEdiToolEdit::Update( float dtime )
{

	tBrush& brush = EdiApp()->m_brush;

	int mx = EdiApp()->GetMouseX() - VIEWX;
	int my = EdiApp()->GetMouseY() - VIEWY;
	BOOL inview = INVIEW(mx,my);

	VIEW2CAM(mx,my);
	if(mx<CAMX1) mx=CAMX1;
	if(my<CAMY1) my=CAMY1;
	if(mx>CAMX2) mx=CAMX2;
	if(my>CAMY2) my=CAMY2;
	SNAP2GRID(mx,my); // grid snap

	// additional keys
	BOOL shift	= (I9_GetKeyValue(I9K_LSHIFT)) || (I9_GetKeyValue(I9K_RSHIFT));
	BOOL alt	= (I9_GetKeyValue(I9K_LALT)) || (I9_GetKeyValue(I9K_RALT)) || (I9_GetKeyValue(I9_MOUSE_B3));
	BOOL ctrl	= (I9_GetKeyValue(I9K_LCONTROL)) || (I9_GetKeyValue(I9K_RCONTROL));
	
	m_selop = 0;
	if( shift )	m_selop++;
	if( alt )	m_selop--;

	if( m_mode==0 && inview )
	{
		if(I9_GetKeyDown(I9_MOUSE_B1))
		{
			if(m_selop==0) BrushDeselect();
			m_rect.p1 = m_rect.p2 = iV2(mx, my);
			m_mode=1;
		}
		else
		if(I9_GetKeyDown(I9_MOUSE_B2))
		{
			m_move = iV2(mx, my);
			m_moved = 0;
			BrushMoveStart();
			m_mode=2;
			App.SetCursor(Cursor::Hand);
		}
		else
		if(I9_GetKeyDown(I9K_DELETE))	BrushDeleteSelected();
		else
		if(ctrl && I9_GetKeyDown(I9K_C)) BrushCopy();
		else
		if(ctrl && I9_GetKeyDown(I9K_V)) { BrushPaste(); g_map.SelectionGoto(); }
		else
		if(ctrl && I9_GetKeyDown(I9K_X)) { BrushCopy(); BrushDeleteSelected(); }
	}	
	else
	if( m_mode==1 )
	{
		m_rect.p2.x = mx;
		m_rect.p2.y = my;
		m_rect.p2.x = std::max(m_rect.p2.x, m_rect.p1.x+1);
		m_rect.p2.y = std::max(m_rect.p2.y, m_rect.p1.y+1);
		if(!I9_GetKeyValue(I9_MOUSE_B1))
		{
			BrushSelect();
			m_mode=0;
		}
	}
	else
	if( m_mode==2 )
	{
		m_moved = iV2(mx, my) - m_move;

		if(!I9_GetKeyValue(I9_MOUSE_B2))
		{
			BrushMove();
			m_mode=0;
			App.SetCursor(Cursor::Arrow);
		}
	}

	// axes
	m_ax = mx;
	m_ay = my;

	// tooltip
	std::ostringstream o;
	if(m_mode!=2)
	{
		if( m_selop==-1 ) o << "sub";
		if( m_selop==1 ) o << "add";
		o << std::endl <<  mx << " " << my;
		if( m_mode==1 ) o << std::endl << m_rect.Width() << " x " << m_rect.Height();
	}
	else
		o << "mov " << m_moved.x << "," << m_moved.y << std::endl << mx << "," << my;
	if(inview)
		g_gui->ToolTip = o.str();
	else 
		g_gui->ToolTip.clear();

	m_isbusy = (m_mode!=0);
}

void cEdiToolEdit::Draw()
{
	int mx = EdiApp()->GetMouseX() - VIEWX;
	int my = EdiApp()->GetMouseY() - VIEWY;
	BOOL inview = INVIEW(mx,my);
	
	g_map.DrawAxes(m_ax,m_ay);

	// offsets
	iV2 d;
	if(m_mode==2)
		d = m_moved;

	// draw selected brushes ( from visible or from dragging )
	int count = (m_mode!=2) ? g_map.brushvis.size() : m_drag.size();
	for(int i=0;i<count;i++)
	{
		tBrush * brush = (m_mode!=2) ? g_map.brushvis[i] : m_drag[i];

		if(!brush->m_data[BRUSH_SELECT]) continue;
		int x = VIEWX + CAMZ * (brush->m_data[BRUSH_X]-CAMX1+d.x);
		int y = VIEWY + CAMZ * (brush->m_data[BRUSH_Y]-CAMY1+d.y);

		int shd = brush->m_data[BRUSH_SHADER];
		int col = brush->m_data[BRUSH_COLOR];
		brush->m_data[BRUSH_SHADER] = -1;
		brush->m_data[BRUSH_COLOR] = g_paint.GetFlashingColorBW();
		g_paint.DrawBrushAt( brush, x, y, (float)CAMZ );
		brush->m_data[BRUSH_SHADER] = shd;
		brush->m_data[BRUSH_COLOR] = col;
	}

	if( m_mode==1 )	
	{
		dword color = 0xffffffff;
		iRect rect = m_rect;
		CAM2VIEW(rect.p1.x,rect.p1.y);
		CAM2VIEW(rect.p2.x,rect.p2.y);
		rect.Offset(iV2(VIEWX, VIEWY));
		
		GUIDrawRectDot( rect,color);
	}

}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushSelect()
{
	for(auto brush: g_map.brushvis)
	{
		int layer = brush->m_data[BRUSH_LAYER];
		if(layer<0 || layer>=LAYER_MAX) continue;
		if(EdiApp()->LayerGet(layer)==0) continue; // hidden

		iRect bb = brush->rect();
		
		if( m_rect.Intersects(brush->rect()) )
		{
			if(m_selop==-1 && (brush->m_data[BRUSH_SELECT]!=0) )
			{
				brush->m_data[BRUSH_SELECT] = 0;
				g_map.m_selectcount--;
			}
			else
			if((m_selop==0 || m_selop==1) && (brush->m_data[BRUSH_SELECT]==0) )
			{
				brush->m_data[BRUSH_SELECT] = 1;
				g_map.m_selectcount++;
			}
		}
	}
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushDeselect()
{
	for(tBrush * b: g_map.m_brush) 
		b->m_data[BRUSH_SELECT] = 0;
	g_map.m_selectcount=0;
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushMoveStart()
{
	// create drag list with those visible and selected
	m_drag.clear();
	for(auto brush: g_map.brushvis)
		if(brush->m_data[BRUSH_SELECT]) 
			m_drag.push_back(brush);
	g_map.m_hideselected = TRUE;
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushMove()
{
	for(auto b: g_map.m_brush)
	{
		if(!b->m_data[BRUSH_SELECT]) continue;
		g_map.PartitionDel(b); // delete before changing
		b->m_data[BRUSH_X] += m_moved.x;
		b->m_data[BRUSH_Y] += m_moved.y;
		g_map.PartitionAdd(b); // readd after changed

	}
	m_drag.clear();
	g_map.m_hideselected = FALSE;
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushDeleteSelected()
{
	BEEP_OK();
	EdiApp()->UndoReset();
	for(int idx=0;idx<g_map.m_brush.size();idx++)
	{
		tBrush * brush = g_map.m_brush[idx];
		if(!brush->m_data[BRUSH_SELECT]) continue;
		g_map.PartitionDel(brush);
		g_map.BrushDel(idx); 
		idx--;
	}
	g_map.m_selectcount=0;
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushCopy()
{
	std::ostringstream o;
	for(tBrush * b: g_map.m_brush)
		if(b->m_data[BRUSH_SELECT])
		{
			PlTermv a(2);
			g_map.UnifyBrush(a[0], b);
			if(g_gui->ScriptPrologDo("brush", "getProps", a))
			{
				std::string props = static_cast<const char *>(a[1]);
				o << props << std::endl;
			}
		}
	std::string clip = o.str();
	if(clip.empty()) return;

	int size = clip.size()+1;
	UINT reg = RegisterClipboardFormat("DizzyAGE_Brushes");
	if(reg == NULL) return;
	if(!OpenClipboard(NULL)) return;
	if(EmptyClipboard())
	{
		HGLOBAL handler = GlobalAlloc(GMEM_MOVEABLE|GMEM_DDESHARE,size); assert(handler!=NULL);
		byte* data = (byte*)GlobalLock(handler); assert(data!=NULL);
		memcpy(data, clip.c_str(), size);
		GlobalUnlock(handler);
		SetClipboardData(reg,handler);
	}
	CloseClipboard();

	BEEP_OK();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushPaste()
{
	UINT reg = RegisterClipboardFormat("DizzyAGE_Brushes");
	if(!IsClipboardFormatAvailable(reg)) return;
	if(!OpenClipboard(NULL)) return;
	HGLOBAL handler = GetClipboardData(reg);
	if(handler)
	{
		char* data = (char*)GlobalLock(handler);
		if(data)
		{
			EdiApp()->UndoReset();
			BrushDeselect();
			std::istringstream s;
			s.str(std::string(data));
			int count = 0;
			while(s)
			{
				std::string props;
				std::getline(s, props);
				if(props.empty()) continue;
				++count;
				PlCompound a(props.c_str());
				if(g_gui->ScriptPrologDo("brush", "new", a))
				{
					tBrush * b = g_map.m_brush.back();
					b->m_data[BRUSH_LAYER] = EdiApp()->LayerActive();
					b->m_data[BRUSH_SELECT] = 1;
					g_map.PartitionAdd(b);	
				}
			}
			g_map.m_selectcount=count;
			g_map.m_refresh=TRUE;
			BEEP_OK();			
			GlobalUnlock(handler); 
		}
	}
	CloseClipboard();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

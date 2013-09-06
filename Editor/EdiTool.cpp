//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiTool.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "EdiTool.h"
#include "EdiApp.h"
#include "EdiMap.h"
#include "GUI.h"

#include <algorithm>

#include "eInput.h"


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
cEdiToolPaint::cEdiToolPaint() : cEdiTool("PAINT"), picked()
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

	bool alt = einput->alt();
	bool ctrl = einput->ctrl();

	picked = nullptr; // clear picked brush
	tBrush * brush = & EdiApp()->m_brush;

	if( m_mode==-1 ) m_mode=0; // draw trick

	if( m_mode==0 )
	{
		int bw = static_cast<int>(brush->mapWith()); // brush.m_data[BRUSH_MAP+2]-brush.m_data[BRUSH_MAP+0];
		int bh = static_cast<int>(brush->mapHeight()); // brush.m_data[BRUSH_MAP+3]-brush.m_data[BRUSH_MAP+1];
		VIEW2CAM(mx,my);
		if(mx<CAMX1+bw)	mx=CAMX1+bw;
		if(my<CAMY1+bh)	my=CAMY1+bh;
		if(mx>CAMX2)	mx=CAMX2;
		if(my>CAMY2)	my=CAMY2;
		mx = mx-bw;
		my = my-bh;
		SNAP2GRID(mx,my); // grid snap

		brush->pos = iV2(mx, my);
		brush->size = iV2(bw, bh);

		if(einput->mouseValue(0)) m_mode=1;
		else
		if(einput->mouseValue(1)) m_mode=2;
		else
		if(einput->mouseValue(2) || alt) m_mode=3;
		else
//		if(ctrl && I9_GetKeyDown(I9K_Z)) EdiApp()->Undo();

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

		int bw = mx - brush->pos.x;
		int bh = my - brush->pos.y;
		if(bw<0) bw=0;
		if(bh<0) bh=0;
		
		if(einput->keyValue(DIK_LSHIFT))
		{
			int mw = static_cast<int>(brush->mapWith()); // brush.m_data[BRUSH_MAP+2]-brush.m_data[BRUSH_MAP+0];
			int mh = static_cast<int>(brush->mapHeight()); // brush.m_data[BRUSH_MAP+3]-brush.m_data[BRUSH_MAP+1];
			if(mw<1) bw=0; else	bw = (bw / mw) * mw;
			if(mh<1) bh=0; else	bh = (bh / mh) * mh;
		}
		else
			SNAP2GRID(bw,bh); // grid snap

		brush->size = iV2(bw, bh);

		// axes
		m_ax = brush->pos.x + bw;
		m_ay = brush->pos.y + bh;

		if(!einput->mouseValue(0)) 
		{
			// add brush !
			if( brush->size.x>0 && brush->size.y>0 && inview)
			{
				PlTermv a(2);
				g_map.UnifyBrush(a[0], brush);
				if(g_gui->ScriptPrologDo("brush", "clone", a))
				{
					tBrush * b = g_map.brushPtr(a[1]);
					b->select = false;
					b->layer = EdiApp()->LayerActive();
					g_map.m_refresh = TRUE;
					g_map.PartitionAdd(b);
				}
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

		picked = g_map.BrushPick(mx,my);
	
		if(m_mode==2 && !einput->mouseValue(1))
		{
			if(picked)
			{
				PlTermv t(1);
				g_map.UnifyBrush(t[0], picked);
				g_gui->ScriptPrologDo("actions", "toolPickMenu", t);
			}
			m_mode=-1; // draw trick
		}
		else
		if(m_mode==3 && !einput->mouseValue(2) && !alt)
		{
			if(picked)
			{
				PlTermv t(2);
				g_map.UnifyBrush(t[0], brush);
				g_map.UnifyBrush(t[1], picked);
				g_gui->ScriptPrologDo("brush", "assign", t);				
				brush = picked;
			}
			m_mode=-1; // draw trick
		}
		else
		{
			// tooltip
			std::ostringstream o;
			o << (m_mode==2 ? "menu" : "pick");
			if(picked)
				o << "#" <<picked;
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

	tBrush * brush = & EdiApp()->m_brush;

	// axes
	g_map.DrawAxes(m_ax,m_ay);

	if(m_mode==0||m_mode==1)
	{
		tBrush tmp = *brush;
		PlTermv br(1);
		g_map.UnifyBrush(br[0], &tmp);
		g_gui->ScriptPrologDo("mod", "brushToolDraw", br);	
		
		int x = CAMZ*(brush->pos.x - CAMX1) + VIEWX;
		int y = CAMZ*(brush->pos.y - CAMY1) + VIEWY;
		
		g_paint.DrawBrushAt( &tmp, x, y, (float)CAMZ, TRUE ); // animated
	}
	else
	if( (m_mode==2 || m_mode==3) && picked )
	{
		int x = CAMZ*(picked->pos.x - CAMX1) + VIEWX;
		int y = CAMZ*(picked->pos.y - CAMY1) + VIEWY;
		g_paint.DrawBrushFlashAt( picked, x, y, (float)CAMZ ); // not animated
	}
}


void cEdiToolPaint::Command( int cmd )
{
	if(!picked) return;
	
	if(cmd==TOOLCMD_PICKBRUSH)
	{
		PlTermv t(2);
		g_map.UnifyBrush(t[0], & EdiApp()->m_brush);
		g_map.UnifyBrush(t[1], picked);
		g_gui->ScriptPrologDo("brush", "assign", t);
	}
	else
	if(cmd==TOOLCMD_PICKCOLOR)
	{
		EdiApp()->m_brush.color = picked->color;
	}
	else
	if(cmd==TOOLCMD_TOFRONT)
	{
		g_map.BrushToFront(picked);
		g_map.m_refresh = TRUE;
	}
	else
	if(cmd==TOOLCMD_TOBACK)
	{
		g_map.BrushToBack(picked);
		g_map.m_refresh = TRUE;
	}
	else
	if(cmd==TOOLCMD_DELETE)
	{
		g_map.PartitionDel(picked);
		g_map.BrushDel(picked);
		picked = nullptr;
		g_map.m_refresh = TRUE;
	}
}

void cEdiToolPaint::UserUpdate()
{
	PlTermv br(1);
	g_map.UnifyBrush(br[0], (m_mode==2 || m_mode==3) && picked ? picked : & EdiApp()->m_brush);
	g_gui->ScriptPrologDo("mod", "userUpdatePaint", br);
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
	bool shift	= einput->shift();;
	bool alt	= einput->alt() || einput->mouseValue(2);
	bool ctrl	= einput->ctrl();
	
	m_selop = 0;
	if( shift )	m_selop++;
	if( alt )	m_selop--;

	if( m_mode==0 && inview )
	{
		if(einput->isMouseDown(0))
		{
			if(m_selop==0) BrushDeselect();
			m_rect.p1 = m_rect.p2 = iV2(mx, my);
			m_mode=1;
		}
		else
		if(einput->isMouseDown(1))
		{
			m_move = iV2(mx, my);
			m_moved = 0;
			BrushMoveStart();
			m_mode=2;
			App.SetCursor(Cursor::Hand);
		}
		else
		if(einput->isKeyDown(DIK_DELETE))	BrushDeleteSelected();
		else
		if(ctrl && einput->isKeyDown(DIK_C)) BrushCopy();
		else
		if(ctrl && einput->isKeyDown(DIK_V)) { BrushPaste(); g_map.SelectionGoto(); }
		else
		if(ctrl && einput->isKeyDown(DIK_X)) { BrushCopy(); BrushDeleteSelected(); }
	}	
	else
	if( m_mode==1 )
	{
		m_rect.p2.x = mx;
		m_rect.p2.y = my;
		m_rect.p2.x = std::max(m_rect.p2.x, m_rect.p1.x+1);
		m_rect.p2.y = std::max(m_rect.p2.y, m_rect.p1.y+1);
		if(!einput->mouseValue(0))
		{
			BrushSelect();
			m_mode=0;
		}
	}
	else
	if( m_mode==2 )
	{
		m_moved = iV2(mx, my) - m_move;

		if(!einput->mouseValue(1))
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

		if(!brush->select) continue;
		int x = VIEWX + CAMZ * (brush->pos.x-CAMX1+d.x);
		int y = VIEWY + CAMZ * (brush->pos.y-CAMY1+d.y);

		Blend shd = brush->shader;
		int col = brush->color;
		brush->shader = Blend::AlphaRep;
		brush->color = g_paint.GetFlashingColorBW();
		g_paint.DrawBrushAt( brush, x, y, (float)CAMZ );
		brush->shader = shd;
		brush->color = col;
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

void cEdiToolEdit::UserUpdate()
{
	g_gui->ScriptPrologDo("mod", "userUpdateEdit");
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushSelect()
{
	for(auto brush: g_map.brushvis)
	{
		int layer = brush->layer;
		if(layer<0 || layer>=LAYER_MAX) continue;
		if(EdiApp()->LayerGet(layer)==0) continue; // hidden

		iRect bb = brush->rect();
		
		if( m_rect.Intersects(brush->rect()) )
		{
			if(m_selop==-1 && brush->select)
			{
				brush->select = false;
				g_map.m_selectcount--;
			}
			else
			if((m_selop==0 || m_selop==1) && !brush->select)
			{
				brush->select = true;
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
		b->select = false;
	g_map.m_selectcount=0;
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushMoveStart()
{
	// create drag list with those visible and selected
	m_drag.clear();
	for(auto brush: g_map.brushvis)
		if(brush->select) 
			m_drag.push_back(brush);
	g_map.m_hideselected = TRUE;
	g_map.m_refresh=TRUE;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushMove()
{
	for(auto b: g_map.m_brush)
	{
		if(!b->select) continue;
		g_map.PartitionDel(b); // delete before changing
		b->pos += m_moved;
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
	for(int idx=0;idx<g_map.m_brush.size();idx++)
	{
		tBrush * brush = g_map.m_brush[idx];
		if(!brush->select) continue;
		g_map.PartitionDel(brush);
		g_map.BrushDel(brush); 
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
		if(b->select)
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
					b->layer = EdiApp()->LayerActive();
					b->select = true;
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

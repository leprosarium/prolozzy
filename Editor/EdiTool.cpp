#include "stdafx.h"
#include "EdiTool.h"
#include "EdiApp.h"
#include "EdiMap.h"
#include "GUI.h"

#include <algorithm>

#include "eInput.h"

inline int Snap(int x, int grid)
{
	int xx = x / grid;
	if (x % grid >= grid / 2)
		++xx;
	return xx * grid;
}

inline void Snap(iV2 & p)
{
	if (Editor::app->m_snap)
		p = iV2(Snap(p.x, Editor::app->m_gridsize), Snap(p.y, Editor::app->m_gridsize));
}


cEdiTool::cEdiTool(const std::string & name) : name(name)
{
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolPaint
//////////////////////////////////////////////////////////////////////////////////////////////////
cEdiToolPaint::cEdiToolPaint() : cEdiTool("paint"), picked(), mode(Mode::Normal)
{

}

void cEdiToolPaint::Reset()
{
	if (mode == Mode::PickMenu)	return;
	mode = Mode::None; // draw trick
}

void cEdiToolPaint::Update( float dtime )
{
	iV2 m = Editor::app->GetMousePos() - iV2(VIEWX, VIEWY);
	BOOL inview = INVIEW(m.x,m.y);
	if(!inview && !IsBusy()) return;

	bool alt = einput->alt();
	bool ctrl = einput->ctrl();

	picked = nullptr; // clear picked brush
	tBrush * brush = & Editor::app->m_brush;

	if( mode == Mode::None ) mode = Mode::Normal; // draw trick

	if( mode == Mode::Normal )
	{
		int bw = static_cast<int>(brush->mapWith()); // brush.m_data[BRUSH_MAP+2]-brush.m_data[BRUSH_MAP+0];
		int bh = static_cast<int>(brush->mapHeight()); // brush.m_data[BRUSH_MAP+3]-brush.m_data[BRUSH_MAP+1];
		VIEW2CAM(m.x,m.y);
		if(m.x<CAMX1+bw)	m.x=CAMX1+bw;
		if(m.y<CAMY1+bh)	m.y=CAMY1+bh;
		if(m.x>CAMX2)	m.x=CAMX2;
		if(m.y>CAMY2)	m.y=CAMY2;
		iV2 sz = iV2(bw, bh);
		m -= sz;
		Snap(m);

		brush->pos = axe = m;
		brush->size = sz;

		if(einput->mouseValue(0)) mode = Mode::Paint;
		else if(einput->mouseValue(1)) mode = Mode::PickMenu;
		else if(einput->mouseValue(2) || alt) mode = Mode::Pick;

	}
	else
	if( mode == Mode::Paint)
	{
		VIEW2CAM(m.x,m.y);
		if(m.x>CAMX2) m.x=CAMX2;
		if(m.y>CAMY2) m.y=CAMY2;

		iV2 sz = m - brush->pos;
		if(sz.x<0) sz.x=0;
		if(sz.y<0) sz.y=0;
		
		if(einput->keyValue(DIK_LSHIFT))
		{
			int mw = static_cast<int>(brush->mapWith()); // brush.m_data[BRUSH_MAP+2]-brush.m_data[BRUSH_MAP+0];
			int mh = static_cast<int>(brush->mapHeight()); // brush.m_data[BRUSH_MAP+3]-brush.m_data[BRUSH_MAP+1];
			if(mw<1) sz.x=0; else	sz.x = (sz.x / mw) * mw;
			if(mh<1) sz.y=0; else	sz.y = (sz.y / mh) * mh;
		}
		else
			Snap(sz); // grid snap

		brush->size = sz;

		axe = brush->pos + brush->size;

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
					g_map.m_refresh = TRUE;
					g_map.PartitionAdd(b);
				}
			}
			mode = Mode::Normal;
		}
	}
	else
	if(mode == Mode::PickMenu || mode == Mode::Pick) // pick mode
	{
		VIEW2CAM(m.x,m.y);
		if(m.x<CAMX1) m.x=CAMX1;
		if(m.y<CAMY1) m.y=CAMY1;
		if(m.x>CAMX2) m.x=CAMX2;
		if(m.y>CAMY2) m.y=CAMY2;
		Snap(m);

		axe = m;

		picked = g_map.BrushPick(m);
	
		if(mode == Mode::PickMenu && !einput->mouseValue(1))
		{
			if(picked)
			{
				PlTermv t(1);
				g_map.UnifyBrush(t[0], picked);
				g_gui->ScriptPrologDo("actions", "toolPickMenu", t);
			}
			mode = Mode::None; // draw trick
		}
		else
		if(mode == Mode::Pick && !einput->mouseValue(2) && !alt)
		{
			if(picked)
			{
				PlTermv t(2);
				g_map.UnifyBrush(t[0], brush);
				g_map.UnifyBrush(t[1], picked);
				g_gui->ScriptPrologDo("brush", "assign", t);				
				brush = picked;
			}
			mode = Mode::None; // draw trick
		}
		else
		{
			// tooltip
			std::ostringstream o;
			o << (mode == Mode::PickMenu ? "menu" : "pick");
			if(picked)
				o << "#" <<picked;
			g_gui->ToolTip = o.str();
		}
	}
}

void cEdiToolPaint::Draw() const
{
	iV2 m = Editor::app->GetMousePos() - iV2(VIEWX, VIEWY);
	if(!INVIEW(m.x,m.y)) return; // && !m_isbusy

	tBrush * brush = & Editor::app->m_brush;

	g_map.DrawAxes(axe.x, axe.y);

	if(mode == Mode::Normal || mode == Mode::Paint)
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
	if( (mode == Mode::PickMenu || mode == Mode::Pick) && picked )
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
		g_map.UnifyBrush(t[0], & Editor::app->m_brush);
		g_map.UnifyBrush(t[1], picked);
		g_gui->ScriptPrologDo("brush", "assign", t);
	}
	else
	if(cmd==TOOLCMD_PICKCOLOR)
	{
		Editor::app->m_brush.color = picked->color;
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
	g_map.UnifyBrush(br[0], (mode == Mode::PickMenu || mode == Mode::Pick) && picked ? picked : & Editor::app->m_brush);
	g_gui->ScriptPrologDo("mod", "userUpdatePaint", br);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolEdit
//////////////////////////////////////////////////////////////////////////////////////////////////
cEdiToolEdit::cEdiToolEdit() : cEdiTool("edit"), selop(SelOp::New), mode(Mode::Normal)
{
}

void cEdiToolEdit::Reset()
{
	if(mode == Mode::Move)
	{
		drag.clear();
		g_map.m_hideselected = FALSE;
		g_map.m_refresh = TRUE;
		Editor::app->SetCursor(App::Cursor::Arrow);
	}
	mode = Mode::Normal;
	selop = SelOp::New;
	rect = iRect();
}

void cEdiToolEdit::Update( float dtime )
{

	tBrush& brush = Editor::app->m_brush;

	iV2 m = Editor::app->GetMousePos() - iV2(VIEWX, VIEWY);
	BOOL inview = INVIEW(m.x,m.y);

	VIEW2CAM(m.x,m.y);
	if(m.x<CAMX1) m.x=CAMX1;
	if(m.y<CAMY1) m.y=CAMY1;
	if(m.x>CAMX2) m.x=CAMX2;
	if(m.y>CAMY2) m.y=CAMY2;
	Snap(m); // grid snap

	// additional keys
	bool shift	= einput->shift();;
	bool alt	= einput->alt() || einput->mouseValue(2);
	bool ctrl	= einput->ctrl();
	
	selop = shift ?
		(alt ? SelOp::New : SelOp::Add) :
		(alt ? SelOp::Sub : SelOp::New);

	if( mode == Mode::Normal && inview )
	{
		if(einput->isMouseDown(0))
		{
			if(selop == SelOp::New) BrushDeselect();
			rect.p1 = rect.p2 = m;
			mode = Mode::Select;
		}
		else
		if(einput->isMouseDown(1))
		{
			move = m;
			moved = 0;
			BrushMoveStart();
			mode = Mode::Move;
			Editor::app->SetCursor(App::Cursor::Hand);
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
	if( mode == Mode::Select)
	{
		rect.p2 = m;
		rect.p2.x = std::max(rect.p2.x, rect.p1.x+1);
		rect.p2.y = std::max(rect.p2.y, rect.p1.y+1);
		if(!einput->mouseValue(0))
		{
			BrushSelect();
			mode = Mode::Normal;
		}
	}
	else
	if(mode == Mode::Move)
	{
		moved = m - move;

		if(!einput->mouseValue(1))
		{
			BrushMove();
			mode = Mode::Normal;
			Editor::app->SetCursor(App::Cursor::Arrow);
		}
	}

	axe = m;

	// tooltip
	std::ostringstream o;
	if(mode != Mode::Move)
	{
		if(selop == SelOp::Sub) o << "sub";
		if(selop == SelOp::Add) o << "add";
		o << std::endl <<  m.x << " " << m.y;
		if(mode == Mode::Select) o << std::endl << rect.Width() << " x " << rect.Height();
	}
	else
		o << "mov " << moved.x << "," << moved.y << std::endl << m.x << "," << m.y;
	if(inview)
		g_gui->ToolTip = o.str();
	else 
		g_gui->ToolTip.clear();
}

void cEdiToolEdit::Draw() const
{
	iV2 m = Editor::app->GetMousePos() - iV2(VIEWX, VIEWY);
	BOOL inview = INVIEW(m.x,m.y);
	
	g_map.DrawAxes(axe.x, axe.y);

	// offsets
	iV2 d;
	if(mode == Mode::Move)
		d = moved;

	// draw selected brushes ( from visible or from dragging )
	int count = (mode != Mode::Move) ? g_map.brushvis.size() : drag.size();
	for(int i=0;i<count;i++)
	{
		tBrush * brush = (mode != Mode::Move) ? g_map.brushvis[i] : drag[i];

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

	if(mode == Mode::Select)	
	{
		dword color = 0xffffffff;
		iRect r = rect;
		CAM2VIEW(r.p1.x,r.p1.y);
		CAM2VIEW(r.p2.x,r.p2.y);
		r.Offset(iV2(VIEWX, VIEWY));
		
		GUIDrawRectDot(r, color);
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
		if (!rect.Intersects(brush->rect())) continue;
		if(selop == SelOp::Sub && brush->select)
		{
			brush->select = false;
			g_map.m_selectcount--;
		}
		else if((selop == SelOp::New || selop == SelOp::Add) && !brush->select)
		{
			brush->select = true;
			g_map.m_selectcount++;
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
	drag.clear();
	for(auto brush: g_map.brushvis)
		if(brush->select) 
			drag.push_back(brush);
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
		b->pos += moved;
		g_map.PartitionAdd(b); // readd after changed

	}
	drag.clear();
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
				if(g_gui->ScriptPrologDo("brush", "paste", a))
				{
					tBrush * b = g_map.m_brush.back();
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

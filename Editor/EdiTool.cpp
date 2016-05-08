#include "stdafx.h"
#include "EdiTool.h"
#include "EdiApp.h"
#include "EdiMap.h"
#include "GUI.h"
#include "PlBrush.h"

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
	iV2 m = Editor::app->GetMousePos();
	bool inview = g_map.view.IsInside(m);
	if(!inview && !IsBusy()) return;

	bool alt = einput->alt();
	bool ctrl = einput->ctrl();

	picked = nullptr; // clear picked brush
	Brush * brush = & Editor::app->m_brush;

	if( mode == Mode::None ) mode = Mode::Normal; // draw trick

	if( mode == Mode::Normal )
	{
		iV2 bsz(brush->mapSize());
		m = g_map.view2cam(m);
		iRect cm = g_map.camRect();
		cm.p1 += bsz;
		m.Clip(cm);
		m -= bsz;
		Snap(m);

		brush->pos = axe = m;
		brush->size = bsz;

		if(einput->mouseValue(0)) mode = Mode::Paint;
		else if(einput->mouseValue(1)) mode = Mode::PickMenu;
		else if(einput->mouseValue(2) || alt) mode = Mode::Pick;

	}
	else
	if( mode == Mode::Paint)
	{
		m = g_map.view2cam(m);
		m.Clip(g_map.camRect());

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
				(PlBrush(brush)) = a[0];
				if(g_gui->ScriptPrologDo("brush", "clone", a))
				{
					Brush * b = PlBrush(a[1]);
					b->select = false;
					g_map.Refresh();
					g_map.partitions.Add(b);
				}
			}
			mode = Mode::Normal;
		}
	}
	else
	if(mode == Mode::PickMenu || mode == Mode::Pick) // pick mode
	{
		m = g_map.view2cam(m);
		m.Clip(g_map.camRect());
		Snap(m);

		axe = m;

		picked = g_map.brushes.Pick(m);
	
		if(mode == Mode::PickMenu && !einput->mouseValue(1))
		{
			if(picked)
			{
				PlTermv t(1);
				(PlBrush(picked)) = t[0];
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
				(PlBrush(brush)) = t[0];
				(PlBrush(picked)) = t[1];
				g_gui->ScriptPrologDo("brush", "assign", t);				
				brush = picked;
			}
			mode = Mode::None; // draw trick
		}
		else
		{
			// tooltip
			std::wostringstream o;
			o << (mode == Mode::PickMenu ? L"menu" : L"pick");
			if(picked)
				o << L"#" <<picked;
			g_gui->ToolTip = o.str();
		}
	}
}

void cEdiToolPaint::Draw() const
{
	if (!g_map.view.IsInside(Editor::app->GetMousePos())) return; // && !m_isbusy

	Brush * brush = & Editor::app->m_brush;

	g_map.DrawAxes(axe.x, axe.y);

	if(mode == Mode::Normal || mode == Mode::Paint)
	{
		Brush tmp = *brush;
		PlTermv br(1);
		(PlBrush (&tmp)) = br[0];
		g_gui->ScriptPrologDo("mod", "brushToolDraw", br);	
		
		iV2 p = g_map.cam2view(tmp.pos);
		g_paint.DrawBrushAt(&tmp, p.x, p.y, static_cast<float>(g_map.camScale), TRUE); // animated
	}
	else
	if( (mode == Mode::PickMenu || mode == Mode::Pick) && picked )
	{
		iV2 p = g_map.cam2view(picked->pos);
		g_paint.DrawBrushFlashAt(picked, p.x, p.y, static_cast<float>(g_map.camScale)); // not animated
	}
}


void cEdiToolPaint::Command( int cmd )
{
	if(!picked) return;
	
	if(cmd==TOOLCMD_PICKBRUSH)
	{
		PlTermv t(2);
		PlBrush(&Editor::app->m_brush) = t[0];
		(PlBrush(picked)) = t[1];
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
		g_map.brushes.ToFront(picked);

	}
	else
	if(cmd==TOOLCMD_TOBACK)
	{
		g_map.brushes.ToBack(picked);

	}
	else
	if(cmd==TOOLCMD_DELETE)
	{
		g_map.partitions.Del(picked);
		g_map.brushes.Del(picked);
		picked = nullptr;
		g_map.Refresh();
	}
}

void cEdiToolPaint::UserUpdate()
{
	PlTermv br(1);
	PlBrush((mode == Mode::PickMenu || mode == Mode::Pick) && picked ? picked : &Editor::app->m_brush) = br[0];
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
		g_map.m_hideselected = false;
		g_map.Refresh();
		Editor::app->SetCursor(App::Cursor::Arrow);
	}
	mode = Mode::Normal;
	selop = SelOp::New;
	rect = iRect();
}

void cEdiToolEdit::Update( float dtime )
{

	Brush& brush = Editor::app->m_brush;

	iV2 m = Editor::app->GetMousePos();
	bool inview = g_map.view.IsInside(m);

	m = g_map.view2cam(m);
	m.Clip(g_map.camRect());
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
		if (einput->isMouseDown(0))
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
		if (ctrl && einput->isKeyDown(DIK_V)) { BrushPaste(); g_map.brushes.SelectionGoto(1); }
		else
		if(ctrl && einput->isKeyDown(DIK_X)) { BrushCopy(); BrushDeleteSelected(); }
	}	
	else
	if( mode == Mode::Select)
	{
		rect.p2 = m;
		rect.p2.x = std::max(rect.p2.x, rect.p1.x+1);
		rect.p2.y = std::max(rect.p2.y, rect.p1.y+1);
		if (!einput->mouseValue(0))
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
	std::wostringstream o;
	if(mode != Mode::Move)
	{
		if(selop == SelOp::Sub) o << L"sub";
		if(selop == SelOp::Add) o << L"add";
		o << std::endl <<  m.x << L" " << m.y;
		if(mode == Mode::Select) o << std::endl << rect.Width() << L" x " << rect.Height();
	}
	else
		o << L"mov " << moved.x << L"," << moved.y << std::endl << m.x << L"," << m.y;
	if(inview)
		g_gui->ToolTip = o.str();
	else 
		g_gui->ToolTip.clear();
}

void cEdiToolEdit::Draw() const
{
	g_map.DrawAxes(axe.x, axe.y);

	// offsets
	iV2 d;
	if(mode == Mode::Move)
		d = moved;

	// draw selected brushes ( from visible or from dragging )
	int count = (mode != Mode::Move) ? g_map.brushvis.size() : drag.size();
	for(int i=0;i<count;i++)
	{
		Brush * brush = (mode != Mode::Move) ? g_map.brushvis[i] : drag[i];

		if(!brush->select) continue;
		iV2 p = g_map.cam2view(brush->pos + d);

		Blend shd = brush->shader;
		int col = brush->color;
		brush->shader = Blend::AlphaRep;
		brush->color = g_paint.GetFlashingColorBW();
		g_paint.DrawBrushAt(brush, p.x, p.y, static_cast<float>(g_map.camScale));
		brush->shader = shd;
		brush->color = col;
	}

	if(mode == Mode::Select)	
		GUIDrawRectDot(iRect(g_map.cam2view(rect.p1), g_map.cam2view(rect.p2)), 0xffffffff);
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
			g_map.brushes.SelectCount--;
		}
		else if((selop == SelOp::New || selop == SelOp::Add) && !brush->select)
		{
			brush->select = true;
			g_map.brushes.SelectCount++;
		}
	}
	g_map.Refresh();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushDeselect()
{
	for (Brush * b : g_map.brushes)
		b->select = false;
	g_map.brushes.SelectCount = 0;
	g_map.Refresh();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushMoveStart()
{
	// create drag list with those visible and selected
	drag.clear();
	for(auto brush: g_map.brushvis)
		if(brush->select) 
			drag.push_back(brush);
	g_map.m_hideselected = true;
	g_map.Refresh();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushMove()
{
	for (auto b : g_map.brushes)
	{
		if(!b->select) continue;
		g_map.partitions.Del(b); // delete before changing
		b->pos += moved;
		g_map.partitions.Add(b); // readd after changed

	}
	drag.clear();
	g_map.m_hideselected = false;
	g_map.Refresh();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushDeleteSelected()
{
	BEEP_OK();
	BrushList selected;
	std::copy_if(g_map.brushes.begin(), g_map.brushes.end(), std::back_inserter(selected), [](Brush * b) { return b->select; });
	for (auto brush : selected)
	{
		g_map.partitions.Del(brush);
		g_map.brushes.Del(brush);
	}
	g_map.brushes.SelectCount = 0;
	g_map.Refresh();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void cEdiToolEdit::BrushCopy()
{
	std::ostringstream o;
	for (Brush * b : g_map.brushes)
		if(b->select)
		{
			PlTermv a(2);
			(PlBrush(b)) = a[0];
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
					Brush * b = g_map.brushes.back();
					b->select = true;
					g_map.partitions.Add(b);	
				}
			}
			g_map.brushes.SelectCount = count;
			g_map.Refresh();
			BEEP_OK();			
			GlobalUnlock(handler); 
		}
	}
	CloseClipboard();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIItem.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIItem.h"
#include "GUI.h"
#include "E9String.h"
#include "eInput.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIItem
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIItem::cGUIItem(cGUIDlg *d) : id(), style(), hidden(), disable(), txtAlign(), 
	txtColor(GUICOLOR_TEXT), 
	txtOffset(6), 
	img0(-1), img1(-1), 
	imgColor(0xffffffff), 
	imgAlign(GUIALIGN_CENTERXY),
	mode(),
	value(),
	group(),
	m_dlg(d)
{
	color[0] = GUICOLOR_GUI;
	color[1] = color[2] = color[3] = 0;
}

cGUIItem::~cGUIItem()
{
}

void cGUIItem::Update()
{
	if(disable) return;
	m_mousein = scrRect().IsInside(g_gui->m_mouse);
	OnUpdate();

	if( m_mousein && !hidden && !tooltip.empty())
		g_gui->ToolTip = tooltip;
}


void cGUIItem::OnUpdate()
{
	if(m_mousein)
	{
		if(einput->isMouseDown(0))	Action(1);
		if(einput->isMouseDown(1))	Action(2);
	}
}

void cGUIItem::OnDraw()
{
	iRect rc = scrRect();

	// background
	if( style & GUISTYLE_BACKGR )
		GUIDrawBar( rc, color[0]);
	else
	if( style & GUISTYLE_GRADIENT )
		GUIDrawGradient( rc, color[0], color[1]);
	
	// image
	GUIDrawImg( rc, img0, imgColor, imgAlign);

	// text
	GUIDrawText( rc, txt, txtColor, txtAlign, txtOffset);
	
	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect(rc, color[2]);
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D(rc, color[2], style & GUISTYLE_PRESSED );

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Utils
//////////////////////////////////////////////////////////////////////////////////////////////////

iRect cGUIItem::scrRect() const
{
	return m_dlg ? iRect(rect).Offset(m_dlg->rect.p1) : rect;
}


void cGUIItem::Capture( bool on )
{
	if(on)
	{
		einput->keyQueue.clear();
		if(!g_gui->m_capture) 
			g_gui->m_capture = this;	
	}
	else
	{
		if(g_gui->m_capture == this) 
			g_gui->m_capture = nullptr;
	}
}

bool cGUIItem::IsCaptured() const 
{
	return g_gui->m_capture == this; 
}

void cGUIItem::Select()	
{
	g_gui->SetLast(m_dlg, this);
}

void cGUIItem::Action(int param)
{
	OnAction();
	if(cmdAction.empty()) return;
	Select();

	std::ostringstream o;	
	o << "gui:itemAction(" << param << ", (" << cmdAction << "))";
	g_gui->ScriptPrologDo(o.str());
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUITitle
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUITitle::cGUITitle(cGUIDlg *d) : cGUIItem(d)
{
	value = 1;
}

void cGUITitle::OnUpdate()
{
	iRect rc = scrRect();

	if(einput->isMouseDown(0))
	{
		if(m_mousein)
		{
			move = g_gui->m_mouse - rc.p1;
			Capture(true);
		}
	}
	else
	if(!einput->mouseValue(0))
		if(IsCaptured()) 
			Capture(false);

	if(IsCaptured() && m_dlg) // move parent dialog
	{
		iV2 p = g_gui->m_mouse;
		p = iV2(std::min(std::max(p.x, 0), R9_GetWidth()-1), std::min(std::max(p.y, 0), R9_GetHeight()-1)) - move - rect.p1;
		m_dlg->rect = iRect(p, p + m_dlg->rect.Size());
	}
	
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIItem.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIItem.h"
#include "GUI.h"
#include "E9String.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIItem
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIItem::cGUIItem() : id(), style(), hidden(), disable(), txtAlign(), 
	txtColor(GUICOLOR_TEXT), 
	txtOffset(6), 
	img0(-1), img1(-1), 
	imgColor(0xffffffff), 
	imgAlign(GUIALIGN_CENTERXY),
	mode(),
	value(),
	group(),
	m_dlg()
{
	color[0] = GUICOLOR_GUI;
	color[1] = color[2] = color[3] = 0;
}

cGUIItem::~cGUIItem()
{
}

void cGUIItem::Update()
{
	m_mousein = scrRect().IsInside(g_gui->m_mouse);
	OnUpdate();
}


void cGUIItem::OnUpdate()
{
	if(m_mousein)
	{
		if(I9_GetKeyDown(I9_MOUSE_B1))	Action(1);
		if(I9_GetKeyDown(I9_MOUSE_B2))	Action(2);
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
	GUIDrawText( rc, txt.c_str(), txtColor, txtAlign, txtOffset);
	
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

int cGUIItem::SetParent( cGUIDlg* dlg)
{
	return dlg ? dlg->ItemAdd(this) : -1;
}

void cGUIItem::Capture( bool on )
{
	if(on)
	{
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
	g_gui->m_lastdlg = m_dlg;
	g_gui->m_lastitem = m_dlg->ItemFind(this);
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
cGUITitle::cGUITitle()
{
	value = 1;
}

void cGUITitle::OnUpdate()
{
	iRect rc = scrRect();

	if(I9_GetKeyDown(I9_MOUSE_B1))
	{
		if(m_mousein)
		{
			move = g_gui->m_mouse - rc.p1;
			Capture(true);
		}
	}
	else
	if(!I9_GetKeyValue(I9_MOUSE_B1))
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

//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIColor.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIColor.h"
#include "GUI.h"
#include "E9App.h"
#include "R9ImgLoader.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIColorPick
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIColorPick::cGUIColorPick()
{
}

cGUIColorPick::~cGUIColorPick()
{
}

void cGUIColorPick::Update()
{
	iRect rc = scrRect();
	m_mousein = rc.IsInside(g_gui->m_mouse);
	
	if(!m_mousein || !m_img.isValid())
		return;
	int mx = g_gui->m_mouse.x - rc.p1.x;
	int my = g_gui->m_mouse.y - rc.p1.y;
	float x = ((float)mx / rc.Width()) * m_img.m_width;
	float y = ((float)my / rc.Height()) * m_img.m_height;
	color[0] = m_img.getColor((int)x,(int)y);

	if(I9_GetKeyUp(I9_MOUSE_B1))
	{
		cmdActionParam = 1;
		Action();
	}
	else
	if(I9_GetKeyUp(I9_MOUSE_B2))
	{
		cmdActionParam = 2;
		Action();
	}
	else
	{
		std::ostringstream o;
		o << std::hex << std::uppercase << std::setfill('0') << std::setw(6) << (color[0] & 0x00ffffff);
		g_gui->ToolTip = o.str();
	}
}

void cGUIColorPick::Draw()
{
	return; // don't draw anything
}

void cGUIColorPick::LoadImg(const std::string & nm )
{
	txt = nm;
	R9_ImgDestroy(&m_img);
	R9_ImgLoadFile(nm, &m_img);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

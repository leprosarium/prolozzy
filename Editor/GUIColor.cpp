#include "stdafx.h"
#include "GUIColor.h"
#include "GUI.h"
#include "R9ImgLoader.h"

void cGUIColorPick::OnUpdate()
{
	if(!m_mousein || !m_img.isValid())
		return;

	iRect rc = scrRect();
	iV2 p = (g_gui->m_mouse - rc.p1) * iV2(m_img.m_width, m_img.m_height) / rc.Size();
	color[0] = m_img.getColor(p.x, p.y);

	if(I9_GetKeyUp(I9_MOUSE_B1))
		Action(1);
	else
	if(I9_GetKeyUp(I9_MOUSE_B2))
		Action(2);
	else
	{
		std::ostringstream o;
		o << std::hex << std::uppercase << std::setfill('0') << std::setw(6) << (color[0] & 0x00ffffff);
		tooltip = o.str();
	}
}

void cGUIColorPick::LoadImg(const std::string & nm )
{
	txt = nm;
	R9_ImgDestroy(&m_img);
	R9_ImgLoadFile(nm, &m_img);
}
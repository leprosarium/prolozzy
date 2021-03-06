#include "stdafx.h"
#include "GUIColor.h"
#include "GUI.h"
#include "R9ImgLoader.h"
#include "eInput.h"

void cGUIColorPick::OnUpdate()
{
	if(!m_mousein || !m_img.isValid())
		return;

	iRect rc = scrRect();
	iV2 p = (g_gui->m_mouse - rc.p1) * iV2(m_img.m_width, m_img.m_height) / rc.Size();
	color[0] = m_img.getColor(p.x, p.y);

	if(einput->isMouseUp(0))
		Action(1);
	else
	if(einput->isMouseUp(1))
		Action(2);
	else
	{
		std::wostringstream o;
		o << std::hex << std::uppercase << std::setfill(L'0') << std::setw(6) << (color[0] & 0x00ffffff);
		tooltip = o.str();
	}
}

void cGUIColorPick::LoadImg(const std::wstring & nm )
{
	txt = nm;
	R9_ImgDestroy(&m_img);
	R9_ImgLoadFile(nm, &m_img);
}
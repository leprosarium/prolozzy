#ifndef __GUICOLOR_H__
#define __GUICOLOR_H__

#include "GUIItem.h"

class cGUIColorPick : public cGUIItem
{
	r9Img m_img;
public:
	cGUIColorPick(cGUIDlg *d) : cGUIItem(d) {}
	virtual void OnUpdate();
	virtual void OnDraw() {};
	
	void LoadImg(const std::string &);
};

#endif




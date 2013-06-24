//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIItem.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIITEM_H__
#define __GUIITEM_H__

#include "GUIUtil.h"
#include <map>

class cGUIDlg;

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIItem
// Obs:
// 1. uses COLOR1=backgr, COLOR2=gradient, COLOR3=border
//////////////////////////////////////////////////////////////////////////////////////////////////

class cGUIItem
{
public:
	int id;
	int style;
	bool hidden;
	bool disable;
	iRect rect;
	std::string txt;
	int txtAlign;
	dword txtColor;
	int txtOffset;
	dword color[4];
	int img0;
	int img1;
	dword imgColor;
	int imgAlign;
	int mode;
	std::string cmdAction;
	int cmdActionParam;

	int value;
	int group;
	std::string tooltip;






						cGUIItem			();
virtual					~cGUIItem			();

virtual	void			Build				();					// build
virtual	void			Update				();					// update 
virtual	void			Draw				();					// draw 


		// util
	iRect scrRect() const;

inline	void			GetCliRect			( RECT &rc )		{ rc.left = 0; rc.top = 0; rc.right = rect.Width(); rc.bottom = rect.Height(); }
		int				SetParent			( cGUIDlg* dlg );	// calls dlg->ItemAdd

	void Capture(bool on);
	bool IsCaptured() const;
virtual	void			Action				();					// action
		void			Select				();					// select item & dlg as last

		// data
		std::map<int, int> vars;								// variable zone
		cGUIDlg*		m_dlg;									// parent
		BOOL			m_mousein;
};


//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUITitle
// uses IV_VALUE for movable
//////////////////////////////////////////////////////////////////////////////////////////////////
class cGUITitle : public cGUIItem
{
	iV2 move;
public:
	cGUITitle();
	virtual void Update();
};


#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

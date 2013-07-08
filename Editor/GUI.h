//////////////////////////////////////////////////////////////////////////////////////////////////
// GUI.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUI_H__
#define __GUI_H__

#include "E9System.h"
#include "E9Math.h"
#include "GUIUtil.h"
#include "GUIDlg.h"

#include "SWI-cpp-m.h"

#define	GUIKEY_MB1			0
#define	GUIKEY_MB2			1
#define	GUIKEY_MB3			2
#define	GUIKEY_CTRL			3
#define	GUIKEY_SHIFT		4
#define	GUIKEY_ALT			5
#define	GUIKEY_MAX			16

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUI
//////////////////////////////////////////////////////////////////////////////////////////////////
class cGUI
{
	cGUIDlg * m_lastdlg;				// last (selected) dlg index
	cGUIItem * m_lastitem;				// last (selected) item index
	std::vector<cGUIDlg *> m_dlg;
	void GetMousePos();

public:
	cGUI();

	bool Init();
	void Done();
	void Update();
	void Draw();
		
	// status bar
	std::string		ToolTip;

	// input
	iV2 m_mouse;						// mouse pos
	int m_key[GUIKEY_MAX];		
	void ReadInput();					// read input ( CALL IT BEFORE UPDATE ! )

	// images
	int ImgLoad( const std::string & image ) { return m_texturepool.Load(image); }
	int ImgFind( const std::string & image ) { return m_texturepool.Find(image); }
	r9TexturePool m_texturepool;
				
	bool DlgSelect(int id);
	cGUIDlg * makeDlg();
	template<class T>
	void makeItem();
	bool makeItem(const std::string & className);
	cGUIDlg * GetLastDlg();
	cGUIItem * GetLastItem(); 	


	r9Font * m_font;

	cGUIItem * m_capture;				// pointer to capturing item
	bool m_isbusy;						// if gi is busy (there is at least one modal dialog or is in a capture)
				
	// script
	bool ScriptPrologDo(const std::string & pred);
	bool ScriptPrologDo(const char *module, const char *predicate, const PlTermv & args = 0);

	void SetLast(cGUIDlg * d = nullptr, cGUIItem * i = nullptr) { m_lastdlg = d; m_lastitem = i; }
};

template<class Item>
void cGUI::makeItem()
{
	cGUIDlg * dlg = GetLastDlg();
	SetLast(dlg, dlg->Add<Item>());
}

extern cGUI *g_gui;
inline bool GUIInit() { g_gui = new cGUI(); return g_gui->Init(); }
inline void GUIDone() { if(g_gui) { g_gui->Done(); delete g_gui; } }	

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

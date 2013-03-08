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

#define	GV_USER				16
#define	GV_MAX				128

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUI
//////////////////////////////////////////////////////////////////////////////////////////////////
class cGUI
{
public:
						cGUI();
virtual					~cGUI();

		BOOL			Init();							// init
		void			Done();							// done
		void			Update();						// update 
		void			Draw();							// draw 
		
		// access
		void			SetInt( int idx, int val );
		int				GetInt( int idx );
		void			SetTxt( int idx, char* text );
		char*			GetTxt( int idx );
		tGuiVar			m_var[GV_MAX];					// variable zone

		// status bar
inline	char*			GetTooltip()					{ return m_tooltip; }
inline	void			SetTooltip( char* text )		{ if(text) strncpy(m_tooltip,text,255); else m_tooltip[0]=0; m_tooltip[255]=0; }
		char			m_tooltip[256];					// tooltip text

		// input
		int				m_mousex;						// mouse x pos
		int				m_mousey;						// mouse y pos
		int				m_key[GUIKEY_MAX];		
		void			ReadInput();					// read input ( CALL IT BEFORE UPDATE ! )

		// images
		int				ImgLoad( const std::string & image ) { return m_texturepool.Load(image); }
		int				ImgFind( const std::string & image ) { return m_texturepool.Find(image); }
		r9TexturePool	m_texturepool;					// textures
				
		// inheritance
inline	int				DlgCount()						{ return m_dlg.size(); }
inline	cGUIDlg*		DlgGet( int idx )				{ if(0<=idx && idx<DlgCount()) return m_dlg[idx]; return 0; }
inline	int				DlgAdd( cGUIDlg* dlg )			{ if(dlg) { int idx = m_dlg.size(); m_dlg.push_back(dlg); return idx; } return -1; }
		void			DlgDel( int idx );
virtual	int				DlgFind( int id );				// return idx
virtual	int				DlgFind( cGUIDlg* dlg );		// return idx

	int makeDlg(char * className);
	int makeItem(char * className);
	cGUIDlg * GetLastDlg();
	cGUIItem * GetLastItem(); 	

		// data
	std::vector<cGUIDlg *> m_dlg;						// cGUIDlg list
		r9Font*			m_font;

		cGUIItem*		m_capture;						// pointer to capturing item
		BOOL			m_isbusy;						// if gi is busy (there is at least one modal dialog or is in a capture)
				
		// script
		int				m_lastdlg;						// last (selected) dlg index
		int				m_lastitem;						// last (selected) item index
	
		bool			ScriptPrologDo(const std::string & pred);
};

// gui replicators (cGUIItem, cGUIDlg)
void* GUICreateClass( char* classname );

extern	cGUI*			g_gui;
inline	BOOL			GUIInit			()						{ g_gui = new cGUI(); return g_gui->Init(); }
inline	void			GUIDone			()						{ if(g_gui)	{ g_gui->Done(); delete g_gui; }}	

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

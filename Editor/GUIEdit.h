//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIEdit.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIEDIT_H__
#define __GUIEDIT_H__

#include "GUIItem.h"
#include "E9String.h"

/////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIEdit
// Obs:
// 1. uses COLOR1=backgr, COLOR2=gradient, COLOR3=border, COLOR4=selection
// 2. text align forced to GUIALIGN_LEFT|GUIALIGN_CENTERY
//////////////////////////////////////////////////////////////////////////////////////////////////

class cGUIEdit : public cGUIItem
{
		int				Pos2Chr				(int pos);				// get char pos from pixel pos
		int				Chr2Pos				(int chr);				// get pixel pos from char pos
inline  void			ShiftLeft			(int chr, int count);	// shifts count chars starting with chr 
		void			ClipboardCopy		();						// copy to windows clipboard
		void			ClipboardPaste		();						// paste from windows clipboard
		void			SelectionCut		();						// cut current selection, if any
		void			SelectionPaste		(const std::wstring & );	// paste some text at cursor (and shift right)

		int				m_sel1;										// selected char start pos
		int				m_sel2;										// selected char end pos (after last selected)
		bool			m_edit;										// if TRUE it is in edit mode
		std::wstring 	m_bktxt;			

		int sel1() const { return std::min(m_sel1, m_sel2); }
		int sel2() const { return std::max(m_sel1, m_sel2); }
public:
	cGUIEdit (cGUIDlg *);
	virtual void OnUpdate();
	virtual void OnDraw();
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// inline
//////////////////////////////////////////////////////////////////////////////////////////////////
inline  void cGUIEdit::ShiftLeft(int chr, int count)
{
	if(!count) return;
	if(chr > txt.size())
		return;
	std::wstring tail = txt.substr(chr);
	int begin = chr - count;
	if(begin <= 0) 
		txt = tail;
	else
		txt = txt.substr(0, begin) + tail;
}

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIEdit.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GUIEDIT_H__
#define __GUIEDIT_H__

#include "GUIItem.h"
#include "E9String.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// Defines
//////////////////////////////////////////////////////////////////////////////////////////////////

#define IV_TXTSIZE				(IV_USER+1)

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIEdit
// Obs:
// 1. uses COLOR1=backgr, COLOR2=gradient, COLOR3=border, COLOR4=selection
// 2. text align forced to GUIALIGN_LEFT|GUIALIGN_CENTERY
//////////////////////////////////////////////////////////////////////////////////////////////////

class cGUIEdit : public cGUIItem
{
public:

						cGUIEdit			();
virtual					~cGUIEdit			();

//virtual	void			Build				();						// build
virtual	void			Update				();						// update 
virtual	void			Draw				();						// draw 
	
		// Util
		int				Pos2Chr				(int pos);				// get char pos from pixel pos
		int				Chr2Pos				(int chr);				// get pixel pos from char pos
inline  void			ShiftLeft			(int chr, int count);	// shifts count chars starting with chr 
inline  void			ShiftRight			(int chr, int count);	// shifts count chars starting with chr 
		void			ClipboardCopy		();						// copy to windows clipboard
		void			ClipboardPaste		();						// paste from windows clipboard
		void			SelectionCut		();						// cut current selection, if any
		void			SelectionPaste		(const std::string & );	// paste some text at cursor (and shift right)

		int				m_sel1;										// selected char start pos
		int				m_sel2;										// selected char end pos (after last selected)
		BOOL			m_edit;										// if TRUE it is in edit mode
		std::string 	m_bktxt;			
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// inline
//////////////////////////////////////////////////////////////////////////////////////////////////
inline  void cGUIEdit::ShiftLeft(int chr, int count)
{
	if(!count) return;
	std::string tail = txt.substr(chr);
	int begin = chr - count;
	if(begin <= 0) 
		txt = tail;
	else
		txt = txt.substr(0, begin) + tail;
}

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////



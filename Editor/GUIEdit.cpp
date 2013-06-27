//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIEdit.cpp
// Action param (while editing): 0=click out, 1=enter, 2=right click, 3=tab
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUIEdit.h"
#include "GUI.h"
#include "E9App.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUIEdit
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUIEdit::cGUIEdit(cGUIDlg *d) : cGUIItem(d), m_sel1(),  m_sel2(), m_edit()
{
}

void cGUIEdit::OnUpdate()
{
	iRect rc = scrRect();
	int mx = g_gui->m_mouse.x - rc.p1.x;
	int my = g_gui->m_mouse.y - rc.p1.y;
	BOOL shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);
	BOOL ctrl = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	BOOL enter = I9_GetKeyDown(I9K_RETURN) || I9_GetKeyDown(I9K_NUMPADENTER);
	
	int width = rect.Width();
	int height = rect.Height();
	int cursorx = Chr2Pos(m_sel2);
	int deltax = 0;
	if(cursorx>width-height/2) deltax = -(cursorx-(width-height/2));

	BOOL captured = (g_gui->m_capture == this);
	BOOL clicked = (I9_GetKeyDown(I9_MOUSE_B1));
	BOOL clickedin = clicked && m_mousein;

	if(clicked)
	{
		if(clickedin)
		{
			if(!m_edit) m_bktxt = txt;
			Capture(true);
			m_edit = true;
			m_sel1 = m_sel2 = Pos2Chr(mx-deltax);
		
			// double click
			static int dblclicktick = 0;
			int tick = GetTickCount();
			if(tick-dblclicktick<500) // select all
			{
				m_sel1=0; m_sel2=txt.size();
				Capture(false);
			}
			dblclicktick = tick;
		}
		else
		{
			if(m_edit)
			{
				Action(0);
				m_edit = false;
				m_sel1 = m_sel2 = 0;
			}
		}
	}
	else
	if(captured)
	{
		m_sel2 = Pos2Chr(mx-deltax);
		if( !I9_GetKeyValue(I9_MOUSE_B1) ) // for selections
		{
			Capture(false);
		}
	}

	// ordered selection
	int s1,s2;
	if(m_sel1<m_sel2)
	{
		s1 = m_sel1;
		s2 = m_sel2;
	}
	else
	{
		s1 = m_sel2;
		s2 = m_sel1;
	}

	if(m_edit)
	{
		if(I9_GetKeyDown(I9K_ESCAPE))
		{
			if(!m_bktxt.empty()) txt = m_bktxt; // restore
			m_edit = false;
			m_sel1 = m_sel2 = 0;
			return;
		}

		if(enter || I9_GetKeyDown(I9K_TAB))
		{
			Action(enter ? 1 : 3);
			m_edit = false;
			m_sel1 = m_sel2 = 0;
			return;
		}

		if(I9_GetKeyDown(I9K_DELETE))
		{
			if(s1!=s2)
				ShiftLeft(s2,s2-s1); // selection cut
			else
				ShiftLeft(s1+1,1);
			m_sel2 = m_sel1 = s1;
			return;
		}	

		if(I9_GetKeyDown(I9K_BACK))
		{
			if(s1!=s2)
				ShiftLeft(s2,s2-s1); // selection cut
			else
				ShiftLeft(s1,1);
			s1--;
			if(s1<0) s1 = 0;
			m_sel2 = m_sel1 = s1;
			return;
		}	
		
		if(I9_GetKeyDown(I9K_HOME))
		{
			m_sel2 = 0;
			if(!shift) m_sel1 = m_sel2;
			return;
		}
		
		if(I9_GetKeyDown(I9K_END))
		{
			m_sel2 = txt.size();
			if(!shift) m_sel1 = m_sel2;
			return;
		}
		
		if(I9_GetKeyDown(I9K_LEFT))
		{
			if(m_sel1==m_sel2 || shift)
			{
				m_sel2--;
				if(m_sel2<0) m_sel2 = 0;
				if(!shift) m_sel1 = m_sel2;
			}
			else // break selection
			{
				if(m_sel2<m_sel1) 
					m_sel1 = m_sel2;
				else 
					m_sel2 = m_sel1;
			}
			return;
		}
		
		if(I9_GetKeyDown(I9K_RIGHT))
		{
			if(m_sel1==m_sel2 || shift)
			{
				m_sel2++;
				if(m_sel2>txt.size()) m_sel2 = txt.size();
				if(!shift) m_sel1 = m_sel2;
			}
			else // break selection
			{
				if(m_sel2<m_sel1) 
					m_sel2 = m_sel1;
				else 
					m_sel1 = m_sel2;
			}
			return;
		}

		if(ctrl && I9_GetKeyDown(I9K_C))
		{
			ClipboardCopy();
			return;
		}
		
		if(ctrl && I9_GetKeyDown(I9K_V))
		{
			ClipboardPaste();
			return;
		}

		if(ctrl && I9_GetKeyDown(I9K_X))
		{
			ClipboardCopy();
			SelectionCut();
			return;
		}

		for(int i=0;i<I9_GetKeyQCount();i++)
		{
			if(I9_GetKeyQValue(i))  
			{
				int key = I9_GetKeyQCode(i);
				char ch = shift ? I9_GetKeyShifted(key) : I9_GetKeyAscii(key); 
				if(ch>=32 && ch<128)
				{
					if(s1!=s2)
						ShiftLeft(s2,s2-s1); // selection cut
					m_sel2 = m_sel1 = s1;
					txt = txt.substr(0, s1) + ch + txt.substr(s1);
					m_sel1++;
					s1 = s2 = m_sel2 = m_sel1;
				}

			}
		}
	}

	// right click action
	if(!IsCaptured() && m_mousein && I9_GetKeyDown(I9_MOUSE_B2))
		Action(2);
}

void cGUIEdit::OnDraw()
{
	iRect rc = scrRect();

	dword col = color[0];
	if(m_edit)	col = GUIColorShift(col,GUICOLORSHIFT/2); 

	int width = rect.Width();
	int height = rect.Height();
	int cursorx = Chr2Pos(m_sel2);
	int deltax = 0;
	if(cursorx>width-height/2) deltax = -(cursorx-(width-height/2));

	// background
	if( style & GUISTYLE_BACKGR )
		GUIDrawBar(rc, col );
	else
	if( style & GUISTYLE_GRADIENT )
		GUIDrawGradient( rc, color[0], color[1]);
	
	// image
	GUIDrawImg( rc, img0, imgColor, imgAlign);

	// clip content
	fRect oldclip = R9_GetClipping();
	fRect newclip = fRect(rc);
	R9_AddClipping(newclip);
	if( R9_IsClipping() )
	{
		// selection
		fRect rc1;
		if(m_sel1!=m_sel2)
		{
			rc1 = rc;
			rc1.p1.x += Chr2Pos(m_sel1) + deltax;
			rc1.p2.x += Chr2Pos(m_sel2) + deltax;
			R9_ClipBar(rc1);
			GUIDrawBar( rc1, color[3]);
		}

		// text
		iRect rr = rc;
		rr.Offset(iV2(deltax, 0));
		GUIDrawText( rr, txt.c_str(), txtColor, GUIALIGN_LEFT|GUIALIGN_CENTERY, txtOffset);

		// cursor
		if(m_edit)
		{
			static dword s_time = 0;
			s_time += App.DeltaTime();
			if(s_time<=400)
			{
				rc1.p1.x = (float)rc.p1.x + Chr2Pos(m_sel2) + deltax;
				rc1.p1.y = (float)rc.p1.y+2;
				rc1.p2.x =  rc1.p1.x+1;
				rc1.p2.y = (float)rc.p2.y-2;
				R9_ClipBar(rc1);
				GUIDrawBar( rc1, txtColor);			
			}
			if(s_time>700) s_time = 0;
		}
	}

	R9_SetClipping(oldclip);

	// border
	if( style & GUISTYLE_BORDER )
		GUIDrawRect(rc, color[2]);
	else
	if( style & GUISTYLE_BORDER3D )
		GUIDrawRect3D(rc, color[2], style & GUISTYLE_PRESSED );

}

/////////////////////////////////////////////////////////////////////////////////////////////////
// Access
//////////////////////////////////////////////////////////////////////////////////////////////////

int cGUIEdit::Pos2Chr(int pos)
{
	int  i,w, size;
	pos = pos - txtOffset;
	size = txt.size();
	w = 0;
	for(i=0;i<size;i++)
	{
		w += (int)(g_gui->m_font->GetCharWidth(txt[i]) + g_gui->m_font->GetOfsX()); 
		if(w>pos)
			return i ;
	}
	return size;
}

int cGUIEdit::Chr2Pos(int chr)
{
	assert(chr<=txt.size());
	return static_cast<int>(g_gui->m_font->GetTextWidth(txt.c_str(),chr) + txtOffset); 
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// clipboard
//////////////////////////////////////////////////////////////////////////////////////////////////
void cGUIEdit::ClipboardCopy()
{
	// ordered selection
	int s1,s2;
	if(m_sel1<m_sel2)
	{
		s1 = m_sel1;
		s2 = m_sel2;
	}
	else
	{
		s1 = m_sel2;
		s2 = m_sel1;
	}
	int size = s2-s1;
	if(size==0) return; // no selection

	if(!OpenClipboard(NULL)) return;
	if(EmptyClipboard())
	{
		HGLOBAL handler = GlobalAlloc(GMEM_MOVEABLE|GMEM_DDESHARE,size+1); assert(handler!=NULL);
		void* data = GlobalLock(handler); assert(data!=NULL);
		memcpy(data,txt.c_str() + s1,size);
		((char*)data)[size]=0;
		GlobalUnlock(handler);
		SetClipboardData(CF_TEXT,handler);
	}
	CloseClipboard();
}

void cGUIEdit::ClipboardPaste()
{
	if(!IsClipboardFormatAvailable(CF_TEXT)) return;
	if(!OpenClipboard(NULL)) return;
	HGLOBAL handler = GetClipboardData(CF_TEXT);
	if(handler)
	{
		void* data = GlobalLock(handler);
		if(data)
		{
			SelectionCut();
			SelectionPaste((char*)data);
			GlobalUnlock(handler); 
		}
	}
	CloseClipboard();
}

void cGUIEdit::SelectionCut()
{
	if(m_sel1==m_sel2) return; // no selection
	// ordered selection
	int s1,s2;
	if(m_sel1<m_sel2)
	{
		s1 = m_sel1;
		s2 = m_sel2;
	}
	else
	{
		s1 = m_sel2;
		s2 = m_sel1;
	}
	ShiftLeft(s2,s2-s1);
	m_sel1 = m_sel2 = s1;
}

void cGUIEdit::SelectionPaste(const std::string & pst)
{
	if(pst.empty()) return;
	txt = txt.substr(0, m_sel1) + pst + txt.substr(m_sel1);
	m_sel1 += pst.size();
	m_sel2 = m_sel1;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

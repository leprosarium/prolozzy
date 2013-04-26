//////////////////////////////////////////////////////////////////////////////////////////////////
// GUI.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUI.h"
#include "E9App.h"
#include "I9Input.h"

#include "GUIItem.h"
#include "GUIButton.h"
#include "GUIEdit.h"
#include "GUITile.h"
#include "GUIColor.h"
#include "GUIDlg.h"

#include <algorithm>

//////////////////////////////////////////////////////////////////////////////////////////////////
// cGUI
//////////////////////////////////////////////////////////////////////////////////////////////////
cGUI* g_gui = NULL;

cGUI::cGUI()
{
	m_mousex		= 0;
	m_mousey		= 0;
	m_font			= NULL;
	m_lastdlg		= -1;
	m_lastitem		= NULL;
	m_capture		= NULL;	
	m_isbusy		= FALSE;
	m_tooltip[0]	=0;
	memset(&m_var,0,sizeof(m_var));
}

cGUI::~cGUI()
{
}

BOOL cGUI::Init()
{
	
	GUIInitResources();

	// fonts
	BOOL ok;
	m_font = new r9Font(); 
	ok = m_font->Create("Editor\\Font\\font.fnt");
	R9TEXTURE tex = R9_TextureLoad("Editor\\Font\\font.tga");
	m_font->SetTexture(tex);
	m_font->SetSpace(4); // !
	
	// mouse
	POINT pt;
	GetCursorPos(&pt);
	ScreenToClient(E9_GetHWND(), &pt); 
	m_mousex = pt.x;//Input()->GetMouseX();
	m_mousey = pt.y;//Input()->GetMouseY();

	return ok;
}
	
void cGUI::Done()
{
	std::for_each(m_dlg.begin(), m_dlg.end(), [](cGUIDlg *d){delete d;});
	m_dlg.clear();
	m_capture = NULL;
	if(m_font) 
	{
		R9_TextureDestroy(m_font->GetTexture());
		delete m_font;
	}
	m_texturepool.Done();
	GUIDoneResources();
}

void cGUI::Update()
{
	m_isbusy = FALSE;
	
	if(m_capture)
	{
		m_capture->Update();
		m_isbusy = TRUE;
	}
	else
	{
		// serach top most modal
		bool modal = false;
		auto i = std::find_if(m_dlg.rbegin(), m_dlg.rend(), [](cGUIDlg * d) {return d->GetInt(DV_MODAL)!=0;});
		if ( i != m_dlg.rend())
		{
			(*i)->Update();
			m_isbusy = TRUE;
		}
		else // no modal -> update all
		{
			std::for_each(m_dlg.begin(), m_dlg.end(), [](cGUIDlg * d) {d->Update();});
		}
	}
	// delete mustclose dialogs
	for(size_t i =0;i<m_dlg.size();)
		if(m_dlg[i]->m_mustclose)
		{
			m_capture = 0; // clear captrure (colud be int the dying dialog)
			delete m_dlg[i];
			m_dlg.erase(m_dlg.begin() + i);
			if(m_lastdlg==static_cast<int>(i)) m_lastdlg=-1; else
			if(m_lastdlg>static_cast<int>(i)) m_lastdlg--; // fix index
		}
		else
			++i;
}

void cGUI::Draw()
{
	R9_SetBlend(Blend::Alpha);

	std::for_each(m_dlg.begin(), m_dlg.end(), [](cGUIDlg * d){ d->Draw(); });
	
	// tooltip
	if(m_tooltip[0])
	{
		float w,h;
		m_font->GetTextBox(m_tooltip,w,h);
		w+=8; h+=4;
		GUIDrawBar(  m_mousex+16, m_mousey+16, m_mousex+16+(int)w, m_mousey+16+(int)h, 0xffffa000 );
		GUIDrawRect( m_mousex+16, m_mousey+16, m_mousex+16+(int)w, m_mousey+16+(int)h, 0xff000000 );
		GUIDrawText( m_mousex+16+4, m_mousey+16+2, m_mousex+16+(int)w-4, m_mousey+16+(int)h-2, m_tooltip, 0xff000000, GUIALIGN_LEFT|GUIALIGN_TOP );
	}

	// mouse cursor (for full screen tests)
	if(!App.Windowed())
		R9_DrawLine( fV2(m_mousex, m_mousey), fV2(m_mousex+10, m_mousey+10), 0xffffffff );

}

/////////////////////////////////////////////////////////////////////////////////////////////////
// Access
//////////////////////////////////////////////////////////////////////////////////////////////////
void cGUI::SetInt( int idx, int val )
{
	assert(0<=idx && idx<GV_MAX);
	m_var[idx].m_int=val;
}

int cGUI::GetInt( int idx )
{
	assert(0<=idx && idx<GV_MAX);
	return m_var[idx].m_int;	
}

void cGUI::SetTxt( int idx, char* text )
{
	assert(0<=idx && idx<GV_MAX);
	char* sz = m_var[idx].m_str;
	if(sz) free(sz);
	m_var[idx].m_str = sstrdup(text);
}

char* cGUI::GetTxt( int idx )
{
	assert(0<=idx && idx<GV_MAX);
	return m_var[idx].m_str;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Input
//////////////////////////////////////////////////////////////////////////////////////////////////
void cGUI::ReadInput()
{

	POINT pt;
	GetCursorPos(&pt);
	ScreenToClient(E9_GetHWND(), &pt); 

	m_mousex = pt.x;//Input()->GetMouseX();
	m_mousey = pt.y;//Input()->GetMouseY();

	m_key[GUIKEY_MB1]	= I9_GetKeyValue(I9_MOUSE_B1); 
	m_key[GUIKEY_MB2]	= I9_GetKeyValue(I9_MOUSE_B2);
	m_key[GUIKEY_MB3]	= I9_GetKeyValue(I9_MOUSE_B3);
	m_key[GUIKEY_CTRL]	= I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	m_key[GUIKEY_SHIFT]	= I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);
	m_key[GUIKEY_ALT]	= I9_GetKeyValue(I9K_LALT) || I9_GetKeyValue(I9K_RALT);

}


//////////////////////////////////////////////////////////////////////////////////////////////////
// Inheritance
//////////////////////////////////////////////////////////////////////////////////////////////////

int cGUI::DlgFind( int id )
{
	auto i = std::find_if(m_dlg.begin(), m_dlg.end(), [&id](cGUIDlg * d) { return d->GetInt(DV_ID) == id;});
	return i == m_dlg.end() ? -1 : i - m_dlg.begin();
}

int cGUI::DlgFind( cGUIDlg *dlg )
{
	auto i = std::find(m_dlg.begin(), m_dlg.end(), dlg);
	return i == m_dlg.end() ? -1 : i - m_dlg.begin();
}


void cGUI::DlgDel( int idx ) 
{
	if(0<=idx && idx<DlgCount()) 
	{ 
		delete m_dlg[idx]; 
		m_dlg.erase(m_dlg.begin() + idx); 
	} 
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// Script
//////////////////////////////////////////////////////////////////////////////////////////////////

bool cGUI::ScriptPrologDo(const std::string & pred)
{
	try
	{
		return PlCall(pred.c_str());
	}
	catch(PlException const & e)
	{
		dlog(L"Exception: %s", static_cast<LPCWSTR>(e));
	}
	return false;
}


cGUIDlg * cGUI::GetLastDlg()
{
	if(cGUIDlg * dlg = DlgGet(m_lastdlg))
		return dlg;
	throw PlException("no selected dialog");
}

cGUIItem * cGUI::GetLastItem()
{
	if(cGUIItem* item = GetLastDlg()->ItemGet(m_lastitem))
		return item;
	throw PlException("no selected item");
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// GUI EXPORT
//////////////////////////////////////////////////////////////////////////////////////////////////


PREDICATE_M(dlg, find, 2)
{
	return A2 = g_gui->DlgFind(A1);
}

PREDICATE_M(gui, itemFind, 2)
{
	int idx = g_gui->GetLastDlg()->ItemFind(A1);
	if(idx == -1)
		return false;
	return A2 = idx;
}


PREDICATE_M(gui, imgLoad, 2)
{
	int img = g_gui->ImgLoad(A1);
	if (img == -1)
		return false;
	return A2 = img;
}

PREDICATE_M(gui, imgFind, 2)
{
	int img = g_gui->ImgFind(A1);
	if(img == -1)
		return false;
	return A2 = img;
}


PREDICATE_M(gui, mouseX, 1)
{
	return A1 = g_gui->m_mousex;
}

PREDICATE_M(gui, mouseY, 1)
{
	return A1 = g_gui->m_mousey;
}



PREDICATE_M(gui, scrW, 1)
{
	RECT rc;
	GetClientRect(E9_GetHWND(), &rc); 
	return A1 = rc.right - rc.left; 
}

PREDICATE_M(gui, scrH, 1)
{
	RECT rc;
	GetClientRect(E9_GetHWND(), &rc); 
	return A1 = rc.bottom - rc.top; 
}

PREDICATE_M(gui, fontH, 1)
{
	if(!g_gui->m_font) return false;
	return A1 = static_cast<int>(g_gui->m_font->GetSize());
}

PREDICATE_M(gui, textW, 2)
{
	if(!g_gui->m_font) return A2 = 0;
	float w, h;
	g_gui->m_font->GetTextBox(A1, w, h);
	return A2 = static_cast<int>(w);

}

PREDICATE_M(gui, textH, 2)
{
	if(!g_gui->m_font) return A2 = 0;
	float w, h;
	g_gui->m_font->GetTextBox(A1, w, h);
	return A2 = static_cast<int>(h);
}

PREDICATE_M(gui, winDlgOpenFile, 4)
{
	static WCHAR filename[256];
	filename[0]=0;
	wcscpy(filename, A1);
	
	if(WinDlgOpenFile( filename, A3, A4))
		return A2 = filename;
	return false;	
}

PREDICATE_M(gui, winDlgOpenFolder, 2)
{
	static WCHAR foldername[256];
	foldername[0]=0;
	wcscpy(foldername, A1);
	if(WinDlgOpenFolder( foldername ))
		return A2 = foldername;
	return false;
}

PREDICATE_M(gui, winDlgOpenColor, 2)
{
	dword c = static_cast<dword>(static_cast<int64>(A1));
	if(WinDlgOpenColor(&c, TRUE))
		return A2 = static_cast<int64>(c);
	return false;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIDlg EXPORT
//////////////////////////////////////////////////////////////////////////////////////////////////

int cGUI::makeDlg(char * className)
{
	if(cGUIDlg* dlg = (cGUIDlg*)GUICreateClass(className))
		return m_lastdlg = DlgAdd(dlg); 
	throw PlException("dialog creation failure");
}

PREDICATE_M(dlg, new, 1)
{
	return A1 = g_gui->makeDlg("cGUIDlg");
}

PREDICATE_M(dlg, new, 2)
{
	return A1 = g_gui->makeDlg(A2);
}


PREDICATE_M(dlg, select, 1)
{
	int idx = A1;
	if(idx < 0 || idx >= g_gui->DlgCount()) 
		throw PlDomainError("invalid dialog index", A1);
	g_gui->m_lastdlg = idx;
	g_gui->m_lastitem = -1;
	return true;
}

PREDICATE_M(dlg, getSelect, 1)
{
	return A1 = g_gui->m_lastdlg;
}

PREDICATE_M(gui, dlgClose, 0)
{
	g_gui->GetLastDlg()->Close(0);
	return true;
}

PREDICATE_M(gui, dlgClose, 1)
{
	g_gui->GetLastDlg()->Close(A1);
	return true;
}

PREDICATE_M(dlg, getRect, 4)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	bool a1 = A1 = dlg->GetInt(DV_X);
	bool a2 = A2 = dlg->GetInt(DV_Y);
	bool a3 = A3 = dlg->GetInt(DV_X2);
	bool a4 = A4 = dlg->GetInt(DV_Y2);
	return a1 && a2 && a3 && a4;
}

PREDICATE_M(dlg, getPos, 2)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	bool a1 = A1 = dlg->GetInt(DV_X);
	bool a2 = A2 = dlg->GetInt(DV_Y);
	return a1 && a2;
}

PREDICATE_M(dlg, getPos2, 2)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	bool a1 = A1 = dlg->GetInt(DV_X2);
	bool a2 = A2 = dlg->GetInt(DV_Y2);
	return a1 && a2;
}

PREDICATE_M(dlg, setID, 1)
{
	g_gui->GetLastDlg()->SetInt(DV_ID, A1);
	return true;
}

PREDICATE_M(dlg, setTestKey, 0)
{
	g_gui->GetLastDlg()->SetInt(DV_TESTKEY, 1);
	return true;
}

PREDICATE_M(dlg, resetTestKey, 0)
{
	g_gui->GetLastDlg()->SetInt(DV_TESTKEY, 0);
	return true;
}

PREDICATE_M(dlg, setModal, 0)
{
	g_gui->GetLastDlg()->SetInt(DV_MODAL,  1);
	return true;
}

PREDICATE_M(dlg, setCloseOut, 0)
{
	g_gui->GetLastDlg()->SetInt(DV_CLOSEOUT,  1);
	return true;
}

PREDICATE_M(dlg, setCloseCmd, 1)
{
	g_gui->GetLastDlg()->SetTxt(DV_CLOSECMD,  A1);
	return true;
}

PREDICATE_M(dlg, setUser, 2)
{
	int idx = A1;
	g_gui->GetLastDlg()->SetInt(DV_USER+idx,  A2);
	return true;
}

PREDICATE_M(dlg, setRect, 4)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	dlg->SetInt(DV_X,  A1);
	dlg->SetInt(DV_Y,  A2);
	dlg->SetInt(DV_X2, A3);
	dlg->SetInt(DV_Y2, A4);
	return true;
}

PREDICATE_M(dlg, addKey, 3)
{
	g_gui->GetLastDlg()->AddKey(A1, A2, A3);
	return true;
}

PREDICATE_M(dlg, count, 1)
{
	return A1 = g_gui->DlgCount();  
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIItem EXPORT
//////////////////////////////////////////////////////////////////////////////////////////////////

int cGUI::makeItem(char * className)
{
	cGUIDlg* dlg = GetLastDlg();
	if(dlg->ItemCount() >= MAX_GUIITEMS)
		throw PlException("too many items in a dialog");
	cGUIItem* item = (cGUIItem*)GUICreateClass(className);
	if(!item)
		throw PlException("item creation failure");
	return m_lastitem = dlg->ItemAdd(item);
}

PREDICATE_M(gui, itemNew, 1)
{
	return A1 = g_gui->makeItem("cGUIItem");
}

PREDICATE_M(gui, itemNew, 2)
{
	return A1 = g_gui->makeItem(A2);
}

PREDICATE_M(gui, itemSelect, 1)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	int idx = A1;
	if(idx<0 || idx>=dlg->ItemCount())
		throw PlDomainError("invalid item index", A1);
	assert(dlg->ItemGet(idx)); // safety
	g_gui->m_lastitem = idx;
	return true;
}

PREDICATE_M(gui, itemGetSelect, 1)
{
	return A1 = g_gui->m_lastitem; 
}

PREDICATE_M(gui, itemBuild, 0)
{
	g_gui->GetLastItem()->Build();
	return true;
}

PREDICATE_M(gui, itemGetX, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_X);
}
PREDICATE_M(gui, itemGetY, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_Y);
}
PREDICATE_M(gui, itemGetX2, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_X2);
}
PREDICATE_M(gui, itemGetY2, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_Y2);
}

PREDICATE_M(gui, itemGetTxt, 1)
{
	return A1 = g_gui->GetLastItem()->GetTxt(IV_TXT);
}

PREDICATE_M(gui, itemGetHidden, 0)
{
	return g_gui->GetLastItem()->GetInt(IV_HIDDEN) == 1;
}

PREDICATE_M(gui, itemGetCmdActionParam, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_CMDACTIONPARAM);
}

PREDICATE_M(gui, itemGetValue, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_VALUE);
}

PREDICATE_M(gui, itemGetDisable, 1)
{
	return A1 = g_gui->GetLastItem()->GetInt(IV_DISABLE);
}


PREDICATE_M(gui, itemGetColor, 1)
{
	return A1 = static_cast<int64>(g_gui->GetLastItem()->GetInt(IV_COLOR));
}


PREDICATE_M(gui, itemSetID, 1)
{
	g_gui->GetLastItem()->SetInt(IV_ID,  A1);
	return true;
}

PREDICATE_M(gui, itemSetRect, 4)
{
	cGUIItem* item = g_gui->GetLastItem();
	item->SetInt(IV_X,  A1);
	item->SetInt(IV_Y,  A2);
	item->SetInt(IV_X2, A3);
	item->SetInt(IV_Y2, A4);
	return true;
}

PREDICATE_M(gui, itemSetX, 1)
{
	cGUIItem* item = g_gui->GetLastItem();
	item->SetInt(IV_X,  A1);
	return true;
}

PREDICATE_M(gui, itemSetY, 1)
{
	cGUIItem* item = g_gui->GetLastItem();
	item->SetInt(IV_Y,  A1);
	return true;
}

PREDICATE_M(gui, itemSetX2, 1)
{
	cGUIItem* item = g_gui->GetLastItem();
	item->SetInt(IV_X2,  A1);
	return true;
}

PREDICATE_M(gui, itemSetY2, 1)
{
	cGUIItem* item = g_gui->GetLastItem();
	item->SetInt(IV_Y2,  A1);
	return true;
}


PREDICATE_M(gui, itemSetTxt, 1)
{
	g_gui->GetLastItem()->SetTxt(IV_TXT,  A1);
	return true;
}

PREDICATE_M(gui, itemSetColor, 2)
{
	cGUIItem* item = g_gui->GetLastItem();
	int ColorIdx = A1;
	if(ColorIdx < 0 || ColorIdx > 3)
		throw PlDomainError("Color index", A1);
	
	int64 Color = A2;
	item->SetInt(IV_COLOR + ColorIdx,  static_cast<int>(Color));
	return true;
}

PREDICATE_M(gui, itemSetStyle, 1)
{
	g_gui->GetLastItem()->SetInt(IV_STYLE,  A1);
	return true;
}

PREDICATE_M(gui, itemSetTxtAlign, 1)
{
	g_gui->GetLastItem()->SetInt(IV_TXTALIGN,  A1);
	return true;
}

PREDICATE_M(gui, itemSetToolTip, 1)
{
	g_gui->GetLastItem()->SetTxt(IV_TOOLTIP,  A1);
	return true;
}

PREDICATE_M(gui, itemSetImg0, 1)
{
	g_gui->GetLastItem()->SetInt(IV_IMG,  A1);
	return true;
}

PREDICATE_M(gui, itemSetImg1, 1)
{
	g_gui->GetLastItem()->SetInt(IV_IMG + 1,  A1);
	return true;
}

PREDICATE_M(gui, itemSetCmdAction, 1)
{
	g_gui->GetLastItem()->SetTxt(IV_CMDACTION,  A1);
	return true;
}

PREDICATE_M(gui, itemSetCmdActionParam, 1)
{
	g_gui->GetLastItem()->SetInt(IV_CMDACTIONPARAM,  A1);
	return true;
}

PREDICATE_M(gui, itemSetHidden, 1)
{
	g_gui->GetLastItem()->SetInt(IV_HIDDEN,  A1);
	return true;
}

PREDICATE_M(gui, itemSetDisable, 1)
{
	g_gui->GetLastItem()->SetInt(IV_DISABLE,  A1);
	return true;
}

PREDICATE_M(gui, itemSetValue, 1)
{
	g_gui->GetLastItem()->SetInt(IV_VALUE,  A1);
	return true;
}

PREDICATE_M(gui, itemSetGroup, 1)
{
	g_gui->GetLastItem()->SetInt(IV_GROUP,  A1);
	return true;
}

PREDICATE_M(gui, itemSetTxtColor, 1)
{
	int64 Color = A1;
	g_gui->GetLastItem()->SetInt(IV_TXTCOLOR,  static_cast<int>(Color));
	return true;
}


PREDICATE_M(gui, itemSetImgAlign, 1)
{
	g_gui->GetLastItem()->SetInt(IV_IMGALIGN,  A1);
	return true;
}

PREDICATE_M(gui, itemSetImgColor, 1)
{
	int64 Color = A1;
	g_gui->GetLastItem()->SetInt(IV_IMGCOLOR,  static_cast<int>(Color));
	return true;
}

PREDICATE_M(gui, itemSetUser, 2)
{
	int idx = A1;
	g_gui->GetLastItem()->SetInt(IV_USER + idx,  A2);
	return true;
}

#define SET_ITEM_PROP(Prop, PROP) PREDICATE_M(gui, itemSet##Prop, 1)\
{\
	g_gui->GetLastItem()->SetInt(IV_##PROP, A1);\
	return true;\
}

#define GET_ITEM_PROP(Prop, PROP) PREDICATE_M(gui, itemGet##Prop, 1)\
{\
	return A1 = g_gui->GetLastItem()->GetInt(IV_##PROP);\
}

#define ITEM_PROP(Prop, PROP) SET_ITEM_PROP(Prop, PROP) \
GET_ITEM_PROP(Prop, PROP)

ITEM_PROP(GuiTileScale, GUITILE_SCALE)
ITEM_PROP(GuiTileShrink, GUITILE_SHRINK)
ITEM_PROP(GuiTileMapScale, GUITILEMAP_SCALE)
ITEM_PROP(GuiTileMapSnap, GUITILEMAP_SNAP)
ITEM_PROP(GuiTileMapGrid, GUITILEMAP_GRID)
ITEM_PROP(GuiTileMapAxes, GUITILEMAP_AXES)

PREDICATE_M(gui, itemSetGuiTileMapMap, 4)
{
	cGUIItem * i = g_gui->GetLastItem();
	i->SetInt(IV_GUITILEMAP_MAP, A1);
	i->SetInt(IV_GUITILEMAP_MAP+1, A2);
	i->SetInt(IV_GUITILEMAP_MAP+2, A3);
	i->SetInt(IV_GUITILEMAP_MAP+3, A4);
	return true;
}

PREDICATE_M(gui, itemGetGuiTileMapMap, 4)
{
	cGUIItem * i = g_gui->GetLastItem();
	bool r1 = A1 = i->GetInt(IV_GUITILEMAP_MAP);
	bool r2 = A2 = i->GetInt(IV_GUITILEMAP_MAP+1);
	bool r3 = A3 = i->GetInt(IV_GUITILEMAP_MAP+2);
	bool r4 = A4 = i->GetInt(IV_GUITILEMAP_MAP+3);
	return r1 & r2 & r3 && r4;
}


PREDICATE_M(gui, itemCount, 1)
{
	return A1 = g_gui->GetLastDlg()->ItemCount();  
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// replicators
//////////////////////////////////////////////////////////////////////////////////////////////////
#define GUI_REPLICATE(name) if(0==strcmp(classname,#name)) return new name();

void* GUICreateClass( char* classname )
{
	GUI_REPLICATE(cGUIItem);
	GUI_REPLICATE(cGUITitle);
	GUI_REPLICATE(cGUIButton);
	GUI_REPLICATE(cGUICheck);
	GUI_REPLICATE(cGUIRadio);
	GUI_REPLICATE(cGUIEdit);
	GUI_REPLICATE(cGUITile);
	GUI_REPLICATE(cGUITileMap);
	GUI_REPLICATE(cGUIColorPick);
	GUI_REPLICATE(cGUIDlg);
	return NULL;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

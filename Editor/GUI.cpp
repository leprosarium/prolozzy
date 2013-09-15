//////////////////////////////////////////////////////////////////////////////////////////////////
// GUI.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "GUI.h"
#include "E9App.h"
#include "eInput.h"

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

cGUI::cGUI() : m_font(), m_lastdlg(), m_lastitem(), m_capture(), m_isbusy()
{
}

bool cGUI::Init()
{
	GUIInitResources();

	// fonts
	m_font = new r9Font(); 
	bool ok = m_font->Create("editor\\font\\font.fnt");
	R9TEXTURE tex = R9_TextureLoad("editor\\font\\font.tga");
	m_font->SetTexture(tex);
	m_font->SetSpace(4); // !
	
	GetMousePos();
	return ok;
}
	
void cGUI::Done()
{
	for(cGUIDlg * d: m_dlg) delete d;
	m_dlg.clear();
	m_capture = nullptr;
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
	m_isbusy = false;
	
	if(m_capture)
	{
		m_capture->Update();
		m_isbusy = true;
	}
	else
	{
		// serach top most modal
		bool modal = false;
		auto i = std::find_if(m_dlg.rbegin(), m_dlg.rend(), [](cGUIDlg * d) { return d->modal;});
		if ( i != m_dlg.rend())
		{
			(*i)->Update();
			m_isbusy = true;
		}
		else // no modal -> update all
		{
			auto tmp(m_dlg);
			std::for_each(tmp.begin(), tmp.end(), [](cGUIDlg * d) {d->Update();});
		}
	}
	// delete mustclose dialogs
	for(size_t i =0;i<m_dlg.size();)
		if(m_dlg[i]->m_mustclose)
		{
			if(m_lastdlg == m_dlg[i]) { SetLast(); }
			m_capture = 0; // clear captrure (colud be int the dying dialog)
			delete m_dlg[i];
			m_dlg.erase(m_dlg.begin() + i);
		}
		else
			++i;
}

void cGUI::Draw()
{
	R9_SetBlend(Blend::Alpha);

	std::for_each(m_dlg.begin(), m_dlg.end(), [](cGUIDlg * d){ d->Draw(); });
	
	// tooltip
	if(!ToolTip.empty())
	{
		fV2 sz = m_font->GetTextBox(ToolTip) + fV2(8, 4);
		iV2 tt = m_mouse + 16;
		iRect rr(tt, tt + iV2(sz)); 
		GUIDrawBar(rr, 0xffffa000);
		GUIDrawRect(rr, 0xff000000);
		iRect rt = rr;
		GUIDrawText(rt.Inflate(iV2(4, 2)), ToolTip, 0xff000000, GUIALIGN_LEFT|GUIALIGN_TOP );
	}

	// mouse cursor (for full screen tests)
	if(!App.Windowed())
		R9_DrawLine( fV2(m_mouse), fV2(m_mouse)+10, 0xffffffff );

}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Input
//////////////////////////////////////////////////////////////////////////////////////////////////
void cGUI::ReadInput()
{
	GetMousePos();

	m_key[GUIKEY_MB1]	= einput->mouseValue(0); 
	m_key[GUIKEY_MB2]	= einput->mouseValue(1);
	m_key[GUIKEY_MB3]	= einput->mouseValue(2);
	m_key[GUIKEY_CTRL]	= einput->ctrl();
	m_key[GUIKEY_SHIFT]	= einput->shift();
	m_key[GUIKEY_ALT]	= einput->alt();
}

void cGUI::GetMousePos()
{
	POINT pt;
	GetCursorPos(&pt);
	ScreenToClient(E9_GetHWND(), &pt); 
	m_mouse = iV2(pt.x, pt.y);
}

bool cGUI::DlgSelect(const std::string & id)
{
	auto i = std::find_if(m_dlg.begin(), m_dlg.end(), [&id](cGUIDlg * d) { return d->id == id;});
	if(i == m_dlg.end())
		return false;
	g_gui->SetLast(*i);
	return true;
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


bool cGUI::ScriptPrologDo(const char *module, const char *predicate, const PlTermv &args)
{
	try
	{
		return PlCall(module, predicate, args);
	}
	catch(PlException const & e)
	{
		dlog(L"Exception: %s", static_cast<LPCWSTR>(e));
	}
	return false;
}

cGUIDlg * cGUI::GetLastDlg()
{
	if(m_lastdlg)
		return m_lastdlg;
	throw PlException("no selected dialog");
}

cGUIItem * cGUI::GetLastItem()
{
	if(m_lastitem)
		return m_lastitem;
	throw PlException("no selected item");
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// GUI EXPORT
//////////////////////////////////////////////////////////////////////////////////////////////////

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
	return A1 = g_gui->m_mouse.x;
}

PREDICATE_M(gui, mouseY, 1)
{
	return A1 = g_gui->m_mouse.y;
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
	fV2 sz = g_gui->m_font->GetTextBox(A1);
	return A2 = static_cast<int>(sz.x);

}

PREDICATE_M(gui, textH, 2)
{
	if(!g_gui->m_font) return A2 = 0;
	fV2 sz = g_gui->m_font->GetTextBox(A1);
	return A2 = static_cast<int>(sz.y);
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

cGUIDlg * cGUI::makeDlg()
{
	cGUIDlg * dlg = new cGUIDlg;
	m_dlg.push_back(dlg);
	SetLast(dlg);
	return dlg;
}

PREDICATE_M(dlg, new, 0)
{
	g_gui->makeDlg();
	return true; 
}

PREDICATE_M(dlg, select, 1)
{
	return g_gui->DlgSelect(A1);
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
	bool a1 = A1 = dlg->rect.p1.x;
	bool a2 = A2 = dlg->rect.p1.y;
	bool a3 = A3 = dlg->rect.p2.x;
	bool a4 = A4 = dlg->rect.p2.y;
	return a1 && a2 && a3 && a4;
}

PREDICATE_M(dlg, getPos, 2)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	bool a1 = A1 = dlg->rect.p1.x;
	bool a2 = A2 = dlg->rect.p1.y;
	return a1 && a2;
}

PREDICATE_M(dlg, getPos2, 2)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	bool a1 = A1 = dlg->rect.p2.x;
	bool a2 = A2 = dlg->rect.p2.y;
	return a1 && a2;
}

PREDICATE_M(dlg, setID, 1)
{
	g_gui->GetLastDlg()->id = static_cast<const char *>(A1);
	return true;
}

PREDICATE_M(dlg, setTestKey, 0)
{
	g_gui->GetLastDlg()->Keys.mode = Dlg::Keys::Mode::always;
	return true;
}

PREDICATE_M(dlg, resetTestKey, 0)
{
	g_gui->GetLastDlg()->Keys.mode = Dlg::Keys::Mode::none;
	return true;
}

PREDICATE_M(dlg, setModal, 0)
{
	g_gui->GetLastDlg()->modal = true;
	return true;
}

PREDICATE_M(dlg, setCloseOut, 0)
{
	g_gui->GetLastDlg()->closeOut=true;
	return true;
}

PREDICATE_M(dlg, setCloseCmd, 1)
{
	g_gui->GetLastDlg()->closeCmd = static_cast<const char *>(A1);
	return true;
}

PREDICATE_M(dlg, setRect, 4)
{
	cGUIDlg * dlg = g_gui->GetLastDlg();
	dlg->rect = iRect(A1, A2, A3, A4);
	return true;
}

PREDICATE_M(dlg, addCmd, 1)
{
	g_gui->GetLastDlg()->Keys.AddCmd(A1);
	return true;
}

PREDICATE_M(dlg, addKey, 1)
{
	g_gui->GetLastDlg()->Keys.Last().KeyboardKey(A1);
	return true;
}

PREDICATE_M(dlg, addMouseKey, 1)
{
	g_gui->GetLastDlg()->Keys.Last().MouseKey(A1);
	return true;
}

PREDICATE_M(dlg, addShift, 0)
{
	g_gui->GetLastDlg()->Keys.Last().Shift();
	return true;
}

PREDICATE_M(dlg, addCtrl, 0)
{
	g_gui->GetLastDlg()->Keys.Last().Ctrl();
	return true;
}

PREDICATE_M(dlg, addAlt, 0)
{
	g_gui->GetLastDlg()->Keys.Last().Alt();
	return true;
}

PREDICATE_M(dlg, addWheelUp, 0)
{
	g_gui->GetLastDlg()->Keys.Last().WheelUp();
	return true;
}

PREDICATE_M(dlg, addWheelDown, 0)
{
	g_gui->GetLastDlg()->Keys.Last().WheelDown();
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// GUIItem EXPORT
//////////////////////////////////////////////////////////////////////////////////////////////////


bool cGUI::makeItem(const std::string & className)
{
	#define GUI_ITEM_NEW(Class) if(className == #Class) { makeItem<Class>(); return true; }

	GUI_ITEM_NEW(cGUIItem)
	GUI_ITEM_NEW(cGUITitle)
	GUI_ITEM_NEW(cGUIButton)
	GUI_ITEM_NEW(cGUICheck)
	GUI_ITEM_NEW(cGUIRadio)
	GUI_ITEM_NEW(cGUIEdit)
	GUI_ITEM_NEW(cGUITile)
	GUI_ITEM_NEW(cGUITileMap)
	GUI_ITEM_NEW(cGUIColorPick)

#undef GUI_ITEM_NEW
	return false;
}

PREDICATE_M(gui, itemNew, 1)
{
	return g_gui->makeItem(A1);
}

PREDICATE_NONDET_M(gui, itemSelect, 1)
{
	auto call = PL_foreign_control(handle);
	if(call == PL_PRUNED)
		return true;
	cGUIDlg * d = g_gui->GetLastDlg();
	size_t idx = call == PL_FIRST_CALL ? 0 : PL_foreign_context(handle);
	DlgItems & items = d->Items;
	PlFrame fr;
	for(;idx < items.size(); ++idx)
	{
		cGUIItem * i = items[idx];
		if(i->id.empty())
			continue;
		if(A1 = PlCompound(i->id.c_str()))
		{
			i->Select();
			if(++idx == items.size())
				return true;
			else
				PL_retry(idx);
		}
		fr.rewind();
	}
	return false;
}

PREDICATE_M(gui, itemGetX, 1)
{
	return A1 = g_gui->GetLastItem()->rect.p1.x;
}
PREDICATE_M(gui, itemGetY, 1)
{
	return A1 = g_gui->GetLastItem()->rect.p1.y;
}
PREDICATE_M(gui, itemGetX2, 1)
{
	return A1 = g_gui->GetLastItem()->rect.p2.x;
}
PREDICATE_M(gui, itemGetY2, 1)
{
	return A1 = g_gui->GetLastItem()->rect.p2.y;
}

PREDICATE_M(gui, itemGetTxt, 1)
{
	return A1 = g_gui->GetLastItem()->txt;
}

PREDICATE_M(gui, itemGetHidden, 0)
{
	return g_gui->GetLastItem()->hidden;
}

PREDICATE_M(gui, itemGetValue, 1)
{
	return A1 = g_gui->GetLastItem()->value;
}

PREDICATE_M(gui, itemGetDisable, 1)
{
	return A1 = g_gui->GetLastItem()->disable;
}


PREDICATE_M(gui, itemGetColor, 1)
{
	return A1 = static_cast<int64>(g_gui->GetLastItem()->color[0]);
}


PREDICATE_M(gui, itemSetID, 1)
{
	g_gui->GetLastItem()->id = static_cast<const char *>(A1);
	return true;
}

PREDICATE_M(gui, itemSetRect, 4)
{
	g_gui->GetLastItem()->rect = iRect(A1, A2, A3, A4);
	return true;
}

PREDICATE_M(gui, itemSetX, 1)
{
	g_gui->GetLastItem()->rect.p1.x = A1;
	return true;
}

PREDICATE_M(gui, itemSetY, 1)
{
	g_gui->GetLastItem()->rect.p1.y = A1;
	return true;
}

PREDICATE_M(gui, itemSetX2, 1)
{
	g_gui->GetLastItem()->rect.p2.x = A1;
	return true;
}

PREDICATE_M(gui, itemSetY2, 1)
{
	g_gui->GetLastItem()->rect.p2.y = A1;
	return true;
}


PREDICATE_M(gui, itemSetTxt, 1)
{
	g_gui->GetLastItem()->txt = static_cast<const char *>(A1);
	return true;
}

PREDICATE_M(gui, itemSetColor, 2)
{
	cGUIItem* item = g_gui->GetLastItem();
	int ColorIdx = A1;
	if(ColorIdx < 0 || ColorIdx > 3)
		throw PlDomainError("Color index", A1);
	item->color[ColorIdx] = static_cast<dword>(static_cast<int64>(A2));
	return true;
}

PREDICATE_M(gui, itemSetStyle, 1)
{
	g_gui->GetLastItem()->style = A1;
	return true;
}

PREDICATE_M(gui, itemSetTxtAlign, 1)
{
	g_gui->GetLastItem()->txtAlign = A1;
	return true;
}

PREDICATE_M(gui, itemSetToolTip, 1)
{
	g_gui->GetLastItem()->tooltip = static_cast<const char *>(A1);
	return true;
}

PREDICATE_M(gui, itemSetImg0, 1)
{
	g_gui->GetLastItem()->img0 = A1;
	return true;
}

PREDICATE_M(gui, itemSetImg1, 1)
{
	g_gui->GetLastItem()->img1 = A1;
	return true;
}

PREDICATE_M(gui, itemSetCmdAction, 1)
{
	g_gui->GetLastItem()->cmdAction = static_cast<const char *>(A1);
	return true;
}

PREDICATE_M(gui, itemSetHidden, 1)
{
	g_gui->GetLastItem()->hidden = static_cast<int>(A1) != 0;
	return true;
}

PREDICATE_M(gui, itemSetDisable, 1)
{
	g_gui->GetLastItem()->disable = static_cast<int>(A1) != 0;
	return true;
}

PREDICATE_M(gui, itemSetValue, 1)
{
	g_gui->GetLastItem()->value = A1;
	return true;
}

PREDICATE_M(gui, itemSetGroup, 1)
{
	g_gui->GetLastItem()->group = A1;
	return true;
}

PREDICATE_M(gui, itemSetTxtColor, 1)
{
	g_gui->GetLastItem()->txtColor = static_cast<dword>(static_cast<int64>(A1));
	return true;
}


PREDICATE_M(gui, itemSetImgAlign, 1)
{
	g_gui->GetLastItem()->imgAlign = A1;
	return true;
}

PREDICATE_M(gui, itemSetImgColor, 1)
{
	g_gui->GetLastItem()->imgColor = static_cast<dword>(static_cast<int64>(A1));
	return true;
}

PREDICATE_M(gui, itemSetGuiTileScale, 1)
{
	if(cGUITile * t = dynamic_cast<cGUITile *>(g_gui->GetLastItem()))
	{
		t->scale = A1;
		return true;
	}
	throw PlResourceError("cGUITile");
}

PREDICATE_M(gui, itemSetGuiTileShrink, 1)
{
	if(cGUITile * t = dynamic_cast<cGUITile *>(g_gui->GetLastItem()))
	{
		t->shrink = A1;
		return true;
	}
	throw PlResourceError("cGUITile");
}

PREDICATE_M(gui, itemGetGuiTileScale, 1)
{
	if(cGUITile * t = dynamic_cast<cGUITile *>(g_gui->GetLastItem()))
		return A1 = t->scale;
	throw PlResourceError("cGUITile");
}

PREDICATE_M(gui, itemGetGuiTileShrink, 1)
{
	if(cGUITile * t = dynamic_cast<cGUITile *>(g_gui->GetLastItem()))
		return A1 = t->shrink;
	throw PlResourceError("cGUITile");
}

PREDICATE_M(gui, itemSetGuiTileMapScale, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
	{
		t->scale = A1;
		return true;
	}
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemGetGuiTileMapScale, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
		return A1 = t->scale;
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemSetGuiTileMapSnap, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
	{
		t->snap = static_cast<int>(A1) != 0;
		return true;
	}
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemGetGuiTileMapSnap, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
		return A1 = t->snap ? 1 : 0;
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemSetGuiTileMapGrid, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
	{
		t->grid = static_cast<int>(A1) != 0;
		return true;
	}
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemGetGuiTileMapGrid, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
		return A1 = t->grid ? 1 : 0;
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemSetGuiTileMapAxes, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
	{
		t->axes = static_cast<int>(A1) != 0;
		return true;
	}
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemGetGuiTileMapAxes, 1)
{
	if(cGUITileMap * t = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
		return A1 = t->axes ? 1 : 0;
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemSetGuiTileMapMap, 4)
{
	if(cGUITileMap * i = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
	{
		i->SetMap(iRect(A1, A2, A3, A4));
		return true;
	}
	throw PlResourceError("cGUITileMap");

}

PREDICATE_M(gui, itemGetGuiTileMapMap, 4)
{
	if(cGUITileMap * i = dynamic_cast<cGUITileMap *>(g_gui->GetLastItem()))
	{
		iRect m = i->GetMap();
		bool r1 = A1 = m.p1.x;
		bool r2 = A2 = m.p1.y;
		bool r3 = A3 = m.p2.x;
		bool r4 = A4 = m.p2.y;
		return r1 & r2 & r3 && r4;
	}
	throw PlResourceError("cGUITileMap");
}

PREDICATE_M(gui, itemColorPickLoadImg, 1)
{
	if(cGUIColorPick * i = dynamic_cast<cGUIColorPick *>(g_gui->GetLastItem()))
	{
		i->LoadImg(A1);
		return true;
	}
	throw PlResourceError("cGUIColorPick");

}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9System.h"
#include "E9App.h"
#include "DizScript.h"
#include "DizGame.h"
#include "DizSound.h"
#include "DizApp.h"
#include "DizDebug.h"

#include <SWI-Stream.h>


#include <string>
#include <iostream>
#include <set>

static ssize_t Log_write(void *handle, char *buffer, size_t size)
{ 
	size_t ch = reinterpret_cast<size_t>(handle);
	d9Log::printBuf(ch, buffer, size);
	return size;
}

cDizScript g_script;

//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript class
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizScript::cDizScript()
{
	
	Soutput->functions->write = &Log_write;
	Serror->functions->write = &Log_write;

	Soutput->handle = reinterpret_cast<void *>(LOGGS);
	Serror->handle  = reinterpret_cast<void *>(LOGAPP);

}

bool cDizScript::Init()
{

	module = PL_new_module(PL_new_atom("user"));
	_event= PL_pred(PL_new_functor(PL_new_atom("event"), 1), PL_new_module(PL_new_atom("handlers")));

	_gameInit = PL_new_functor(PL_new_atom("gameInit"), 0);
	_gameStart = PL_new_functor(PL_new_atom("gameStart"), 0);
	_gameUpdate = PL_new_functor(PL_new_atom("gameUpdate"), 0);
	_gameAfterUpdate = PL_new_functor(PL_new_atom("gameAfterUpdate"), 0);
	_roomOpen = PL_new_functor(PL_new_atom("roomOpen"), 0);
	_roomClose = PL_new_functor(PL_new_atom("roomClose"), 0);
	_roomOut = PL_new_functor(PL_new_atom("roomOut"), 0);
	_collision = PL_new_functor(PL_new_atom("collision"), 2);
	_fall = PL_new_functor(PL_new_atom("fall"), 0);
	_jump = PL_new_functor(PL_new_atom("jump"), 3);
	_playerUpdate = PL_new_functor(PL_new_atom("playerUpdate"), 0);
	_action = PL_new_functor(PL_new_atom("action"), 0);
	_menu = PL_new_functor(PL_new_atom("menu"), 0);
	_drawHud = PL_new_functor(PL_new_atom("drawHud"), 0);
	_musicLoop = PL_new_functor(PL_new_atom("musicLoop"), 0);
	_debug = PL_new_functor(PL_new_atom("debug"), 0);
	_reloadMap = PL_new_functor(PL_new_atom("reloadMap"), 0);

	gameInit();

	return true;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// HANDLERS
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizScript::CallHandler(functor_t handler, const PlTermv &av )
{		
	try
	{
		PlCall(module, _event, PlCompound(handler, av));
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		dlog(L"Dizzy.pl 1111 not found: %s", static_cast<LPCWSTR>(ee));
	}
}

ssize_t read_stream(void *handle, char *buf, size_t bufsize)
{
	if(f9File * f = reinterpret_cast<f9File *>(handle))
		return static_cast<ssize_t>(f->Read(buf, bufsize));
	return -1;
}

int close_stream(void *handle)
{
	if(f9File * f = reinterpret_cast<f9File *>(handle))
		f->Close();
	return 0;
}


static IOFUNCTIONS stream_functions =
{ (Sread_function)  read_stream,
  (Swrite_function) nullptr,
  (Sseek_function)  nullptr,
  (Sclose_function) close_stream,
					nullptr
};


PREDICATE_M(core, open_resource, 2)
{
	if(f9File * f = F9_FileOpen(A1))
		return PL_unify_stream(A2, Snew(f, SIO_INPUT|SIO_FBUF, &stream_functions));
	return false;
}



PREDICATE_M(core, ticktime, 1)
{
	return A1 = static_cast<int>(GetTickCount());
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// MATERIALS
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, materialDensity, 2)
{
	int mat = A1;
	if(mat<0 || mat>=MAT_MAX)
		throw PlDomainError("material number", A1);
	if(A2.type() == PL_VARIABLE)
		return A2 = g_game.materials[mat].density;
	g_game.materials[mat].density = PlAtom(A2);
	return true;
}

PREDICATE_M(core, materialColor, 2)
{
	int mat = A1;
	if(mat<0 || mat>=MAT_MAX)
		throw PlDomainError("material number", A1);
	if(A2.type() == PL_VARIABLE)
		return A2 = g_game.materials[mat].color;
	int64 l = A2;
	g_game.materials[mat].color = static_cast<int>(l);
	return true;
}

PREDICATE_M(core, materialRead, 5)
{
	int x1 = A1;
	int y1 = A2;
	int w = A3;
	int h = A4;
	int x2 = x1 + w;
	int y2 = y1 + h;
	int mat = 0;
	for(int y=y1;y<y2;y++)
		mat |= g_game.MatMap(x1, x2, y);
	A5 = mat;
	return true;
}

PREDICATE_M(core, materialDensityRead, 5)
{
	std::set<PlAtom> dens; 
	int x1 = A1;
	int y1 = A2;
	int w = A3;
	int h = A4;
	int x2 = x1 + w;
	int y2 = y1 + h;
	for(iV2 p(x1, y1);p.y<y2;p.y++)
		for(p.x=x1;p.x<x2;p.x++)
			dens.insert(g_game.DensMap(p));
	PlTail l(A5);
	for(const PlAtom &a: dens)
		l.append(a);
	return l.close();
}


PREDICATE_M(core, materialCheckFree, 4)
{
	int x1 = A1;
	int y1 = A2;
	int x2 = A3;
	int y2 = A4;
	
	for(iV2 p(x1, y1);p.y<y2;p.y++)
		for(p.x=x1;p.x<x2;p.x++)
			if( g_game.DensMap(p) == g_game.hard ) 
				return false;
	return true;
}

PREDICATE_M(core, materialGetFreeDist, 7)
{
	int x1 = A1;
	int y1 = A2;
	int x2 = A3;
	int y2 = A4;
	int dir = A5;
	bool hardonly = (A6 == g_game.hard); // hard or void
	iV2 i;
	if(dir==0) // [top to bottom)
	{
		for( i.y=y1; i.y<y2; i.y++ )
		{
			for(i.x=x1;i.x<x2;i.x++)
			{
				if(hardonly)
				{
					if( g_game.DensMap(i) == g_game.hard ) 
						return A7 = i.y-y1; // blocked
				}
				else
				{
					if( g_game.DensMap(i) != g_game._void ) 
						return A7 = i.y-y1; // blocked
				}
			}
		}

		return A7 = y2-y1; // all free
	}
	else
	if(dir==1) // (bottom to top]
	{
		for( i.y=y2-1; i.y>=y1; i.y-- )
		{
			for(i.x=x1;i.x<x2;i.x++)
			{
				if(hardonly)
				{
					if( g_game.DensMap(i) == g_game.hard ) 
						return A7 = y2-1-i.y; // blocked
				}
				else
				{
					if( g_game.DensMap(i) != g_game._void ) 
						return A7 = y2-1-i.y; // blocked
				}
			}
		}
		return A7 = y2-y1; // all free
	}
	else 
		throw PlDomainError("direction unsuported yet", A5);
	return A7 = 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// OBJECTS
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, objPresent, 1)
{
	int idx = A1;
	if(g_map.InvalidObjIndex(idx)) 
		throw PlException("invalid object index");
	g_game.ObjPresent(idx); 
	return true;
}

PREDICATE_M(core, objPresentCount, 1)
{
	return A1 = static_cast<int>(g_game.m_obj.size());
}

PREDICATE_M(core, objPresentIdx, 2)
{
	int presentidx = A1;
	if(presentidx < 0 || static_cast<size_t>(presentidx) >= g_game.m_obj.size()) 
		throw PlException("invalid present index");	
	return A2 = g_game.m_obj[presentidx];
}

PREDICATE_M(core, objPresentGather, 0)
{
	g_game.ObjGather();
	return true;
}

PREDICATE_M(core, colliderSnapDistance, 5)
{
	int x1 = A1;
	int y1 = A2;
	int x2 = A3;
	int y2 = A4;

	int dist = 0; // max distance from box bottom to collider top (if collider top is inside box)
	int cx1,cy1,cx2,cy2;
	for(int idx: g_game.m_collider)
	{
		tBrush & obj = g_map.ObjGet(idx);
		if( obj.Get(BRUSH_DISABLE)!=0 ) continue; // only enabled objects
		if(!(obj.Get(BRUSH_COLLIDER) & COLLIDER_HARD)) continue; // only those that need it
		obj.MakeBBW(cx1,cy1,cx2,cy2);
		if( x2<=cx1 || x1>=cx2 ) continue; // not intersecting
		if(y1<=cy1 && cy1<y2) dist = std::max(dist,y2-cy1);
	}
	return A5 = dist;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// FONT
//////////////////////////////////////////////////////////////////////////////////////////////////
PREDICATE_M(core, fontLoad, 1)
{
	return g_paint.fonts.Load(A1, 0);
}

PREDICATE_M(core, fontLoad, 2)
{
	return g_paint.fonts.Load(A1, A2);
}

PREDICATE_M(core, fontUnload, 1)
{
	g_paint.fonts.Unload(A1);
	return true;
}

PREDICATE_M(core, fontUnload, 0)
{
	g_paint.fonts.Unload(0);
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// HUD
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, hudDrawTile, 5)
{
	PlTerm r1 = A2;
	PlTerm r2 = A3;
	iV2 p1(r1[1], r1[2]);
	iV2 p2(r2[1], r2[2]);
	g_paint.hud.DrawTile(A1,
		iRect(p1, p1 + iV2(r1[3], r1[4])),
		iRect(p2, p2 + iV2(r2[3], r2[4])),
		static_cast<int>(A4), A5 );
	return true;
}

PREDICATE_M(core, hudDrawText, 4)
{
	PlTerm r = A2;
	iV2 p(r[1], r[2]);
	g_paint.hud.DrawText( A1, iRect(p, p + iV2(r[3], r[4])), A3, A4 );
	return true;
}

PREDICATE_M(core, hudGetTextWidth, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = w;
}


PREDICATE_M(core, hudGetTextHeight, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = h;
}

PREDICATE_M(core, hudGetTextColumns, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = c;
}

PREDICATE_M(core, hudGetTextRows, 2)
{
	int w,h,c,r;
	g_paint.hud.GetTextSize(A1, w, h, c, r);
	return A2 = r;
}

PREDICATE_M(core, hudClipping, 1)
{
	PlTerm r = A1;	
	iRect dst( r[0], r[1], r[2], r[3] );
	dst.p2 += dst.p1;
	g_paint.hud.SetClipping( dst );
	return true;
}

PREDICATE_M(core, hudFont, 1)
{
	g_paint.hud.font(A1);
	return true;
}

PREDICATE_M(core, hudShader, 1)
{
	g_paint.hud.shader(BlendAtom().Get(A1));
	return true;
}

PREDICATE_M(core, hudColor, 1)
{
	int64 v;
	if(!PL_get_int64(A1, &v))
		return false;
	g_paint.hud.color(static_cast<dword>(v));
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// KEYBOARD
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, keyboardRead, 1)
{
	return A1 = I9_IsReady() ? (I9_GetKeyQCount() ? I9_GetKeyQValue(0) : 0) : 0;
}

PREDICATE_M(core, keyboardStatus, 1)
{
	if(!I9_IsReady()) return false;
	int key = A1;
	if( key < 0 || key >= I9_KEYBOARD_KEYS ) return false;
	return I9_GetKeyValue(key);
}

PREDICATE_M(core, keyboardCodeToChar, 2)
{
	if(!I9_IsReady()) return false;
	int key = A1;
	if( key<0 || key>=I9_KEYBOARD_KEYS ) return false;
	return A2 = I9_GetKeyAscii(key);
}

PREDICATE_M(core, keyboardCharToCode, 2)
{
	if(!I9_IsReady()) return false;
	int chr = A1;
	return A2 = I9_FindKeyByAscii(chr);
}

PREDICATE_M(core, joystickStatus, 1)
{
	if(!I9_IsReady()) return false;
	int key = A1;
	if( key < 0 || key >= I9_JOY_KEYS ) return false;
	return I9_GetKeyValue(I9_JOY_FIRSTKEY(0) + key);
}

PREDICATE_M(core, joystickAxe, 1)
{
	if(!I9_IsReady()) return false;
	int axe = A1;
	if( axe < 0 || axe >= I9_JOY_AXES ) return false;
	return A2 = I9_GetAxeValue(I9_JOY_FIRSTAXE(0)+axe);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// DEBUG
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, dlog, 1)
{
	LPCWSTR msg = A1;
	dlog(LOGGS, L"%s", msg);
	return true;
}

PREDICATE_M(core, dl, 1)
{
	LPCWSTR msg = A1;
	dlog(LOGGS, L"%s", msg);
	dlog(LOGGS, L"\n");
	return true;
}

PREDICATE_M(core, debugData, 2)
{
	int64 slot = A1;
	g_dizdebug.SlotSet(static_cast<size_t>(slot), A2);
	return true;
}

PREDICATE_M(core, ini, 4)
{
	static char tmp_fullpath[256];
	if(!GetFullPathName(A1, 255, tmp_fullpath, NULL )) 
		return false;

	PlTerm val = A4;	
	if(val.type() == PL_VARIABLE)
	{
		char value[256]; value[0]=0;
		if(GetPrivateProfileString(A2, A3, "", value, 256, tmp_fullpath ) == 0)
			return false;
		return A4 = value;
	}
	WritePrivateProfileString(A2, A3, val, tmp_fullpath );
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

std::string cDizScript::UpdateStack() const
{
	PlFrame fr;
	try
	{
		PlTermv av(1);
		PlQuery q("update", "debug", av);
		if(q.next_solution())
			return static_cast<char *>(av[0]);
	}
	catch(PlException const &)
	{
		return std::string("<exception>");
	}
	return std::string("[]");
}
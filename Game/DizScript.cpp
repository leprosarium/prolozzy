//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9System.h"
#include "DizScript.h"
#include "DizGame.h"
#include "DizSound.h"
#include "DizApp.h"
#include "DizDebug.h"
#include "eInput.h"

#include <SWI-Stream.h>


#include <string>
#include <iostream>
#include <set>

static ssize_t Log_write(void *handle, char *buffer, size_t size)
{ 
	Channel ch = static_cast<Channel>(reinterpret_cast<size_t>(handle));
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

	Soutput->handle = reinterpret_cast<void *>(Channel::scr);
	Serror->handle  = reinterpret_cast<void *>(Channel::app);

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
		files->FileClose(f);
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
	if(f9File * f = files->OpenFile(A1))
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

PREDICATE_M(core, setObjPresent, 1)
{
	g_game.ObjPresent(g_map.brushPtr(A1)); 
	return true;
}

PREDICATE_NONDET_M(core, objPresent, 1)
{
	auto call = PL_foreign_control(handle);
	if(call == PL_PRUNED)
		return true;
	PlTerm t = A1;
	if(!(t = g_map.brush))
		return false;
	PlTerm br = t[1];
	if (br.type() != PL_VARIABLE)
		return std::find(g_game.m_obj.begin(), g_game.m_obj.end(), g_map.brushPtrNoEx(br)) != g_game.m_obj.end();

	size_t idx = call == PL_FIRST_CALL ? 0 : PL_foreign_context(handle);
	if(idx < g_game.m_obj.size() && (br = g_game.m_obj[idx]))
		if(++idx == g_game.m_obj.size())
			return true;
		else
			PL_retry(idx);
	return false;
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
	for(auto obj: g_game.m_collider)
	{
		if(obj->disable) continue; // only enabled objects
		if(!(obj->collider & COLLIDER_HARD)) continue; // only those that need it
		iRect c = obj->rect();
		if( x2<=c.p1.x || x1 >= c.p2.x ) continue; // not intersecting
		if(y1 <= c.p1.y && c.p1.y < y2) dist = std::max(dist,y2-c.p1.y);
	}
	return A5 = dist;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// KEYBOARD
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, keyboardStatus, 1)
{
	int key = A1;
	if( key < 0 || key >= 0xFF ) return false;
	return einput->keyValue(key);
}

PREDICATE_M(core, joystickStatus, 1)
{
	int key = A1;
	if( key < 0 || key >= 32 ) return false;
	return einput->joystickButtonValue(key);
}

PREDICATE_M(core, joystickAxe, 1)
{
	int axe = A1;
	if( axe < 0 || axe >= 8) return false;
	return A2 = einput->joystickAxeValue(axe);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// DEBUG
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, dlog, 1)
{
	LPCWSTR msg = A1;
	dlog(Channel::scr, L"%s", msg);
	return true;
}

PREDICATE_M(core, dl, 1)
{
	LPCWSTR msg = A1;
	dlog(Channel::scr, L"%s", msg);
	dlog(Channel::scr, L"\n");
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
		std::string value;
		if(ini_get(tmp_fullpath, A2, A3) >> value)
			return A4 = value;
		return false;
	}
	ini_set<std::string>(tmp_fullpath, A2, A3, val);
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
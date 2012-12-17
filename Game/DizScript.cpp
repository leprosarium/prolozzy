//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "E9System.h"
#include "D9Debug.h"
#include "E9App.h"
#include "DizScript.h"
#include "DizGame.h"
#include "DizSound.h"
#include "DizApp.h"
#include "DizDebug.h"

#include "SWI-Stream.h"

#include <string>
#include <iostream>




static ssize_t Log_write(void *handle, char *buffer, size_t size)
{ 
  int ch = reinterpret_cast<int>(handle);
  D9_LogBuf(ch, buffer, size);
  return size;
}

cDizScript g_script;

//////////////////////////////////////////////////////////////////////////////////////////////////
// DizScript class
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizScript::cDizScript()
{
	guard(cDizScript::cDizScript)
	
	Soutput->functions->write = &Log_write;
	Serror->functions->write = &Log_write;

	Soutput->handle = reinterpret_cast<void *>(LOGGS);
	Serror->handle  = reinterpret_cast<void *>(LOGAPP);

	unguard()
}

void registerHandler(functor_t &handler, const char * name, int ar = 0) 
{
	atom_t na = PL_new_atom(name);
	handler = PL_new_functor(na, ar);
	PL_unregister_atom(na);
}

bool cDizScript::Init()
{
	guard(cDizScript::Init)

	bool ok = Consult();

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
	_reloadScript = PL_new_functor(PL_new_atom("reloadScript"), 0);


	gameInit();

	return true;
	unguard()
}

void cDizScript::Start()
{
	guard(cDizScript::Start)
	gameStart();
	unguard()
}

void cDizScript::Update()
{
	guard(cDizScript::Update)
	unguard()
}

bool cDizScript::Consult()
{
	PlFrame fr;
	bool ok = true;
	D9_LogStore(true);
	try
	{
		ok = PlCall("consult", PlTerm("Data/Scripts/dizzy")) == TRUE;
	}
	catch(const PlException & e)
	{
		dlog("Consult exception: %s", static_cast<LPCSTR>(e));
		ok = false;
	}
	if(!ok)
		sys_msgbox( E9_GetHWND(), D9_LogGetBuffer(), "DizzyAGE Consult Error", MB_OK );
	D9_LogStore(false);
	return ok;
}

bool cDizScript::Reload()
{
	guard(cDizScript::Reload)
	Consult();
	dlog(LOGAPP,"Script reloaded.\n");
	return true;
	unguard()
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// HANDLERS
//////////////////////////////////////////////////////////////////////////////////////////////////

void cDizScript::CallHandler(functor_t handler, const PlTermv &av )
{		

	guard(cDizScript::CallHandler)	

	try
	{
		PlCall(module, _event, PlCompound(handler, av));
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		dlog("Dizzy.pl 1111 not found: %s", static_cast<LPCSTR>(ee));
	}

	unguard()
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
		return A2 = g_game.m_matcolor[mat];
	int64 l;
	if(PL_get_int64(A2, &l))
		g_game.m_matcolor[mat] = l;
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
	{
		for(int x=x1;x<x2;x++)
		{
			mat |= (1<<g_game.MatMap(x,y));
		}
	}
	A5 = mat;
	return true;
}

PREDICATE_M(core, materialDensityRead, 5)
{

	int x1 = A1;
	int y1 = A2;
	int w = A3;
	int h = A4;
	int x2 = x1 + w;
	int y2 = y1 + h;
	int density = 0;
	for(int y=y1;y<y2;y++)
	{
		for(int x=x1;x<x2;x++)
		{
			density |= (1<<(g_game.m_matdensity[g_game.MatMap(x,y)]));
		}
	}
	A5 = density;
	return true;
}


PREDICATE_M(core, materialCheckFree, 4)
{
	int x1 = A1;
	int y1 = A2;
	int x2 = A3;
	int y2 = A4;
	
	for(int iy=y1;iy<y2;iy++)
		for(int ix=x1;ix<x2;ix++)
			if( g_game.DensMap(ix,iy) == g_game.hard ) 
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
	int ix,iy;
	if(dir==0) // [top to bottom)
	{
		for( iy=y1; iy<y2; iy++ )
		{
			for(ix=x1;ix<x2;ix++)
			{
				if(hardonly)
				{
					if( g_game.DensMap(ix,iy) == g_game.hard ) 
						return A7 = iy-y1; // blocked
				}
				else
				{
					if( g_game.DensMap(ix,iy) != g_game._void ) 
						return A7 = iy-y1; // blocked
				}
			}
		}

		return A7 = y2-y1; // all free
	}
	else
	if(dir==1) // (bottom to top]
	{
		for( iy=y2-1; iy>=y1; iy-- )
		{
			for(ix=x1;ix<x2;ix++)
			{
				if(hardonly)
				{
					if( g_game.DensMap(ix,iy) == g_game.hard ) 
						return A7 = y2-1-iy; // blocked
				}
				else
				{
					if( g_game.DensMap(ix,iy) != g_game._void ) 
						return A7 = y2-1-iy; // blocked
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
	if(presentidx < 0 || presentidx >= g_game.m_obj.size()) 
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
	for(std::vector<int>::const_iterator i = g_game.m_collider.begin(), e = g_game.m_collider.end(); i != e; ++i)
	{
		tBrush & obj = g_map.ObjGet(*i);
		if( obj.Get(BRUSH_DISABLE)!=0 ) continue; // only enabled objects
		if(!(obj.Get(BRUSH_COLLIDER) & COLLIDER_HARD)) continue; // only those that need it
		obj.MakeBBW(cx1,cy1,cx2,cy2);
		if( x2<=cx1 || x1>=cx2 ) continue; // not intersecting
		if(y1<=cy1 && cy1<y2) dist = MAX(dist,y2-cy1);
	}
	return A5 = dist;
}


//////////////////////////////////////////////////////////////////////////////////////////////////
// Sound
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, sampleLoad, 1)
{
	return g_sound.SampleLoad(A1, 0);
}

PREDICATE_M(core, sampleLoad, 2)
{
	return g_sound.SampleLoad(A1, A2);
}

PREDICATE_M(core, sampleUnload, 0)
{
	g_sound.SampleUnload(0);
	return true;
}

PREDICATE_M(core, sampleUnload, 1)
{
	int group = A1;
	g_sound.SampleUnload(group);
	return true;
}


PREDICATE_M(core, samplePlay, 1)
{
	g_sound.SamplePlay(PlAtom(A1), 0);
	return true;
}

PREDICATE_M(core, samplePlay, 2)
{
	return A2 = g_sound.SamplePlay(PlAtom(A1), 0);
}

PREDICATE_M(core, samplePlay, 3)
{
	return A3 = g_sound.SamplePlay(PlAtom(A1), A2);
}

PREDICATE_M(core, samplePlaying, 2)
{
	return A2 = g_sound.SamplePlaying(A1);
}

PREDICATE_M(core, sampleStop, 1)
{
	g_sound.SampleStop(A1);
	return true;
}

PREDICATE_M(core, sampleStopAll, 0)
{
	g_sound.SampleStopAll(-1);
	return true;
}

PREDICATE_M(core, sampleStopAll, 1)
{
	g_sound.SampleStopAll(PlAtom(A1));
	return true;
}

PREDICATE_M(core, sampleVolume, 1)
{
	g_sound.SampleVolume(A1);
	return true;
}

PREDICATE_M(core, musicLoad, 1)
{
	return g_sound.MusicLoad(A1, 0);
}


PREDICATE_M(core, musicLoad, 2)
{
	return g_sound.MusicLoad(A1, A2);
}

PREDICATE_M(core, musicUnload, 1)
{
	g_sound.MusicUnload(A1);
	return true;
}

PREDICATE_M(core, musicUnload, 0)
{
	g_sound.MusicUnload(0);
	return true;
}

PREDICATE_M(core, musicFade, 2)
{
	g_sound.MusicFade(A1, A2);
	return true;
}

PREDICATE_M(core, musicPlay, 1)
{
	return  g_sound.MusicPlay(PlAtom(A1), 0) == 0;
}

PREDICATE_M(core, musicPlay, 2)
{
	return  g_sound.MusicPlay(PlAtom(A1), A2) == 0;
}

PREDICATE_M(core, musicPlaying, 1)
{
	return A1 = g_sound.MusicPlaying();
}

PREDICATE_M(core, musicPosition, 1)
{
	return A1 = g_sound.MusicPosition();
}

PREDICATE_M(core, musicStop, 0)
{
	g_sound.MusicStop();
	g_sound.m_musicnext = -1;
	g_sound.m_musicvol = 0.0;
	return true;
}

PREDICATE_M(core, musicVolume, 1)
{
	g_sound.MusicVolume(A1);
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// FONT
//////////////////////////////////////////////////////////////////////////////////////////////////
PREDICATE_M(core, fontLoad, 1)
{
	return g_paint.FontLoad(A1, 0);
}

PREDICATE_M(core, fontLoad, 2)
{
	return g_paint.FontLoad(A1, A2);
}

PREDICATE_M(core, fontUnload, 1)
{
	g_paint.FontUnload(A1);
	return true;
}

PREDICATE_M(core, fontUnload, 0)
{
	g_paint.FontUnload(0);
	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// HUD
//////////////////////////////////////////////////////////////////////////////////////////////////

PREDICATE_M(core, hudDrawTile, 5)
{
	int tileid = A1; 
	PlTerm r1 = A2;
	PlTerm r2 = A3;
	int flip  = A4;
	int frame = A5;


	iRect dst( r1[1], r1[2], r1[3], r1[4] );
	dst.x2 += dst.x1;
	dst.y2 += dst.y1;
	iRect src( r2[1], r2[2], r2[3], r2[4] );
	src.x2 += src.x1;
	src.y2 += src.y1;
	g_paint.HUDDrawTile( tileid, dst, src, flip, frame );
	return true;
}

PREDICATE_M(core, hudDrawText, 4)
{
	PlTerm r = A2;
	iRect dst( r[1], r[2], r[3], r[4] );
	dst.x2 += dst.x1;
	dst.y2 += dst.y1;
	g_paint.HUDDrawText( A1, dst, A3, A4 );
	return true;
}

PREDICATE_M(core, hudGetTextWidth, 2)
{
	int w,h,c,r;
	g_paint.HUDGetTextSize(A1, w, h, c, r);
	return A2 = w;
}


PREDICATE_M(core, hudGetTextHeight, 2)
{
	int w,h,c,r;
	g_paint.HUDGetTextSize(A1, w, h, c, r);
	return A2 = h;
}

PREDICATE_M(core, hudGetTextColumns, 2)
{
	int w,h,c,r;
	g_paint.HUDGetTextSize(A1, w, h, c, r);
	return A2 = c;
}

PREDICATE_M(core, hudGetTextRows, 2)
{
	int w,h,c,r;
	g_paint.HUDGetTextSize(A1, w, h, c, r);
	return A2 = r;
}

PREDICATE_M(core, hudClipping, 1)
{
	PlTerm r = A1;	
	iRect dst( r[0], r[1], r[2], r[3] );
	dst.x2 += dst.x1;
	dst.y2 += dst.y1;
	g_paint.HudClipping( dst );
	return true;
}

PREDICATE_M(core, hudFont, 1)
{
	g_paint.m_hudfont = A1;
	return true;
}

PREDICATE_M(core, hudShader, 1)
{
	int shd = A1;
	if(shd < 0 || shd < SHADER_MAX) 
		shd = SHADER_BLEND;
	g_paint.m_hudshader = shd;
	return true;
}

PREDICATE_M(core, hudColor, 1)
{
	int64 v;
	if(!PL_get_int64(A1, &v))
		return false;
	g_paint.m_hudcolor = static_cast<int>(v);
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
	char * msg = A1;
	dlog(LOGGS, msg);
	return true;
}

PREDICATE_M(core, dl, 1)
{
	char * msg = A1;
	dlog(LOGGS, msg);
	dlog(LOGGS, "\n");
	return true;
}

PREDICATE_M(core, debugData, 2)
{
	int slot = A1;
	char * msg = A2;
	g_dizdebug.SlotSet(slot, msg);
	return true;
}

//int gsDebugCool( gsVM* vm )
//{
//	guard(gsDebugCool)
//	if(gs_params(vm)==0)
//	{
//		gs_pushint(vm,E9_AppGetInt(E9_APP_COOL));
//		return 1;
//	}
//	if(!gs_ckparams(vm,1))		return 0;
//	if(!gs_cktype(vm,0,GS_INT)) return 0;
//	dlog(LOGAPP,"DebugCool %s\n", gs_toint(vm,0)?"on":"off");
//	int val = gs_toint(vm,0);
//	E9_AppSetInt(E9_APP_COOL,val);
//	return 0;
//	unguard()
//}
//
//int gsDebugLog( gsVM* vm )
//{
//	guard(gsDebugLog)
//	if(!gs_ckparams(vm,1))		GS_RETURNINT(vm,0);
//	if(!gs_cktype(vm,0,GS_INT)) GS_RETURNINT(vm,0);
//	int i;
//	if(gs_toint(vm,0))
//	{
//		for(i=0;i<=D9_LOG_CHANNELMAX;i++) D9_LogOpenChannel(i,true);
//		dlog(LOGAPP,"DebugLog on\n");
//	}
//	else
//	{
//		dlog(LOGAPP,"DebugLog off\n");
//		for(i=0;i<=D9_LOG_CHANNELMAX;i++) D9_LogOpenChannel(i,false);
//		D9_LogOpenChannel(LOGSYS,true); // always open
//		D9_LogOpenChannel(LOGERR,true); // always open
//	}
//	return 0;
//	unguard()
//}
//
//int gsDebugDev( gsVM* vm )
//{
//	guard(gsDebugDev)
//	if(gs_params(vm)==0)
//	{
//		gs_pushint(vm,cDizDebug::m_developer);
//		return 1;
//	}
//	if(!gs_ckparams(vm,1))		return 0;
//	if(!gs_cktype(vm,0,GS_INT)) return 0;
//	dlog(LOGAPP,"DebugDev %s\n", gs_toint(vm,0)?"on":"off");
//	int val = gs_toint(vm,0);
//	cDizDebug::m_developer = val;
//	g_paint.Layout();
//	return 0;
//	unguard()
//}
//

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
///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"

#include <sstream>

#include "DizDebug.h"
#include "DizGame.h"
#include "SWI-cpp-m.h"

#include "SWI-Stream.h"

#include "E9App.h"

cDizDebug g_dizdebug;

BOOL cDizDebug::m_developer = false;

bool IS_WORD_CHAR(char c)
{
	return ('0'<=c && c<='9') || ('A'<=c && c<='Z') || ('a'<=c && c<='z') || c=='_';
}
#define COLOR_INFO		0xff408040
#define COLOR_SLOTS		0xff004080
#define COLOR_INPUT		0xff808080
#define COLOR_BBKGR		0xa0000000


int PrologDebugDraw()
{
	g_dizdebug.PrologDraw();
	return 0;
}

void cDizDebug::PrologDraw()
{
	if(!R9_IsReady()) return; // avoid painting if render is not ready
	R9_CheckDevice(); // check for lost device
	if(R9_BeginScene())
	{
		R9_Clear(g_game.mapColor()|0xff000000);
		g_dizdebug.con.Draw();
		g_dizdebug.slots.Draw();
		g_dizdebug.input.Draw();

		R9_EndScene();
		R9_Present();
	}
}

void cDizDebug::PrologUpdate()
{
	float dtime = (float)E9_AppGetInt(E9_APP_DELTATIME) / 1000.0f;
	if(I9_IsReady()) I9_Update(dtime);
	if(m_console) con.Update();
}

ssize_t cDizDebug::PrologRead(char *buffer, size_t size)
{

	e9AppCallback pnt = E9_AppSetCallback(E9_APP_ONPAINT, PrologDebugDraw);
	MSG	msg;
	std::string cmd = con.lastLine();
	input.setCmd(cmd);
	input.Open();
 	while(true)
	{
		while( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
		{
			if( GetMessage( &msg, NULL, 0, 0) )
			{
				TranslateMessage( &msg );
				DispatchMessage( &msg );
			}
			else
			{
				return 0;
			}
		}
		PrologDraw();
		PrologUpdate();
		bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);
		size_t pos = 0;
		for(int i=0;i<I9_GetKeyQCount();i++)
		{
			if(!I9_GetKeyQValue(i))
				continue;
			int key = I9_GetKeyQCode(i);
			char ascii = shift ? I9_GetKeyShifted(key) : I9_GetKeyAscii(key);
			if(key == I9K_RETURN)
				ascii = '\n';
			if(!ascii) 
				continue;
			buffer[pos++]=ascii;
			cmd += ascii;
		}
		if(pos)
		{
			input.setCmd(cmd);
			D9_LogBuf(LOGDBG, buffer, pos);
			E9_AppSetCallback(E9_APP_ONPAINT, pnt);
			return pos;
		}
		Sleep(10);
	}
	return 0;
}

static ssize_t Log_read(void *handle, char *buffer, size_t size)
{
	return g_dizdebug.PrologRead(buffer, size);

}

///////////////////////////////////////////////////////////////////////////////////////////////////
cDizDebug::cDizDebug() : con(CON_LINES)
{
	Soutput->functions->read = Log_read;
	m_console = false;
}


bool cDizDebug::Init()
{
	Layout();
	
	// config
	char inifile[256];
	strcpy( inifile, file_getfullpath(GetIniFile()) );
	ini_getint( inifile, "ADVANCED",	"dev",		&m_developer );
	if(*g_cfg.GetInfoValue("game_protection")) 
		m_developer=false; // no developer for protected games

	// console
	m_console = false;
	D9_LogSetCallback( Con_LogCallback );

	//log
	dlog(LOGAPP, L"%S v%S\n",GAME_NAME,GAME_VERSION);
	R9_LogCfg(R9_GetCfg(),R9_GetApi());
	if(m_developer) dlog(LOGAPP, L"Developer mode on.\n");

	return true;
}

void cDizDebug::Done()
{
}

bool cDizDebug::Update()
{
	if(!I9_IsReady()) return true;

	// debug developer hidden key
	if(DeveloperKey() && !input.IsOpened())
	{
		m_developer = !m_developer; 
		g_paint.Layout();
	}
	if(!m_developer) return true;

	// console input
	if(m_console) input.Update(); // update console input command
	if(input.IsOpened())	// if during console input command
	{
		g_player.m_debug = 1; // don't update player
		return true;
	}

	// console
	if(I9_GetKeyDown(I9K_GRAVE) || I9_GetKeyDown(I9K_F2)) // show-hide
	{
		m_console=!m_console;
		g_paint.Layout();
		Layout();
	}
	if(m_console) 
		con.Update();

	// navigation
	NavigationUpdate();

	// draw mode
	if(I9_GetKeyDown(I9K_F4))
	{
		g_game.m_drawmode++;
		if(g_game.m_drawmode>=DRAWMODE_MAX) g_game.m_drawmode=0;
		dlog( LOGAPP, L"draw mode %i\n", g_game.m_drawmode );
	}

	// map reload full WARNING: all items are reloaded too, so your inventory...
	if(I9_GetKeyDown(I9K_F6))
	{
		g_map.Reload();
		g_game.ObjGather();
	}
	
	// resize
	bool ctrl  = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);
	if(ctrl && I9_GetKeyDown(I9K_MINUS)) // -
	{
		if(g_cfg.m_scale==0) g_cfg.m_scale = g_paint.m_scale;
		g_cfg.m_scale--;
		if(g_cfg.m_scale==0) g_cfg.m_scale=1;
		g_paint.Layout();
	}
	else
	if(ctrl && I9_GetKeyDown(I9K_EQUALS)) // +
	{
		g_cfg.m_scale++;
		iV2 size = g_game.screenSizeBorder * g_cfg.m_scale;
		if( size.x > R9_GetWidth() || size.y > R9_GetHeight() )
			g_cfg.m_scale--;
		g_paint.Layout();
	}

	return true;
}

#ifdef _DEBUG
//#define GAME_DRAWDEBUG				// draw debug (only on developper mode)
#endif

void cDizDebug::Draw()
{
	if(!m_developer) return;
	if(!m_console)
	{
		R9_DrawBar(fRect(2,2,4+8*R9_CHRW,4+R9_CHRH),0xffff0000);
		R9_DrawText(fV2(4,4),"DEV-MODE",0xffffffff);
		return;
	}
	info.Draw();
	con.Draw();
	slots.Draw();
	input.Draw();

}

void cDizDebug::Layout()
{
	renderSize = iV2(R9_GetWidth(), R9_GetHeight());
	iV2 scr = g_game.screenSize * g_paint.m_scale;
	con.Layout(iRect(iV2(), renderSize - iV2(0, (INFO_LINES+1)*R9_CHRH)));
	slots.Layout(iRect(scr.x, 0, renderSize.x, scr.y));
	info.Layout(iRect(iV2(0, renderSize.y - INFO_LINES * R9_CHRH), renderSize));
	input.Layout(iRect(0, renderSize.y - (INFO_LINES+1)*R9_CHRH, renderSize.x, renderSize.y - INFO_LINES * R9_CHRH));
}

bool cDizDebug::DeveloperKey()
{
	// quick type D E V E L O P E R to toggle
	static int tickold = 0;
	static char keybuf[16] = {32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,0};
	int keycnt = 16;
	if( I9_GetKeyQCount() )
	{
		for(int i=0;i<I9_GetKeyQCount();i++)
		{
			if(!I9_GetKeyQValue(i)) // up
			{
				char chr = I9_GetKeyAscii(I9_GetKeyQCode(i));
				if(chr<=0) chr=32;
				keybuf[keycnt-1] = chr; 
				memmove(keybuf,keybuf+1,keycnt-1);
				keybuf[keycnt-1]=0;
			}
		}
		if(strstr(keybuf,"developer"))
		{
			memset(keybuf,32,keycnt-1);
			return true;
		}
	}
	// erase each 10 sec
	int tick = sys_gettickcount();
	if(tick-tickold<10000)
	{
		tickold=tick;
		memset(keybuf,32,keycnt-1);
	}
	return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INFO
///////////////////////////////////////////////////////////////////////////////////////////////////
void Info::Draw()
{
	R9_DrawBar(rect,COLOR_BBKGR);
	fV2 p(rect.x1 + R9_CHRW, rect.y1);
	
	// script
	R9_DrawText( p, g_script.UpdateStack().c_str(), COLOR_INFO);
	p.y+=R9_CHRH;

	// player
	std::ostringstream os;
	os << "room=(" << g_game.roomX() << "," << g_game.roomY() << "), player=(" << g_player.x() << "," << g_player.y() << ")";
	R9_DrawText( p, os.str().c_str(), COLOR_INFO);

}

///////////////////////////////////////////////////////////////////////////////////////////////////
// NAVIGATION
///////////////////////////////////////////////////////////////////////////////////////////////////
void cDizDebug::NavigationUpdate()
{
	bool ctrl  = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);

	int mw = g_map.Width();
	int mh = g_map.Height();
	int px = g_player.x();
	int py = g_player.y();
	if(ctrl)
	{
		if(I9_GetKeyDown(I9K_LEFT))		px-=Room::Size.x;
		if(I9_GetKeyDown(I9K_RIGHT))	px+=Room::Size.x;
		if(I9_GetKeyDown(I9K_UP))		py-=Room::Size.y;
		if(I9_GetKeyDown(I9K_DOWN))		py+=Room::Size.y;
	}
	else
	if(shift)
	{
		if(I9_GetKeyDown(I9K_LEFT))		px-=4;
		if(I9_GetKeyDown(I9K_RIGHT))	px+=4;
		if(I9_GetKeyDown(I9K_UP))		py-=4;
		if(I9_GetKeyDown(I9K_DOWN))		py+=4;
	}
	
	if(px>=mw*Room::Size.x) px=mw*Room::Size.x-1;
	if(py>=mh*Room::Size.y) py=mh*Room::Size.y-1;
	if(px<0) px=0;
	if(py<0) py=0;
	g_player.x(px);
	g_player.y(py);
	g_player.m_debug = (ctrl||shift);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// CONSOLE
///////////////////////////////////////////////////////////////////////////////////////////////////

void Console::Update()
{
	int step = (I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT)) ? 1 : lines;
	if(I9_GetKeyDown(I9K_PGUP))		PageUp(step); else
	if(I9_GetKeyDown(I9K_PGDN))		PageDown(step); else
	if(I9_GetKeyDown(I9K_HOME))		PageBegin = 0; else
	if(I9_GetKeyDown(I9K_END))		End();
}

void Console::Draw()
{
	if(empty()) return;
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(rect);
	R9_DrawBar(rect, COLOR_BBKGR);
	fV2 p(rect.x1 + R9_CHRW, rect.y1 + R9_CHRH);
	for(auto line = begin() + PageBegin; p.y < rect.y2 && line != end(); ++line, p.y += static_cast<float>(R9_CHRH)) // show as many as you can
		R9_DrawText(p, line->c_str(), D9_LogGetChannelColor(line->Ch));
	R9_SetClipping(oldclip);
}

void Console::Push(int ch, const std::string & str)
{
	std::string::size_type st = 0, e = 0;
	while ((e = str.find('\n', st)) != std::string::npos) {
		PushToken(Line(ch, str.substr(st, e - st)));
		PushNewLine();
		st = e + 1;
	}
	PushToken(Line(ch, str.substr(st)));
}

void Console::PushToken(const Line & line)
{
	if(empty())
		push_back(line);
	else
	{
		Line & last = back();
		if(last.empty())
			last.Ch = line.Ch;
		last += line;
	}
}

void Console::PushNewLine()
{
	push_back(Line());
	if (size() > Cap)
		pop_front();
	if(PageBegin + lines + 1 == size())
		End();
}


void cDizDebug::ConsolePush( int ch, LPCWSTR msg )
{
	if(msg)
		con.Push(ch, WideStringToMultiByte(msg));
}

void cDizDebug::Con_LogCallback( int ch, LPCWSTR msg )
{
	g_dizdebug.ConsolePush( ch, msg );
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// SLOTS
///////////////////////////////////////////////////////////////////////////////////////////////////

void Slots::Draw()
{
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(rect);
	R9_DrawBar(rect, COLOR_BBKGR);
	fV2 p(rect.x1 + R9_CHRW, rect.y1 + R9_CHRH);
	for(size_t i=0; i < SLOT_COUNT && p.y < rect.y2; i++, p.y += R9_CHRH)
		R9_DrawText(p, slots[i].c_str(), 0xff0040a0);
	R9_SetClipping(oldclip);
}

void cDizDebug::SlotSet( size_t slot, LPCWSTR text )
{
	if(text)
		slots.Set(slot, WideStringToMultiByte(text));
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INPUT
///////////////////////////////////////////////////////////////////////////////////////////////////
void Input::Update()
{
	if(I9_GetKeyDown(I9K_INSERT) && !open) 
	{ 
		open = true; 
		complete = 0; 
		histcrt = hist.end();
		return;
	}
	if(!open) return;

	// action keys
	bool ctrl = I9_GetKeyValue(I9K_LCONTROL) || I9_GetKeyValue(I9K_RCONTROL);
	bool shift = I9_GetKeyValue(I9K_LSHIFT) || I9_GetKeyValue(I9K_RSHIFT);

	if(I9_GetKeyUp(I9K_ESCAPE) || I9_GetKeyDown(I9K_INSERT))
	{	
		open = false; 
		complete = 0; 
		return; 
	}
	if(I9_GetKeyUp(I9K_RETURN)) 
	{ 
		open = false; 
		complete = 0; 
		Execute(); 
		return; 
	}
	if(I9_GetKeyDown(I9K_LEFT) && crt > cmd.begin())	
	{
		if(!ctrl || !SkipWordLeft())
			crt--;
		complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_RIGHT) && crt < cmd.end())
	{
		if(!ctrl || !SkipWordRight()) 
			crt++;
		complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_BACKSPACE) && crt > cmd.begin()) // delete back - shift left
	{
		crt = cmd.erase(--crt);
		complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_DELETE) && crt < cmd.end()) // delete - shift left
	{
		auto c = crt - cmd.begin();
		if(ctrl)
			cmd = cmd.substr(0, c); 
		else
			cmd.erase(crt);
		crt = cmd.begin() + c;
		complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_HOME)) { crt = cmd.begin(); complete = 0; }
	else
	if(I9_GetKeyDown(I9K_END)) { crt = cmd.end(); complete = 0; }
	else
	if(I9_GetKeyDown(I9K_TAB) )	{ AutoComplete(); }
	else
	if(I9_GetKeyDown(I9K_DOWN))
	{
		if(histcrt < hist.end()) ++histcrt;
		if(histcrt != hist.end())
		{
			cmd = *histcrt;
			crt = cmd.end();
		}
		complete = 0;
	}
	else
	if(I9_GetKeyDown(I9K_UP))
	{
		if(histcrt > hist.begin()) --histcrt;
		if(histcrt != hist.end())
		{
			cmd = *histcrt;
			crt = cmd.end();
		}
		complete = 0;
	}
	else
	{
		// char keys
		for(int i=0;i<I9_GetKeyQCount();i++)
		{
			if(!I9_GetKeyQValue(i)) continue;
			int key = I9_GetKeyQCode(i);
			char ascii = shift ? I9_GetKeyShifted(key) : I9_GetKeyAscii(key);
			if(!ascii) continue;
			crt = cmd.insert(crt, ascii);
			crt++;
			complete = 0;
		}
	}

	
}

void Input::Draw()
{
	if(!open) return;
	R9_DrawBar(rect,COLOR_BBKGR);
	fV2 p(rect.x1, rect.y1);
	R9_DrawText(p, ">", COLOR_INPUT);
	p.x += R9_CHRW;
	int t = sys_gettickcount() % 800;
	float crtx = p.x + R9_CHRW * (crt-cmd.begin())-1;
	if(t<500) R9_DrawLine(fV2(crtx, p.y-1), fV2(crtx, rect.y2 + 1));
//	if(t<400) R9_DrawLine(fV2(crtx, rect.y2), fV2(crtx+R9_CHRW, rect.y2));
//	if(t<500) R9_DrawBar(fRect(crtx, p.y-1, crtx + R9_CHRW, rect.y2 + 1), 0xff808080);
	R9_DrawText(p, cmd.c_str(), COLOR_INPUT);
}

void Input::Execute()
{
	if(cmd.empty()) return;
	hist.push_back(cmd);
	if(hist.size() == INPUT_HISTORY) hist.pop_front();
	histcrt = --hist.end();

	try
	{
		PlCall(cmd.c_str());
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		dlog(L"PlException: %s", static_cast<LPCWSTR>(ee));
	}
	cmd.clear();
	crt = cmd.begin();
}

std::string::const_iterator Input::CmdWordBegin() const
{
	std::string::const_iterator b = crt;
	while(b > cmd.begin())
	{
		--b;		
		if(!IS_WORD_CHAR(*b)) { ++b; break; }
	}
	return b;

}
std::string::const_iterator Input::CmdWordEnd() const {

	std::string::const_iterator e = crt;
	for(;e < cmd.end() && IS_WORD_CHAR(*e); ++e);
	return e;
}

void Input::AutoComplete()
{
	auto cmpl = complete;
	complete = 0;

	auto start = CmdWordBegin();
	if(start == cmd.end() || !islower(*start)) return;
	std::string buf = cmd.substr(start - cmd.begin(), crt - start);
	auto name = PL_atom_generator(buf.c_str(), 0);
	if (!name) return;
	std::string prefix = cmd.substr(0, start - cmd.begin());
	std::string suffix = cmd.substr(CmdWordEnd() - cmd.begin());
	std::string match = name;
	size_t nmatches = 1;
	for(;name = PL_atom_generator(buf.c_str(), 1); nmatches++)
		match = match.substr(0, 
					std::mismatch(match.begin(), match.end(), name, 
					[](char c1, char c2) { return tolower(c1) == tolower(c2); }).first - match.begin());

	cmd = prefix + match + suffix;
	auto crtDif = prefix.size() + match.size();
	crt = cmd.begin() + crtDif;
	if(nmatches <= 1) return;
	for(;name = PL_atom_generator(match.c_str(), complete ? 1 : 0); ++complete)
		if(cmpl == complete) 
		{
			++complete;
			cmd = prefix + name + suffix;
			crt = cmd.begin() + crtDif;
			return;
		}	
	complete = 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////

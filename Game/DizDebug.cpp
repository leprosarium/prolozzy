///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"

#include <sstream>
#include <cctype>

#include "DizDebug.h"
#include "DizGame.h"
#include "DizSound.h"
#include "SWI-cpp-m.h"

#include "SWI-Stream.h"

#include "E9App.h"
#include "eInput.h"

cDizDebug g_dizdebug;

bool IS_WORD_CHAR(char c)
{
	return ('0'<=c && c<='9') || ('A'<=c && c<='Z') || ('a'<=c && c<='z') || c=='_';
}
#define COLOR_INFO		0xff408040
#define COLOR_SLOTS		0xff004080
#define COLOR_INPUT		0xff808080
#define COLOR_BBKGR		0xa0000000


int Prolog::Draw()
{
	if(!R9_IsReady()) return 0; // avoid painting if render is not ready
	R9_CheckDevice(); // check for lost device
	if(R9_BeginScene())
	{
		R9_Clear(g_game.mapColor|0xff000000);
		con.Draw();
		slots.Draw();
		input.Draw();

		R9_EndScene();
		R9_Present();
	}
	return 0;
}

static ssize_t Log_read(void *handle, char *buffer, size_t size)
{
	return static_cast<Prolog *>(handle)->Read(buffer, size);
}

Prolog::Prolog(Console & con, Slots & slots) : 
	con(con), 
	slots(slots)
{ 	
	input.setAction([this](const std::string &s){ this->Ready(s); });
}

void Prolog::Open(bool o) {
	PL_set_prolog_flag("tty_control", PL_BOOL, o ? TRUE : FALSE);
	Sinput->functions->read = o ? Log_read : 0;
	Sinput->handle = this;
}

ssize_t Prolog::Read(char *buffer, size_t size)
{
	struct Pnt
	{
		e9App::Callback tmp;
		Pnt(e9App::Callback pnt) : tmp(App.OnPaint) { App.OnPaint = pnt; }; 
		~Pnt() { App.OnPaint = tmp; }
	} pnt([this]() { return this->Draw(); });

	MSG	msg;
	input.Open();

	int mode = PL_ttymode(Suser_input);
	bool single = mode == PL_RAWTTY;
	if(auto prompt = PL_prompt_string(0))
		input.setPrompt(prompt);

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
		App.UpdateClocks();
		eInput::Update(App.DeltaTime() / 1000.0f);
		if(I9_IsReady()) { I9_Update(App.DeltaTime() / 1000.0f); }
		con.Update();
		if(!single) input.Update();
		Draw();
		if(single)
		{
			if(!einput->keyQueue.empty())
			{
				std::string str = WideStringToMultiByte(einput->keyQueue.c_str());
				einput->keyQueue = einput->keyQueue.substr(1);
				if(!str.empty())
				{
					char ch = str[0];
					if(std::isprint(ch))
					{
						buffer[0] = ch;
						return 1;
					}
				}
			}
		}
		else
		{
			if(CmdReady) {
				CmdReady = false;
				Cmd += "\n";
				size_t sz = std::min(Cmd.size(), size);
				memcpy(buffer, Cmd.c_str(), sz);
				Cmd = "";
				PL_prompt_next(0);
				d9Log::printBuf(LOGDBG, buffer, sz);
				return sz;
			}
		}
		Sleep(10);
	}
	return 0;
}



///////////////////////////////////////////////////////////////////////////////////////////////////
cDizDebug::cDizDebug() : con(CON_LINES), _visible(), _active(), prolog(con, slots)
{
	input.setAction(Call);
}


bool cDizDebug::Init()
{
	Layout();
	
	// config
	int dev = 0;
	ini_get(file_getfullpath(GetIniFile()), "ADVANCED",	"dev") >> dev;
	_active = dev != 0;
	if(!g_cfg.Info("game_protection").empty()) 
		_active=false; // no developer for protected games

	// console
	_visible = false;
	d9Log::setCallback([this](int ch,LPCWSTR msg) {this->ConsolePush( ch, msg );});

	//log
	dlog(LOGAPP, L"%S v%S\n",GAME_NAME,GAME_VERSION);
	R9_LogCfg(R9_GetCfg(),R9_GetApi());
	if(_active) dlog(LOGAPP, L"Developer mode on.\n");

	return true;
}

bool cDizDebug::Update()
{
	if(!I9_IsReady()) return true;

	// debug developer hidden key
	if(dev.Update() && !input.IsOpened())
	{
		_active = !_active; 
		g_paint.Layout();
	}
	if(!_active) return true;

	// console input
	if(_visible) input.Update(); // update console input command
	if(input.IsOpened())	// if during console input command
	{
		g_player.m_debug = true; // don't update player
		return true;
	}

	// console
	if(einput->isKeyDown(DIK_GRAVE) || einput->isKeyDown(DIK_F2)) // show-hide
	{
		_visible=!_visible;
		g_paint.Layout();
		Layout();
		prolog.Open(_visible);
	}
	if(_visible) 
		con.Update();

	// navigation
	NavigationUpdate();

	// draw mode
	if(einput->isKeyDown(DIK_F4))
		g_game.NextDrawMode();

	// map reload full WARNING: all items are reloaded too, so your inventory...
	if(einput->isKeyDown(DIK_F6))
	{
		g_map.Reload();
		g_game.ObjGather();
	}
	
	// resize
	if(einput->ctrl())
	if(einput->isKeyDown(DIK_MINUS))
	{
		if(g_cfg.m_scale==0) g_cfg.m_scale = g_paint.scale;
		g_cfg.m_scale--;
		if(g_cfg.m_scale==0) g_cfg.m_scale=1;
		g_paint.Layout();
	}
	else
	if(einput->isKeyDown(DIK_EQUALS))
	{
		g_cfg.m_scale++;
		iV2 size = g_game.screenSizeBorder * g_cfg.m_scale;
		if( size.x > R9_GetWidth() || size.y > R9_GetHeight() )
			g_cfg.m_scale--;
		g_paint.Layout();
	}

	return true;
}

void cDizDebug::Draw()
{
	if(!_active) return;
	if(!_visible)
	{
		R9_DrawBar(fRect(2,2,4+8*ChrW,4+ChrH),0xffff0000);
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
	iV2 renderSize(R9_GetWidth(), R9_GetHeight());
	iV2 scr = g_game.screenSize * g_paint.scale;
	con.Layout(iRect(iV2(), renderSize - iV2(0, (INFO_LINES+1)*ChrH)));
	slots.Layout(iRect(scr.x, 0, renderSize.x, scr.y));
	info.Layout(iRect(iV2(0, renderSize.y - INFO_LINES * ChrH), renderSize));
	iRect inp = iRect(0, renderSize.y - (INFO_LINES+1)*ChrH, renderSize.x, renderSize.y - INFO_LINES * ChrH);
	input.Layout(inp);
	prolog.Layout(inp);
}

bool Developer::Update()
{
	// quick type D E V E L O P E R to toggle
	if(!einput->keyQueue.empty())
	{
		buf += einput->keyQueue;
		if(buf.find(L"developer") != std::wstring::npos)
		{
			buf.clear();
			return true;
		}
	}
	// erase each 10 sec
	int tick = sys_gettickcount();
	if(tick-tickold<10000)
	{
		tickold = tick;
		buf.clear();
	}
	return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// INFO
///////////////////////////////////////////////////////////////////////////////////////////////////
void Info::Draw()
{
	R9_DrawBar(rect,COLOR_BBKGR);
	fV2 p = rect.p1 + fV2(ChrW, 0);
	
	// script
	R9_DrawText( p, g_script.UpdateStack(), COLOR_INFO);
	p.y+=ChrH;

	// player
	std::ostringstream os;
	os << "room=(" << g_game.roomPos.x << "," << g_game.roomPos.y << "), player=(" << g_player.pos.x << "," << g_player.pos.y << ")";
	os << " voices=" << g_sound.samples.playingVoices();
	R9_DrawText( p, os.str(), COLOR_INFO);

}

///////////////////////////////////////////////////////////////////////////////////////////////////
// NAVIGATION
///////////////////////////////////////////////////////////////////////////////////////////////////
void cDizDebug::NavigationUpdate()
{
	bool ctrl  = einput->ctrl();
	bool shift = einput->shift();

	iV2 p = g_player.pos;
	if(ctrl)
	{
		if(einput->isKeyDown(DIK_LEFT))		p.x-=Room::Size.x;
		if(einput->isKeyDown(DIK_RIGHT))	p.x+=Room::Size.x;
		if(einput->isKeyDown(DIK_UP))		p.y-=Room::Size.y;
		if(einput->isKeyDown(DIK_DOWN))		p.y+=Room::Size.y;
	}
	else
	if(shift)
	{
		if(einput->isKeyDown(DIK_LEFT))		p.x-=4;
		if(einput->isKeyDown(DIK_RIGHT))	p.x+=4;
		if(einput->isKeyDown(DIK_UP))		p.y-=4;
		if(einput->isKeyDown(DIK_DOWN))		p.y+=4;
	}
	
	iV2 sz = g_map.size() * Room::Size;
	if(p.x >= sz.x) p.x = sz.x - 1;
	if(p.y >= sz.y) p.y = sz.y - 1;
	if(p.x<0) p.x=0;
	if(p.y<0) p.y=0;
	g_player.pos = p;
	g_player.m_debug = (ctrl||shift);
}

void cDizDebug::Call(const std::string & cmd)
{
	try
	{
		PlCall(cmd.c_str());
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		dlog(L"PlException: %s", static_cast<LPCWSTR>(ee));
	}
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// CONSOLE
///////////////////////////////////////////////////////////////////////////////////////////////////

void Console::Update()
{
	int step = einput->shift() ? 1 : lines;
	if(einput->isKeyDown(DIK_PGUP))		PageUp(step); else
	if(einput->isKeyDown(DIK_PGDN))		PageDown(step); else
	if(einput->isKeyDown(DIK_HOME))		PageBegin = 0; else
	if(einput->isKeyDown(DIK_END))		End();
}

void Console::Draw()
{
	if(empty()) return;
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(rect);
	R9_DrawBar(rect, COLOR_BBKGR);
	fV2 p = rect.p1 + fV2(ChrW, ChrH);
	for(auto line = begin() + PageBegin; p.y < rect.p2.y && line != end(); ++line, p.y += static_cast<float>(ChrH)) // show as many as you can
		R9_DrawText(p, *line, d9Log::getColor(line->Ch));
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// SLOTS
///////////////////////////////////////////////////////////////////////////////////////////////////

void Slots::Draw()
{
	fRect oldclip = R9_GetClipping();
	R9_SetClipping(rect);
	R9_DrawBar(rect, COLOR_BBKGR);
	fV2 p  = rect.p1 + fV2(ChrW, ChrH);
	for(size_t i=0; i < SLOT_COUNT && p.y < rect.p2.y; i++, p.y += ChrH)
		R9_DrawText(p, slots[i], 0xff0040a0);
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
	if(einput->isKeyDown(DIK_INSERT) && !open) 
	{ 
		einput->keyQueue.clear();
		open = true; 
		complete = 0; 
		histcrt = hist.end();
		return;
	}
	if(!open) return;

	// action keys
	bool ctrl = einput->ctrl();
	bool shift = einput->shift();

	if(einput->isKeyUp(DIK_ESCAPE) || einput->isKeyDown(DIK_INSERT))
	{	
		open = false; 
		complete = 0; 
		return; 
	}
	if(einput->isKeyUp(DIK_RETURN)) 
	{ 
		open = false; 
		complete = 0; 
		Execute(); 
		return; 
	}
	if(einput->isKeyDown(DIK_LEFT) && crt > cmd.begin())	
	{
		if(!ctrl || !SkipWordLeft())
			crt--;
		complete = 0;
	}
	else
	if(einput->isKeyDown(DIK_RIGHT) && crt < cmd.end())
	{
		if(!ctrl || !SkipWordRight()) 
			crt++;
		complete = 0;
	}
	else
	if(einput->isKeyDown(DIK_BACKSPACE) && crt > cmd.begin()) // delete back - shift left
	{
		crt = cmd.erase(--crt);
		complete = 0;
	}
	else
	if(einput->isKeyDown(DIK_DELETE) && crt < cmd.end()) // delete - shift left
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
	if(einput->isKeyDown(DIK_HOME)) { crt = cmd.begin(); complete = 0; }
	else
	if(einput->isKeyDown(DIK_END)) { crt = cmd.end(); complete = 0; }
	else
	if(einput->isKeyDown(DIK_TAB) )	{ AutoComplete(); }
	else
	if(einput->isKeyDown(DIK_DOWN))
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
	if(einput->isKeyDown(DIK_UP))
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
		if(!einput->keyQueue.empty())
		{
			std::string str = WideStringToMultiByte(einput->keyQueue.c_str());
			for(auto ch: str)
			{
				if(!std::isprint(ch)) continue;
				crt = cmd.insert(crt, ch);
				crt ++;
				complete = 0;
			}
			einput->keyQueue.clear();
		}
	}

	
}

void Input::Draw()
{
	if(!open) return;
	R9_DrawBar(rect,COLOR_BBKGR);
	fV2 p = rect.p1;
	R9_DrawText(p, prompt, COLOR_INPUT);
	p.x += prompt.size() * ChrW;
	int t = sys_gettickcount() % 800;
	float crtx = p.x + ChrW * (crt-cmd.begin())-1;
	if(t<500) R9_DrawLine(fV2(crtx, p.y-1), fV2(crtx, rect.p2.y + 1));
//	if(t<400) R9_DrawLine(fV2(crtx, rect.y2), fV2(crtx+ChrW, rect.y2));
//	if(t<500) R9_DrawBar(fRect(crtx, p.y-1, crtx + ChrW, rect.y2 + 1), 0xff808080);
	R9_DrawText(p, cmd, COLOR_INPUT);
}

void Input::Execute()
{
	if(cmd.empty()) return;
	hist.push_back(cmd);
	if(hist.size() == INPUT_HISTORY) hist.pop_front();
	histcrt = --hist.end();
	if(Action)
		Action(cmd);
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

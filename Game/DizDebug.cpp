///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"

#include "DizDebug.h"
#include "DizApp.h"
#include "DizGame.h"
#include "DizSound.h"
#include "SWI-cpp-m.h"

#include "SWI-Stream.h"

#include "eInput.h"

cDizDebug g_dizdebug;

bool IS_WORD_CHAR(wchar_t c)
{
	return (L'0'<=c && c<=L'9') || (L'A'<=c && c<=L'Z') || (L'a'<=c && c<=L'z') || c==L'_';
}
#define COLOR_INFO		0xff408040
#define COLOR_SLOTS		0xff004080
#define COLOR_INPUT		0xff808080
#define COLOR_BBKGR		0xa0000000


void Prolog::Draw()
{
	if(!R9_IsReady()) return ; // avoid painting if render is not ready
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
}

static ssize_t Log_read(void *handle, char *buffer, size_t size)
{
	return static_cast<Prolog *>(handle)->Read(buffer, size);
}

Prolog::Prolog(Console & con, Slots & slots) : 
	con(con), 
	slots(slots)
{ 	
	input.setAction([this](const std::wstring &s){ this->Ready(s); });
}

void Prolog::Open(bool o) {
	PL_set_prolog_flag("tty_control", PL_BOOL, o ? TRUE : FALSE);
	Sinput->functions->read = o ? Log_read : 0;
	Sinput->handle = this;
	Sinput->encoding = ENC_WCHAR;
}

ssize_t Prolog::Read(char *buffer, size_t size)
{
	input.Open();

	int mode = PL_ttymode(Suser_input);
	bool single = mode == PL_RAWTTY;
	if(auto prompt = PL_prompt_string(0))
		input.setPrompt(MultiByteToWideString(prompt));
	ssize_t readed = 0;

	struct Events 
	{
		App * app;
		std::function<bool()> OnUpdate;
		std::function<void()> OnPaint;
		Events() : app(DizApp::app), OnUpdate(app->OnUpdate), OnPaint(app->OnPaint) 
		{
		}
		~Events() 
		{ 
			app->OnUpdate = OnUpdate;
			app->OnPaint = OnPaint;
		}
	} events;

	DizApp::app->OnPaint = [this]() { Draw(); };
	DizApp::app->OnUpdate = 
	[&]() -> bool
	{
		eInput::Update(DizApp::app->DeltaTime() / 1000.0f);
		con.Update();
		if(!single) input.Update();
		if(single)
		{
			if(!einput->keyQueue.empty())
			{
				std::wstring str = einput->keyQueue;
				einput->keyQueue = einput->keyQueue.substr(1);
				if(!str.empty())
				{
					wchar_t ch = str[0];
					if(iswprint(ch))
					{
						memcpy(buffer, &ch, sizeof(wchar_t));
						readed = 1;
						return false;
					}
				}
			}
		}
		else
		{
			if(CmdReady)
			{
				CmdReady = false;
				Cmd += L"\n";
				size_t sz = std::min(Cmd.size(), size / sizeof(wchar_t));
				memcpy(buffer, Cmd.c_str(), sz * sizeof(wchar_t));
				Cmd = L"";
				PL_prompt_next(0);
				LPWSTR wbuf = reinterpret_cast<LPWSTR>(buffer);
				elog::dbg() << std::wstring(wbuf,  wbuf + sz);
				readed = sz;
				return false;
			}
		}
		return true;
	};

	DizApp::app->Loop();

	return readed * sizeof(wchar_t);
}



///////////////////////////////////////////////////////////////////////////////////////////////////
cDizDebug::cDizDebug() : con(CON_LINES), _visible(), _active(), prolog(con, slots)
{
	input.setAction(Call);
}

class Console::conbuf : public elog::outbuf
{
	dword color;
	Console & con;
public:
	conbuf(dword color, Console & con) : color(color), con(con) {}
	virtual ~conbuf() { sync(); }
	virtual int flushBuffer(int num)
	{
		buffer[num] = 0;
		con.Push(color, buffer);
		return num;
	}
};

bool cDizDebug::Init()
{
	Layout();
	
	// config
	int dev = 0;
	ini_get(file_getfullpath(GetIniFile()), L"ADVANCED", L"dev") >> dev;
	_active = dev != 0;
	if(!g_cfg.Info(L"game_protection").empty()) 
		_active=false; // no developer for protected games

	// console
	_visible = false;
	con.Init();

	//log
	elog::app() << GAME_NAME << " v" << GAME_VERSION << std::endl
		<< "api = " << R9_GetApi() << " " << R9_GetCfg() << std::endl;
	if(_active) elog::app() << "Developer mode on." << std::endl;

	return true;
}

bool cDizDebug::Update()
{
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
		R9_DrawText(fV2(4,4), L"DEV-MODE", 0xffffffff);
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
	std::wostringstream os;
	os << L"room=(" << g_game.roomPos.x << L"," << g_game.roomPos.y << L"), player=(" << g_player.pos.x << L"," << g_player.pos.y << L")";
	os << L" voices=" << g_sound.samples.playingVoices();
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

void cDizDebug::Call(const std::wstring & cmd)
{
	try
	{
		PlCall(cmd.c_str());
	}
	catch(PlException const & e)
	{
		PlException ee(e);
		elog::sys() << "PlException: " << static_cast<LPCWSTR>(ee) << std::endl;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// CONSOLE
///////////////////////////////////////////////////////////////////////////////////////////////////

Console::conbuf * Console::newBuf(dword color)
{
	conbuf * b = new conbuf(color, *this);
	bufs.push_back(b);
	return b;
}

void Console::Init()
{
	conbuf * red = newBuf(DWORD_RED);
	elog::sys().add(red);
	elog::err().add(red);
	if(elog::elog.open())
	{
		elog::nul().add(newBuf(DWORD_GREY));
		elog::eng().add(newBuf(DWORD_BLUE));
		elog::dbg().add(newBuf(DWORD_ORANGE));
		elog::fil().add(newBuf(DWORD_DGREEN));
		conbuf * green = newBuf(DWORD_GREEN);
		conbuf * lred = newBuf(DWORD_LRED);
		elog::inp().add(green);
		elog::rnd().add(lred);
		elog::snd().add(lred);
		elog::scr().add(newBuf(DWORD_LBLUE));
		elog::app().add(green);
	}
}

Console::~Console()
{
	for(auto b: bufs) delete b;
}

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
		R9_DrawText(p, *line, line->color);
	R9_SetClipping(oldclip);
}

void Console::Push(dword color, const std::wstring & str)
{
	std::wstring::size_type st = 0, e = 0;
	while ((e = str.find(L'\n', st)) != std::wstring::npos) {
		PushToken(Line(color, str.substr(st, e - st)));
		PushNewLine();
		st = e + 1;
	}
	PushToken(Line(color, str.substr(st)));
}

void Console::PushToken(const Line & line)
{
	if(empty())
		push_back(line);
	else
	{
		Line & last = back();
		if(last.empty())
			last.color = line.color;
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
		slots.Set(slot, text);
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
			for(auto ch: einput->keyQueue)
			{
				if(!iswprint(ch)) continue;
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



std::wstring::const_iterator Input::CmdWordBegin() const
{
	std::wstring::const_iterator b = crt;
	while(b > cmd.begin())
	{
		--b;		
		if(!IS_WORD_CHAR(*b)) { ++b; break; }
	}
	return b;

}
std::wstring::const_iterator Input::CmdWordEnd() const {

	std::wstring::const_iterator e = crt;
	for(;e < cmd.end() && IS_WORD_CHAR(*e); ++e);
	return e;
}



void Input::AutoComplete()
{
	static std::vector<wchar_t> candidate(255);
	auto cmpl = complete;
	complete = 0;

	auto start = CmdWordBegin();
	if(start == cmd.end() || !islower(*start)) return;
	std::wstring buf = cmd.substr(start - cmd.begin(), crt - start);
	auto name = PL_atom_generator_w(buf.c_str(), & candidate[0], candidate.size(), 0);
	if (!name) return;
	std::wstring prefix = cmd.substr(0, start - cmd.begin());
	std::wstring suffix = cmd.substr(CmdWordEnd() - cmd.begin());
	std::wstring match = name;
	size_t nmatches = 1;
	for(;name = PL_atom_generator_w(buf.c_str(), &candidate[0], candidate.size(), 1); nmatches++)
		match = match.substr(0, 
					std::mismatch(match.begin(), match.end(), name, 
					[](char c1, char c2) { return tolower(c1) == tolower(c2); }).first - match.begin());

	cmd = prefix + match + suffix;
	auto crtDif = prefix.size() + match.size();
	crt = cmd.begin() + crtDif;
	if(nmatches <= 1) return;
	for(;name = PL_atom_generator_w(match.c_str(), &candidate[0], candidate.size(), complete ? 1 : 0); ++complete)
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GAMEDEBUG_H__
#define __GAMEDEBUG_H__

#include "E9System.h"
#include "R9Render.h"
#include "E9Math.h"
#include "SWI-Stream.h"

#include <deque>
#include <functional>

const size_t CON_LINES = 2048;	// total lines
const size_t SLOT_COUNT = 16;	// total slots
const size_t INFO_LINES = 2;	// info slot lines
const size_t INPUT_HISTORY = 16;// input history count

///////////////////////////////////////////////////////////////////////////////////////////////////
// cDizDebug
///////////////////////////////////////////////////////////////////////////////////////////////////
class Line : public std::wstring
{
public:
	Line() : color(DWORD_GREY) {}
	Line(dword color, const std::wstring & str) : std::wstring(str), color(color) {}  
	dword color;
};

class Console : std::deque<Line>
{
	size_t Cap;
	size_t PageBegin;
	fRect rect;
	size_t lines;

	void PagePosUp(size_t pos, size_t step) { PageBegin = pos < step ? 0 : pos - step; }
	void PushToken(const Line & line);
	void PushNewLine();
	void PageUp(size_t step) { PagePosUp(PageBegin, step); } 
	void PageDown(size_t step) { if (PageBegin + step + lines >= size()) End(); else PageBegin += step; }
	void End() { PagePosUp(size(), lines); }
	class conbuf;
	std::vector<conbuf *> bufs;
	conbuf * newBuf(dword color);
public:
	Console(size_t Cap) : Cap(Cap), PageBegin(), lines() {}
	~Console();
	void Init();
	void Layout(const iRect & r) { rect = r; int h = r.Height(); lines = h < ChrH ? 0 : (h / ChrH - 1);}
	void Update();
	void Draw();
	void Push(dword color, const std::wstring & str);
	std::wstring lastLine() const { return empty() ? std::wstring() : back(); }
};

class Slots
{
	std::wstring slots[SLOT_COUNT];
	fRect rect;
public:
	void Layout(const iRect & r) { rect = r; }
	void Draw();
	void Set( size_t slot, const std::wstring & str) { if(slot < SLOT_COUNT) slots[slot] = str; }
};

class Info
{
	fRect rect;
public:
	void Layout(const iRect & r) { rect = r; }
	void Draw();
};

class Input
{
	typedef std::function<void(const std::wstring &)> ActionType;
	bool	open;										// input dialog opend
	int		complete;									// auto-complete count (0=first, 1=next, etc)
	std::wstring prompt;
	std::wstring cmd;									// input command string
	std::wstring::const_iterator crt;					// input currsor
	std::deque<std::wstring> hist;					// input history list of commands
	std::deque<std::wstring>::const_iterator histcrt;// current history cursor
	fRect	rect;
	std::wstring::const_iterator CmdWordBegin() const;
	std::wstring::const_iterator CmdWordEnd() const;
	bool MoveCrt(std::wstring::const_iterator v) { bool moved = v != crt; crt = v; return moved; }
	ActionType Action;
	bool SkipWordLeft() { return MoveCrt(CmdWordBegin()); }	// move cursor left over a word
	bool SkipWordRight() { return MoveCrt(CmdWordEnd()); }	// move cursor left over a word
	void AutoComplete();								// auto-complete function
	void Execute();

public:
	Input(const std::wstring & prompt = L">") : open(), prompt(prompt), crt(cmd.end()), complete(), histcrt(hist.begin()) {}
	void Layout(const iRect & r) { rect = r; }
	void Open() { open = true; }
	bool IsOpened() const { return open; }
	void Update();
	void Draw();
	void setCmd(const std::wstring & c) { cmd = c; crt = cmd.end(); complete = 0;}
	void setPrompt(const std::wstring & p) { prompt = p; }
	void setAction(ActionType a) { Action = a; }
};

class Prolog
{
	Console & con;
	Slots & slots;
	Input input;
	void Draw();
	void Ready(const std::wstring & cmd) { Cmd = cmd; CmdReady = true; }
	std::wstring Cmd;
	bool CmdReady;
public:
	Prolog(Console & con, Slots & slots);
	ssize_t Read(char *buffer, size_t size);
	void Layout( const iRect & rect) { input.Layout(rect); }
	void Open(bool o);
};

class Developer
{
	int tickold;
	std::wstring buf;
public:
	Developer() : tickold() {}
	bool Update();
};

class cDizDebug
{
	Console	con;
	Slots slots;
	Info info;
	Input input;
	Prolog prolog;
	Developer dev;
	bool _visible;									// developer console active
	bool _active;								// developer debug active (set from ini)
	static void Call(const std::wstring & cmd);
	void NavigationUpdate();
public:
	cDizDebug();

	bool Init();
	bool Update();
	void Draw();
	void Layout();					

	void SlotSet( size_t slot, LPCWSTR text );

	bool active() const { return _active; }
	bool visible() const { return _visible; }
};

extern cDizDebug g_dizdebug;

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

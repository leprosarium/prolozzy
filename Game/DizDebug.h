///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GAMEDEBUG_H__
#define __GAMEDEBUG_H__

#include "E9System.h"
#include "D9Debug.h"
#include "R9Render.h"
#include "E9Math.h"
#include "SWI-Stream.h"

#include <deque>

const size_t CON_LINES = 2048;	// total lines
const size_t SLOT_COUNT = 16;	// total slots
const size_t INFO_LINES = 2;	// info slot lines
#define INPUT_SIZE			256		// input cmd size
#define INPUT_HISTORY		16		// input history count

#define	IS_DEVELOPER()		(cDizDebug::m_developer)

///////////////////////////////////////////////////////////////////////////////////////////////////
// cDizDebug
///////////////////////////////////////////////////////////////////////////////////////////////////
class Line : public std::string
{
public:
	Line() : Ch(0) {}
	Line(int Ch, const std::string & str) : std::string(str), Ch(Ch) {}  
	int Ch;
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

public:
	Console(size_t Cap) : Cap(Cap), PageBegin(), lines() {}
	void Layout(const iRect & r) { rect = r; int h = r.Height(); lines = h < R9_CHRH ? 0 : (h / R9_CHRH - 1);}
	void Update();
	void Draw();
	void Push(int ch, const std::string & str);
	std::string lastLine() const { return empty() ? std::string() : back(); }
};

class Slots
{
	std::string slots[SLOT_COUNT];
	fRect rect;
public:
	void Layout(const iRect & r) { rect = r; }
	void Draw();
	void Set( size_t slot, const std::string & str) { if(slot < SLOT_COUNT) slots[slot] = str; }
};

class Info
{
	fRect rect;
public:
	void Layout(const iRect & r) { rect = r; }
	void Draw();
};

class cDizDebug
{
public:
				cDizDebug();

		bool	Init();
		void	Done();
		bool	Update();
		void	Draw();
		void	Layout();					// get render size from render; call it if render changes

		ssize_t PrologRead(char *buffer, size_t size);
		void PrologUpdate();
		void PrologDraw();

		bool	DeveloperKey();

		// navigation
		void	NavigationUpdate();

		// console



		void	ConsolePush( int ch, LPCWSTR msg );
static	void	Con_LogCallback( int ch, LPCWSTR msg );
		void	SlotSet( size_t slot, LPCWSTR text );

		Console	con;
		Slots	slots;
		Info	info;


		// input
inline	bool	InputIsOpened()								{ return m_input_open; }
		void	InputUpdate();
		void	InputDraw();
		void	InputExecute();								// execute command
		void	InputSkipWord( int dir );					// move cursor over a word; dir=-1/1
		void	InputAutoComplete();						// auto-complete function
		bool	m_input_open;								// input dialog opend
		int		m_input_crt;								// input currsor
		int		m_input_complete;							// auto-complete count (0=first, 1=next, etc)
		char	m_input_cmd[INPUT_SIZE];					// input command string
		char	m_input_historycrt;							// current history cursor
		char	m_input_historycnt;							// current history count
		char	m_input_history[INPUT_HISTORY][INPUT_SIZE];	// input history list of commands

		iV2		renderSize;
		bool	m_console;									// developer console active
static	BOOL	m_developer;								// developer debug active (set from ini)
};

extern cDizDebug g_dizdebug;

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

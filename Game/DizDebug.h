///////////////////////////////////////////////////////////////////////////////////////////////////
// DizDebug.h
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __GAMEDEBUG_H__
#define __GAMEDEBUG_H__

#include "E9System.h"
#include "D9Debug.h"
#include "E9Math.h"
#include "SWI-Stream.h"

#include <deque>

#define CON_LINESIZE		80		// characters per line
#define CON_LINES			2048	// total lines
#define SLOT_COUNT			16		// total slots
#define SLOT_SIZE			48		// slot line size
#define	INFO_LINES			2		// info slot lines
#define INPUT_SIZE			256		// input cmd size
#define INPUT_HISTORY		16		// input history count

#define	IS_DEVELOPER()		(cDizDebug::m_developer)

///////////////////////////////////////////////////////////////////////////////////////////////////
// cDizDebug
///////////////////////////////////////////////////////////////////////////////////////////////////
class Line : public std::string
{
public:
	Line(int Ch, const std::string & str) : std::string(str), Ch(Ch) {}  
	int Ch;
};

class Console : std::deque<Line>
{
	typedef std::deque<Line> Strings; 
	size_t Cap;
	size_t PageBegin;
	iterator last;

	void PagePosUp(size_t pos, size_t step) { if (pos < step) Begin(); else PageBegin = pos - step; }
	void PushToken(size_t page, int ch, const std::string & str);
public:
	using Strings::empty;
	using Strings::begin;
	using Strings::end;
	using Strings::const_iterator;
	
	Console(size_t Cap) : Cap(Cap), PageBegin(0), last(end()) {}

	void PageUp(size_t step) { PagePosUp(PageBegin, step); } 
	void PageDown(size_t step, size_t page) { if (PageBegin + step + page >= size()) End(page); else PageBegin += step; }
	void Begin() {	PageBegin = 0; }
	void End(size_t page) {	PagePosUp(size(), page); }

	const_iterator page() const { return begin() + PageBegin; }
	void Push(size_t page, int ch, const std::string & str);
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

		// info
		void	InfoDraw();

		// navigation
		void	NavigationUpdate();

		// console
		iRect	ConsoleGetRect();
		void	ConsoleUpdate();
		void	ConsoleDraw();
		void	ConsolePush( int ch, LPCWSTR msg );
static	void	Con_LogCallback( int ch, LPCWSTR msg );
		Console	con;

		// slots
		iRect	SlotGetRect();
		void	SlotDraw();
		void	SlotSet( int slot, char* text );
		char	m_slot[SLOT_COUNT][SLOT_SIZE];

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

		// util
		void	ConsumeInput();

		int		m_renderw;
		int		m_renderh;
		bool	m_console;									// developer console active
static	BOOL	m_developer;								// developer debug active (set from ini)
};

extern cDizDebug g_dizdebug;

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

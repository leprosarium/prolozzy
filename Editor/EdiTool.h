//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiTool.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __EDITOOL_H__
#define __EDITOOL_H__

#include "EdiDef.h"
#include "EdiPaint.h"

#include "SWI-cpp-m.h"

#define TOOLCMD_PICKBRUSH	0
#define TOOLCMD_PICKCOLOR	1
#define TOOLCMD_TOFRONT		2
#define TOOLCMD_TOBACK		3
#define TOOLCMD_DELETE		4

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiTool
//////////////////////////////////////////////////////////////////////////////////////////////////
class cEdiTool
{
public:
	cEdiTool(const std::string & name);
	virtual ~cEdiTool() {};
	virtual void Switch(bool) { Reset(); }	// called when switched on or off
	virtual	void Reset() = 0;				// always interrupt tool and set it in default mode
	virtual	void Command(int cmd) {};		// general command
	virtual void Update(float dtime) = 0;	// update tool stuff
	virtual void Draw() const = 0;				// draw tool stuff
	virtual	void UserUpdate() = 0;
	virtual bool OnClose() const = 0;
	virtual bool IsBusy() const = 0;

	iV2 axe;
	PlAtom name;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolPaint
//////////////////////////////////////////////////////////////////////////////////////////////////
class cEdiToolPaint : public cEdiTool
{
	Brush * picked;
	enum class Mode {None, Normal, Paint, PickMenu, Pick} mode;
public:
	cEdiToolPaint();
	virtual	void Reset();
	virtual	void Command(int cmd);
	virtual	void UserUpdate();
	virtual void Update(float dtime);
	virtual void Draw() const;
	virtual bool OnClose() const { return mode == Mode::Paint; }
	virtual bool IsBusy() const { return mode == Mode::Paint || mode == Mode::PickMenu || mode == Mode::Pick; }
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolEdit
//////////////////////////////////////////////////////////////////////////////////////////////////
class cEdiToolEdit : public cEdiTool
{
	enum class Mode {Normal, Select, Move} mode;
public:
	cEdiToolEdit();
	virtual	void Reset();
	virtual void Update(float dtime);
	virtual void Draw() const;
	virtual	void UserUpdate();
	virtual bool OnClose() const { return mode == Mode::Select || mode == Mode::Move; }
	virtual bool IsBusy() const { return mode != Mode::Normal; }

	void BrushSelect();					// select brushes in m_rect using m_selop
	void BrushDeselect();				// select brushes in m_rect using m_selop
	void BrushDeleteSelected();
	void BrushCopy();
	void BrushPaste();
	void BrushMoveStart();
	void BrushMove();

	iRect rect;
	enum class SelOp { Sub, New, Add } selop;		// select operation
	iV2 move;		// movement start
	iV2 moved;		// movement offset

	std::vector<Brush *> drag;	 // drag list
};

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

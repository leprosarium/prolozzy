//////////////////////////////////////////////////////////////////////////////////////////////////
// EdiTool.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __EDITOOL_H__
#define __EDITOOL_H__

#include "EdiDef.h"
#include "EdiPaint.h"

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
virtual				~cEdiTool			()							{};

virtual void		Init				()							{}; // first time init
virtual void		Done				()							{}; // last time done
virtual void		Switch				( BOOL on )					{}; // called when switched on or off
virtual	void		Reset				()							{ m_mode=0; } // always interrupt tool and set it in default mode
virtual	void		Command				( int cmd )					{}; // general command

virtual void		Update				( float dtime )				{}; // update tool stuff
virtual void		Draw				()							{}; // draw tool stuff

virtual	void		BeginUserUpdate		()							{};	// called before user update handler
virtual	void		EndUserUpdate		()							{}; // called after user update handler


	std::string m_name;	// tool name
		int			m_mode;				// tool mode (0=default)
		int			m_ax,m_ay;			// axes
		bool		m_isbusy;			// tool is busy - don't mess with it :)
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolPaint
// mode -1=none, 0=normal, 1=paint, 2=pick menu, 3=pick
//////////////////////////////////////////////////////////////////////////////////////////////////
class cEdiToolPaint : public cEdiTool
{
	tBrush m_brushtemp;		// temporary brush
	int m_brushidx;			// picked brush
public:
					cEdiToolPaint		();

virtual void		Switch				( BOOL on );
virtual	void		Reset				();
virtual	void		Command				( int cmd );

virtual	void		BeginUserUpdate		();
virtual	void		EndUserUpdate		();

virtual void		Update				( float dtime );
virtual void		Draw				();



};

//////////////////////////////////////////////////////////////////////////////////////////////////
// cEdiToolEdit
// mode -1=none, 0=normal, 1=select, 2=move
//////////////////////////////////////////////////////////////////////////////////////////////////
class cEdiToolEdit : public cEdiTool
{
public:
					cEdiToolEdit		();
virtual void		Done				();
virtual void		Switch				( BOOL on );
virtual	void		Reset				();

virtual void		Update				( float dtime );
virtual void		Draw				();

		void		BrushSelect			();					// select brushes in m_rect using m_selop
		void		BrushDeselect		();					// select brushes in m_rect using m_selop
		void		BrushDelete			();
		void		BrushCopy			();
		void		BrushPaste			();
		void		BrushMoveStart		();
		void		BrushMove			();

		iRect		m_rect;
		int			m_selop;								// select operation -1=sub, 0=new, 1=add
		iV2			m_move;			// movement start
		iV2			m_moved;		// movement offset

		std::vector<tBrush *> m_drag;	 // drag list


};

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

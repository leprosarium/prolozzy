#ifndef __BRUSH__
#define __BRUSH__

#include "E9Math.h"
#include "R9Render.h"

struct Brush
{
public:
	int layer;		// layer idx
	iV2 pos;		//  position in world
	iV2 size;
	int tile;		// tile id
	int frame;		// current tile animation frame (starts with 0)
	iRect map;		
	int flip;		// flip: 0=none, 1=x, 2=y, 3=xy
	dword color;
	Blend shader;
	int scale;
	std::wstring id;	// id used to access from script
	int material;	// material that brushes will write in material map if draw set correctly (brush only)
	int draw;		// draw mode: 0=don't draw, 1=draw in view, 2=write material in material map (brush only), 3=both (brush only)
	bool disable;	// don't draw, don't update
	int delay;		// frame delay ( should be updated only once in delay frames )
	int anim;		// animation mode: 0=none, 1=play once, 2=play loop
	int collider;	// colider mode: 0=none, 1=call handler, 2=hard collision, 3=both
	bool collision;	// if collision with player 1, else 0 (updated per frame)
	bool select;
		
	Brush() : shader(Blend::Opaque), 
		layer(), 
		tile(-1), 
		frame(), 
		flip(), 
		color(0xffffffff), 
		scale(0), 
		material(),
		draw(),
		disable(),
		delay(),
		anim(),
		collider(),
		collision(),
		select()
	{}
	float mapScale() const { return scale > 0 ? scale / 100.0f : 1.0f; }		
	float mapWith() const   { return ( Is<Flip::R>(flip) ? map.Height() : map.Width())  * mapScale(); }
	float mapHeight() const { return ( Is<Flip::R>(flip) ? map.Width( ) : map.Height()) * mapScale(); }

 	iRect rect() const { return iRect(pos, pos + size); }
	fV2 mapSize() const { fV2 sz = map.Size(); return (Is<Flip::R>(flip) ? sz.Tran() : sz) * mapScale(); }
};

#endif
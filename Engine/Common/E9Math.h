#ifndef __E9MATH_H__
#define __E9MATH_H__

#include "stdafx.h"
#include "E9System.h"

struct iV2;
struct iRect;

struct fV2
{
	float x, y;
	
	fV2()					: x(), y() {}
	fV2( float x, float y )	: x(x), y(y) {}
	fV2( int x, int y) : x(static_cast<float>(x)), y(static_cast<float>(y)) {}
	fV2( float v) : x(v), y(v) {}
	fV2( const iV2 & v );

	fV2		operator - () const { return fV2(-x, -y); }
	fV2	&	operator *=(const fV2 & v) { x *= v.x; y *= v.y; return *this; }
	fV2	&	operator *=(float f) { x *= f; y *= f; return *this; }
	fV2	&	operator /=(const fV2 & v) { x /= v.x; y /= v.y; return *this; }
	fV2	&	operator /=(float f) { x /= f; y /= f; return *this; }

	fV2 &	operator +=(const fV2 & v) { x += v.x; y += v.y; return *this; }
	fV2 &	operator +=(float f) { x += f; y += f; return *this; }
	fV2 &	operator -=(const fV2 & v) { x -= v.x; y -= v.y; return *this; }
	fV2 &	operator -=(float f) { x -= f; y -= f; return *this; }
	float	operator ! () { return sqrt(x*x + y*y); }
	fV2	Tran() const { return fV2(y, x); }

	bool operator <(float v) const { return x < v && y < v; }
    bool operator >(float v) const { return x > v && y > v; }
    bool operator <=(const fV2 & v) const { return x <= v.x && y <= v.y; }
    bool operator >=(const fV2 & v) const { return x >= v.x && y >= v.y; }
    bool operator <(const fV2 & v) const { return x < v.x && y < v.y; }
    bool operator >(const fV2 & v) const { return x > v.x && y > v.y; }	
	bool operator ==(const fV2 & v) const { return x == v.x && y == v.y; }
	bool operator !=(const fV2 & v) const { return ! operator==(v);}

	friend fV2	operator *	( const fV2 & v, float f )			{ return fV2( v.x * f, v.y * f ); }
	friend fV2	operator *	( float f, const fV2 & v )			{ return v * f; }
	friend fV2	operator /	( const fV2 & v, float f )			{ return fV2( v.x / f, v.y / f ); }
	friend fV2	operator +	( const fV2 & v1, const fV2 & v2 )		{ return fV2( v1.x + v2.x, v1.y + v2.y ); }
	friend fV2	operator +	( const fV2 & v, float f )		{ return fV2( v.x + f, v.y + f ); }
	friend fV2	operator +	( float f, const fV2 & v)		{ return v + f; }
	friend fV2	operator -	( const fV2& v1, const fV2 & v2 )		{ return fV2( v1.x - v2.x, v1.y - v2.y ); }
	friend fV2	operator -	( const fV2 & v, float f )		{ return fV2( v.x - f, v.y - f ); }
	friend float operator & ( const fV2 & v1, const fV2 & v2 )		{ return v1.x*v2.x + v1.y*v2.y; }
	friend bool	Nor			( fV2 & v1 ) { float t = !v1; if( t==0.0f ) return false; else { v1 *= (1.0f/t); return true; } }
	friend fV2	Unit		( const fV2 & v1 ) { fV2 v = v1; Nor(v); return v; }
	friend fV2	Lerp		( const fV2 & v1, const fV2 & v2, float s ) { return v1 + s * (v2 - v1); }
	friend fV2	Rotate(const fV2 & v, float angle)
	{
		float sn = std::sin(angle);
		float cs = std::cos(angle);
		return fV2(v.x * cs + v.y * -sn, v.x * sn + v.y * cs);
	}
};

struct iV2
{
	int x, y;
	iV2()						: x(), y() {}
	iV2( int x, int y )			: x(x), y(y) {}
	iV2( int v )				: x(v), y(v) {}
	iV2( float x, float y )		: x(static_cast<int>(x)), y(static_cast<int>(y)) {}
	iV2( const fV2 & v )		: x(static_cast<int>(v.x)), y(static_cast<int>(v.y)) {}

	iV2 &	operator*=	( const  iV2 & v )			{ x *= v.x; y *= v.y; return *this; }
	iV2 &	operator*=	( int s )					{ x *= s; y *= s; return *this; }
	iV2 &	operator/=	( const  iV2 & v )			{ x /= v.x; y /= v.y; return *this; }
	iV2 &	operator/=	( int s )					{ x /= s; y /= s; return *this; }
	iV2 &	operator+=	( const iV2 & v)			{ x += v.x; y += v.y; return *this; }
	iV2 &	operator+=	( int s)					{ x += s; y += s; return *this; }
	iV2 &	operator-=	( const iV2 & v)			{ x -= v.x; y -= v.y; return *this; }
	iV2 &	operator-=	( int s)					{ x -= s; y -= s; return *this; }
	iV2		operator-	() const					{ return iV2(-x, -y); }
	iV2	Tran() const { return iV2(y, x); }
	iV2 & Clip(const iRect & c);

	bool operator <(int v) const { return x < v && y < v; }
    bool operator >(int v) const { return x > v && y > v; }
    bool operator <=(const iV2 & v) const { return x <= v.x && y <= v.y; }
    bool operator >=(const iV2 & v) const { return x >= v.x && y >= v.y; }
    bool operator <(const iV2 & v) const { return x < v.x && y < v.y; }
	bool operator >(const iV2 & v) const { return x > v.x && y > v.y; }
	bool operator ==(const iV2 & v) const { return x == v.x && y == v.y; }
	bool operator ==(int v) const { return x == v && y == v; }
	bool operator !=(const iV2 & v) const { return ! operator==(v);}

	friend iV2	operator*	( const iV2 & v, int s )			{ return iV2( v.x * s, v.y * s ); }
	friend iV2	operator*	( int s, const iV2 & v )			{ return v * s; }
	friend iV2	operator*	( const iV2 & v1, const iV2 & v2 )	{ return iV2( v1.x * v2.x, v1.y * v2.y); }
	friend iV2	operator/	( const iV2 & v, int s )			{ return iV2( v.x / s, v.y / s ); }
	friend iV2	operator/	( const iV2 & v1, const iV2 & v2 )	{ return iV2( v1.x / v2.x, v1.y / v2.y ); }
	friend iV2	operator+	( const iV2 & v1, const iV2 & v2 )	{ return iV2( v1.x + v2.x, v1.y + v2.y ); }
	friend iV2	operator+	( const iV2 & v, int s )			{ return iV2( v.x + s, v.y + s ); }
	friend iV2	operator+	( int s, const iV2 & v )			{ return v + s; }
	friend iV2	operator-	( const iV2 & v1, const iV2 & v2 )	{ return iV2( v1.x - v2.x, v1.y - v2.y ); }
	friend iV2	operator-	( const iV2 & v, int s )			{ return iV2( v.x - s, v.y - s ); }
	friend iV2	operator-	( int s, const iV2 & v )			{ return s + -v;}
};

struct fRect
{
	fV2 p1, p2;

	fRect() {}
	fRect(const fV2 & p1, const fV2 & p2) : p1(p1), p2(p2) {}
	fRect(float x1, float y1, float x2, float y2 )  : p1(x1, y1), p2(x2, y2) {}
	fRect(int x1, int y1, int x2, int y2) : p1(x1, y1), p2(x2, y2) {}
	fRect( const iRect & r );

	fV2 Size() const { return p2 - p1; }
	float Width() const { return p2.x-p1.x; }
	float Height() const { return p2.y-p1.y; }
	fV2 Center() const { return (p1 + p2) * 0.5f; }
	fRect &	Inflate( const fV2 & v ) { p1 += v; p2 -= v; return *this; }
	fRect &	Deflate( const fV2 & v ) { p1 -= v; p2 += v; return *this; }
	fRect &	Offset( const fV2 & v ) { p1 += v; p2 += v; return *this; }
	fRect & Clip(const fRect & c)
	{ 
		if(p1.x < c.p1.x) p1.x = c.p1.x;
		if(p1.y < c.p1.y) p1.y = c.p1.y;
		if(p2.x > c.p2.x) p2.x = c.p2.x;
		if(p2.y > c.p2.y) p2.y = c.p2.y; 
		return *this;
	}

	bool IsInside( const fV2 & v ) const { return p1 <= v && v < p2; }
	bool Intersects(const fRect & r) const { return p2.x > r.p1.x && p1.x < r.p2.x && p2.y > r.p1.y && p1.y < r.p2.y; }
	bool Ordered() const { return p1.x<p2.x && p1.y<p2.y; }
};

inline	bool	operator==	( const fRect& r1, const fRect& r2 )	{ return r1.p1 == r2.p1 && r1.p2 == r2.p2; }
inline	bool	operator!=	( const fRect& r1, const fRect& r2 )	{ return ! (r1 == r2); }
inline fRect	operator*	( const fRect& r1, const fRect& r2 )	{ return fRect( std::max(r1.p1.x,r2.p1.x), std::max(r1.p1.y,r2.p1.y), std::min(r1.p2.x, r2.p2.x), std::min(r1.p2.y,r2.p2.y)); }

struct iRect
{
	iV2 p1, p2;

	struct SZ : iV2 { SZ(const iV2 & s) : iV2(s) {} };

	iRect() {}
	iRect(const iV2 & p, const SZ & s) : p1(p), p2(p + s) {}
	iRect(const iV2 & p1, const iV2 & p2) : p1(p1), p2(p2) {}
	iRect( int x1, int y1, int x2, int y2 ) : p1(x1, y1), p2(x2, y2) {}
	iRect( float x1, float y1, float x2, float y2 )	: p1(x1, y1), p2(x2, y2) {}
	iRect( const fRect & r ) : p1(r.p1), p2(r.p2) {}
	
	iV2 Size() const { return p2 - p1; }
	int Width()	const { return p2.x-p1.x; }
	int Height() const { return p2.y-p1.y; }
	iV2 Center() const { return (p1 + p2) / 2; }
	iV2 LeftUp() const { return p1; }
	iV2 RightDown() const { return p2; }
	iV2 RightUp() const { return iV2(p2.x, p1.y); }
	iV2 LeftDown() const { return iV2(p1.x, p2.y); }
	iRect &	Inflate( const iV2 & v ) { p1 += v; p2 -= v; return *this; }
	iRect & Deflate( const iV2 & v ) { p1 -= v; p2 += v; return *this; }
	iRect & Offset( const iV2 & v ) { p1+= v; p2 +=v; return *this; }
	iRect & Clip(const iRect & c)
	{ 
		if(p1.x < c.p1.x) p1.x = c.p1.x;
		if(p1.y < c.p1.y) p1.y = c.p1.y;
		if(p2.x > c.p2.x) p2.x = c.p2.x;
		if(p2.y > c.p2.y) p2.y = c.p2.y; 
		return *this;
	}
	bool IsInside( const iV2 & v ) const { return p1 <= v && v < p2; }
	bool Intersects(const iRect & r) const { return p2.x > r.p1.x && p1.x < r.p2.x && p2.y > r.p1.y && p1.y < r.p2.y; }
	bool Ordered() const { return p1.x<p2.x && p1.y<p2.y; }
};

inline	bool	operator==	( const iRect& r1, const iRect& r2 )	{ return r1.p1 == r2.p1 && r1.p2 == r2.p2;; }
inline	bool	operator!=	( const iRect& r1, const iRect& r2 )	{ return ! (r1 == r2); }
inline iRect	operator*	( const iRect& r1, const iRect& r2 )	{ return iRect( std::max(r1.p1.x,r2.p1.x), std::max(r1.p1.y,r2.p1.y), std::min(r1.p2.x, r2.p2.x), std::min(r1.p2.y,r2.p2.y)); }
inline iV2 & iV2::Clip(const iRect & c)
{
	if (x < c.p1.x) x = c.p1.x;
	if (y < c.p1.y) y = c.p1.y;
	if (x > c.p2.x) x = c.p2.x;
	if (y > c.p2.y) y = c.p2.y;
	return *this;
}


struct fColor
{
	union
	{
		struct { float b,g,r,a; };
		float v[4];
	};
	
	fColor() : b(), g(), r(), a() { }
	fColor( float a, float r, float g, float b )	: a(a), r(r), g(g), b(b) { }
	fColor( dword color ) : 
		b((color & 0x000000ff) / 255.0f), 
		g(((color & 0x0000ff00) >> 8) / 255.0f),
		r(((color & 0x00ff0000) >> 16) / 255.0f),
		a(((color & 0xff000000) >> 24)/ 255.0f)
	{}
	
	inline	operator float *()				{ return (float*)this; }
};


inline fV2::fV2( const iV2 & v ) : x(static_cast<float>(v.x)), y(static_cast<float>(v.y)) {}
inline fRect::fRect( const iRect & r ) : p1(r.p1), p2(r.p2) {}

// get the nearst power of 2 (but lower than value)
inline int GetPow2LO( int value )	
{
	for(int i=16; i>=1; i--)
		if( (1<<i) <= value ) return (1<<i);
	return 2;
}

// get the nearst power of 2 (but higher than value)
inline int GetPow2HI( int value )
{
	for(int i=1; i<16; i++)
		if( (1<<i) >= value ) return (1<<i);
	return 1<<16; //!
}

#endif


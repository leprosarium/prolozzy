///////////////////////////////////////////////////////////////////////////////////////////////////
// E9Math.h
// math structures and functions
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __E9MATH_H__
#define __E9MATH_H__

#include "E9System.h"
///////////////////////////////////////////////////////////////////////////////////////////////////
// defines
///////////////////////////////////////////////////////////////////////////////////////////////////
#define PI							(3.1415926535897932385f)	// pi
#define RAD2DEG(r)					((r) * 180.0f / PI)			// convert from radians to degrees
#define DEG2RAD(d)					((d) * PI / 180.0f)			// convert from degrees to radians

#define INRECT( x,y,r )				( r.left<=x && x<r.right && r.top<=y && y<r.bottom )
#define RECT2RECT( r1,r2 )			( !( (r1.x2 <= r2.x1) || (r1.x1 >= r2.x2) || (r1.y2 <= r2.y1) || (r1.y1 >= r2.y2) ) )

#define RGB2BGR( argb )				( (argb & 0xff00ff00) | ((argb & 0x00ff0000)>>16) | ((argb & 0x000000ff)<<16) )

struct iV2;
struct iRect;

///////////////////////////////////////////////////////////////////////////////////////////////////
// fV2 
// float vector 2
///////////////////////////////////////////////////////////////////////////////////////////////////
struct fV2
{
	float x, y;
	
	fV2()					: x(), y() {}
	fV2( float x, float y )	: x(x), y(y) {}
	fV2( int x, int y) : x(static_cast<float>(x)), y(static_cast<float>(y)) {}
	fV2( float v) : x(v), y(v) {}
	fV2( const iV2 & v );

	fV2	& __fastcall operator - () { x = -x; y = -y; return *this; }
	fV2	& __fastcall operator *=(const fV2 & v) { x *= v.x; y *= v.y; return *this; }
	fV2	& __fastcall operator *=(float f) { x *= f; y *= f; return *this; }
	fV2	& __fastcall operator /=(const fV2 & v) { x /= v.x; y /= v.y; return *this; }
	fV2	& __fastcall operator /=(float f) { x /= f; y /= f; return *this; }

	fV2 & __fastcall operator +=(const fV2 & v) { x += v.x; y += v.y; return *this; }
	fV2 & __fastcall operator +=(float f) { x += f; y += f; return *this; }
	fV2 & __fastcall operator -=(float f) { x -= f; y -= f; return *this; }
	float __fastcall operator ! () { return sqrt(x*x + y*y); }


	friend bool	__fastcall operator ==	( const fV2 & v1, const fV2 & v2 )		{ return v1.x==v2.x && v1.y==v2.y; }
	friend bool	__fastcall operator !=	( const fV2 & v1, const fV2 & v2 )		{ return v1.x!=v2.x || v1.y!=v2.y; }
	friend fV2	__fastcall operator *	( const fV2 & v, float f )			{ return fV2( v.x * f, v.y * f ); }
	friend fV2	__fastcall operator *	( float f, const fV2 & v )			{ return v * f; }
	friend fV2	__fastcall operator /	( const fV2 & v, float f )			{ return fV2( v.x / f, v.y / f ); }
	friend fV2	__fastcall operator +	( const fV2 & v1, const fV2 & v2 )		{ return fV2( v1.x + v2.x, v1.y + v2.y ); }
	friend fV2	__fastcall operator +	( const fV2 & v, float f )		{ return fV2( v.x + f, v.y + f ); }
	friend fV2	__fastcall operator +	( float f, const fV2 & v)		{ return v + f; }
	friend fV2	__fastcall operator -	( const fV2& v1, const fV2 & v2 )		{ return fV2( v1.x - v2.x, v1.y - v2.y ); }
	friend fV2	__fastcall operator -	( const fV2 & v, float f )		{ return fV2( v.x - f, v.y - f ); }
	friend float __fastcall operator & ( const fV2 & v1, const fV2 & v2 )		{ return v1.x*v2.x + v1.y*v2.y; }
	friend bool	__fastcall Nor			( fV2 & v1 ) { float t = !v1; if( t==0.0f ) return false; else { v1 *= (1.0f/t); return true; } }
	friend fV2	__fastcall Unit			( const fV2 & v1 ) { fV2 v = v1; Nor(v); return v; }
	friend fV2	__fastcall Lerp			( const fV2 & v1, const fV2 & v2, float s ) { return v1 + s * (v2 - v1); }
	friend fV2	__fastcall Rotate		( const fV2 & v1, float angsin, float angcos )	{ return  fV2( v1.x * angcos + v1.y * -angsin, v1.x * angsin + v1.y * angcos ); }
	friend fV2	__fastcall Rotate		( const fV2 & v1, float angle )				{ float rad = DEG2RAD(angle); return Rotate(v1, sin(rad), cos(rad)); }
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// iV2 
// int vector 2
///////////////////////////////////////////////////////////////////////////////////////////////////
struct iV2
{
	int x, y;
	iV2()						: x(), y() {}
	iV2( int x, int y )			: x(x), y(y) {}
	iV2( int v )				: x(v), y(v) {}
	iV2( float x, float y )		: x(static_cast<int>(x)), y(static_cast<int>(y)) {}
	iV2( const fV2 & v )		: x(static_cast<int>(v.x)), y(static_cast<int>(v.y)) {}

	iV2 & __fastcall operator*=	( const  iV2 & v )			{ x *= v.x; y *= v.y; return *this; }
	iV2 & __fastcall operator*=	( int s )					{ x *= s; y *= s; return *this; }
	iV2 & __fastcall operator/=	( const  iV2 & v )			{ x /= v.x; y /= v.y; return *this; }
	iV2 & __fastcall operator/=	( int s )					{ x /= s; y /= s; return *this; }
	iV2 & __fastcall operator+=	( const iV2 & v)			{ x += v.x; y += v.y; return *this; }
	iV2 & __fastcall operator+=	( int s)					{ x += s; y += s; return *this; }
	iV2 & __fastcall operator-=	( const iV2 & v)			{ x -= v.x; y -= v.y; return *this; }
	iV2 & __fastcall operator-=	( int s)					{ x -= s; y -= s; return *this; }
	iV2	& __fastcall operator-	()							{ x = -x; y = -y; return *this; }

	friend bool	__fastcall operator==	( const iV2 & v1, const iV2 & v2 )		{ return v1.x==v2.x && v1.y==v2.y; }
	friend bool __fastcall operator==	( const iV2 & v, int s)					{ return v.x == s && v.y == s; }
	friend bool	__fastcall operator!=	( const iV2 & v1, const iV2 & v2 )		{ return v1.x!=v2.x || v1.y!=v2.y; }
	friend iV2	__fastcall operator*	( const iV2 & v, int s )			{ return iV2( v.x * s, v.y * s ); }
	friend iV2	__fastcall operator*	( int s, const iV2 & v )			{ return v * s; }
	friend iV2	__fastcall operator/	( const iV2 & v, int s )			{ return iV2( v.x / s, v.y / s ); }
	friend iV2	__fastcall operator/	( const iV2 & v1, const iV2 & v2 )	{ return iV2( v1.x / v2.x, v1.y / v2.y ); }
	friend iV2	__fastcall operator+	( const iV2 & v1, const iV2 & v2 )		{ return iV2( v1.x + v2.x, v1.y + v2.y ); }
	friend iV2	__fastcall operator-	( const iV2 & v1, const iV2 & v2 )		{ return iV2( v1.x - v2.x, v1.y - v2.y ); }
};

///////////////////////////////////////////////////////////////////////////////////////////////////
// fRect
// float rectangle
///////////////////////////////////////////////////////////////////////////////////////////////////
struct fRect
{
		union
		{
			struct { float x1, y1, x2, y2; };
			struct { float left, top, right, bottom; };
		};

		fRect()												: x1(), y1(), x2(), y2() {}
		fRect(const fV2 & p1, const fV2 & p2)				: x1(p1.x), y1(p1.y), x2(p2.x), y2(p2.y) {}
		fRect( float x1, float y1, float x2, float y2 )		: x1(x1), y1(y1), x2(x2), y2(y2) {}
		fRect( int x1, int y1, int x2, int y2 )				: x1((float)x1), y1((float)y1), x2((float)x2), y2((float)y2) {}
		fRect( const iRect & r );

		fV2		Size() const								{ return fV2(Width(), Height()); }
		float 	Width()	const								{ return x2-x1; }
		float 	Height() const								{ return y2-y1; }
		fV2		Center() const								{ return fV2((x1+x2) * 0.5f,(y1+y2) * 0.5f); }
		void	Inflate( const fV2 & v )					{ x1+=v.x; y1+=v.y; x2-=v.x; y2-=v.y; }
		void	Deflate( const fV2 & v )					{ x1-=v.x; y1-=v.y; x2+=v.x; y2+=v.y; }
		void	Offset( const fV2 & v )						{ x1+=v.x; y1+=v.y; x2+=v.x; y2+=v.y; }
		BOOL	IsInside( const fV2 & v ) const				{ return (x1<=v.x && v.x<x2 && y1<=v.y && v.y<y2); }

};

inline	BOOL	__fastcall operator==	( const fRect& r1, const fRect& r2 )	{ return (r1.x1==r2.x1 && r1.y1==r2.y1 && r1.x2==r2.x2 && r1.y2==r2.y2); }
inline	BOOL	__fastcall operator!=	( const fRect& r1, const fRect& r2 )	{ return (r1.x1!=r2.x1 || r1.y1!=r2.y1 || r1.x2!=r2.x2 || r1.y2!=r2.y2); }
inline fRect	__fastcall operator+	( const fRect& r1, const fRect& r2 )	{ return fRect( (r1.x1<r2.x1)?r1.x1:r2.x1, (r1.y1<r2.y1)?r1.y1:r2.y1, (r1.x2>r2.x2)?r1.x2:r2.x2, (r1.y2>r2.y2)?r1.y2:r2.y2 ); }
inline fRect	__fastcall operator+=	( fRect& r1, const fRect& r2 )			{ if(r2.x1<r1.x1) r1.x1=r2.x1; if(r2.y1<r1.y1) r1.y1=r2.y1; if(r2.x2>r1.x2) r1.x2=r2.x2; if(r2.y2>r1.y2) r1.y2=r2.y2; return r1; }
inline fRect	__fastcall operator*	( const fRect& r1, const fRect& r2 )	{ return fRect( (r1.x1>r2.x1)?r1.x1:r2.x1, (r1.y1>r2.y1)?r1.y1:r2.y1, (r1.x2<r2.x2)?r1.x2:r2.x2, (r1.y2<r2.y2)?r1.y2:r2.y2 ); }
inline fRect	__fastcall operator*=	( fRect& r1, const fRect& r2 )			{ if(r2.x1>r1.x1) r1.x1=r2.x1; if(r2.y1>r1.y1) r1.y1=r2.y1; if(r2.x2<r1.x2) r1.x2=r2.x2; if(r2.y2<r1.y2) r1.y2=r2.y2; return r1; }

///////////////////////////////////////////////////////////////////////////////////////////////////
// iRect
// int rectangle
///////////////////////////////////////////////////////////////////////////////////////////////////
struct iRect
{
		union
		{
			struct { int x1, y1, x2, y2; };
			struct { int left, top, right, bottom; };
		};

		iRect()												: x1(), y1(), x2(), y2() {}
		iRect(const iV2 & p1, const iV2 & p2)				: x1(p1.x), y1(p1.y), x2(p2.x), y2(p2.y) {}
		iRect( int x1, int y1, int x2, int y2 )				: x1(x1), y1(y1), x2(x2), y2(y2) {}
		iRect( float x1, float y1, float x2, float y2 )		: x1((int)x1), y1((int)y1), x2((int)x2), y2((int)y2) {}
		iRect( const fRect & r )							: x1((int)r.x1), y1((int)r.y1), x2((int)r.x2), y2((int)r.y2) {}


		iV2		Size() const								{ return iV2(Width(), Height()); }
	 	int 	Width()	const								{ return x2-x1; }
	 	int 	Height() const								{ return y2-y1; }
	 	iV2		Center() const								{ return iV2((x1+x2)/2.0f,(y1+y2)/2.0f); }
	 	void	Inflate( const iV2 & v )					{ x1+=v.x; y1+=v.y; x2-=v.x; y2-=v.y; }
	 	void	Deflate( const iV2 & v )					{ x1-=v.x; y1-=v.y; x2+=v.x; y2+=v.y; }
	 	void	Offset( const iV2 & v )						{ x1+=v.x; y1+=v.y; x2+=v.x; y2+=v.y; }
		BOOL	IsInside( const iV2 & v ) const				{ return (x1<=v.x && v.x<x2 && y1<=v.y && v.y<y2); }

};

inline	BOOL	__fastcall operator==	( const iRect& r1, const iRect& r2 )	{ return (r1.x1==r2.x1 && r1.y1==r2.y1 && r1.x2==r2.x2 && r1.y2==r2.y2); }
inline	BOOL	__fastcall operator!=	( const iRect& r1, const iRect& r2 )	{ return (r1.x1!=r2.x1 || r1.y1!=r2.y1 || r1.x2!=r2.x2 || r1.y2!=r2.y2); }
inline iRect	__fastcall operator+	( const iRect& r1, const iRect& r2 )	{ return iRect( (r1.x1<r2.x1)?r1.x1:r2.x1, (r1.y1<r2.y1)?r1.y1:r2.y1, (r1.x2>r2.x2)?r1.x2:r2.x2, (r1.y2>r2.y2)?r1.y2:r2.y2 ); }
inline iRect	__fastcall operator+=	( iRect& r1, const iRect& r2 )			{ if(r2.x1<r1.x1) r1.x1=r2.x1; if(r2.y1<r1.y1) r1.y1=r2.y1; if(r2.x2>r1.x2) r1.x2=r2.x2; if(r2.y2>r1.y2) r1.y2=r2.y2; return r1; }
inline iRect	__fastcall operator*	( const iRect& r1, const iRect& r2 )	{ return iRect( (r1.x1>r2.x1)?r1.x1:r2.x1, (r1.y1>r2.y1)?r1.y1:r2.y1, (r1.x2<r2.x2)?r1.x2:r2.x2, (r1.y2<r2.y2)?r1.y2:r2.y2 ); }
inline iRect	__fastcall operator*=	( iRect& r1, const iRect& r2 )			{ if(r2.x1>r1.x1) r1.x1=r2.x1; if(r2.y1>r1.y1) r1.y1=r2.y1; if(r2.x2<r1.x2) r1.x2=r2.x2; if(r2.y2<r1.y2) r1.y2=r2.y2; return r1; }

///////////////////////////////////////////////////////////////////////////////////////////////////
// fColor
// float color
///////////////////////////////////////////////////////////////////////////////////////////////////
struct fColor
{
	union
	{
		struct { float b,g,r,a; };
		float v[4];
	};
	
	fColor() : b(), g(), r(), a() { }
	fColor( float a, float r, float g, float b )	: a(a), r(r), g(g), b(b) { }
	fColor( dword color ) { b=(float)((color & 0x000000ff)) / 255.0f; g=(float)((color & 0x0000ff00)>>8) / 255.0f; r=(float)((color & 0x00ff0000)>>16) / 255.0f; a=(float)((color & 0xff000000)) / 255.0f; }
	
	inline	operator float *()				{ return (float*)this; }
};


///////////////////////////////////////////////////////////////////////////////////////////////////
// inlines
///////////////////////////////////////////////////////////////////////////////////////////////////
inline fV2::fV2( const iV2 & v ) : x(static_cast<float>(v.x)), y(static_cast<float>(v.y)) {}
inline fRect::fRect( const iRect & r )	: x1((float)r.x1), y1((float)r.y1), x2((float)r.x2), y2((float)r.y2) {}

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
///////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef __I9INPUT_H__
#define __I9INPUT_H__

#include "E9System.h"
#include "E9Engine.h"
#include "I9Def.h"
#include <assert.h>
#include <vector>

#define I9_API_DEFAULT			0				// default api

// devices
#define I9_DEVICE_KEYBOARD		0				// id for keyboard device
#define I9_DEVICE_MOUSE			1				// id for mouse device
#define I9_DEVICE_JOYSTICK1		2				// id for joystick device
#define I9_DEVICE_JOYSTICK2		3				// id for joystick device
#define I9_DEVICES				4				// supported devices count

#define	I9_QUEUESIZE			64				// size of key queue
#define	I9_KEYMASK_VALUE		0xf000			// key queue mask for value state
#define	I9_KEYMASK_CODE			0x0fff			// key queue mask for key code
#define	I9_KEYUNKNOWN			"unknown"		// unknwn name
#define I9_TRESHHOLD			0.18f			// axes bogus

struct i9Key
{
	int	m_value;			// value of the key 0=off, 1=on
	int	m_frm;				// frame ( refresh ) count
};								

struct i9Axe
{
	int	m_value;			// value of the axe
	int	m_delta;			// delta value of a key ( axe ) from the last refresh
	int	m_min;				// min clipping value
	int	m_max;				// max clipping value
	int	m_speed;			// speed multiplier factor
};

struct i9KeyName
{
	char		m_ascii;	// ascii character
	char		m_shifted;	// ascii character when shift is pressed
	const char*	m_name;		// key name
};

class i9Input
{
public:
	i9Input();
	virtual ~i9Input();

	virtual	BOOL Init( HWND hwnd, HINSTANCE hinstance );		// init input
	virtual	void Done();										// done input
	virtual void Update( float dtime );							// refresh keys for installed devices
	virtual void Clear();										// clear keys and axes for installed devices
	virtual	void Acquire();										// aquire input; call to aquire when input is needed again
	virtual	void Unacquire();									// unaquire input; call to unaquire when input is not needed for a while

// devices
	virtual	BOOL DeviceInit( int device );						// install device
	virtual	void DeviceDone( int device );						// uninstall device
	virtual	BOOL DeviceIsPresent( int device );					// if device is installed
	virtual	void DeviceUpdate( int device );					// update device to refresh it's keys and axes
	virtual	void DeviceClear( int device );						// clear device to reset it's keys and axes
	virtual	BOOL DeviceFFInit( int device );					// init force feedback support on a device
	virtual	void DeviceFFSet( int device, int mag, int per );	// set force feedback magnitude and period properties on a device
	virtual	void DeviceFFPlay( int device );					// play force feedback on a device
	virtual	void DeviceFFStop( int device );					// stop force feedback on a device
	virtual	BOOL DeviceFFIsPlaying( int device );				// test if force feedback is playing on a device
		
// keys management
	void PushKey( int key, BOOL value );				// Push a key in the stack (set key too)
	dword PopKey();										// Pop a key from the stack
	dword PeekKey();									// return key from the stack - do not pop

// keys access
	i9Key & GetKey( int key )	{ return m_key[key];		}
	BOOL GetKeyValue( int key )	{ return m_key[key].m_value;	}
	BOOL GetKeyDown( int key )	{ return (  m_key[key].m_value && m_key[key].m_frm == m_frm ); }
	BOOL GetKeyUp( int key )	{ return ( !m_key[key].m_value && m_key[key].m_frm == m_frm ); }
																
	int GetKeyQCount()			{ return m_nkq; }
	int	GetKeyQCode( int e=0 )	{ return m_keyq[e] & I9_KEYMASK_CODE; }
	BOOL GetKeyQValue( int e=0 ){ return (m_keyq[e] & I9_KEYMASK_VALUE)?1:0; }
	void ClearKeyQ()			{ m_nkq = 0; }
																
	char GetKeyAscii( int key )			{ if( key<0 || key>=I9_KEYS ) return 0; else return m_keyname[ key ].m_ascii; }
	char GetKeyShifted( int key )		{ if( key<0 || key>=I9_KEYS ) return 0; else return m_keyname[ key ].m_shifted; }
	const char * GetKeyName( int key )	{ if( key<0 || key>=I9_KEYS ) return m_keyname[0].m_name; else return m_keyname[ key ].m_name; }
	int FindKeyByAscii( char ascii );					// find key (0=unknown if not found)
	int FindKeyByName( const char* name );				// find key (0=unknown if not found)

// axes access
	i9Axe & GetAxe( int axe )						{ return m_axe[axe]; }
	int GetAxeValue( int axe )						{ return m_axe[axe].m_value; }
	int GetAxeDelta( int axe )						{ return m_axe[axe].m_delta; }
	void SetAxeClip( int axe, int min, int max )	{ m_axe[axe].m_min = min; m_axe[axe].m_max = max; }
	void SetAxeSpeed( int axe, int speed )			{ m_axe[axe].m_speed = speed; }

public:
	HWND m_hwnd;					// hwnd
	HINSTANCE m_hinstance;			// hinstance
	int m_frm;						// crt input frame (refresh count)
	float m_time;					// crt input time
	i9Key m_key[I9_KEYS];			// keys map
	i9Axe m_axe[I9_AXES];			// axes map
	dword m_keyq[I9_QUEUESIZE];		// que buffer

private:
	static i9KeyName m_keyname[I9_KEYS];					// key names
	int m_nkq;												// key que entries
};


///////////////////////////////////////////////////////////////////////////////////////////////////
// INTERFACE
///////////////////////////////////////////////////////////////////////////////////////////////////
extern	i9Input*	i9_input;		// global instance, created by platform

inline BOOL	I9_DeviceIsPresent( int device )				{ return false;}//assert(i9_input); return i9_input->DeviceIsPresent(device); }

// -----------------------------

inline BOOL	I9_DeviceFFInit( int device	)					{ assert(i9_input); return i9_input->DeviceFFInit(device); }
inline void	I9_DeviceFFSet( int device, int mag, int per )	{ assert(i9_input); i9_input->DeviceFFSet(device,mag,per); }
inline void	I9_DeviceFFPlay( int device )					{ assert(i9_input); i9_input->DeviceFFPlay(device); }
inline void	I9_DeviceFFStop( int device )					{ assert(i9_input); i9_input->DeviceFFStop(device); }
inline BOOL	I9_DeviceFFIsPlaying( int device )				{ assert(i9_input); return i9_input->DeviceFFIsPlaying(device); }

inline bool I9_GetKeyValue(int) { return false;}

inline int I9_GetAxeValue( int axe )						{ return 0;}//assert(i9_input); return i9_input->GetAxeValue(axe); }

inline int I9_GetJoystickHAT( int joy, int hatdir )		{ return 0;}//assert(i9_input); return i9_input->GetKeyValue( I9_JOY_H1(joy)+hatdir ); }

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

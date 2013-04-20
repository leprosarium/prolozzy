///////////////////////////////////////////////////////////////////////////////////////////////////
// I9InputDX.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "i9InputDX.h"
#include "D9Log.h"

#define I9_LOGERROR( prefix )	dlog( LOGINP, L"INPUT: %S (%S)\n", prefix, ErrorDesc(err) );

int i9InputDX::m_tjoyidx = 0;
int i9InputDX::m_tjoyskip = 0;

i9InputDX::i9InputDX()
{
	m_di = NULL;
	for(int i=0;i<I9_DEVICES;i++)
		m_device[i] = NULL;
}

i9InputDX::~i9InputDX()
{
}

BOOL i9InputDX::Init( HWND hwnd, HINSTANCE hinstance )
{
	i9Input::Init( hwnd, hinstance );

	// direct input
	int err = DirectInput8Create( hinstance, DIRECTINPUT_VERSION, IID_IDirectInput8, (void**)&m_di, NULL );
	if(FAILED(err)) { I9_LOGERROR("DirectInput creation failed"); Done(); return FALSE; }

	SetAxeSpeed( I9_MOUSE_X, 1 );
	SetAxeSpeed( I9_MOUSE_Y, 1 );
	SetAxeSpeed( I9_MOUSE_Z, 1 );

	return TRUE;
}

void i9InputDX::Done()
{
	i9Input::Done();
	if(m_di!=NULL) { m_di->Release(); m_di = NULL; }
}

void i9InputDX::Acquire()
{
	i9Input::Acquire();
	for(int i=0;i<I9_DEVICES;i++)
		if(DeviceIsPresent(i))
			m_device[i]->Acquire();
}

void i9InputDX::Unacquire()
{
	i9Input::Unacquire();
	for(int i=0;i<I9_DEVICES;i++)
		if(DeviceIsPresent(i))
			m_device[i]->Unacquire();
}

BOOL i9InputDX::DeviceInit( int device )
{
	assert(0<=device && device<I9_DEVICES);
	BOOL ok = FALSE;
	
	if(device==I9_DEVICE_KEYBOARD)
	{
		if(m_device[device]) return TRUE;
		m_device[device] = new i9DeviceDXKeyboard();
		ok = m_device[device]->Init(0, &GUID_SysKeyboard);
		if(!ok)
		{
			dlog(LOGINP, L"Keyboard failed.\n");
			m_device[device]->Done();
			delete m_device[device];
			m_device[device]=NULL;
			return FALSE;
		}
		else
			dlog(LOGINP, L"Keyboard ok.\n");
	}
	else
	if(device==I9_DEVICE_MOUSE)
	{
		if(m_device[device]) return TRUE;
		m_device[device] = new i9DeviceDXMouse();
		ok = m_device[device]->Init(0, &GUID_SysMouse);
		if(!ok)
		{
			dlog(LOGINP, L"Mouse failed.\n");
			m_device[device]->Done();
			delete m_device[device];
			m_device[device]=NULL;
			return FALSE;
		}
		else
			dlog(LOGINP, L"Mouse ok.\n");
	}
	else
	if(device==I9_DEVICE_JOYSTICK1 || device==I9_DEVICE_JOYSTICK2)
	{
		if(m_device[device]) return TRUE;
		m_tjoyidx = device-I9_DEVICE_JOYSTICK1;
		m_tjoyskip = 0;
		int err = m_di->EnumDevices( DI8DEVCLASS_GAMECTRL, EnumJoysticksCallback, (void*)this, DIEDFL_ATTACHEDONLY );
		if(FAILED(err)) I9_LOGERROR("EnumDevices failed");
		ok = DeviceIsPresent(device);
		if(!ok) 
		{
			dlog(LOGINP, L"Joystick %i failed.\n", device-I9_DEVICE_JOYSTICK1+1);
			return FALSE;
		}
		else
			dlog(LOGINP, L"Joystick %i ok.\n", device-I9_DEVICE_JOYSTICK1+1);
	}
	else
	{
		dlog(LOGINP, L"Unknown input device (%i).\n", device);
		return FALSE;
	}

	return TRUE;
}

void i9InputDX::DeviceDone( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return;
	m_device[device]->Done();
	delete m_device[device];
	m_device[device]=NULL;
}

BOOL i9InputDX::DeviceIsPresent( int device )
{
	assert(0<=device && device<I9_DEVICES);
	return m_device[device]!=NULL;
}

void i9InputDX::DeviceUpdate( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return;
	m_device[device]->Update();
}

void i9InputDX::DeviceClear( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return;
	m_device[device]->Clear();
}

BOOL i9InputDX::DeviceFFInit( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return FALSE;
	assert(m_device[device]->m_didevice);
	if(m_device[device]->m_ff) return TRUE;

	m_device[device]->m_ff = new i9FFDX();
	BOOL ok = m_device[device]->m_ff->Init(m_device[device]->m_didevice, -1); // default type
	if(!ok)
	{
		delete m_device[device]->m_ff;
		m_device[device]->m_ff = NULL;
		return FALSE;
	}

	return TRUE;
}

void i9InputDX::DeviceFFSet( int device, int mag, int per )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return;
	if(m_device[device]->m_ff==NULL) return;
	m_device[device]->m_ff->Set(mag,per);
}

void i9InputDX::DeviceFFPlay( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return;
	if(m_device[device]->m_ff==NULL) return;
	m_device[device]->m_ff->Play();
}

void i9InputDX::DeviceFFStop( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return;
	if(m_device[device]->m_ff==NULL) return;
	m_device[device]->m_ff->Stop();
}

BOOL i9InputDX::DeviceFFIsPlaying( int device )
{
	assert(0<=device && device<I9_DEVICES);
	if(m_device[device]==NULL) return FALSE;
	if(m_device[device]->m_ff==NULL) return FALSE;
	return m_device[device]->m_ff->IsPlaying();
}

BOOL CALLBACK i9InputDX::EnumJoysticksCallback( const DIDEVICEINSTANCE* didevicei, void* user )
{
	i9InputDX* _this = (i9InputDX*)user;
	dlog(LOGINP, L"EnumJoystick: %S\n", didevicei->tszProductName);
	i9DeviceDXJoystick* pdevice = new i9DeviceDXJoystick();
	if( !pdevice->Init( m_tjoyidx, &didevicei->guidInstance ) )
	{
		pdevice->Done();
		delete pdevice;
		return DIENUM_CONTINUE;
	}

	if(m_tjoyidx==m_tjoyskip) // the one i want
	{
		_this->m_device[I9_DEVICE_JOYSTICK1+m_tjoyidx] = pdevice;
		return DIENUM_STOP;
	}

	pdevice->Done();
	delete pdevice;
	m_tjoyskip++;
	return DIENUM_CONTINUE;
}

const char* i9InputDX::ErrorDesc( int err )
{
	switch( err )
	{
		case DIERR_BETADIRECTINPUTVERSION:		return "DIERR_BETADIRECTINPUTVERSION";
		case DIERR_INVALIDPARAM:				return "DIERR_INVALIDPARAM";
		case DIERR_OLDDIRECTINPUTVERSION:		return "DIERR_OLDDIRECTINPUTVERSION";
		case DIERR_OUTOFMEMORY:					return "DIERR_OUTOFMEMORY";
		case DIERR_NOTINITIALIZED:				return "DIERR_NOTINITIALIZED";
		case DIERR_OTHERAPPHASPRIO:				return "DIERR_OTHERAPPHASPRIO";
		case DIERR_DEVICENOTREG:				return "DIERR_DEVICENOTREG";
		case DIERR_NOINTERFACE:					return "DIERR_NOINTERFACE";
		case DIERR_ACQUIRED:					return "DIERR_ACQUIRED";
		case DIERR_OBJECTNOTFOUND:				return "DIERR_OBJECTNOTFOUND";
		case DIERR_UNSUPPORTED:					return "DIERR_UNSUPPORTED";
		case E_HANDLE:							return "E_HANDLE";
	}
	return "DIERR_UNKNOWN";
}

///////////////////////////////////////////////////////////////////////////////////////////////////

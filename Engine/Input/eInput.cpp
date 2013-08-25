#include "stdafx.h"
#include "D9Log.h"

eInput * einput = nullptr;

eInput::eInput(HWND hwnd, HINSTANCE hinstance) : hwnd(hwnd), hinstance(hinstance), frm(), time(), mouse(), keyboard(), joystick()
{
}

eInput::~eInput()
{
	for(Device *d: devices)
		delete d;
}

bool eInput::Init(HWND hwnd, HINSTANCE hinstance)
{
	if(Ready()) return true;
	dlog(LOGINP, L"Input init.\n");
	einput = new eInput(hwnd, hinstance);
	return true;
}

void eInput::Done()
{
	if(!Ready()) return;
	delete einput;
	einput = nullptr;
	dlog(LOGINP, L"Input done.\n");
}

void eInput::_Update(float dtime)
{
	frm++;
	time += dtime;
	for(Device *d: devices) d->Update(static_cast<int>(dtime));
	if(dtime > 4.0f) Clear();
}

void eInput::_Acquire()
{
	for(Device *d: devices)
		d->Acquire();
}

void eInput::_Unacquire()
{
	for(Device *d: devices)
		d->Unacquire();
}


void eInput::Clear()
{	
	for(Device *d: devices)
		d->Clear();
	time = 0.0f;
}

template<>
bool eInput::_Init<Devices::Mouse>()
{
	try
	{
		devices.push_back(mouse = new DeviceDXMouse());
	}
	catch(const std::exception & e)
	{
		dlog(L"Mouse ini error: %S", e.what());
		return false;
	}
	return true;
}

template<> 
bool eInput::_Init<Devices::Keyboard>()
{
	try
	{
		devices.push_back(keyboard = new DeviceDXKeyboard());
	}
	catch(const std::exception & e)
	{
		dlog(L"Keyboak ini error: %S", e.what());
		return false;
	}
	return true;
}
template<> 
bool eInput::_Init<Devices::Joystick>()
{
	return true;
}


std::shared_ptr<IDirectInput8> InputDX::Instance()
{
	static std::weak_ptr<IDirectInput8> inst;
	if(auto i = inst.lock())
		return i;
	IDirectInput8 * di;
	int err = DirectInput8Create( E9_GetHINSTANCE(), DIRECTINPUT_VERSION, IID_IDirectInput8, (void**)&di, NULL);
	if(FAILED(err)) throw std::exception("DirectInput creation failed"); 
	std::shared_ptr<IDirectInput8> p = std::shared_ptr<IDirectInput8>(di, [](IDirectInput8 * di) { di->Release(); } );
	inst = p;
	return p;
}

DeviceDX::DeviceDX(std::shared_ptr<IDirectInput8> input, const GUID & guid) : 	
	acquired(),
	input(input)
{
	int err = input->CreateDevice(guid, &device, NULL);
	if(FAILED(err))  throw std::exception("DX device CreateDevice failed");
}

bool DeviceDX::Acquire()
{
	int err = device->Acquire();
	if(FAILED(err)) return false;
	acquired = true;
	return true;
}

bool DeviceDX::Unacquire()
{
	int err = device->Unacquire();
	if(FAILED(err)) return false;
	acquired = false;
	return true;
}

void MouseDevice::Clear()
{
	for(Key & k: b)
		k = Key();
	WheelUp = WheelDown = Key();
	x.Clear();
	y.Clear();
	z.Clear();
}

DeviceDXMouse::DeviceDXMouse() : DeviceDX(InputDX::Instance(), GUID_SysMouse)
{
	int err = device->SetDataFormat(& c_dfDIMouse2);
	if(FAILED(err)) throw std::exception("MOUSE SetDataFormat failed");
	err = device->SetCooperativeLevel(E9_GetHWND(), DISCL_NONEXCLUSIVE | DISCL_BACKGROUND );
	if(FAILED(err))	throw std::exception("MOUSE SetCooperativeLevel failed");

	DIPROPDWORD	dip;
	dip.diph.dwSize			= sizeof(DIPROPDWORD);
	dip.diph.dwHeaderSize	= sizeof(DIPROPHEADER);
	dip.diph.dwObj			= 0;
	dip.diph.dwHow			= DIPH_DEVICE;
	dip.dwData				= BufferSize;
	err = device->SetProperty( DIPROP_BUFFERSIZE, &dip.diph );
	if(FAILED(err)) throw std::exception("MOUSE SetProperty failed");
	if(!DeviceDX::Acquire()) throw std::exception("MOUSE can't aquire" );
}

void DeviceDXMouse::Update(int frm)
{
	if(! acquired) return;

	x.delta = y.delta = z.delta = 0;
	
	// get data
	DIDEVICEOBJECTDATA didod[BufferSize];
	unsigned long elements = BufferSize;
	int err = device->GetDeviceData( sizeof(DIDEVICEOBJECTDATA), didod, &elements, 0 );
	if(FAILED(err))
	{
		if( err==DIERR_INPUTLOST )
		{
			dlog(LOGINP, L"Input lost\n");
			Acquire();
			return;
		}
		dlog(LOGINP, L"INPUT: MOUSE GetDeviceData failed");
		return;
	}

	for(unsigned long i = 0; i < elements; i++)
	{
		switch(didod[i].dwOfs)
		{
		
		// axes
		case DIMOFS_X:
			x.delta	+= *((int*)&didod[i].dwData);
			break;
		
		case DIMOFS_Y:
			y.delta += *((int*)&didod[i].dwData);
			break;
		
		case DIMOFS_Z:
			z.delta += *((int*)&didod[i].dwData);
			break;

		// keys
		case DIMOFS_BUTTON0:
		case DIMOFS_BUTTON1:
		case DIMOFS_BUTTON2:
		case DIMOFS_BUTTON3:
		case DIMOFS_BUTTON4:
		case DIMOFS_BUTTON5:
		case DIMOFS_BUTTON6:
		case DIMOFS_BUTTON7:
			b[didod[i].dwOfs - DIMOFS_BUTTON0] = Key(didod[i].dwData & 0x80 != 0, frm); 
		}
	}

	// fake wheel - not qued
	if (z.delta > 0)
		WheelUp = Key(true, frm);		
	else if(WheelUp.value)
		WheelUp = Key(false, frm);	
	if(z.delta < 0)
		WheelDown = Key(true, frm);
	else if(WheelDown.value)
		WheelDown = Key(false, frm);

	// set axes values and clip
	x.value += x.delta * x.speed;
	y.value += y.delta * y.speed;
	z.value += z.delta * z.speed;
	
	x.value = std::min(std::max(x.value, x.min), x.max);
	y.value = std::min(std::max(y.value, y.min), y.max);
	z.value = std::min(std::max(z.value, z.min), z.max);
}

DeviceDXKeyboard::DeviceDXKeyboard() : DeviceDX(InputDX::Instance(), GUID_SysKeyboard)
{
	int err = device->SetDataFormat( &c_dfDIKeyboard );
	if(FAILED(err))	throw std::exception("KEYBOARD SetDataFormat failed");

	err = device->SetCooperativeLevel(E9_GetHWND(), DISCL_NONEXCLUSIVE | DISCL_BACKGROUND );
	if(FAILED(err)) throw std::exception("KEYBOARD SetCooperativeLevel");

	DIPROPDWORD	dip;
	dip.diph.dwSize			= sizeof(DIPROPDWORD);
	dip.diph.dwHeaderSize	= sizeof(DIPROPHEADER);
	dip.diph.dwObj			= 0;
	dip.diph.dwHow			= DIPH_DEVICE;
	dip.dwData				= BufferSize;
	err = device->SetProperty(DIPROP_BUFFERSIZE, &dip.diph);
	if(FAILED(err)) throw std::exception("KEYBOARD SetProperty failed");

	if(!DeviceDX::Acquire()) throw std::exception("KEYBOARD can't aquire");
}

void KeyboardDevice::Clear()
{
	for(Key & k: keys)
		k = Key();
}

void DeviceDXKeyboard::Update(int frm)
{
	if(! acquired) return;
	DIDEVICEOBJECTDATA didod[BufferSize];
	unsigned long elements = BufferSize;
	int err = device->GetDeviceData(sizeof(DIDEVICEOBJECTDATA), didod, &elements, 0);
	if(FAILED(err))
	{
		if( err==DIERR_INPUTLOST )
		{
			dlog(LOGINP, L"Input lost\n");
			Acquire();
			return;
		}
		dlog(LOGINP, L"KEYBOARD GetDeviceData failed");
		return;
	}
	for(int i=0; i<(int)elements; i++)
		keys[static_cast<BYTE>(didod[i].dwOfs)] = Key(didod[i].dwData & 0x80 != 0, frm); 
}


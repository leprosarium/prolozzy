#include "stdafx.h"
#include "D9Log.h"

eInput * einput = nullptr;

eInput::eInput(HWND hwnd, HINSTANCE hinstance) : hwnd(hwnd), hinstance(hinstance), frm(), time(), joystick()
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
	for(Device *d: devices) d->Update(frm);
	if(dtime > 4.0f) Clear();
}

void eInput::_Acquire()
{
	for(Device *d: devices)
		d->Acquire();
	keyQueue.clear();
}

void eInput::_Unacquire()
{
	for(Device *d: devices)
		d->Unacquire();
	keyQueue.clear();
}


void eInput::Clear()
{	
	for(Device *d: devices)
		d->Clear();
	time = 0.0f;
	keyQueue.clear();
}

template<>
bool eInput::_Init<Devices::Mouse>()
{
	try
	{
		devices.push_back(new DeviceDXMouse(mouse));
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
		devices.push_back(new DeviceDXKeyboard(keyboard));
	}
	catch(const std::exception & e)
	{
		dlog(L"Keyboard ini error: %S", e.what());
		return false;
	}
	return true;
}
template<> 
bool eInput::_Init<Devices::Joystick>()
{
	try
	{
		devices.push_back(new DeviceDXJoystick(joystick));
	}
	catch(const std::exception & e)
	{
		dlog(L"Joystick ini error: %S", e.what());
		return false;
	}
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

void MouseDevice::State::clear()
{
	for(Key & k: b)
		k = Key();
	WheelUp = WheelDown = Key();
	x.Clear();
	y.Clear();
	z.Clear();
}

DeviceDXMouse::DeviceDXMouse(MouseDevice::State & state) : DeviceDX(InputDX::Instance(), GUID_SysMouse), MouseDevice(state)
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

	state.x.delta = state.y.delta = state.z.delta = 0;
	DIDEVICEOBJECTDATA didod[BufferSize];
	unsigned long elements = GetDeviceData(didod);
	if(!elements)
		return;

	for(unsigned long i = 0; i < elements; i++)
	{
		switch(didod[i].dwOfs)
		{
		
		// axes
		case DIMOFS_X:
			state.x.delta	+= *((int*)&didod[i].dwData);
			break;
		
		case DIMOFS_Y:
			state.y.delta += *((int*)&didod[i].dwData);
			break;
		
		case DIMOFS_Z:
			state.z.delta += *((int*)&didod[i].dwData);
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
			state.b[didod[i].dwOfs - DIMOFS_BUTTON0] = Key((didod[i].dwData & 0x80) != 0, frm); 
		}
	}

	// fake wheel - not qued
	if (state.z.delta > 0)
		state.WheelUp = Key(true, frm);		
	else if(state.WheelUp.value)
		state.WheelUp = Key(false, frm);	
	if(state.z.delta < 0)
		state.WheelDown = Key(true, frm);
	else if(state.WheelDown.value)
		state.WheelDown = Key(false, frm);

	// set axes values and clip
	state.x.value += state.x.delta * state.x.speed;
	state.y.value += state.y.delta * state.y.speed;
	state.z.value += state.z.delta * state.z.speed;
	
	state.x.value = std::min(std::max(state.x.value, state.x.min), state.x.max);
	state.y.value = std::min(std::max(state.y.value, state.y.min), state.y.max);
	state.z.value = std::min(std::max(state.z.value, state.z.min), state.z.max);
}

DeviceDXKeyboard::DeviceDXKeyboard(KeyboardDevice::State & state) : DeviceDX(InputDX::Instance(), GUID_SysKeyboard), KeyboardDevice(state)
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

void KeyboardDevice::State::clear()
{
	for(Key & k: keys)
		k = Key();
}

void DeviceDXKeyboard::Update(int frm)
{
	if(! acquired) return;
	DIDEVICEOBJECTDATA didod[BufferSize];
	if(unsigned long elements = GetDeviceData(didod))
		for(int i=0; i<(int)elements; i++)
			state.keys[static_cast<BYTE>(didod[i].dwOfs)] = Key((didod[i].dwData & 0x80) != 0, frm); 
}

void JoystickDevice::State::clear()
{
	for(Axe & ax: a)
		ax.Clear();
	for(Key & k: b)
		k = Key();
	//pov[0] = pov[1] = pov[2] = pov[3] = Key();
}

DeviceDXJoystick::DeviceDXJoystick(JoystickDevice::State & state) : DeviceDX(InputDX::Instance(), GUID_Joystick), JoystickDevice(state)
{
	int err = device->SetDataFormat(& c_dfDIJoystick);
	if(FAILED(err)) throw std::exception("JOYSTICK SetDataFormat failed");
	err = device->SetCooperativeLevel(E9_GetHWND(), DISCL_EXCLUSIVE | DISCL_FOREGROUND );
	if(FAILED(err))	throw std::exception("JOYSTICK SetCooperativeLevel failed");

	DIPROPDWORD	dip;
	dip.diph.dwSize			= sizeof(DIPROPDWORD);
	dip.diph.dwHeaderSize	= sizeof(DIPROPHEADER);
	dip.diph.dwObj			= 0;
	dip.diph.dwHow			= DIPH_DEVICE;
	dip.dwData				= BufferSize;
	err = device->SetProperty( DIPROP_BUFFERSIZE, &dip.diph );
	if(FAILED(err)) throw std::exception("JOYSTICK SetProperty failed");
}

void DeviceDXJoystick::Update(int frm)
{
	if(FAILED(device->Poll()))
	    while(device->Acquire() == DIERR_INPUTLOST);
	DIDEVICEOBJECTDATA didod[BufferSize];
	unsigned long elements = GetDeviceData(didod);
	if(!elements)
		return;

	for(int i=0; i<(int)elements; i++)
	{
		if(didod[i].dwOfs >= DIJOFS_BUTTON0 && didod[i].dwOfs <= DIJOFS_BUTTON31)
		{
			state.b[static_cast<BYTE>(didod[i].dwOfs - DIJOFS_BUTTON0)] = Key((didod[i].dwData & 0x80) != 0, frm);
		}
		else
		if(didod[i].dwOfs==DIJOFS_POV(0)) // more hats can be supported here...
		{
			//int pov; // -1=none, 0=up, 1=up-right, ... 7=left-up
			//int povmask[9] = { 0, 1, 1+2, 2, 2+4, 4, 4+8, 8, 8+1 }; // direction bits for each pov value
			//if(*((int*)&(didod[i].dwData))>=0)
			//	pov = didod[i].dwData / 4500;
			//else
			//	pov = -1;
			//
			//int hat = povmask[pov+1]; // current hat direction bits

			//for(int d=0;d<4;d++) // check each direction: if was set push false, 
			//{
			//	key = I9_JOY_H1(m_idx) + d;
			//	if( (m_hat & (1<<d)) != (hat & (1<<d)) ) // direction has changed
			//	{
			//		if(m_hat & (1<<d))	i9_input->PushKey(key, FALSE);
			//		if(  hat & (1<<d))	i9_input->PushKey(key, TRUE);
			//	}
			//}
			//m_hat = hat; // remember setting
		}
		else
		{
			int axe = -1;
			switch(didod[i].dwOfs)
			{
				case DIJOFS_X:			axe = 0; break;
				case DIJOFS_Y:			axe = 1; break;
				case DIJOFS_Z:			axe = 2; break;
				case DIJOFS_RX:			axe = 3; break;
				case DIJOFS_RY:			axe = 4; break;
				case DIJOFS_RZ:			axe = 5; break;
				case DIJOFS_SLIDER(0):	axe = 6; break;
				case DIJOFS_SLIDER(1):	axe = 7; break;
			};
			if(axe != -1)
				state.a[axe].value = Filter(didod[i].dwData);
		}
	}

}

const float DeviceDXJoystick::threshold = 0.18f;

int DeviceDXJoystick::Filter(int value)
{
	value = (value - 32767) * 1000 / 32768;
	const int th = static_cast<int>(threshold * 1000.f);
	
	if( value < th && -value < th ) return 0;
	if( value > 0 )		{ value -= th; if(value < 0) return 0; }
	if( value < 0 )		{ value += th; if(value > 0) return 0; }
	if( value < -1000 )	value = -1000;
	if( value > 1000 )	value = 1000;
	value = value * 1000 / (1000 - th);
	return value;
}
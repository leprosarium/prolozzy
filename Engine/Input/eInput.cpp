#include "stdafx.h"
#include "App.h"
#include "D9Log.h"
#include "E9Math.h"
#include "Input/eInput.h"

#include <XInput.h>  

#pragma comment(lib, "XInput.lib")

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
	elog::inp() << "Input init." << std::endl;
	einput = new eInput(hwnd, hinstance);
	return true;
}

void eInput::Done()
{
	if(!Ready()) return;
	delete einput;
	einput = nullptr;
	elog::inp() << "Input done." << std::endl;
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
	vibra.Pause(false);
}

void eInput::_Unacquire()
{
	for(Device *d: devices)
		d->Unacquire();
	keyQueue.clear();
	vibra.Pause(true);
}


void eInput::Clear()
{	
	for(Device *d: devices)
		d->Clear();
	time = 0.0f;
	keyQueue.clear();
}

template<>
bool eInput::_Init<Mouse>()
{
	try
	{
		devices.push_back(new DeviceDXMouse(mouse));
	}
	catch(const std::exception & e)
	{
		elog::sys() << "Mouse ini error: " << e.what() << std::endl;
		return false;
	}
	return true;
}

template<> 
bool eInput::_Init<Keyboard>()
{
	try
	{
		devices.push_back(new DeviceDXKeyboard(keyboard));
	}
	catch(const std::exception & e)
	{
		elog::sys() << "Keyboard ini error: " << e.what() << std::endl;
		return false;
	}
	return true;
}
template<> 
bool eInput::_Init<Joystick>()
{
	try
	{
		Joystick * joy;
		if(XBox360::isConnected(0))
			joy = new XBox360(0, joystick);
		else
			joy = new DeviceDXJoystick(joystick);
		vibra.Init(joy);
		devices.push_back(joy);
	}
	catch(const std::exception & e)
	{
		elog::sys() << "Joystick ini error: " << e.what() << std::endl;
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
	HRESULT err = DirectInput8Create( App::Instance(), DIRECTINPUT_VERSION, IID_IDirectInput8, (void**)&di, NULL);
	if(FAILED(err)) throw std::exception("DirectInput creation failed"); 
	std::shared_ptr<IDirectInput8> p = std::shared_ptr<IDirectInput8>(di, [](IDirectInput8 * di) { di->Release(); } );
	inst = p;
	return p;
}

DeviceDX::DeviceDX(std::shared_ptr<IDirectInput8> input,
				   const std::string & name,
				   const GUID & guid,
				   LPCDIDATAFORMAT dataFormat,
				   DWORD cooperativeLevel,
				   DWORD BufferSize) : 	
	name(name),
	acquired(),
	input(input)
{
	HRESULT hr = input->CreateDevice(guid, &device, NULL);
	if(FAILED(hr)) Throw("CreateDevice failed");

	hr = device->SetDataFormat(dataFormat);
	if(FAILED(hr)) Throw("SetDataFormat failed");

	hr = device->SetCooperativeLevel(App::Wnd(), cooperativeLevel);
	if(FAILED(hr)) Throw("SetCooperativeLevel failed");

	needPolling = GetPolling();

	if(!SetBufferSize(BufferSize)) Throw("Set BufferSize failed");
}

bool DeviceDX::GetPolling() const
{
	DIDEVCAPS dicaps;
	memset(&dicaps, 0, sizeof(DIDEVCAPS));
	dicaps.dwSize = sizeof(DIDEVCAPS);
	device->GetCapabilities(&dicaps);
	return (dicaps.dwFlags & DIDC_POLLEDDEVICE) != 0;
}

void DeviceDX::Throw(const std::string & msg) const
{
	throw std::exception((name + ": " + msg).c_str());
}

bool DeviceDX::Acquire()
{
	HRESULT err = device->Acquire();
	if(FAILED(err)) return false;
	acquired = true;
	return true;
}

bool DeviceDX::Unacquire()
{
	HRESULT err = device->Unacquire();
	if(FAILED(err)) return false;
	acquired = false;
	return true;
}

bool DeviceDX::SetBufferSize(DWORD BufferSize)
{
	DIPROPDWORD	dip;
	dip.diph.dwSize			= sizeof(DIPROPDWORD);
	dip.diph.dwHeaderSize	= sizeof(DIPROPHEADER);
	dip.diph.dwObj			= 0;
	dip.diph.dwHow			= DIPH_DEVICE;
	dip.dwData				= BufferSize;
	HRESULT hr = device->SetProperty( DIPROP_BUFFERSIZE, &dip.diph );
	return SUCCEEDED(hr);
}

void Mouse::State::clear()
{
	for(Key & k: b)
		k = Key();
	for(Axe & a: axe) a.Clear();
}

DeviceDXMouse::DeviceDXMouse(Mouse::State & state) :
	DeviceDX(InputDX::Instance(),
		"MOUSE",
		GUID_SysMouse,
		& c_dfDIMouse2,
		DISCL_NONEXCLUSIVE | DISCL_BACKGROUND),
	Mouse(state)
{
}

void DeviceDXMouse::Update(int frm)
{
	if(! acquired) return;

	state.axe[0].delta = state.axe[1].delta = state.axe[2].delta = 0;
	DIDEVICEOBJECTDATA didod[BufferSize];
	unsigned long elements = GetDeviceData(didod);
	if(!elements)
		return;

	for(unsigned long i = 0; i < elements; i++)
	{
		DWORD ofs = didod[i].dwOfs;
		switch(ofs)
		{
		
		// axes
		case DIMOFS_X:
		case DIMOFS_Y:
		case DIMOFS_Z:
			state.axe[(ofs - DIMOFS_X) / sizeof(LONG)].delta += *((int*)&didod[i].dwData);
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
			state.b[ofs - DIMOFS_BUTTON0] = Key((didod[i].dwData & 0x80) != 0, frm); 
		}
	}

	for(Axe & a: state.axe)
	{
		a.value += a.delta * a.speed;
		a.value = std::min(std::max(a.value, a.min), a.max);
	}
}

DeviceDXKeyboard::DeviceDXKeyboard(Keyboard::State & state) :
	DeviceDX(InputDX::Instance(),
		"KEYBOARD",
		GUID_SysKeyboard,
		& c_dfDIKeyboard,
		DISCL_NONEXCLUSIVE | DISCL_BACKGROUND),
	Keyboard(state)
{
}

void Keyboard::State::clear()
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

void Joystick::State::clear()
{
	for(Axe & ax: a)
		ax.Clear();
	for(Key & k: b)
		k = Key();
	left = right = up = down = Key();
}

DeviceDXJoystick::DeviceDXJoystick(Joystick::State & state) : 
	DeviceDX(InputDX::Instance(), 
		"JOYSTICK",
		GUID_Joystick,
		& c_dfDIJoystick,
		DISCL_EXCLUSIVE | DISCL_FOREGROUND),
	Joystick(state)
{
}

void DeviceDXJoystick::Update(int frm)
{
	if(!acquired) return;
	if(needPolling)
		device->Poll();
	DIDEVICEOBJECTDATA didod[BufferSize];
	unsigned long elements = GetDeviceData(didod);
	if(!elements)
		return;

	for(int i=0; i<(int)elements; i++)
	{
		if(didod[i].dwOfs >= DIJOFS_BUTTON0 && didod[i].dwOfs <= DIJOFS_BUTTON31)
		{
			int bt = didod[i].dwOfs - DIJOFS_BUTTON0;
			state.b[static_cast<BYTE>(bt)] = Key((didod[i].dwData & 0x80) != 0, frm);
		}
		else
		if(didod[i].dwOfs==DIJOFS_POV(0)) // more hats can be supported here...
		{
			//The rgdwPOV member contains the position of up to four point-of-view controllers in hundredths of a degree clockwise from north (or forward). The center (neutral) position is reported as -1.
			//- 1 
			//0 
			//90 * DI_DEGREES 
			//180 * DI_DEGREES 
			//270 * DI_DEGREES.


			//BOOL POVCentered = (LOWORD(dwPOV) == 0xFFFF);

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

XBox360::XBox360(int num, Joystick::State & state) : Joystick(state), num(num)
{

}

void XBox360::Update(int frm)
{
	XINPUT_STATE st;
	ZeroMemory(& st, sizeof(XINPUT_STATE));

    DWORD dwResult = XInputGetState(num, & st);

    if(dwResult != ERROR_SUCCESS)
		return;
	const WORD msk[] = { XINPUT_GAMEPAD_A, XINPUT_GAMEPAD_B,
						XINPUT_GAMEPAD_X, XINPUT_GAMEPAD_Y,
						XINPUT_GAMEPAD_LEFT_SHOULDER, XINPUT_GAMEPAD_RIGHT_SHOULDER,
						XINPUT_GAMEPAD_BACK, XINPUT_GAMEPAD_START,
						XINPUT_GAMEPAD_LEFT_THUMB, XINPUT_GAMEPAD_RIGHT_THUMB };
	for(int i = 0; i < 10; ++i)
		state.b[i].Set((st.Gamepad.wButtons & msk[i]) != 0, frm);
	
	state.a[0].value = Filter(st.Gamepad.sThumbLX, XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE);
	state.a[1].value = Filter(-st.Gamepad.sThumbLY, XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE);
	state.a[2].value = Filter(st.Gamepad.sThumbRX, XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE);
	state.a[3].value = Filter(-st.Gamepad.sThumbRY, XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE);

	state.left.Set((st.Gamepad.wButtons & XINPUT_GAMEPAD_DPAD_LEFT) != 0, frm);
	state.right.Set((st.Gamepad.wButtons & XINPUT_GAMEPAD_DPAD_RIGHT) != 0, frm);
	state.up.Set((st.Gamepad.wButtons & XINPUT_GAMEPAD_DPAD_UP) != 0, frm);
	state.down.Set((st.Gamepad.wButtons & XINPUT_GAMEPAD_DPAD_DOWN) != 0, frm);
}

int XBox360::Filter(int v, int th)
{
	v = v * 1000 / 32768;
	th = th * 1000 / 32768;
	
	if( v < th && -v < th ) return 0;
	if( v > 0 )		{ v -= th; if(v < 0) return 0; }
	if( v < 0 )		{ v += th; if(v > 0) return 0; }
	if( v < -1000 )	v = -1000;
	if( v > 1000 )	v = 1000;
	v = v * 1000 / (1000 - th);
	return v;
}

void XBox360::Vibrate(int speed1, int speed2)
{
	XINPUT_VIBRATION vibration;
	ZeroMemory( & vibration, sizeof(XINPUT_VIBRATION) );
	vibration.wLeftMotorSpeed = speed1; // use any value between 0-65535 here
	vibration.wRightMotorSpeed = speed2; // use any value between 0-65535 here
	XInputSetState(num, & vibration );
}

bool XBox360::isConnected(int num)
{
	XINPUT_STATE state;
	ZeroMemory( &state, sizeof(XINPUT_STATE) );
	DWORD dwResult = XInputGetState(num, & state);
    return dwResult == ERROR_SUCCESS;
}

void Vibrator::Vibrate(int left, int right, int time)
{ 
	Cmd cmd(left, right, time);
	if(queue.empty())
		Vibrate(cmd);
	queue.push_back(cmd);
}

void Vibrator::Update()
{
	if(!joystick || paused || queue.empty()) return;
	if(--queue.front().time > 0)
		return;
	queue.pop_front();
	Vibrate(queue.empty() ? Cmd() : queue.front());
}

void Vibrator::Pause(bool p) 
{
	if(paused == p)
		return;
	paused = p;
	if(paused)
		Vibrate(Cmd());
	else if(!queue.empty())
		Vibrate(queue.front());
}
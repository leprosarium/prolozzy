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


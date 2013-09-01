#ifndef __E_INPUT__
#define __E_INPUT__

#include "E9System.h"
#include "E9Engine.h"
#include "dinput.h"
#include <vector>
#include <memory>

struct Axe
{
	int	value;			
	int	delta;			// delta value of a key ( axe ) from the last refresh
	int	min;			// min clipping value
	int	max;			// max clipping value
	int	speed;			// speed multiplier factor
	Axe() : value(), delta(), min(), max(), speed() {}
	void Clear() { value = 0; delta = 0; }
};

struct Key
{
	bool value;
	int	frm;			// frame ( refresh ) count
	Key(bool value = false, int frm = 0) : value(value), frm(frm) {}

	bool isDown(int frame) const { return value && frm == frame; }
	bool isUp(int frame) const { return !value && frm == frame; }

};

class Device
{
public:
	class State
	{
	public:
		bool active;
		operator bool() const { return active; }
		State() : active() {}
	};
	virtual ~Device() {};
	virtual void Update(int frm) = 0;
	virtual void Clear() = 0;
	virtual void Acquire() = 0;
	virtual void Unacquire() = 0;
};


class MouseDevice : public Device
{
public:
	class State : public Device::State
	{
	public:
		Axe x, y, z;
		Key b[8];
		Key WheelUp;
		Key WheelDown;
		void clear();
	} & state;
	MouseDevice(State & state) : state(state) {}
	virtual void Clear() { state.clear(); }
};

class KeyboardDevice : public Device
{
public:
	class State : public Device::State
	{
	public:
		Key keys[256];
		void clear();
	} & state;
	KeyboardDevice(State & state) : state(state) {}
	virtual void Clear() { state.clear(); }

};

class eInput;
extern eInput * einput;

enum class Devices
{
	Mouse,
	Keyboard,
	Joystick
};

class eInput
{
	HWND hwnd;
	HINSTANCE hinstance;
	int frm;
	float time;
	std::vector<Device *> devices;

	Device * joystick;

	eInput(HWND hwnd, HINSTANCE hinstance);
	void _Update(float dtime);
	void _Acquire();
	void _Unacquire();
	void Clear();

	template<Devices D>
	bool _Init() { return false; }

	template<Devices D>
	bool _Present() { return false; }

public:
	std::wstring keyQueue;

	bool keyValue(int k) const { return keyboard.keys[k].value; } 
	bool isKeyDown(int k) const { return keyboard.keys[k].isDown(frm); }
	bool isKeyUp(int k) const { return keyboard.keys[k].isUp(frm); }
	bool ctrl() const { return keyValue(DIK_LCONTROL) || keyValue(DIK_RCONTROL); }
	bool shift() const { return keyValue(DIK_LSHIFT) || keyValue(DIK_RSHIFT); }

	MouseDevice::State mouse;
	KeyboardDevice::State keyboard;

	~eInput();
	static bool Init(HWND hwnd, HINSTANCE hinstance);
	static void Done();
	static bool Ready() { return einput != nullptr; }
	static void Update(float dtime) { if(Ready()) einput->_Update(dtime); } 
	static void Acquire() { if(Ready())  einput->_Acquire(); }
	static void Unacquire() { if(Ready())  einput->_Unacquire(); }

	template<Devices D>
	static bool Init() { return Ready() ? einput->_Init<D>() : false; }

	template<Devices D>
	static bool Present() { return Ready() ? einput->_Present<D> : false; }

};

template<> bool eInput::_Init<Devices::Mouse>();
template<> bool eInput::_Init<Devices::Keyboard>();
template<> bool eInput::_Init<Devices::Joystick>();

class InputDX
{
public:
	static std::shared_ptr<IDirectInput8> Instance();
};

class DeviceDX
{
	std::shared_ptr<IDirectInput8> input;
protected:
	bool acquired;
	IDirectInputDevice8 * device;		// direct input device object
	template<class Data, int BufferSize>
	unsigned long GetDeviceData(Data (&data)[BufferSize]);
public:
	DeviceDX(std::shared_ptr<IDirectInput8> input, const GUID & guid);
	~DeviceDX() { device->Release(); }

	bool Acquire();
	bool Unacquire();
};

template<class Data, int BufferSize>
unsigned long DeviceDX::GetDeviceData(Data (&data)[BufferSize])
{
	unsigned long elements = BufferSize;
	int err = device->GetDeviceData(sizeof(Data), data, &elements, 0);
	if(!FAILED(err))
		return elements;
	if(err == DIERR_INPUTLOST)
	{
		dlog(LOGINP, L"Input lost\n");
		Acquire();
	} else
		dlog(LOGINP, L"GetDeviceData failed");
	return 0;
}


class DeviceDXMouse : private DeviceDX, public MouseDevice
{
	static const dword BufferSize = 64;
public:
	DeviceDXMouse(MouseDevice::State & state);
	virtual void Update(int frm);

	virtual void Acquire() { DeviceDX::Acquire(); Clear(); }
	virtual void Unacquire() { DeviceDX::Unacquire(); Clear(); }
};

class DeviceDXKeyboard : private DeviceDX, public KeyboardDevice
{
	static const dword BufferSize = 64;
public:
	DeviceDXKeyboard(KeyboardDevice::State & state);
	virtual void Update(int frm);
	virtual void Acquire() { DeviceDX::Acquire(); Clear(); }
	virtual void Unacquire() { DeviceDX::Unacquire(); Clear(); }
};

#endif
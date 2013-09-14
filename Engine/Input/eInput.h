#ifndef __E_INPUT__
#define __E_INPUT__

#include "E9System.h"
#include "E9Engine.h"
#include "dinput.h"
#include <vector>
#include <memory>
#include <deque>

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
	void Set(bool v, int f) { if(v != value) { value = v; frm = f; } }
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


class Mouse : public Device
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
	Mouse(State & state) : state(state) {}
	virtual void Clear() { state.clear(); }
};

class Keyboard : public Device
{
public:
	class State : public Device::State
	{
	public:
		Key keys[256];
		void clear();
	} & state;
	Keyboard(State & state) : state(state) {}
	virtual void Clear() { state.clear(); }

};

class Joystick : public Device
{
public:
	class State : public Device::State
	{
	public:
		Axe a[8];
		Key b[32];
		Key left, right, up, down;
		void clear();
	} & state;
	Joystick(State & state) : state(state) {}
	virtual ~Joystick() { Vibrate(0, 0); }
	virtual void Clear() { state.clear(); }
	virtual void Vibrate(int, int) {}
};


class eInput;
extern eInput * einput;

class Vibrator
{
	Joystick * joystick;
	struct Cmd 
	{ 
		int left, right, time;
		Cmd() : left(), right(), time() {}
		Cmd(int left, int right, int time) : left(left), right(right), time(time) {}
	};
	typedef std::deque<Cmd> CmdQueue;
	CmdQueue queue;
	bool paused;
	void Vibrate(const Cmd & cmd) { if(joystick) joystick->Vibrate(cmd.left, cmd.right); }
public:
	Vibrator() : joystick(), paused(true) {}
	void Init(Joystick * j) { joystick = j; }
	operator bool() const { return joystick != nullptr; }
	void Update(int frm);
	void Pause(bool p);
	void Stop() { Vibrate(Cmd()); queue.clear(); }
	void Vibrate(int left, int right, int time);
};

class eInput
{
	HWND hwnd;
	HINSTANCE hinstance;
	int frm;
	float time;

	std::vector<Device *> devices;
	eInput(HWND hwnd, HINSTANCE hinstance);
	void _Update(float dtime);
	void _Acquire();
	void _Unacquire();
	void Clear();

	template<class Dev>
	bool _Init() { return false; }

	template<class D>
	bool _Present() { return false; }

public:
	Vibrator vibra;
	std::wstring keyQueue;

	bool keyValue(int k) const { return keyboard.keys[k].value; } 
	bool isKeyDown(int k) const { return keyboard.keys[k].isDown(frm); }
	bool isKeyUp(int k) const { return keyboard.keys[k].isUp(frm); }
	bool ctrl() const { return keyValue(DIK_LCONTROL) || keyValue(DIK_RCONTROL); }
	bool shift() const { return keyValue(DIK_LSHIFT) || keyValue(DIK_RSHIFT); }
	bool alt() const { return keyValue(DIK_LALT) || keyValue(DIK_RALT); }

	bool mouseValue(int k) const { return mouse.b[k].value; } 
	bool isMouseDown(int k) const { return mouse.b[k].isDown(frm); }
	bool isMouseUp(int k) const { return mouse.b[k].isUp(frm); }

	int joystickAxeValue(int ax) const { return joystick.a[ax].value; }
	bool joystickButtonValue(int bt) const { return joystick.b[bt].value; }

	void Vibrate(int left, int right, int time) { if(vibra) vibra.Vibrate(left, right, time); }

	Mouse::State mouse;
	Keyboard::State keyboard;
	Joystick::State joystick;

	~eInput();
	static bool Init(HWND hwnd, HINSTANCE hinstance);
	static void Done();
	static bool Ready() { return einput != nullptr; }
	static void Update(float dtime) { if(Ready()) einput->_Update(dtime); } 
	static void Acquire() { if(!Ready()) return; einput->_Acquire(); }
	static void Unacquire() { if(Ready())  einput->_Unacquire(); }

	template<class D>
	static bool Init() { return Ready() ? einput->_Init<D>() : false; }

	template<class D>
	static bool Present() { return Ready() ? einput->_Present<D> : false; }

};

template<> bool eInput::_Init<Mouse>();
template<> bool eInput::_Init<Keyboard>();
template<> bool eInput::_Init<Joystick>();

class InputDX
{
public:
	static std::shared_ptr<IDirectInput8> Instance();
};

class DeviceDX
{
	std::shared_ptr<IDirectInput8> input;
protected:
	static const dword BufferSize = 64;
	const std::string name;
	bool acquired;
	bool needPolling;
	IDirectInputDevice8 * device;		// direct input device object
	template<class Data, int bufferSize>
	unsigned long GetDeviceData(Data (&data)[bufferSize]);
	bool SetBufferSize(unsigned long);
	bool GetPolling() const;
	void Throw(const std::string & msg) const;
public:
	DeviceDX(std::shared_ptr<IDirectInput8> input, const std::string & name, const GUID & guid, LPCDIDATAFORMAT dataFormat, DWORD cooperativeLevel, DWORD bufferSize = BufferSize);
	~DeviceDX() { device->Release(); }

	bool Acquire();
	bool Unacquire();
};

template<class Data, int bufferSize>
unsigned long DeviceDX::GetDeviceData(Data (&data)[bufferSize])
{
	unsigned long elements = bufferSize;
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

class DeviceDXMouse : private DeviceDX, public Mouse
{
public:
	DeviceDXMouse(Mouse::State & state);
	virtual void Update(int frm);
	virtual void Acquire() { DeviceDX::Acquire(); Clear(); }
	virtual void Unacquire() { DeviceDX::Unacquire(); Clear(); }
};

class DeviceDXKeyboard : private DeviceDX, public Keyboard
{
public:
	DeviceDXKeyboard(Keyboard::State & state);
	virtual void Update(int frm);
	virtual void Acquire() { DeviceDX::Acquire(); Clear(); }
	virtual void Unacquire() { DeviceDX::Unacquire(); Clear(); }
};

class DeviceDXJoystick : private DeviceDX, public Joystick
{
	static const float threshold;
	static int Filter(int);
public:
	DeviceDXJoystick(Joystick::State & state);
	virtual void Update(int frm);
	virtual void Acquire() { DeviceDX::Acquire(); Clear(); }
	virtual void Unacquire() { DeviceDX::Unacquire(); Clear(); }
};


class XBox360 : public Joystick
{
	int num;
	int Filter(int v, int th);
public:
	XBox360(int num, Joystick::State & state);

	static bool isConnected(int num);

	virtual void Update(int frm);
	virtual void Acquire() { Clear(); }
	virtual void Unacquire() { Clear(); }
	virtual void Vibrate(int speed1, int speed2);
};

#endif
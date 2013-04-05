//////////////////////////////////////////////////////////////////////////////////////////////////
// DizSound.h
//////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __DIZSOUND_H__
#define __DIZSOUND_H__

#include "E9System.h"
#include "D9Debug.h"
#include "A9Audio.h"

#include "SWI-cpp-m.h"

const size_t SOUND_VOICES = 16;		// max number of samples playing at once

//////////////////////////////////////////////////////////////////////////////////////////////////
// structs
//////////////////////////////////////////////////////////////////////////////////////////////////
class tSoundProto
{
	tSoundProto(const tSoundProto &);
	tSoundProto & operator = (const tSoundProto &);
public:
	tSoundProto(PlAtom id, int group, int instances, A9BUFFERPROTO bufferproto) : m_id(id), m_group(group), m_instances(instances),  m_bufferproto(bufferproto) {}
	~tSoundProto()			{ if(valid()) A9_BufferDeprecache(m_bufferproto); }

	tSoundProto(tSoundProto && p) : m_id(p.m_id), m_group(p.m_group), m_instances(p.m_instances), m_bufferproto(p.m_bufferproto) { p.m_bufferproto = 0; }
	tSoundProto & operator = (tSoundProto && p) { m_id = p.m_id; m_group = p.m_group; m_instances = p.m_instances; m_bufferproto = p.m_bufferproto; p.m_bufferproto = 0; return *this; }

	bool valid() const { return m_bufferproto != nullptr; }

	PlAtom					m_id;			// sample id
	int						m_group;		// resource group
	int						m_instances;	// how many instances of this proto are allowed to play simultaneous
	A9BUFFERPROTO			m_bufferproto;	// bufferproto
};

class tMusicProto
{
	tMusicProto(const tMusicProto &);
	tMusicProto & operator = (const tMusicProto &);
public:
	tMusicProto(PlAtom id, int group, A9STREAM stream) : m_id(id), m_group(group),  m_stream(stream) {}
	~tMusicProto()			{ if(m_stream) A9_StreamDestroy(m_stream); }

	tMusicProto(tMusicProto && p) : m_id(p.m_id), m_group(p.m_group), m_stream(p.m_stream) { p.m_stream = 0; }
	tMusicProto & operator = (tMusicProto && p) { m_id = p.m_id; m_group = p.m_group; m_stream = p.m_stream; p.m_stream = 0; return *this; }
	
	PlAtom					m_id;			// music id
	int						m_group;		// resource group
	A9STREAM				m_stream;		// stream proto
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Sound manager
//////////////////////////////////////////////////////////////////////////////////////////////////

bool isSupportedExt(const char * ext);

class Samples : std::vector<tSoundProto>
{
	bool LoadFile(const char* filepath, size_t & total, size_t & fail, size_t & duplicates, int group);	// load a sample file (proto)
	int	Find(PlAtom id) { for(size_t i = 0; i < size(); i++) if((*this)[i].m_id == id) return i; return -1; }

	A9BUFFER m_voice[SOUND_VOICES];						// list with voices buffers
	int _playingVoices;									// playing voices (debug info)
	static PlAtom all;

	bool invalidVoice(size_t voiceidx) { return m_voice[voiceidx] == nullptr; }
public:
	Samples();
	void Done() { StopAll(); clear(); }
	void Update();

	bool Load(const char* path, int group=0);		// load samples from a path (protos)
	void Unload(int group = 0);							// destroy all samples (proto)
	int Play(PlAtom id, int loop = 0);					// play a proto sample; return voiceidx or -1 if failed
	int Playing(size_t voiceidx);						// return sample id if playing or -1 if not playing
	void Stop(size_t voiceidx);							// stop a voice
	void StopAll(PlAtom id = all);						// stop all voices of a sample id, or all voices if id is -1
	void Volume(int volume);							// set samples volume
	size_t playingVoices() const { return _playingVoices; }
};

class Music : std::vector<tMusicProto>
{
	bool LoadFile(const char * filename, size_t & total, size_t & fail, size_t & duplicates, int group);			// load a music file (proto)
	int Find(PlAtom id) { for(size_t i = 0; i < size(); i++) if((*this)[i].m_id == id) return i; return -1; }

	A9STREAM _stream;	// music stream
	int	_idx;			// current playing music index in proto list (-1 if none)
	int _next;			// next music programmed to play (-1 if none); this is considered the current music by the logic
	int _start;			// position where the next music will start in miliseconds; 0=begining of music
	int _fadein;		// music fade in (seconds)
	int _fadeout;		// music fade out (seconds)
	int _pos;			// current music position (used to know when music ends) in samples
	bool _paused;		// music paused
	float _volume;		// current playing music volume factor [0..1] (used for fades)
public:
	Music();

	void Done() { Stop(); clear(); }

	bool Load(const char* path, int group = 0);			// load all musics from a path (protos)
	void Update(float dtime);							// deals with the play, stop and volume management
	void Unload(int group = 0);							// destroy all musics (proto)
	void Fade(int out, int in) { _fadeout = out; _fadein = in; }	// set fade values
	int Play(PlAtom id, int start = 0);					// set a music to play next from a specified position; returns -1 if failed; don't necessarily plays at once
	PlAtom Playing() const { if(_next==-1) return -1; return (*this)[_next].m_id; }	// return id of next music (-1 if none) = _next
	int Position();										// return position of next music in miliseconds; -1 if no next music programmed
	void Stop();										// music stop immediately
	void Volume(int volume);							// set music volume
	void Pause(bool pause);								// music pause; stop, but remember where it was
	bool paused() const { return _paused; }
};

class cDizSound
{
public:
	void Done() { samples.Done(); music.Done(); }
	void Update();		// update called every frame
						
	Samples samples;
	Music	music;
};

extern cDizSound g_sound;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////

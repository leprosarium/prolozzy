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

class cDizSound
{
public:
							cDizSound			();
							~cDizSound			();

		bool				Init				();								// init
		void				Done				();								// done
		void				Update				();								// update called every frame
						
		// music
		bool				MusicLoad			( const char* path, int group=0 );	// load all musics from a path (protos)
		bool				MusicLoadFile		( const char* filename, int group=0 );// load a music file (proto)
		void				MusicUnload			( int group=0 );				// destroy all musics (proto)
		void				MusicFade			( int out, int in );			// set fade values
		int					MusicPlay			( PlAtom id, int start=0 );		// set a music to play next from a specified position; returns -1 if failed; don't necessarily plays at once
		PlAtom				MusicPlaying		();								// return id of next music (-1 if none) = m_musicnext
		int					MusicPosition		();								// return position of next music in miliseconds; -1 if no next music programmed
		void				MusicStop			();								// music stop immediately
		void				MusicPause			( bool pause );					// music pause; stop, but remember where it was
		void				MusicUpdate			( float dtime );				// deals with the play, stop and volume management
		void				MusicVolume			( int volume );					// set music volume
inline	int					MusicFind			( PlAtom id )						{ for(size_t i=0;i<m_musicproto.size();i++) if(m_musicproto[i].m_id==id) return i; return -1; }

		A9STREAM			m_music;											// music stream
		int					m_musicidx;											// current playing music index in proto list (-1 if none)
		int					m_musicnext;										// next music programmed to play (-1 if none); this is considered the current music by the logic
		int					m_musicstart;										// position where the next music will start in miliseconds; 0=begining of music
		int					m_musicfadein;										// music fade in (seconds)
		int					m_musicfadeout;										// music fade out (seconds)
		int					m_musicpos;											// current music position (used to know when music ends) in samples
		bool				m_musicpaused;										// music paused
		float				m_musicvol;											// current playing music volume factor [0..1] (used for fades)
		std::vector<tMusicProto> m_musicproto;								// list with loaded musics

		int					m_music_total;										// status report on total musics declared (load+failed)
		int					m_music_fail;										// status report on musics failed to load
		int					m_music_duplicates;									// status report on id duplicates
		int					m_music_group;										// current loading group


	Samples samples;
};

extern cDizSound g_sound;

#endif
//////////////////////////////////////////////////////////////////////////////////////////////////

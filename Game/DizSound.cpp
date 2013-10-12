//////////////////////////////////////////////////////////////////////////////////////////////////
// DizSound.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizSound.h"
#include "DizCfg.h"
#include "DizDef.h"
#include "DizScript.h"
#include "DizDebug.h"
#include "A9Codec.h"

PlAtom Samples::all("all");
cDizSound g_sound;


PREDICATE_M(sample, load, 1)
{
	return g_sound.samples.Load(A1, 0);
}

PREDICATE_M(sample, load, 2)
{
	return g_sound.samples.Load(A1, A2);
}

PREDICATE_M(sample, unload, 0)
{
	g_sound.samples.Unload();
	return true;
}

PREDICATE_M(sample, unload, 1)
{
	int group = A1;
	g_sound.samples.Unload(group);
	return true;
}


PREDICATE_M(sample, play, 1)
{
	g_sound.samples.Play(PlAtom(A1));
	return true;
}

PREDICATE_M(sample, play, 2)
{
	return A2 = g_sound.samples.Play(PlAtom(A1));
}

PREDICATE_M(sample, play, 3)
{
	return A3 = g_sound.samples.Play(PlAtom(A1), A2);
}

PREDICATE_M(sample, playing, 2)
{
	return A2 = g_sound.samples.Playing(static_cast<int>(A1));
}

PREDICATE_M(sample, stop, 1)
{
	g_sound.samples.Stop(static_cast<int>(A1));
	return true;
}

PREDICATE_M(sample, stopAll, 0)
{
	g_sound.samples.StopAll();
	return true;
}

PREDICATE_M(sample, stopAll, 1)
{
	g_sound.samples.StopAll(PlAtom(A1));
	return true;
}

PREDICATE_M(sample, volume, 1)
{
	g_sound.samples.Volume(A1);
	return true;
}


PREDICATE_M(music, load, 1)
{
	return g_sound.music.Load(A1);
}


PREDICATE_M(music, load, 2)
{
	return g_sound.music.Load(A1, A2);
}

PREDICATE_M(music, unload, 1)
{
	g_sound.music.Unload(A1);
	return true;
}

PREDICATE_M(music, Unload, 0)
{
	g_sound.music.Unload(0);
	return true;
}

PREDICATE_M(music, fade, 2)
{
	g_sound.music.Fade(A1, A2);
	return true;
}

PREDICATE_M(music, play, 1)
{
	return  g_sound.music.Play(PlAtom(A1)) == 0;
}

PREDICATE_M(music, play, 2)
{
	return  g_sound.music.Play(PlAtom(A1), A2) == 0;
}

PREDICATE_M(music, playing, 1)
{
	return A1 = g_sound.music.Playing();
}

PREDICATE_M(music, position, 1)
{
	return A1 = g_sound.music.Position();
}

PREDICATE_M(music, stop, 0)
{
	g_sound.music.Stop();
	return true;
}

PREDICATE_M(music, volume, 1)
{
	g_sound.music.Volume(A1);
	return true;
}


void cDizSound::Update(float dtime)
{
	if(A9_IsReady())
	{
		samples.Update();	
		music.Update(dtime);
		A9_Update();
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// SAMPLES
//////////////////////////////////////////////////////////////////////////////////////////////////

bool isSupportedExt(const std::string & ext) 
{
	static const std::string supported_ext[] = {"wav", "ogg", "ym", "mod", "it", "xm", "s3m"};
	for(const std::string & e: supported_ext)
		if(e == ext) return true;
	return false;
}

Samples::Samples() : _playingVoices()
{
	for(size_t i = 0; i < SOUND_VOICES; i++)	m_voice[i] = nullptr;
}

void Samples::Update()
{
	_playingVoices = 0;
	for(size_t i = 0; i < SOUND_VOICES; i++)
		if(Playing(i) != -1) _playingVoices++;
}

bool Samples::LoadFile(const std::string & filepath, size_t & total, size_t & fail, size_t & duplicates, int group)
{
	if(!isSupportedExt(file_path2ext(filepath))) return false; // ignore files with unsupported extensions

	total++;

	// check name format
	std::istringstream fname(file_path2name(filepath));
	std::string nm;
	if(!(fname >> nm))
	{ 
		fail++; 
		elog::app() << "! " << filepath.c_str() << " (bad name)" << std::endl; 
		return false; 
	}
	int instances = 1;
	fname >> instances; 
	if(instances<1) instances=1;

	// check unique id
	PlAtom id(nm);
	if(Find(id) != -1)
	{
		fail++;
		duplicates++;
		elog::sys() << "! " << filepath.c_str() << " (duplicate id)" << std::endl;
		return false;
	}

	// load and decompress
	A9BUFFERPROTO bufferproto = A9_BufferPrecache(filepath);
	if(!bufferproto)
	{
		fail++;
		elog::sys() << "! " << filepath.c_str() << " (failed to load)" << std::endl;
		return false;
	}

	// add to list
	push_back(tSoundProto(id, group, instances, bufferproto));

	if(g_dizdebug.active()) // log for developers
		elog::app() << "  " << filepath.c_str() << " [" << instances << "]" << std::endl;

	return true;
}

bool Samples::Load(const std::string & path, int group)
{
	if(!A9_IsReady()) { elog::app() << "Sound disabled - no samples are loaded." << std::endl; return false; }
	elog::app() << "Loading samples from \"" << path.c_str() << "\" (group=" << group << ")" << std::endl;
	size_t total = 0;
	size_t fail = 0;
	size_t duplicates = 0;
	files->FindFiles(path, [this, &total, &fail, &duplicates, group](const std::string & filepath) { LoadFile(filepath, total, fail, duplicates, group); } );
	elog::app() << "Samples report: total=" << total << ", failed=" << fail << " (duplicates=" << duplicates << ")" << std::endl << std::endl;
	return true;
}

void Samples::Unload( int group )
{
	for(size_t i=0;i<size();)
		if((*this)[i].m_group == group)
		{
			StopAll((*this)[i].m_id);
			erase(begin() + i);
		}
		else
			++i;
}

int	Samples::Play(PlAtom id, int loop)
{
	int protoidx = Find(id);
	if(protoidx==-1) return -1; // invalid id
	tSoundProto & proto = (*this)[protoidx];
	if( !proto.valid() ) return -1; // invalid proto

	// find instances and free voice
	int count = 0;
	int voiceidx=-1;
	for(size_t i = 0; i < SOUND_VOICES; i++)
	{
		if(Playing(i)==id.handle) count++; // destroy if stopped
		if(m_voice[i] == nullptr) voiceidx = i;
	}
	if(count>=proto.m_instances) return -1; // too many instances playing
	if(voiceidx==-1) return -1; // no free voices
	
	// add new voice
	A9BUFFER buffer = A9_BufferCreateFromProto(proto.m_bufferproto);
	if(!buffer) return -1; // wrong format
	A9_BufferSet(buffer,A9_USER,id.handle);
	A9_BufferSet(buffer,A9_VOLUME,A9_VolumePercentToDecibel(g_cfg.m_volfx));
	A9_BufferPlay(buffer,loop);
	m_voice[voiceidx] = buffer;
	
	return voiceidx;
}

int	Samples::Playing(size_t voiceidx)
{
	assert(voiceidx < SOUND_VOICES); 
	if(invalidVoice(voiceidx)) return -1;
	if(!A9_BufferIsPlaying(m_voice[voiceidx]))
	{
		A9_BufferDestroy(m_voice[voiceidx]);
		m_voice[voiceidx] = nullptr;
		return -1;
	}
	return A9_BufferGet(m_voice[voiceidx], A9_USER);
}

void Samples::Stop(size_t voiceidx)
{
	assert(voiceidx < SOUND_VOICES); 
	if(invalidVoice(voiceidx)) return;
	A9_BufferStop(m_voice[voiceidx]);
	A9_BufferDestroy(m_voice[voiceidx]);
	m_voice[voiceidx] = nullptr;
}

void Samples::StopAll( PlAtom id )
{
	for(size_t i = 0; i < SOUND_VOICES; i++)
		if(A9BUFFER buffer = m_voice[i])
			if(id == all || A9_BufferGet(buffer, A9_USER) == id.handle)
				Stop(i);
}

void Samples::Volume( int volume )
{
	volume = std::max(std::min(volume, 100), 0);
	if(volume == g_cfg.m_volfx) return; // same volume
	g_cfg.m_volfx = volume;

	int volumedb = A9_VolumePercentToDecibel(volume);
	for(size_t i = 0; i < SOUND_VOICES; i++)
		if(A9BUFFER buffer = m_voice[i])
			A9_BufferSet(buffer,A9_VOLUME,volumedb);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// MUSIC
//////////////////////////////////////////////////////////////////////////////////////////////////
Music::Music() :_stream(nullptr),
				_idx(-1),
				_next(-1),
				_start(),
				_fadein(),
				_fadeout(),
				_pos(),
				_paused(),
				_volume()
{
}


bool Music::LoadFile( const std::string & filepath, size_t & total, size_t & fail, size_t & duplicates, int group)
{
	if(!isSupportedExt(file_path2ext(filepath))) return false; // ignore files with unsupported extensions

	total++;

	PlAtom id(file_path2name(filepath));
	// check unique id
	if(Find(id) != -1)
	{
		fail++;
		duplicates++;
		elog::sys() << "! " << filepath.c_str() << " (duplicate id)" << std::endl;
		return false;
	}

	// load and decompress
	A9STREAM stream = A9_StreamCreate(filepath.c_str());
	if(!stream)
	{
		fail++;
		elog::sys() << "! " << filepath.c_str() << " (failed to load)" << std::endl;
		return false;
	}

	// add to list
	push_back(tMusicProto(id, group, stream));

	if(g_dizdebug.active()) // log for developers
		elog::app() << "  " << filepath.c_str() << std::endl;

	return true;
}

bool Music::Load(const std::string & path, int group)
{
	if(!A9_IsReady()) { elog::app() << "Sound disabled - no musics are loaded." << std::endl; return false; }
	elog::app() << "Loading musics from \"" << path.c_str() << "\" (group=" << group << ")" << std::endl;
	size_t total = 0;
	size_t fail	= 0;
	size_t duplicates = 0;
	files->FindFiles(path, [this, &total, &fail, &duplicates, group](const std::string & filepath) { LoadFile(filepath, total, fail, duplicates, group); } );
	elog::app() << "Music report: total=" << total << ", failed=" << fail << " (duplicates=" << duplicates << ")" << std::endl;
	return true;
}

void Music::Unload(int group)
{
	for(size_t i = 0; i < size();)
		if((*this)[i].m_group == group)
		{
			if(i==_idx) Stop(); // stop if current
			if(_next==i) _next=-1; // clear if next
			erase(begin() + i);
		}
		else
			++i;
}

int Music::Play( PlAtom id, int start )
{
	int protoidx = Find(id);
	_next = protoidx; // store for later play
	_start = start>=0 ? start : 0; // store valid for later start
	if(_fadeout==0)  // @HM
		if(_next!=_idx)
			_volume = 0.0f; // update will stop current music immediately, if any
	return 0; // ok
}

int	Music::Position()
{
	if(_next==-1) return -1; // no next music
	if(_idx!=_next) return _start; // current is fading, next is about to start from _start
	// we are playing the next music, report curent position
	if(!_stream) return _start; // should not happend
	int musicpos = (int)( (float)A9_StreamGetPosition(_stream) / (float)A9_StreamGet(_stream,A9_FREQUENCY) * 1000.0f );
	return musicpos; // in ms
}

void Music::Stop()
{
	if(_idx==-1) return;
	if(!_stream) { _idx=-1; return; } // should not happened
	A9_StreamSet(_stream,A9_VOLUME,A9_VOLUMEMIN); // silence
	A9_StreamStop(_stream);
	_stream = NULL;
	_idx = -1;
	_paused = false;
	_next = -1;
	_volume = 0.0;
}

void Music::Pause(bool pause)
{
	if( _idx==-1) return;
	if(!_stream) { _idx=-1; return; } // should not happened
	if (pause == paused()) return;
	if(pause)
		A9_StreamStop(_stream);
	else
		A9_StreamPlay(_stream, true);
	_paused = pause;
}

void Music::Update(float dtime)
{

	if(_idx==-1) // nothing is playing
	{
		if(_next!=-1) // music scheduled
		{
			// set current stream
			_stream = (*this)[_next].m_stream;
			if(!_stream) { _next=-1; return; } // cration failure (invalid music file)

			// prepare volume
			if(_fadein>0)
			{
				A9_StreamSet(_stream,A9_VOLUME,A9_VOLUMEMIN); //start from silence
				_volume = 0.0f;
			}
			else
			{
				A9_StreamSet(_stream,A9_VOLUME,A9_VolumePercentToDecibel(g_cfg.m_volmusic));
				_volume = 1.0f;
			}

			// play stream (always looping)
			int start = (int)( (float)_start / 1000.0f * (float)A9_StreamGet(_stream,A9_FREQUENCY) );
			if(start<0 || start>=A9_StreamGet(_stream,A9_SIZE)) start=0;
			A9_StreamSetPosition(_stream,start);
			A9_StreamPlay(_stream,true);
			_idx = _next;
			_pos = start;
		}
	}
	else // music is playing
	{
		if(!_stream) { _idx=-1; return; } // should not happened
		if(_paused) return; // don't update volumes if paused
		if(!A9_StreamIsPlaying(_stream)) return; // don't update volumes if it's not yet playing

		float fadeoutstep	= (_fadeout>0) ? (dtime / (float)_fadeout) : 1.0f;
		float fadeinstep	= (_fadein>0)  ? (dtime / (float)_fadein)  : 1.0f;

		if( _idx == _next ) // we are keeping this song
		{
			if(_volume<1.0f) // have not reached full volume yet so we have fadein
			{
				_volume+=fadeinstep;
				if(_volume>1.0f) _volume=1.0f;
				A9_StreamSet(_stream, A9_VOLUME, A9_VolumePercentToDecibel( (int)(_volume * g_cfg.m_volmusic) ));
			}
			// else we are very happy

			// check music end
			int musicpos = A9_StreamGetPosition(_stream);
			int musicsize = A9_StreamGet(_stream,A9_SIZE);
			if( musicpos < _pos ) // reached the end of loop and it just started from the begining
				g_script.musicLoop(); // notify scripts; user can stop or play other music in the handler
			_pos = musicpos;
		}
		else // we must fade this one out, because a another is waiting next
		{
			if(_volume>0.0f) // have not reached silence yet, but we have fade out
			{
				_volume-=fadeoutstep;
				if(_volume<0.0f) _volume=0.0f;
				A9_StreamSet(_stream, A9_VOLUME, A9_VolumePercentToDecibel( (int)(_volume * g_cfg.m_volmusic) ));
			}
			else
			{
				Stop(); // stop and destroy current music
				// in the next update the next song will be made current and played
			}
		}
	}

}

void Music::Volume( int volume )
{
	if(volume<0) volume=0;
	if(volume>100) volume=100;
	if(volume==g_cfg.m_volmusic) return; // same volume
	g_cfg.m_volmusic = volume;

	int volumedb = A9_VolumePercentToDecibel( (int)(_volume * g_cfg.m_volmusic) );
	if(_idx!=-1 && _stream)
		A9_StreamSet(_stream,A9_VOLUME,volumedb);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

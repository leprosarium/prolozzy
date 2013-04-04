//////////////////////////////////////////////////////////////////////////////////////////////////
// DizSound.cpp
//////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "DizSound.h"
#include "DizCfg.h"
#include "DizDef.h"
#include "DizScript.h"
#include "DizDebug.h"
#include "E9App.h"
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


//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizSound::cDizSound()
{


	// music
	m_music			= NULL;
	m_musicidx		= -1;
	m_musicnext		= -1;
	m_musicstart	= 0;
	m_musicfadein	= 0;
	m_musicfadeout	= 0;
	m_musicpos		= 0;
	m_musicpaused	= 0;
	m_musicvol		= 0.0f;

	m_music_total = 0;
	m_music_fail = 0;	
	m_music_duplicates = 0;
	m_music_group = 0;

}

cDizSound::~cDizSound()
{
}

bool cDizSound::Init()
{
	return true;
}

void cDizSound::Done()
{
	samples.Done();
	MusicStop();
	m_musicproto.clear();
}

void cDizSound::Update()
{
	if(!A9_IsReady()) return;
	
	// samples - stop (remove) those that have finished playing
	samples.Update();	
	// music
	MusicUpdate( E9_AppGetInt(E9_APP_DELTATIME) / 1000.f);

	A9_Update();

}



//////////////////////////////////////////////////////////////////////////////////////////////////
// SAMPLES
//////////////////////////////////////////////////////////////////////////////////////////////////

bool isSupportedExt(const char * ext) 
{
	static const char * supported_ext[] = {".wav",".ogg",".ym",".mod",".it",".xm",".s3m"};
	static const size_t count = sizeof(supported_ext) / sizeof(*supported_ext);
	for(size_t i = 0, e = count; i < e; ++i)
		if(strcmp(supported_ext[i], ext) == 0) return true;
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

bool Samples::LoadFile(const char* filepath, size_t & total, size_t & fail, size_t & duplicates, int group)
{
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];
	file_pathsplit(filepath, 0, 0, fname, ext);
	
	if(!isSupportedExt(ext)) return false; // ignore files with unsupported extensions

	total++;

	// check name format
	char nm[_MAX_FNAME];
	int instances = 1;
	int ret = sscanf(fname,"%s %i",nm,&instances);
	if(ret==0) 
	{ 
		fail++; 
		dlog(LOGAPP, L"! %S (bad name)\n", filepath); 
		return false; 
	}
	if(instances<1) instances=1;

	// check unique id
	PlAtom id(nm);
	if(Find(id) != -1)
	{
		fail++;
		duplicates++;
		dlog(LOGSYS, L"! %S (duplicate id)\n", filepath);
		return false;
	}

	// load and decompress
	A9BUFFERPROTO bufferproto = A9_BufferPrecache(filepath);
	if(!bufferproto)
	{
		fail++;
		dlog(LOGSYS, L"! %S (failed to load)\n", filepath);
		return false;
	}

	// add to list
	push_back(tSoundProto(id, group, instances, bufferproto));

	if(g_dizdebug.active()) // log for developers
		dlog(LOGAPP, L"  %S [%i]\n", filepath, instances );

	return true;
}

bool Samples::Load(const char* path, int group)
{
	if(!A9_IsReady()) { dlog(LOGAPP, L"Sound disabled - no samples are loaded.\n"); return false; }

	if(!path || !path[0]) return false; // invalid path
	char spath[256];
	strcpy(spath, path);

	int szlen = (int)strlen(spath);
	if(spath[szlen-1]!='\\') strcat(spath,"\\");
	_strlwr(spath);
	dlog(LOGAPP, L"Loading samples from \"%S\" (group=%i)\n", spath, group);

	// init
	size_t total = 0;
	size_t fail = 0;
	size_t duplicates = 0;

	auto Callback = [this, &total, &fail, &duplicates, group](const char* filepath, BOOL dir) { if(!dir) LoadFile(filepath, total, fail, duplicates, group); };

	// find files on disk
	int archivefiles = F9_ArchiveGetFileCount(0);
	if(archivefiles == 0) // if no archive found then open from disk
		file_findfiles( spath, Callback, FILE_FINDREC );
	else // if archive opened, load from it
		for(int i=0;i<archivefiles;i++)
		{
			std::string filename = F9_ArchiveGetFileName(0,i);
			if(strstr(filename.c_str(), spath)==filename)
				Callback(filename.c_str(),false);
		}

	// report
	dlog(LOGAPP, L"Samples report: total=%i, failed=%i (duplicates=%i)\n\n", total, fail, duplicates);

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
bool cDizSound::MusicLoadFile( const char* filepath, int group )
{
	// check name and extension
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];
	file_pathsplit(filepath, 0, 0, fname, ext);
	
	if(!isSupportedExt(ext)) return false; // ignore files with unsupported extensions

	m_music_total++;

	PlAtom id(fname);
	// check unique id
	int idx = MusicFind(id);
	if( idx!=-1 )
	{
		m_music_fail++;
		m_music_duplicates++;
		dlog(LOGSYS, L"! %S (duplicate id)\n", filepath, id);
		return false;
	}

	// load and decompress
	A9STREAM stream = A9_StreamCreate(filepath);
	if(!stream)
	{
		m_music_fail++;
		dlog(LOGSYS, L"! %S (failed to load)\n", filepath);
		return false;
	}

	// add to list
	m_musicproto.push_back(tMusicProto(id, group, stream));

	if(g_dizdebug.active()) // log for developers
		dlog(LOGAPP, L"  %S\n", filepath );

	return true;
}

void FFCallback_Music( const char* filepath, BOOL dir )
{
	if(dir) return;
	bool ret = g_sound.MusicLoadFile(filepath,g_sound.m_music_group);
}

bool cDizSound::MusicLoad( const char* path, int group )
{
	if(!A9_IsReady()) { dlog(LOGAPP, L"Sound disabled - no musics are loaded.\n"); return false; }

	if(!path || !path[0]) return false; // invalid path
	char spath[256];
	strcpy(spath, path);

	int szlen = (int)strlen(spath);
	if(spath[szlen-1]!='\\') strcat(spath,"\\");
	_strlwr(spath);
	dlog(LOGAPP, L"Loading musics from \"%S\" (group=%i)\n", spath, group);

	// init
	m_music_total		= 0;
	m_music_fail		= 0;
	m_music_duplicates	= 0;
	m_music_group		= group;

	// find files on disk
	int archivefiles = F9_ArchiveGetFileCount(0);
	if(archivefiles==0) // if no archive found then open from disk
	{
		file_findfiles( spath, FFCallback_Music, FILE_FINDREC );
	}
	else // if archive opened, load from it
	{
		for(int i=0;i<archivefiles;i++)
		{
			std::string filename = F9_ArchiveGetFileName(0,i);
			if(strstr(filename.c_str(), spath)==filename)
			{
				FFCallback_Music(filename.c_str(),false);
			}
		}
	}

	// report
	dlog(LOGAPP, L"Music report: total=%i, failed=%i (duplicates=%i)\n\n", m_music_total, m_music_fail, m_music_duplicates );

	return true;
}

void cDizSound::MusicUnload( int group )
{
	for(size_t i=0;i<m_musicproto.size();)
		if(m_musicproto[i].m_group == group)
		{
			if(i==m_musicidx) MusicStop(); // stop if current
			if(m_musicnext==i) m_musicnext=-1; // clear if next
			m_musicproto.erase(m_musicproto.begin() + i);
		}
		else
			++i;
}

void cDizSound::MusicFade( int out, int in )
{
	m_musicfadeout = out;
	m_musicfadein = in;
}

int cDizSound::MusicPlay( PlAtom id, int start )
{

	int protoidx = MusicFind(id);
//@	if(protoidx==-1) return -1; // invalid id >> allow -1 for next music, to stop with fade
	m_musicnext = protoidx; // store for later play
	m_musicstart = start>=0 ? start : 0; // store valid for later start
	if(m_musicfadeout==0)  // @HM
	{
		if(m_musicnext!=m_musicidx)
			m_musicvol = 0.0f; // update will stop current music immediately, if any
	}
	return 0; // ok
}

PlAtom cDizSound::MusicPlaying()
{
	// we are interested in what is the scheduled song, ignoring the short fadeout of the current one
	if(m_musicnext==-1) return -1;
	return m_musicproto[m_musicnext].m_id;
}

int	cDizSound::MusicPosition()
{
	if(m_musicnext==-1) return -1; // no next music
	if(m_musicidx!=m_musicnext) return m_musicstart; // current is fading, next is about to start from m_musicstart
	// we are playing the next music, report curent position
	if(!m_music) return m_musicstart; // should not happend
	int musicpos = (int)( (float)A9_StreamGetPosition(m_music) / (float)A9_StreamGet(m_music,A9_FREQUENCY) * 1000.0f );
	return musicpos; // in ms
}

void cDizSound::MusicStop()
{
	if(m_musicidx==-1) return;
	if(!m_music) { m_musicidx=-1; return; } // should not happened
	A9_StreamSet(m_music,A9_VOLUME,A9_VOLUMEMIN); // silence
	A9_StreamStop(m_music);
	m_music = NULL;
	m_musicidx = -1;
	m_musicpaused = false;
}

void cDizSound::MusicPause( bool pause )
{
	if( m_musicidx==-1) return;
	if(!m_music) { m_musicidx=-1; return; } // should not happened
	if(pause)
	{
		if(m_musicpaused) return; // already paused
		A9_StreamStop(m_music); // stop
	}
	else
	{
		if(!m_musicpaused) return; // not paused
		A9_StreamPlay(m_music,true); // play
	}
	m_musicpaused = pause;
}

void cDizSound::MusicUpdate( float dtime )
{

	if(m_musicidx==-1) // nothing is playing
	{
		if(m_musicnext!=-1) // music scheduled
		{
			// set current stream
			m_music = m_musicproto[m_musicnext].m_stream;
			if(!m_music) { m_musicnext=-1; return; } // cration failure (invalid music file)

			// prepare volume
			if(m_musicfadein>0)
			{
				A9_StreamSet(m_music,A9_VOLUME,A9_VOLUMEMIN); //start from silence
				m_musicvol = 0.0f;
			}
			else
			{
				A9_StreamSet(m_music,A9_VOLUME,A9_VolumePercentToDecibel(g_cfg.m_volmusic));
				m_musicvol = 1.0f;
			}

			// play stream (always looping)
			int start = (int)( (float)m_musicstart / 1000.0f * (float)A9_StreamGet(m_music,A9_FREQUENCY) );
			if(start<0 || start>=A9_StreamGet(m_music,A9_SIZE)) start=0;
			A9_StreamSetPosition(m_music,start);
			A9_StreamPlay(m_music,true);
			m_musicidx = m_musicnext;
			m_musicpos = start;
		}
	}
	else // music is playing
	{
		if(!m_music) { m_musicidx=-1; return; } // should not happened
		if(m_musicpaused) return; // don't update volumes if paused
		if(!A9_StreamIsPlaying(m_music)) return; // don't update volumes if it's not yet playing

		float fadeoutstep	= (m_musicfadeout>0) ? (dtime / (float)m_musicfadeout) : 1.0f;
		float fadeinstep	= (m_musicfadein>0)  ? (dtime / (float)m_musicfadein)  : 1.0f;

		if( m_musicidx == m_musicnext ) // we are keeping this song
		{
			if(m_musicvol<1.0f) // have not reached full volume yet so we have fadein
			{
				m_musicvol+=fadeinstep;
				if(m_musicvol>1.0f) m_musicvol=1.0f;
				A9_StreamSet(m_music, A9_VOLUME, A9_VolumePercentToDecibel( (int)(m_musicvol * g_cfg.m_volmusic) ));
			}
			// else we are very happy

			// check music end
			int musicpos = A9_StreamGetPosition(m_music);
			int musicsize = A9_StreamGet(m_music,A9_SIZE);
			if( musicpos < m_musicpos ) // reached the end of loop and it just started from the begining
				g_script.musicLoop(); // notify scripts; user can stop or play other music in the handler
			m_musicpos = musicpos;
		}
		else // we must fade this one out, because a another is waiting next
		{
			if(m_musicvol>0.0f) // have not reached silence yet, but we have fade out
			{
				m_musicvol-=fadeoutstep;
				if(m_musicvol<0.0f) m_musicvol=0.0f;
				A9_StreamSet(m_music, A9_VOLUME, A9_VolumePercentToDecibel( (int)(m_musicvol * g_cfg.m_volmusic) ));
			}
			else
			{
				MusicStop(); // stop and destroy current music
				// in the next update the next song will be made current and played
			}
		}
	}

}

void cDizSound::MusicVolume( int volume )
{
	if(volume<0) volume=0;
	if(volume>100) volume=100;
	if(volume==g_cfg.m_volmusic) return; // same volume
	g_cfg.m_volmusic = volume;

	int volumedb = A9_VolumePercentToDecibel( (int)(m_musicvol * g_cfg.m_volmusic) );
	if(m_musicidx!=-1 && m_music)
	{
		A9_StreamSet(m_music,A9_VOLUME,volumedb);
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

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

PlAtom cDizSound::all("all");
cDizSound g_sound;

//////////////////////////////////////////////////////////////////////////////////////////////////
// INIT
//////////////////////////////////////////////////////////////////////////////////////////////////
cDizSound::cDizSound()
{
	// samples
	for(int i=0;i<SOUND_VOICES;i++)	m_voice[i] = NULL;
	m_voicecount = 0;

	m_sample_total = 0;		
	m_sample_fail = 0;		
	m_sample_duplicates = 0;
	m_sample_group = 0;

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
	SampleStopAll();
	MusicStop();
	for(; !m_sampleproto.empty(); m_sampleproto.pop_back()) delete m_sampleproto.back();
	m_sampleproto.clear();
	for(; !m_musicproto.empty(); m_musicproto.pop_back()) delete m_musicproto.back();
	m_musicproto.clear();
}

void cDizSound::Update()
{
	if(!A9_IsReady()) return;
	
	// samples - stop (remove) those that have finished playing
	int i;
	m_voicecount = 0;
	for(i=0;i<SOUND_VOICES;i++)
	{
		if(SamplePlaying(i)!=-1) m_voicecount++;
	}
	
	// music
	float dtime = (float)E9_AppGetInt(E9_APP_DELTATIME) / 1000.f;
	MusicUpdate( dtime );

	A9_Update();

}

bool cDizSound::isSupportedExt(const char * ext) 
{
	static const char * supported_ext[] = {".wav",".ogg",".ym",".mod",".it",".xm",".s3m"};
	static const size_t count = sizeof(supported_ext) / sizeof(*supported_ext);
	for(size_t i = 0, e = count; i < e; ++i) {
		if(strcmp(supported_ext[i], ext) == 0) return true;
	}
	return false;
}



//////////////////////////////////////////////////////////////////////////////////////////////////
// SAMPLES
//////////////////////////////////////////////////////////////////////////////////////////////////
bool cDizSound::SampleLoadFile( const char* filepath, int group )
{

	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];
	file_pathsplit(filepath, 0, 0, fname, ext);
	
	if(!isSupportedExt(ext)) return false; // ignore files with unsupported extensions

	m_sample_total++;

	// check name format
	char nm[_MAX_FNAME];
	int instances = 1;
	int ret = sscanf(fname,"%s %i",nm,&instances);
	if(ret==0) 
	{ 
		m_sample_fail++; 
		dlog(LOGAPP, L"! %S (bad name)\n", filepath); 
		return false; 
	}
	if(instances<1) instances=1;

	// check unique id
	PlAtom id(nm);

	int idx = SampleFind(id);
	if( idx!=-1 )
	{
		m_sample_fail++;
		m_sample_duplicates++;
		dlog(LOGSYS, L"! %S (duplicate id)\n", filepath, id);
		return false;
	}

	// load and decompress
	A9BUFFERPROTO bufferproto = A9_BufferPrecache(filepath);
	if(!bufferproto)
	{
		m_sample_fail++;
		dlog(LOGSYS, L"! %S (failed to load)\n", filepath);
		return false;
	}

	// add to list
	tSoundProto* proto = new tSoundProto(id, group, instances, bufferproto);
	m_sampleproto.push_back(proto);

	if(IS_DEVELOPER()) // log for developers
		dlog(LOGAPP, L"  %S [%i]\n", filepath, instances );

	return true;
}

void FFCallback_Sample( const char* filepath, BOOL dir )
{
	if(dir) return;
	bool ret = g_sound.SampleLoadFile(filepath,g_sound.m_sample_group);
}

bool cDizSound::SampleLoad( const char* path, int group )
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
	m_sample_total		= 0;
	m_sample_fail		= 0;
	m_sample_duplicates	= 0;
	m_sample_group		= group;

	// find files on disk
	int archivefiles = F9_ArchiveGetFileCount(0);
	if(archivefiles==0) // if no archive found then open from disk
	{
		file_findfiles( spath, FFCallback_Sample, FILE_FINDREC );
	}
	else // if archive opened, load from it
	{
		for(int i=0;i<archivefiles;i++)
		{
			std::string filename = F9_ArchiveGetFileName(0,i);
			if(strstr(filename.c_str(), spath)==filename)
			{
				FFCallback_Sample(filename.c_str(),false);
			}
		}
	}

	// report
	dlog(LOGAPP, L"Samples report: total=%i, failed=%i (duplicates=%i)\n\n", m_sample_total, m_sample_fail, m_sample_duplicates );

	return true;
}

void cDizSound::SampleUnload( int group )
{
	int i;
	for(i=0;i<m_sampleproto.size();i++)
	{
		if(m_sampleproto[i]->m_group == group)
		{
			SampleStopAll(m_sampleproto[i]->m_id);
			delete m_sampleproto[i];
			m_sampleproto.erase(m_sampleproto.begin() + i);
			i--;
		}
	}
}

int	cDizSound::SamplePlay( PlAtom id, int loop )
{
	int i;
	int protoidx = SampleFind(id);
	if(protoidx==-1) return -1; // invalid id
	tSoundProto* proto = m_sampleproto[protoidx];
	if( proto->m_bufferproto == NULL ) return -1; // invalid proto

	// find instances and free voice
	int count = 0;
	int voiceidx=-1;
	for(i=0;i<SOUND_VOICES;i++)
	{
		if(SamplePlaying(i)==id.handle) count++; // destroy if stopped
		if(m_voice[i]==NULL) voiceidx = i;
	}
	if(count>=proto->m_instances) return -1; // too many instances playing
	if(voiceidx==-1) return -1; // no free voices
	
	// add new voice
	A9BUFFER buffer = A9_BufferCreateFromProto(proto->m_bufferproto);
	if(!buffer) return -1; // wrong format
	A9_BufferSet(buffer,A9_USER,id.handle);
	A9_BufferSet(buffer,A9_VOLUME,A9_VolumePercentToDecibel(g_cfg.m_volfx));
	A9_BufferPlay(buffer,loop);
	m_voice[voiceidx] = buffer;
	
	return voiceidx;
}

int	cDizSound::SamplePlaying( int voiceidx )
{
	assert(0<=voiceidx && voiceidx<SOUND_VOICES);
	if(m_voice[voiceidx]==NULL) return -1;
	if(!A9_BufferIsPlaying(m_voice[voiceidx]))
	{
		A9_BufferDestroy(m_voice[voiceidx]);
		m_voice[voiceidx] = NULL;
		return -1;
	}
	return A9_BufferGet(m_voice[voiceidx],A9_USER);
}

void cDizSound::SampleStop( int voiceidx )
{
	assert(0<=voiceidx && voiceidx<SOUND_VOICES);
	if(m_voice[voiceidx]==NULL) return;
	A9_BufferStop(m_voice[voiceidx]);
	A9_BufferDestroy(m_voice[voiceidx]);
	m_voice[voiceidx] = NULL;
}

void cDizSound::SampleStopAll( PlAtom id )
{
	for(int i=0;i<SOUND_VOICES;i++)
	{
		A9BUFFER buffer = m_voice[i];
		if( buffer && (id == all || A9_BufferGet(buffer,A9_USER)==id.handle) )
			SampleStop(i);
	}
}

void cDizSound::SampleVolume( int volume )
{
	if(volume<0) volume=0;
	if(volume>100) volume=100;
	if(volume==g_cfg.m_volfx) return; // same volume
	g_cfg.m_volfx = volume;

	int volumedb = A9_VolumePercentToDecibel(volume);
	for(int i=0;i<SOUND_VOICES;i++)
	{
		A9BUFFER buffer = m_voice[i];
		if(!buffer) continue;
		A9_BufferSet(buffer,A9_VOLUME,volumedb);
	}

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
	tMusicProto* proto = new tMusicProto(id, group, stream);
	m_musicproto.push_back(proto);

	if(IS_DEVELOPER()) // log for developers
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
	int i;
	for(i=0;i<m_musicproto.size();i++)
	{
		if(m_musicproto[i]->m_group == group)
		{
			if(i==m_musicidx) MusicStop(); // stop if current
			if(m_musicnext==i) m_musicnext=-1; // clear if next
			delete m_musicproto[i];
			m_musicproto.erase(m_musicproto.begin() + i);
			i--;
		}
	}
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
	return m_musicproto[m_musicnext]->m_id;
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
			m_music = m_musicproto[m_musicnext]->m_stream;
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

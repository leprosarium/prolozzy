///////////////////////////////////////////////////////////////////////////////////////////////////
// A9Codec_wav.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "A9Codec_wav.h"

a9Codec_wav::a9Codec_wav()
{
	m_type = A9_CODEC_WAV;
	m_file = NULL;
	m_datapos = 0;
}

a9Codec_wav::~a9Codec_wav()
{
}

int a9Codec_wav::Init()
{
	return A9_OK;
}

int a9Codec_wav::Done()
{
	return A9_OK;
}

int	a9Codec_wav::Open( const std::string & name )
{
	if(m_status!=A9_CODEC_CLOSED) return A9_FAIL;
	
	m_file = files->OpenFile(name); 
	if(!m_file) return A9_FAIL;

	int ret;

	// read header
	int datapos = 0;
	int datasize = 0;
	WAVEFORMATEX wfe;
	ret = ReadWaveInfo( m_file, &wfe, &datapos, &datasize );
	if(ret!=A9_OK) { files->FileClose(m_file); return ret; }
	if(datasize<=0) { files->FileClose(m_file); return A9_FAIL; }

	m_info.m_depth		= wfe.wBitsPerSample;
	m_info.m_signed		= 1;
	m_info.m_channels	= wfe.nChannels;
	m_info.m_frequency	= wfe.nSamplesPerSec;
	m_info.m_size		= datasize / m_info.SampleSize();
	m_datapos			= datapos;
	
	assert(wfe.nBlockAlign==m_info.SampleSize()); // safe

	m_status = A9_CODEC_OPENED;
	return A9_OK;
}

int	a9Codec_wav::BeginRender( int pos, int loop )
{
	if(m_status!=A9_CODEC_OPENED) return A9_FAIL;
	m_loop = loop;
	int ret = m_file->Seek(m_datapos + pos * m_info.SampleSize(), 0);
	if(ret!=0) return A9_FAIL; 

	m_status = A9_CODEC_RENDERING;
	return A9_OK;
}

int	a9Codec_wav::Render( byte* buffer, int size )
{
	if(m_status!=A9_CODEC_RENDERING) return A9_FAIL;
	if(size<=0) return A9_FAIL;
	int buffersize	= size * m_info.SampleSize();			// read request size in bytes
	int datasize	= m_info.m_size*m_info.SampleSize();	// size of file pcm data in bytes
	int renderpos	= 0;									// current render position in buffer in bytes
	int rendersize	= 0;									// current rendered size in bytes

	while(1)
	{
		int datapos = static_cast<int>(m_file->Tell()) - m_datapos;				// current position in file pcm data in bytes
		int dataeof = datapos > datasize-m_info.SampleSize();	// eof in file pcm data

		if( m_loop && dataeof ) // reached end of data from file
		{
			m_file->Seek(m_datapos, 0 ); // rewind for read
			datapos = 0;
		}

		// prevent read overflow
		int readsize = buffersize-rendersize;
		if( datapos + readsize > datasize ) readsize = datasize-datapos;

		int ret = static_cast<int>(m_file->Read(buffer + renderpos, readsize));
		if( ret==0 ) return 0; // nothing to read eof
		renderpos += ret;
		rendersize += ret;
		datapos += ret;
		
		if( rendersize>=buffersize || !m_loop ) // finished or noloop
			return rendersize/m_info.SampleSize(); 
	}
}

int	a9Codec_wav::EndRender()
{
	if(m_status!=A9_CODEC_RENDERING) return A9_FAIL;
	m_status = A9_CODEC_OPENED;
	return A9_OK;
}

int	a9Codec_wav::Close()
{
	if(m_status!=A9_CODEC_OPENED) return A9_FAIL;
	if(m_file) files->FileClose(m_file);
	m_file = NULL;
	m_status = A9_CODEC_CLOSED;
	return A9_OK;
}

int a9Codec_wav::ReadWaveInfo( F9FILE file, WAVEFORMATEX* wfe, int* datapos, int* datasize )
{
	int64			r;
	int				size;
	char			header[] = "RIFF";
	WAVEFORMATEX*	pwfe = NULL;

	*datasize = 0;
	*datapos = 0;

	r = file->Read(header, 4); if(r!=4) return A9_FAIL;
	if(0!=strcmp(header, "RIFF")) return A9_FAIL;

	r = file->Read(&size, 4); if(r!=4) return A9_FAIL;

	r = file->Read(header, 4);	if(r!=4) return A9_FAIL;
	if(0!=strcmp(header, "WAVE")) return A9_FAIL;
	
	do
	{
		r = file->Read(header, 4); if(r!=4) return A9_FAIL;
		if(0==strcmp(header, "fmt "))
		{
			r = file->Read(&size, 4); if(r!=4) return A9_FAIL;
			pwfe = (WAVEFORMATEX*)malloc(size);
			r = file->Read(pwfe, size);	if(r!=size) { free(pwfe); return A9_FAIL; }
			if(size>sizeof(WAVEFORMATEX)) size = sizeof(WAVEFORMATEX);
			memcpy( wfe, pwfe, size );
			free(pwfe);
			if(wfe->wFormatTag!=WAVE_FORMAT_PCM) return A9_UNSUPORTED;
		}
		else
		if(0==strcmp(header, "data"))
		{
			r = file->Read(&size, 4); if(r!=4) return A9_FAIL;
			*datapos = static_cast<int>(file->Tell());
			*datasize = size;
			return A9_OK;
		}
		else // unknown chunk
		{
			r = file->Read(&size, 4); if(r!=4) return A9_FAIL;
			file->Seek(size, 1);
		}
	}
	while(!file->Eof());

	return A9_FAIL;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
/* sample code hm
void WriteWaveHeader( FILE* file )
{
	// write header
	int data_written = 0;
	long l;
	const int fmt_size = 8 + 16;
	const int data_size = 8 + data_written;
	const int file_size = fmt_size + data_size;
	int bps = 44100 * 2 * 16 / 8;
	int align = 2*16/8;
	fwrite("RIFF",1,4,file);
	fwrite(&file_size,1,4,file);
	fwrite("WAVE",1,4,file);
	fwrite("fmt ",1,4,file);
	l=16;	fwrite(&l,1,4,file);
	l=1;	fwrite(&l,1,2,file);
	l=2;	fwrite(&l,1,2,file);
	l=44100;fwrite(&l,1,4,file);
	l=bps;	fwrite(&l,1,4,file);
	l=align;fwrite(&l,1,2,file);
	l=16;	fwrite(&l,1,2,file);
	fwrite("data",1,4,file);
	l=0;	fwrite(&l,1,4,file);

	//...

	// repair header
	const int fmt_size = 8 + 16;
	const int data_size = 8 + data_written;
	const int file_size = fmt_size + data_size;
	fseek(file, 4, SEEK_SET);
	fwrite(&file_size,1,4,file);
	fseek(file, 12 + fmt_size + 4, SEEK_SET);
	fwrite(&data_written,1,4,file);

}
*/
///////////////////////////////////////////////////////////////////////////////////////////////////

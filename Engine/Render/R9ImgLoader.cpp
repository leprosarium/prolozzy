///////////////////////////////////////////////////////////////////////////////////////////////////
// R9ImgLoader.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include <string>

#include "E9Config.h"
#include "R9ImgLoader.h"
#include "F9FileDisk.h"
#include "D9Log.h"

#ifdef R9_ENABLE_PNG
#ifdef _DEBUG
#pragma comment( lib, "..\\Libs\\Lpng\\lpng_d.lib" )
#else
#pragma comment( lib, "..\\Libs\\Lpng\\lpng.lib" )
#endif

#define PNG_INTERNAL 1
#include "png.h"
#endif


#ifdef R9_ENABLE_JPG
#ifdef _DEBUG
#pragma comment( lib, "..\\Libs\\Ljpeg\\ljpeg_d.lib" )
#else
#pragma comment( lib, "..\\Libs\\Ljpeg\\ljpeg.lib" )
#endif

#undef FAR
#define XMD_H
#define HAVE_BOOLEAN
//#define NEED_FAR_POINTERS ???
#include "jpeglib.h"
//#include <setjmp.h> ???
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////
// LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
bool R9_ImgLoadFile(const std::wstring & name, r9Img* img )
{
	if(!img || img->isValid() ) return FALSE;
	img->clear();

	// type
	int type = R9_IMG_UNKNOWN;
	std::wstring ext = file_path2ext(name);
	if( ext == L"tga") type = R9_IMG_TGA; else
	if( ext == L"jpg") type = R9_IMG_JPG; else
	if( ext == L"png") type = R9_IMG_PNG; // else...
	if( type==R9_IMG_UNKNOWN ) return FALSE; // unsupported file format

	// read
	bool ret = false;
	if(f9File * file = files->OpenFile(name))
	{
		switch(type)
		{
			case R9_IMG_TGA:	ret = R9_ImgReadTGA( file, img ); break;
			case R9_IMG_PNG:	ret = R9_ImgReadPNG( file, img ); break;
			case R9_IMG_JPG:	ret = R9_ImgReadJPG( file, img ); break;
		}
		files->FileClose(file);
	}
	return ret;
}

bool R9_ImgSaveFile( const std::wstring & name, r9Img* img )
{
	if(!img || !img->isValid()) return FALSE;

	// type
	int type = R9_IMG_UNKNOWN;
	std::wstring ext = file_path2ext(name);
	if(ext == L"png") type = R9_IMG_PNG; else
	if(ext == L"jpg") type = R9_IMG_JPG; else
	if(ext == L"tga") type = R9_IMG_TGA; // else...
	if( type==R9_IMG_UNKNOWN ) return false; // unsupported file format

	// write
	bool ret = false;
	F9FILE file = Files::OpenFile<f9FileDisk>(name, F9_WRITE); 
	if(!file) return ret;
	switch(type)
	{
		case R9_IMG_TGA:	ret = R9_ImgWriteTGA( file, img ); break;
		case R9_IMG_PNG:	ret = R9_ImgWritePNG( file, img ); break;
		case R9_IMG_JPG:	ret = R9_ImgWriteJPG( file, img ); break;
	}
	files->FileClose(file);

	return ret;
}

bool R9_ImgLoadHeader( const std::wstring & name, r9Img* img )
{
	if(!img || img->isValid() ) return false;
	img->clear();

	// type
	int type = R9_IMG_UNKNOWN;
	std::wstring ext = file_path2ext(name);
	if(ext == L"tga") type = R9_IMG_TGA; else
	if(ext == L"jpg") type = R9_IMG_JPG; else
	if(ext == L"png") type = R9_IMG_PNG; // else...
	if( type==R9_IMG_UNKNOWN ) return false; // unsupported file format

	// read
	bool ret = false;
	F9FILE file = files->OpenFile(name);
	if(!file) return false;
	switch(type)
	{
		case R9_IMG_TGA:	ret = R9_ImgReadHeaderTGA( file, img ); break;
		case R9_IMG_PNG:	ret = R9_ImgReadHeaderPNG( file, img ); break;
		case R9_IMG_JPG:	ret = R9_ImgReadHeaderJPG( file, img ); break;
	}
	files->FileClose(file);

	return ret;
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// TGA LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
bool R9_ImgReadTGA( F9FILE file, r9Img* img )
{
	if(!file || !img || img->isValid()) return false;
	img->clear();

	int64 size;
	r9ImgHeaderTGA header;

	// header
	size = file->Read(&header, sizeof(header));
	if(size<sizeof(header))return false;
	if(header.m_ImageType==3 && header.m_PixelDepth!=8) return false;
	if(header.m_ImageType==2 && (header.m_PixelDepth!=24 && header.m_PixelDepth!=32)) return false;
	if(header.m_width==0 || header.m_height==0) return false;

	// set img
	img->m_width	= header.m_width;
	img->m_height	= header.m_height;
	img->m_pf		= (header.m_PixelDepth==8) ? R9_PF_A : ((header.m_PixelDepth==24) ? R9_PF_RGB : R9_PF_ARGB);
	R9_ImgCreate(img);

	// read
	size = file->Read( img->m_data, img->m_size);
	if(size!=img->m_size) return false; // read error

	// upsidedown
	if((header.m_ImageDescriptor & 32) == 0) 
		R9_ImgFlipV(img);

	return true;
}

bool R9_ImgWriteTGA( F9FILE file, r9Img* img )
{
	if(!file || !img || !img->isValid()) return false;

	R9_ImgFlipV(img);

	r9ImgHeaderTGA header;
	memset(&header, 0, sizeof (header));
	header.m_IDLength					= 0;
	header.m_ColormapType				= 0;
	header.m_ImageType					= (img->m_pf==R9_PF_A)?3:2;
	header.m_ColormapSpecification[0]	= 0;
	header.m_ColormapSpecification[1]	= 0;
	header.m_ColormapSpecification[2]	= 0;
	header.m_ColormapSpecification[3]	= 0;
	header.m_ColormapSpecification[4]	= R9_PFBpp(img->m_pf);
	header.m_XOrigin					= 0;
	header.m_YOrigin					= 0;
	header.m_width						= img->m_width;
	header.m_height						= img->m_height;
	header.m_PixelDepth					= R9_PFBpp(img->m_pf);
	header.m_ImageDescriptor			= 0; // upside down

	file->Write( &header, sizeof(header));
	file->Write( img->m_data, img->m_size);

	R9_ImgFlipV(img);

	return true;
}

bool R9_ImgReadHeaderTGA( F9FILE file, r9Img* img )
{
	if(!file || !img || img->isValid()) return false;
	img->clear();

	int64 size;
	r9ImgHeaderTGA header;

	// header
	size = file->Read(&header,sizeof(header));
	if(size<sizeof(header))return false;
	if(header.m_PixelDepth!=24 && header.m_PixelDepth!=32) return false;
	if(header.m_width==0 || header.m_height==0) return false;

	// set img
	img->m_width	= header.m_width;
	img->m_height	= header.m_height;
	img->m_pf		= header.m_PixelDepth==24 ? R9_PF_RGB : R9_PF_ARGB;

	return true;
}


///////////////////////////////////////////////////////////////////////////////////////////////////
// PNG LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef R9_ENABLE_PNG

static F9FILE r9_imgpng_file = NULL;

static void	R9_ImgPNG_ReadData( png_structp png_ptr, png_bytep data, png_size_t length )
{
	assert(r9_imgpng_file!=NULL);
	auto ret = r9_imgpng_file->Read( data, length);
	assert(ret==length);
}

static void R9_ImgPNG_WriteData( png_structp png_ptr, png_bytep data, png_size_t length )
{
	assert(r9_imgpng_file!=NULL);
	auto ret = r9_imgpng_file->Write(data, length);
	assert(ret==length);
}

static png_voidp R9_ImgPNG_Malloc( png_structp png_ptr, png_size_t size )
{
	return new byte[size];
}

static void R9_ImgPNG_Free( png_structp png_ptr, png_voidp data )
{
	delete [] data;
}

static void R9_ImgPNG_FatalError( png_structp png_ptr, png_const_charp message )
{
	elog::sys() << "ERROREXIT: " << message << std::endl;
	exit(-1);
}

static void R9_ImgPNG_Warning( png_structp png_ptr, png_const_charp message )
{
	elog::rnd() << message;
}

bool R9_ImgReadPNG( F9FILE file, r9Img* img )
{
	if(file==NULL || img==NULL) return false;
	R9_ImgDestroy(img);
	
	// check png sig
	const int headersize = 8;
	byte header[headersize];
	if(file->Read( header, headersize) != headersize) return false;

	BOOL ispng = png_check_sig(header, headersize) != 0;
	if(!ispng) { elog::rnd() << "png sig failed" << std::endl; return false; }

	// create png structures
	png_structp png_ptr = png_create_read_struct_2( PNG_LIBPNG_VER_STRING,
												NULL,
												R9_ImgPNG_FatalError,
												R9_ImgPNG_Warning,
												NULL,
												R9_ImgPNG_Malloc,
												R9_ImgPNG_Free );

	if(png_ptr==NULL) return false;

	png_infop info_ptr = png_create_info_struct(png_ptr);
	if(info_ptr == NULL) 
	{
		png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
		return false;
	}

	png_infop end_info = png_create_info_struct(png_ptr);
	if(end_info == NULL) 
	{
		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
		return false;
	}

	// set global file
	r9_imgpng_file = file;
	png_set_read_fn(png_ptr, NULL, R9_ImgPNG_ReadData);

	// read off the info on the image
	png_set_sig_bytes(png_ptr, headersize);
	png_read_info(png_ptr, info_ptr);

	png_uint_32 pngwidth;
	png_uint_32 pngheight;	
	int32 pngbits;
	int32 pngpf;

	png_get_IHDR(png_ptr, info_ptr,
				&pngwidth, &pngheight,
				&pngbits, &pngpf,
				NULL,                        // interlace
				NULL,                        // compression_type
				NULL);                       // filter_type

	BOOL alpha = FALSE;
	img->m_width = pngwidth;
	img->m_height = pngheight;
	img->m_pf = R9_PF_RGB;
	assert(img->m_width!=0 && img->m_height!=0);

	// Strip off any 16 bit info
	if(pngbits==16) png_set_strip_16(png_ptr);

	// Expand a transparency channel into a full alpha channel...
	if(png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) 
	{
		png_set_expand(png_ptr);
		alpha = TRUE;
	}

	if( pngpf == PNG_COLOR_TYPE_RGB) 
	{
		png_set_expand(png_ptr);
		img->m_pf = alpha ? R9_PF_ARGB : R9_PF_RGB;
	} 
	else 
	if( pngpf == PNG_COLOR_TYPE_RGB_ALPHA) 
	{
		png_set_expand(png_ptr);
		img->m_pf = R9_PF_ARGB;
	}
	else
	{
		elog::rnd() << "png wrong color format" << std::endl;
		png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
		return false;
	}

	// Update the info pointer with the result of the transformations above...
	png_read_update_info(png_ptr, info_ptr);

	png_uint_32 rowbytes = png_get_rowbytes(png_ptr, info_ptr);
	if(img->m_pf==R9_PF_RGB) assert(rowbytes==img->m_width*3);
	else
	if(img->m_pf==R9_PF_RGB) assert(rowbytes==img->m_width*4);

	// create image
	R9_ImgCreate(img);

	// set up the row pointers...
	png_bytep* rowpointers = new png_bytep[img->m_height];
	for(int i=0; i<img->m_height; i++)
	  rowpointers[i] = img->m_data + (i*rowbytes);

	// read image
	png_read_image(png_ptr, rowpointers);

	// release
	png_read_end(png_ptr, NULL);
	png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
	delete [] rowpointers;
	r9_imgpng_file = NULL;

	R9_ImgFlipRGB(img); // it was BGR, we want it RGB !
	return true;
}

bool R9_ImgWritePNG( F9FILE file, r9Img* img )
{
	
	const int32 compressionlevel = 6;
	const int32 strategy = 0;
	const int32 filter = PNG_ALL_FILTERS;
	
	if(img->m_pf!=R9_PF_RGB && img->m_pf!=R9_PF_ARGB && img->m_pf!=R9_PF_BGR && img->m_pf!=R9_PF_ABGR )
	{
		elog::rnd() << "png format not supported" << std::endl; return false;
	}

	png_structp png_ptr = png_create_write_struct_2( PNG_LIBPNG_VER_STRING,
												 NULL,
												 R9_ImgPNG_FatalError,
												 R9_ImgPNG_Warning,
												 NULL,
												 R9_ImgPNG_Malloc,
												 R9_ImgPNG_Free );
	if(png_ptr==NULL) return false;

	png_infop info_ptr = png_create_info_struct(png_ptr);
	if(info_ptr==NULL)
	{
		png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
		return false;
	}

	r9_imgpng_file = file;
	png_set_write_fn(png_ptr, NULL, R9_ImgPNG_WriteData, NULL); // last one was imgpng_flush (fflush) that did nothing

	// set the compression level, image filters, and compression strategy...
	png_ptr->flags |= PNG_FLAG_ZLIB_CUSTOM_STRATEGY;
	png_ptr->zlib_strategy = strategy;
	png_set_compression_window_bits(png_ptr, 15);
	png_set_compression_level(png_ptr, compressionlevel);
	png_set_filter(png_ptr, 0, filter);

	// flip rgb temporary, if necessarily - png likes BGR!
	if(img->m_pf==R9_PF_ARGB || img->m_pf==R9_PF_RGB) R9_ImgFlipRGB(img);

	// Set the image information here.  Width and height are up to 2^31,
	// bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
	// the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
	// PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
	// or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
	// PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
	// currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE. REQUIRED
	
	if(img->m_pf==R9_PF_RGB || img->m_pf==R9_PF_BGR)
	{
		png_set_IHDR(png_ptr, info_ptr, 
				   img->m_width, img->m_height,	// the width & height
				   8, PNG_COLOR_TYPE_RGB,       // bit_depth, color_type,
				   NULL,                        // no interlace
				   NULL,                        // compression type
				   NULL);                       // filter type
	}
	else 
	if(img->m_pf==R9_PF_ARGB || img->m_pf==R9_PF_ABGR)
	{
		png_set_IHDR(png_ptr, info_ptr, 
				   img->m_width, img->m_height, // the width & height
				   8, PNG_COLOR_TYPE_RGB_ALPHA, // bit_depth, color_type,
				   NULL,                        // no interlace
				   NULL,                        // compression type
				   NULL);                       // filter type
	}

	png_write_info(png_ptr, info_ptr);
	png_bytep* rowpointers = new png_bytep[img->m_height];
	for(int i=0; i<img->m_height; i++)
		rowpointers[i] = (png_bytep)(img->m_data+(i*img->lineSize()));

	png_write_image(png_ptr, rowpointers);

	// release
	png_write_end(png_ptr, info_ptr);
	png_destroy_write_struct(&png_ptr, &info_ptr);
	delete [] rowpointers;
	r9_imgpng_file = NULL;

	// flip rgb back, if necessarily
	if(img->m_pf==R9_PF_ARGB || img->m_pf==R9_PF_RGB) R9_ImgFlipRGB(img);

	return true;
}

bool R9_ImgReadHeaderPNG( F9FILE file, r9Img* img )
{
	if(file==NULL || img==NULL) return false;
	R9_ImgDestroy(img);
	
	// check png sig
	const int headersize = 8;
	byte header[headersize];
	if(file->Read( header, headersize)!=headersize) return false;

	BOOL ispng = png_check_sig(header, headersize) != 0;
	if(!ispng) { elog::rnd() << "png sig failed" << std::endl; return false; }

	// create png structures
	png_structp png_ptr = png_create_read_struct_2( PNG_LIBPNG_VER_STRING,
												NULL,
												R9_ImgPNG_FatalError,
												R9_ImgPNG_Warning,
												NULL,
												R9_ImgPNG_Malloc,
												R9_ImgPNG_Free );

	if(png_ptr==NULL) return false;

	png_infop info_ptr = png_create_info_struct(png_ptr);
	if(info_ptr == NULL) 
	{
		png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
		return false;
	}

	png_infop end_info = png_create_info_struct(png_ptr);
	if(end_info == NULL) 
	{
		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
		return false;
	}

	// set global file
	r9_imgpng_file = file;
	png_set_read_fn(png_ptr, NULL, R9_ImgPNG_ReadData);

	// read off the info on the image
	png_set_sig_bytes(png_ptr, headersize);
	png_read_info(png_ptr, info_ptr);

	png_uint_32 pngwidth;
	png_uint_32 pngheight;	
	int32 pngbits;
	int32 pngpf;

	png_get_IHDR(png_ptr, info_ptr,
				&pngwidth, &pngheight,
				&pngbits, &pngpf,
				NULL,                        // interlace
				NULL,                        // compression_type
				NULL);                       // filter_type

	BOOL alpha = FALSE;
	img->m_width = pngwidth;
	img->m_height = pngheight;
	img->m_pf = R9_PF_RGB;

	// release
	png_read_end(png_ptr, NULL);
	png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
	r9_imgpng_file = NULL;

	return true;
}

#else

bool	R9_ImgReadPNG		( F9FILE file, r9Img* img )	{ return false; }
bool	R9_ImgWritePNG		( F9FILE file, r9Img* img )	{ return false; }
bool	R9_ImgReadHeaderPNG	( F9FILE file, r9Img* img )	{ return false; }

#endif


///////////////////////////////////////////////////////////////////////////////////////////////////
// JPG LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef R9_ENABLE_JPG

int r9_imgjpg_quality = 32; // 0..100

static int R9_ImgJPG_ReadData( void* file, unsigned char* data, int length )
{
	assert(file!=NULL);
	int ret = F9_FileRead( data, length, (F9FILE)file );
	return ret;
}
static int R9_ImgJPG_WriteData( void* file, unsigned char* data, int length )
{
	assert(file!=NULL);
	int ret = F9_FileWrite( data, length, (F9FILE)file );
	return ret;
}
static int R9_ImgJPG_Flush( void* file )
{
	return 0;
}
static int R9_ImgJPG_Error( void* file )
{
	assert(file!=NULL);
	return FALSE; // report no error (called after file write)
}

BOOL R9_ImgReadJPG( F9FILE file, r9Img* img )
{

	if(file==NULL || img==NULL) return FALSE;
	R9_ImgDestroy(img);

	JFREAD  = R9_ImgJPG_ReadData;
	JFERROR = R9_ImgJPG_Error;

	jpeg_decompress_struct cinfo;
	jpeg_error_mgr jerr;

	cinfo.err = jpeg_std_error(&jerr);	// set up the normal JPEG error routines.
	cinfo.client_data = file;			// set the file into the client_data

	// Now we can initialize the JPEG decompression object.
	jpeg_create_decompress(&cinfo); // !!! in asta crapa ca nu da bine in sizeof(jpeg_decompress_struct) >>> se rezolva dc inlocuiesc in librarie boolean cu BOOL

	jpeg_stdio_src(&cinfo);

	// Read file header, set default decompression parameters
	jpeg_read_header(&cinfo, true);

	if(cinfo.out_color_space != JCS_RGB)
	{
		jpeg_destroy_decompress(&cinfo);
		return FALSE;
	}

	// Start decompressor
	jpeg_start_decompress(&cinfo);

	// allocate the bitmap space and init internal variables...
	img->m_width = cinfo.output_width;
	img->m_height = cinfo.output_height;
	img->m_pf = R9_PF_RGB;
	R9_ImgCreate(img);

	// Set up the row pointers...
	int rowbytes = cinfo.output_width * cinfo.output_components;
	assert(rowbytes==R9_ImgLineSize(img));
	for(int i=0; i<img->m_height; i++)
	{
	  JSAMPROW rowpointer = img->m_data + (i * rowbytes);
	  jpeg_read_scanlines(&cinfo, &rowpointer, 1);
	}

	// Finish decompression
	jpeg_finish_decompress(&cinfo);

	// Release JPEG decompression object
	// This is an important step since it will release a good deal of memory.
	jpeg_destroy_decompress(&cinfo);

	R9_ImgFlipRGB(img); // jpegs are BGR
	return TRUE;
}

bool R9_ImgWriteJPG( F9FILE file, r9Img* img )
{
	if(file==NULL || img==NULL) return false;
	if(!R9_ImgIsValid(img)) return false;
	if(R9_PFSpp(img->m_pf)!=3) return false; // 24bpp / 3bytes

	R9_ImgFlipRGB(img); // jpegs are BGR

	JFWRITE = R9_ImgJPG_WriteData;
	JFFLUSH = R9_ImgJPG_Flush;
	JFERROR = R9_ImgJPG_Error;

	jpeg_compress_struct cinfo;
	jpeg_error_mgr jerr;

	cinfo.err = jpeg_std_error(&jerr);	// set up the normal JPEG error routines.
	cinfo.client_data = file;			// set the file into the client_data

	// Now we can initialize the JPEG decompression object.
	jpeg_create_compress(&cinfo); // !!!

	jpeg_stdio_dest(&cinfo);

	// Params for compression
	cinfo.image_width = img->m_width;
	cinfo.image_height = img->m_height;
	cinfo.input_components = 3;
	cinfo.in_color_space = JCS_RGB;
	
	jpeg_set_defaults(&cinfo);
	jpeg_set_quality(&cinfo, r9_imgjpg_quality, TRUE); // limit to baseline-JPEG values

	// Start compressor
	jpeg_start_compress(&cinfo, TRUE);

	JSAMPROW row_pointer[1];
	int row_stride;	// physical row width in image buffer
	row_stride = img->m_width * 3;

	while (cinfo.next_scanline < cinfo.image_height) 
	{
		row_pointer[0] = & img->m_data[cinfo.next_scanline * row_stride];
		(void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
	}

	// Finish compression
	jpeg_finish_compress(&cinfo);
	jpeg_destroy_compress(&cinfo);

	R9_ImgFlipRGB(img); // back to RGB
	return true;
}

BOOL R9_ImgReadHeaderJPG( F9FILE file, r9Img* img )
{

	if(file==NULL || img==NULL) return FALSE;
	R9_ImgDestroy(img);

	JFREAD  = R9_ImgJPG_ReadData;
	JFERROR = R9_ImgJPG_Error;

	jpeg_decompress_struct cinfo;
	jpeg_error_mgr jerr;

	cinfo.err = jpeg_std_error(&jerr);	// set up the normal JPEG error routines.
	cinfo.client_data = file;			// set the file into the client_data

	// Now we can initialize the JPEG decompression object.
	jpeg_create_decompress(&cinfo); // !!! in asta crapa ca nu da bine in sizeof(jpeg_decompress_struct) >>> se rezolva dc inlocuiesc in librarie boolean cu BOOL

	jpeg_stdio_src(&cinfo);

	// Read file header, set default decompression parameters
	jpeg_read_header(&cinfo, true);

	if(cinfo.out_color_space != JCS_RGB)
	{
		jpeg_destroy_decompress(&cinfo);
		return FALSE;
	}

	// allocate the bitmap space and init internal variables...
	img->m_width = cinfo.image_width;
	img->m_height = cinfo.image_height;
	img->m_pf = R9_PF_RGB; // cinfo.num_components 3

	// Release JPEG decompression object
	// This is an important step since it will release a good deal of memory.
	jpeg_destroy_decompress(&cinfo);

	void* v=malloc(1);

	return TRUE;
}

void R9_ImgSetQualityJPG( int quality )
{
	if(quality<0)	quality=0;
	if(quality>100)	quality=100;
	r9_imgjpg_quality = quality;
}

#else

bool	R9_ImgReadJPG		( F9FILE file, r9Img* img )	{ return false; }
bool	R9_ImgWriteJPG		( F9FILE file, r9Img* img )	{ return false; }
bool	R9_ImgReadHeaderJPG	( F9FILE file, r9Img* img )	{ return false; }
void	R9_ImgSetQualityJPG	( int quality )				{}

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

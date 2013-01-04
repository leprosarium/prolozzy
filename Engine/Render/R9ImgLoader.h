///////////////////////////////////////////////////////////////////////////////////////////////////
// R9ImgLoader.h
// Image Library loading and saving
// Control: R9_ENABLE_PNG, R9_ENABLE_JPG
// Interface:
// R9_ImgLoadFile, R9_ImgSaveFile, R9_ImgLoadHeader
// R9_ImgSetQualityJPG, ...
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __R9IMGLOADER_H__
#define __R9IMGLOADER_H__

#include "R9Img.h"
#include "F9Files.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
// LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL	R9_ImgLoadFile	( const char* name, r9Img* img );
BOOL	R9_ImgSaveFile	( char* name, r9Img* img );
BOOL	R9_ImgLoadHeader( char* name, r9Img* img );

///////////////////////////////////////////////////////////////////////////////////////////////////
// TGA LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
struct r9ImgHeaderTGA
{
	byte	m_IDLength;
	byte	m_ColormapType;
	byte	m_ImageType;
	byte	m_ColormapSpecification[5];
	word	m_XOrigin;
	word	m_YOrigin;
	word	m_width;
	word	m_height;
	byte	m_PixelDepth;
	byte	m_ImageDescriptor;
};

BOOL	R9_ImgReadTGA		( F9FILE file, r9Img* img );
BOOL	R9_ImgWriteTGA		( F9FILE file, r9Img* img );
BOOL	R9_ImgReadHeaderTGA	( F9FILE file, r9Img* img );

///////////////////////////////////////////////////////////////////////////////////////////////////
// PNG LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL	R9_ImgReadPNG		( F9FILE file, r9Img* img );
BOOL	R9_ImgWritePNG		( F9FILE file, r9Img* img );
BOOL	R9_ImgReadHeaderPNG	( F9FILE file, r9Img* img );

///////////////////////////////////////////////////////////////////////////////////////////////////
// JPG LOADER
///////////////////////////////////////////////////////////////////////////////////////////////////
BOOL	R9_ImgReadJPG		( F9FILE file, r9Img* img );
BOOL	R9_ImgWriteJPG		( F9FILE file, r9Img* img );
BOOL	R9_ImgReadHeaderJPG	( F9FILE file, r9Img* img );
void	R9_ImgSetQualityJPG	( int quality );				// 0..100 jpeg saving quality (32,64,80,90,98); default=32

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

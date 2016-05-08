///////////////////////////////////////////////////////////////////////////////////////////////////
// R9TexturePool.h
// Texture pool manager
///////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef __R9TEXTUREPOOL_H__
#define __R9TEXTUREPOOL_H__

#include "R9Render.h"

#include <vector>
#include <unordered_map>

class r9TexturePool : private std::vector<R9TEXTURE>
{
public:
					r9TexturePool()									{};
		void		Done();

		int			Add( R9TEXTURE texture, const std::wstring & name );		// add a specific texture, return index
		int			Load( const std::wstring & name, bool noduplicate = true );// load a texture by file; if noduplicate and texture already in pool, the same index is returned
		int			Find( const std::wstring & name );						// search for a texture; return index
		R9TEXTURE	Get( int idx )							{ if(idx >= 0 && static_cast<size_type>(idx) < size()) return (*this)[idx]; return 0; }

		typedef std::unordered_map<std::wstring, int> Hash;
		Hash index;
};

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////
// R9TexturePool.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "E9String.h"
#include "R9TexturePool.h"

void r9TexturePool::Done()
{
	// destroy any left content

	for(iterator i = begin(), e = end(); i != e; ++i)
		if(*i)	
			R9_TextureDestroy(*i); 
	index.clear();
	clear();
}

int r9TexturePool::Add( R9TEXTURE texture, const std::string & name )
{
	assert(texture!=NULL);
	int i = size();
	push_back(texture);
	index.insert(Hash::value_type(name, i));
	return i;
}

int r9TexturePool::Load( const std::string & name, bool noduplicate )
{
	if(noduplicate)
	{
		int idx = Find(name);
		if(idx!=-1) return idx;
	}
	R9TEXTURE texture = R9_TextureLoad(name);
	if(!texture) return -1;
	return Add(texture, name);
}

int r9TexturePool::Find( const std::string & name )
{
	Hash::const_iterator i = index.find(name);
	if( i == index.end())
		return -1;
	return i->second;
}
///////////////////////////////////////////////////////////////////////////////////////////////////

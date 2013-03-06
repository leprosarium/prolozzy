///////////////////////////////////////////////////////////////////////////////////////////////////
// R9TexturePool.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "E9String.h"
#include "R9TexturePool.h"

void r9TexturePool::Done()
{
	guard(r9TexturePool::Done);
	// destroy any left content

	for(iterator i = begin(), e = end(); i != e; ++i)
		if(*i)	
			R9_TextureDestroy(*i); 
	index.clear();
	clear();
	unguard();
}

int r9TexturePool::Add( R9TEXTURE texture, const std::string & name )
{
	guard(r9TexturePool::Add);
	sassert(texture!=NULL);
	int i = size();
	push_back(texture);
	index.insert(Hash::value_type(name, i));
	return i;
	unguard();
}

int r9TexturePool::Load( const std::string & name, bool noduplicate )
{
	guard(r9TexturePool::Load);
	if(noduplicate)
	{
		int idx = Find(name);
		if(idx!=-1) return idx;
	}
	R9TEXTURE texture = R9_TextureLoad(name.c_str());
	if(!texture) return -1;
	return Add(texture, name);
	unguard();
}

int r9TexturePool::Find( const std::string & name )
{
	guard(r9TexturePool::Find);
	Hash::const_iterator i = index.find(name);
	if( i == index.end())
		return -1;
	return i->second;
	unguard();
}
///////////////////////////////////////////////////////////////////////////////////////////////////

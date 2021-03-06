///////////////////////////////////////////////////////////////////////////////////////////////////
// F9Files.cpp
///////////////////////////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"

#include <algorithm>

#include "F9Files.h"
#include "F9FileMem.h"
#include "F9FileRes.h"
#include "F9ArchiveZip.h"
#include "F9ArchivePak.h"
#include "D9Log.h"

Files * files = nullptr;

void Files::FileClose(f9File * file)
{
	if(!file) return;
	file->Close();
	delete file;
}

bool F9_Init()
{
	if(files) return true;
	elog::fil() << "Files init." << std::endl;
	files = new Files();
	return true;
}

void F9_Done()
{
	if(!files) return;
	delete files;
	files = nullptr;
	elog::fil() << "Files done." << std::endl;
}

void Files::DoIndex(f9Archive * arc, const std::wstring & file) {

	if(!arc->Open(file, F9_READ))
	{
		delete arc;
		return;
	}
	Archives.push_back(arc);
	for(int idx = 0, e = arc->FileCount(); idx != e; ++idx)
		Index[arc->FileGetName(idx)] = arc;
}

void Files::MakeIndex(const std::wstring & path)
{
	file_findfiles(L"", L"*.pak", [this](const std::wstring & s, bool) { DoIndex<f9ArchivePak>(s); }, 0);
	file_findfiles(path, L"*.*", [this](const std::wstring & s, bool) { Index[s] = 0; }, FILE_FINDREC);

	for(Map::value_type i: Index)
		elog::fil() << "idx: " << i.first.c_str() << " " << i.second << std::endl;
}

f9File * Files::OpenFile(const std::wstring & name)
{
	if( name[0]==L'#' )
		if(f9File * file = OpenFile<f9FileMem>(name))
			return file;
	
	if(_searchInResources)
		if(f9File * file = OpenFile<f9FileRes>(name))
			return file;
	if(f9File * file = OpenFile<f9FileDisk>(name))
		return file;
	
	auto f = Index.find(name);
	if(f != Index.end() && f->second)
		return f->second->FileOpen(name);
	return 0;
}

void Files::FindFiles(const std::wstring & path, std::function<void(const std::wstring &)> callback)
{
	for(auto i = Index.lower_bound(path), e = Index.end(); i != e; ++i)
		if(std::mismatch(path.begin(), path.end(), i->first.begin()).first == path.end()) 
			callback(i->first);
		else
			return;
}

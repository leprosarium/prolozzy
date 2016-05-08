#ifndef __E9STRING_H__
#define __E9STRING_H__

std::string WideStringToMultiByte(LPCWSTR wszSrc);  
std::wstring MultiByteToWideString(LPCSTR szSrc);

const std::wstring WHITESPACE = L" \n\r\t";
inline std::wstring ltrim(const std::wstring & s)
{
    auto start = s.find_first_not_of(WHITESPACE);
    return start == std::wstring::npos ? std::wstring() : s.substr(start);
}

inline std::wstring rtrim(const std::wstring & s)
{
    auto end = s.find_last_not_of(WHITESPACE);
    return end == std::wstring::npos ? std::wstring() : s.substr(0, end + 1);
}
inline std::wstring trim(const std::wstring & s)
{
    return rtrim(ltrim(s));
}

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

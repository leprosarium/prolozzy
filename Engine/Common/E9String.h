#ifndef __E9STRING_H__
#define __E9STRING_H__

std::string WideStringToMultiByte(LPCWSTR wszSrc);  
std::wstring MultiByteToWideString(LPCSTR szSrc);

const std::string WHITESPACE = " \n\r\t";
inline std::string ltrim(const std::string & s)
{
    auto start = s.find_first_not_of(WHITESPACE);
    return start == std::string::npos ? "" : s.substr(start);
}

inline std::string rtrim(const std::string & s)
{
    auto end = s.find_last_not_of(WHITESPACE);
    return end == std::string::npos ? "" : s.substr(0, end + 1);
}
inline std::string trim(const std::string & s)
{
    return rtrim(ltrim(s));
}

#endif
///////////////////////////////////////////////////////////////////////////////////////////////////

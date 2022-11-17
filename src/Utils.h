#ifndef UTILS_H
#define UTILS_H
#include <windows.h>
#include <string>

extern void* g_hInst;

#define IDS_OPENCONTAININGFOLDER L"Open containing folder" 
#define IDS_COPYPATH            L"Copy path to clipboard"
#define IDS_COPYPATHS           L"Copy paths to clipboard"
#define IDS_COPYFILENAME        L"Copy filename to clipboard"
#define IDS_COPYFILENAMES       L"Copy filenames to clipboard"

class CSearchInfo{
  public:
    std::wstring              filePath;
};

inline std::wstring TranslatedString(void*, const wchar_t* s){
    return s;
}

bool WriteAsciiStringToClipboard(const wchar_t* sClipdata, HWND hOwningWnd);


class CStringUtils {
   public:
    static inline std::string& trim(std::string& s) { return ltrim(rtrim(s)); }

    static inline std::string& trim(std::string& s,
                                    const std::string& trimchars) {
        return ltrim(rtrim(s, trimchars), trimchars);
    }
    static inline std::string& trim(std::string& s, wint_t trimchar) {
        return ltrim(rtrim(s, trimchar), trimchar);
    }

    // trim from start
    static inline std::string& ltrim(std::string& s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(),
                                        [](wint_t c) { return !iswspace(c); }));
        return s;
    }
    static inline std::string& ltrim(std::string& s,
                                     const std::string& trimchars) {
        s.erase(s.begin(),
                std::find_if(s.begin(), s.end(), [&trimchars](wint_t c) {
                    return trimchars.find(static_cast<char>(c)) ==
                           std::string::npos;
                }));
        return s;
    }
    static inline std::string& ltrim(std::string& s, wint_t trimchar) {
        s.erase(s.begin(),
                std::find_if(s.begin(), s.end(),
                             [&trimchar](wint_t c) { return c != trimchar; }));
        return s;
    }

    // trim from end
    static inline std::string& rtrim(std::string& s) {
        s.erase(std::find_if(s.rbegin(), s.rend(),
                             [](wint_t c) { return !iswspace(c); })
                    .base(),
                s.end());
        return s;
    }
    static inline std::string& rtrim(std::string& s,
                                     const std::string& trimchars) {
        s.erase(std::find_if(s.rbegin(), s.rend(),
                             [&trimchars](wint_t c) {
                                 return trimchars.find(static_cast<char>(c)) ==
                                        std::string::npos;
                             })
                    .base(),
                s.end());
        return s;
    }
    static inline std::string& rtrim(std::string& s, wint_t trimchar) {
        s.erase(std::find_if(s.rbegin(), s.rend(),
                             [&trimchar](wint_t c) { return c != trimchar; })
                    .base(),
                s.end());
        return s;
    }

    // trim from both ends
    static inline std::wstring& trim(std::wstring& s) {
        return ltrim(rtrim(s));
    }
    static inline std::wstring& trim(std::wstring& s,
                                     const std::wstring& trimchars) {
        return ltrim(rtrim(s, trimchars), trimchars);
    }
    static inline std::wstring& trim(std::wstring& s, wint_t trimchar) {
        return ltrim(rtrim(s, trimchar), trimchar);
    }

    // trim from start
    static inline std::wstring& ltrim(std::wstring& s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(),
                                        [](wint_t c) { return !iswspace(c); }));
        return s;
    }
    static inline std::wstring& ltrim(std::wstring& s,
                                      const std::wstring& trimchars) {
        s.erase(s.begin(),
                std::find_if(s.begin(), s.end(), [&trimchars](wint_t c) {
                    return trimchars.find(c) == std::wstring::npos;
                }));
        return s;
    }
    static inline std::wstring& ltrim(std::wstring& s, wint_t trimchar) {
        s.erase(s.begin(),
                std::find_if(s.begin(), s.end(),
                             [&trimchar](wint_t c) { return c != trimchar; }));
        return s;
    }

    // trim from end
    static inline std::wstring& rtrim(std::wstring& s) {
        s.erase(std::find_if(s.rbegin(), s.rend(),
                             [](wint_t c) { return !iswspace(c); })
                    .base(),
                s.end());
        return s;
    }
    static inline std::wstring& rtrim(std::wstring& s,
                                      const std::wstring& trimchars) {
        s.erase(std::find_if(s.rbegin(), s.rend(),
                             [&trimchars](wint_t c) {
                                 return trimchars.find(c) == std::wstring::npos;
                             })
                    .base(),
                s.end());
        return s;
    }
    static inline std::wstring& rtrim(std::wstring& s, wint_t trimchar) {
        s.erase(std::find_if(s.rbegin(), s.rend(),
                             [&trimchar](wint_t c) { return c != trimchar; })
                    .base(),
                s.end());
        return s;
    }
};

#endif /* UTILS_H */

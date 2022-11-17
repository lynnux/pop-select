#include "Utils.h"

void* g_hInst = 0;
#define AUTOOUTOFSCOPE_TOKEN_PASTEx(x, y) x##y
#define AUTOOUTOFSCOPE_TOKEN_PASTE(x, y)  AUTOOUTOFSCOPE_TOKEN_PASTEx(x, y)

template <typename T>
class AutoOutOfScope
{
  public:
    AutoOutOfScope(T& destructor)
            : m_destructor(destructor)
    {
    }
    ~AutoOutOfScope() { m_destructor(); }
    // no copies of this class, also to avoid compiler warnings
    AutoOutOfScope(const AutoOutOfScope&) = delete;
    AutoOutOfScope& operator=(const AutoOutOfScope& tmp) = delete;

  private:
    T& m_destructor;
};

#define AUTOOUTOFSCOPE__INTERNAL(Destructor, counter)                   \
    auto                                                                      AUTOOUTOFSCOPE_TOKEN_PASTE(auto_func_, counter) = [&]() { Destructor; }; \
    AutoOutOfScope<decltype(AUTOOUTOFSCOPE_TOKEN_PASTE(auto_func_, counter))> AUTOOUTOFSCOPE_TOKEN_PASTE(auto_, counter)(AUTOOUTOFSCOPE_TOKEN_PASTE(auto_func_, counter));

#define OnOutOfScope(Destructor) AUTOOUTOFSCOPE__INTERNAL(Destructor, __COUNTER__)

bool WriteAsciiStringToClipboard(const wchar_t* sClipdata, HWND hOwningWnd)
{
    // OpenClipboard may fail if another application has opened the clipboard.
    // Try up to 8 times, with an initial delay of 1 ms and an exponential back off
    // for a maximum total delay of 127 ms (1+2+4+8+16+32+64).
    for (int attempt = 0; attempt < 8; ++attempt)
    {
        if (attempt > 0)
        {
            ::Sleep(1 << (attempt - 1));
        }
        if (OpenClipboard(hOwningWnd))
        {
            OnOutOfScope(
                CloseClipboard(););
            EmptyClipboard();
            size_t  sLen           = wcslen(sClipdata);
            HGLOBAL hClipboardData = GlobalAlloc(GMEM_MOVEABLE, (sLen + 1) * sizeof(wchar_t));
            if (hClipboardData)
            {
                wchar_t* pchData = static_cast<wchar_t*>(GlobalLock(hClipboardData));
                if (pchData)
                {
                    wcscpy_s(pchData, sLen + 1, sClipdata);
                    GlobalUnlock(hClipboardData);
                    if (SetClipboardData(CF_UNICODETEXT, hClipboardData) == nullptr)
                        return true;
                }
            }
        }
    }

    return false;
}


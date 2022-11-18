// 参考代码：
// https://github.com/electrotype/vscode-windows-explorer-context-menu/blob/master/executables/AutohotkeyContextMenu.ahk
// https://github.com/Unknown6656/TabbedExplorer/blob/91fb81f07b7df36639b03b723ec818e60437bf1c/NativeInterop/main.cpp
// https://github.com/baohaojun/system-config/blob/e936ae4cae5764abfe4a3657ae7470532963e8ef/gcode/oc2/oc2/ShellMenu.cpp
// https://github.com/tmpprj/tmpprj/blob/1b1e19d01f6dbfdacdb3000778ca822e57862ca6/SearchGUI/src/contextmenu.cc
// shell弹窗最后用的grepwin的代码，shell的copy/cut/paste代码来自https://github.com/VioletGiraffe/file-commander
#include "ShellContextMenu.h"
#include <algorithm>

bool copyObjectsToClipboard(const std::vector<std::wstring>& objects,
                            void* parentWindow);
bool cutObjectsToClipboard(const std::vector<std::wstring>& objects,
                           void* parentWindow);
bool pasteFilesAndFoldersFromClipboard(std::wstring destFolder,
                                       void* parentWindow);

extern "C" {
void PopupShellMenu(HWND emacs, const wchar_t** path, LONG x, LONG y,
                    LONG showExtraHead) {
    POINT pt;
    if (x == 0 && y == 0) {
        GetCursorPos(&pt);
    }

    CShellContextMenu cm;
    std::vector<CSearchInfo> paths;
    while (*path) {
        CSearchInfo cs;
        cs.filePath = *path;
        paths.push_back(cs);
        ++path;
    }

    cm.SetObjects(paths);
    cm.ShowContextMenu(emacs, pt, showExtraHead != 0);
}

// 模拟CTRL+C
void CopyPathsToClipboard(const wchar_t** path) {
    std::vector<std::wstring> paths;
    while (*path) {
        paths.push_back(*path);
        ++path;
    }
    copyObjectsToClipboard(paths, GetForegroundWindow());
}

void CutPathsToClipboard(const wchar_t** path) {
    std::vector<std::wstring> paths;
    while (*path) {
        paths.push_back(*path);
        ++path;
    }
    cutObjectsToClipboard(paths, GetForegroundWindow());
}

// 模拟CTRL+V，需要目标路径
void PasteToPathFromClipboard(const wchar_t* target) {
    pasteFilesAndFoldersFromClipboard(target, GetForegroundWindow());
}
}

// https://github.com/VioletGiraffe/file-commander/blob/master/file-commander-core/src/shell/cshell.cpp
struct ComInitializer {
    ComInitializer() {
        const auto result = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED);
        // assert_r(SUCCEEDED(result));
    }

    ~ComInitializer() { CoUninitialize(); }
};

class CComInterfaceReleaser {
   public:
    explicit CComInterfaceReleaser(IUnknown* i) : _i(i) {}
    ~CComInterfaceReleaser() {
        if (_i) _i->Release();
    }

   private:
    IUnknown* _i;
};

class CItemIdArrayReleaser {
   public:
    explicit CItemIdArrayReleaser(const std::vector<ITEMIDLIST*>& idArray)
        : _array(idArray) {}
    ~CItemIdArrayReleaser() {
        for (ITEMIDLIST* item : _array) CoTaskMemFree(item);
    }

    CItemIdArrayReleaser& operator=(const CItemIdArrayReleaser&) = delete;

   private:
    const std::vector<ITEMIDLIST*>& _array;
};

bool prepareContextMenuForObjects(std::vector<std::wstring> objects,
                                  void* parentWindow, HMENU& hmenu,
                                  IContextMenu*& imenu) {
    // CO_INIT_HELPER(COINIT_APARTMENTTHREADED);

    if (objects.empty()) return false;

    std::vector<ITEMIDLIST*> ids;
    std::vector<LPCITEMIDLIST> relativeIds;
    IShellFolder* ifolder = 0;
    for (size_t i = 0, nItems = objects.size(); i < nItems; ++i) {
        auto& item = objects[i];
        std::replace(item.begin(), item.end(), '/', '\\');
        // item.pop_back(); // TODO: ???
        ids.emplace_back(nullptr);
        HRESULT result =
            SHParseDisplayName(item.c_str(), nullptr, &ids.back(), 0,
                               nullptr);  // TODO: avoid c_str() somehow?
        if (!SUCCEEDED(result) || !ids.back()) {
            ids.pop_back();
            continue;
        }

        relativeIds.emplace_back(nullptr);
        result = SHBindToParent(ids.back(), IID_IShellFolder, (void**)&ifolder,
                                &relativeIds.back());
        if (!SUCCEEDED(result) || !relativeIds.back())
            relativeIds.pop_back();
        else if (i < nItems - 1 && ifolder) {
            ifolder->Release();
            ifolder = nullptr;
        }
    }

    CItemIdArrayReleaser arrayReleaser(ids);

    if (!parentWindow) return false;
    if (!ifolder) return false;
    if (relativeIds.empty()) return false;

    // assert_r(parentWindow);
    // assert_and_return_message_r(ifolder, "Error getting ifolder", false);
    // assert_and_return_message_r(!relativeIds.empty(), "RelativeIds is empty",
    // false);

    imenu = 0;
    HRESULT result =
        ifolder->GetUIObjectOf((HWND)parentWindow, (UINT)relativeIds.size(),
                               (const ITEMIDLIST**)relativeIds.data(),
                               IID_IContextMenu, 0, (void**)&imenu);
    if (!SUCCEEDED(result) || !imenu) return false;

    hmenu = CreatePopupMenu();
    if (!hmenu) return false;
    return (
        SUCCEEDED(imenu->QueryContextMenu(hmenu, 0, 1, 0x7FFF, CMF_NORMAL)));
}

bool copyObjectsToClipboard(const std::vector<std::wstring>& objects,
                            void* parentWindow) {
    // CO_INIT_HELPER(COINIT_APARTMENTTHREADED);
    ComInitializer ci;

    IContextMenu* imenu = 0;
    HMENU hMenu = NULL;
    if (!prepareContextMenuForObjects(objects, parentWindow, hMenu, imenu) ||
        !hMenu || !imenu)
        return false;

    CComInterfaceReleaser menuReleaser(imenu);

    const char command[] = "Copy";

    CMINVOKECOMMANDINFO info = {0};
    info.cbSize = sizeof(info);
    info.hwnd = (HWND)parentWindow;
    info.lpVerb = command;
    info.nShow = SW_SHOWNORMAL;
    const auto result = imenu->InvokeCommand((LPCMINVOKECOMMANDINFO)&info);

    DestroyMenu(hMenu);

    return SUCCEEDED(result);
}

bool cutObjectsToClipboard(const std::vector<std::wstring>& objects,
                           void* parentWindow) {
    // CO_INIT_HELPER(COINIT_APARTMENTTHREADED);
    ComInitializer ci;

    IContextMenu* imenu = 0;
    HMENU hMenu = NULL;
    if (!prepareContextMenuForObjects(objects, parentWindow, hMenu, imenu) ||
        !hMenu || !imenu)
        return false;

    CComInterfaceReleaser menuReleaser(imenu);

    const char command[] = "Cut";

    CMINVOKECOMMANDINFO info = {0};
    info.cbSize = sizeof(info);
    info.hwnd = (HWND)parentWindow;
    info.lpVerb = command;
    info.nShow = SW_SHOWNORMAL;
    const auto result = imenu->InvokeCommand((LPCMINVOKECOMMANDINFO)&info);

    DestroyMenu(hMenu);

    return SUCCEEDED(result);
}

bool pasteFilesAndFoldersFromClipboard(std::wstring destFolder,
                                       void* parentWindow) {
    // CO_INIT_HELPER(COINIT_APARTMENTTHREADED);
    ComInitializer ci;

    IContextMenu* imenu = 0;
    HMENU hMenu = NULL;
    if (!prepareContextMenuForObjects(std::vector<std::wstring>(1, destFolder),
                                      parentWindow, hMenu, imenu) ||
        !hMenu || !imenu)
        return false;

    CComInterfaceReleaser menuReleaser(imenu);

    const char command[] = "Paste";

    CMINVOKECOMMANDINFO info = {0};
    info.cbSize = sizeof(info);
    info.hwnd = (HWND)parentWindow;
    info.lpVerb = command;
    info.nShow = SW_SHOWNORMAL;
    const auto result = imenu->InvokeCommand((LPCMINVOKECOMMANDINFO)&info);

    DestroyMenu(hMenu);

    return SUCCEEDED(result);
}

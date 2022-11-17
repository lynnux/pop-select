// 参考代码：
// https://github.com/electrotype/vscode-windows-explorer-context-menu/blob/master/executables/AutohotkeyContextMenu.ahk
// https://github.com/Unknown6656/TabbedExplorer/blob/91fb81f07b7df36639b03b723ec818e60437bf1c/NativeInterop/main.cpp
// https://github.com/baohaojun/system-config/blob/e936ae4cae5764abfe4a3657ae7470532963e8ef/gcode/oc2/oc2/ShellMenu.cpp
// https://github.com/tmpprj/tmpprj/blob/1b1e19d01f6dbfdacdb3000778ca822e57862ca6/SearchGUI/src/contextmenu.cc
// 最后用的grepwin的代码
#include "ShellContextMenu.h"

extern "C"{
    void PopupShellMenu(HWND emacs, const wchar_t** path, LONG x, LONG y){
        POINT pt;
        if(x == 0 && y == 0){
            GetCursorPos(&pt);
        }
        
        CShellContextMenu cm;
        std::vector<CSearchInfo> paths;
        while(*path){
            CSearchInfo cs;
            cs.filePath = *path;
            paths.push_back(cs);
            ++path;
        }

        cm.SetObjects(paths);
        cm.ShowContextMenu(emacs, pt);
    }
}

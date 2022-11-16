// ²Î¿¼´úÂë£º
// https://github.com/electrotype/vscode-windows-explorer-context-menu/blob/master/executables/AutohotkeyContextMenu.ahk
// https://github.com/Unknown6656/TabbedExplorer/blob/91fb81f07b7df36639b03b723ec818e60437bf1c/NativeInterop/main.cpp
// https://github.com/baohaojun/system-config/blob/e936ae4cae5764abfe4a3657ae7470532963e8ef/gcode/oc2/oc2/ShellMenu.cpp
// https://github.com/tmpprj/tmpprj/blob/1b1e19d01f6dbfdacdb3000778ca822e57862ca6/SearchGUI/src/contextmenu.cc
#include "ShellMenu.h"
#include <winuser.h>

extern "C"{
    void PopupShellMenu(HWND emacs, const wchar_t* path, LONG x, LONG y){
        CShellMenu cm(emacs, (TCHAR*)path);
        POINT pt;
        if(x == 0 && y == 0){
            GetCursorPos(&pt);
        }
        cm.Show(pt.x, pt.y);
    }
}

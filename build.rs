extern crate embed_resource;

fn main() {
    embed_resource::compile("src/res.rc");
    cc::Build::new()
        .define("UNICODE", "1")
        .file("src/ShellContextMenu.cpp")
        .file("src/Utils.cpp")
        .file("src/shellmenu_wrap.cpp")
        .compile("foo");
}

extern crate embed_resource;

fn main() {
    embed_resource::compile("src/res.rc");
    cc::Build::new().define("UNICODE", "1").file("src/ShellMenu.cpp").file("src/shellmenu_wrap.cpp").compile("foo");
}

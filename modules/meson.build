fs = import('fs')
module_objects = []
bulid_GUILE_EXTENSIONS_PATH=['GUILE_EXTENSIONS_PATH=' + meson.build_root() / 'src']
foreach src : ['glfw.scm', 'imgui.scm']
  obj = fs.replace_suffix(src,'.go')
  module_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: libdir / guile_compiled_module_dir,
    output: obj,
    depends: [shlib,gglfw],
    env: bulid_GUILE_EXTENSIONS_PATH
  )
endforeach
subdir('imgui')

fs = import('fs')
module_objects = []
bulid_GUILE_EXTENSIONS_PATH=['GUILE_EXTENSIONS_PATH=' + meson.build_root() / 'src']
foreach src : ['imgui.scm'] +  (glfw_dep.found() ?  ['glfw.scm'] : [])
  obj = fs.replace_suffix(src,'.go')
  module_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: libdir / guile_compiled_module_dir,
    output: obj,
    depends: [shlib] + (glfw_dep.found() ? [gglfw] : []),
    env: bulid_GUILE_EXTENSIONS_PATH
  )
endforeach
subdir('imgui')

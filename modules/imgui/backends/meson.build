backend_sources = [
  'sdl2.scm',
]
fs = import('fs')
backend_module_objects = []

foreach src : backend_sources
  obj = fs.replace_suffix(src,'.go')
  backend_module_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: libdir / guile_compiled_module_dir,
    output: obj,
    depends: [shlib,gglfw]
  )
endforeach

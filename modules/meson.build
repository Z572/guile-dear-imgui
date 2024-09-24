module_sources = [
  'glfw.scm',
  'imgui.scm'
]

module_objects = []

foreach src : module_sources
  obj = src.split('.')[0] + '.go'
  module_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: libdir / guile_compiled_module_dir,
    output: obj,
    depends: [shlib,gglfw]
  )
endforeach

backend_module_objects = []

foreach src : backends
  obj = src +'.go'
  backend_module_objects += custom_target(
    obj,
    command: guile_compile,
    input: src + '.scm',
    install: true,
    install_dir: libdir / guile_compiled_module_dir / 'imgui/backends',
    output: obj,
    depends: [shlib,gglfw],
    env: bulid_GUILE_EXTENSIONS_PATH
  )
endforeach

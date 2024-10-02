foreach src : ['inputs.scm','window.scm','item.scm','viewport.scm']
  obj = fs.replace_suffix(src,'.go')
  module_objects += custom_target(
    obj,
    command: guile_compile,
    input: src,
    install: true,
    install_dir: libdir / guile_compiled_module_dir / 'imgui',
    output: obj,
    depends: [shlib,gglfw],
    env: bulid_GUILE_EXTENSIONS_PATH
  )
endforeach
subdir('backends')

tests = ['none.scm']
foreach t : tests
  test('test ' + t,
       pre_inst_env,
       args: [
         guile,
         '--no-auto-compile',
         meson.current_source_dir() / t
       ])
endforeach

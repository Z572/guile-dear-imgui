tests = ['none.scm','sdl2.scm','glfw.scm','table.scm','tablebar.scm','tree.scm','draw.scm']
foreach t : tests
  test('test ' + t,
       pre_inst_env,
       args: [
         guile,
         '--no-auto-compile', '-c',
         '(primitive-load "'  + meson.current_source_dir() /  'helper.scm")',
         meson.current_source_dir() / t
       ],
       env :['SDL_VIDEODRIVER=offscreen','HOME=/tmp'])
endforeach
configure_file(input: 'helper.scm',
               output: 'helper.scm',
              copy: true)

tests = ['none.scm','sdl2.scm','glfw.scm']
foreach t : tests
  test('test ' + t,
       pre_inst_env,
       args: [
         guile,
         '--no-auto-compile',
         meson.current_source_dir() / t
       ],
       env :['SDL_VIDEODRIVER=offscreen','HOME=/tmp'])
endforeach

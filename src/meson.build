glfw_dep=dependency('glfw3')
shlib = shared_module('guile_dear_imgui', ['guile_dear_imgui.cpp', 'guile.hpp'],
  install : true,
  cpp_args : lib_args,
  dependencies : [guile_dep,dearimgui_dep],
  install_dir : guile_extension_dir
)
sdl2_dep=dependency('sdl2',required: get_option('sdl2'))
if sdl2_dep.found()
  guile_imgui_backend_sdl2 =shared_module('guile_dear_imgui_backend_sdl2',
                  ['backend_sdl2.cpp', 'guile.hpp'],
                  install : true,
                  cpp_args : lib_args,
                  dependencies : [guile_dep,dearimgui_dep,sdl2_dep],
                  install_dir : guile_extension_dir)
endif

gglfw = shared_module('guile_glfw', ['guile_glfw.cpp', 'guile.hpp'],
  install : true,
  cpp_args : lib_args,
  dependencies : [guile_dep,glfw_dep],
  install_dir : guile_extension_dir
)

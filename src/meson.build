glfw_dep=dependency('glfw3')
magic_enum=dependency('magic_enum')
shlib = shared_module('guile_dear_imgui', ['guile_dear_imgui.cpp', 'guile.hpp'],
  install : true,
  cpp_args : lib_args,
  dependencies : [guile_dep,dearimgui_dep,magic_enum],
  install_dir : guile_extension_dir)

backends=['sdl2','glfw3','gl']
foreach i : backends
  backend_dep=dependency(i,required: get_option(i))
  if backend_dep.found()
    guile_imgui_backend_sdl2 =shared_module('guile_dear_imgui_backend_' + i,
                  ['backend_' + i + '.cpp', 'guile.hpp'],
                  install : true,
                  cpp_args : lib_args,
                  dependencies : [guile_dep,dearimgui_dep,backend_dep],
                  install_dir : guile_extension_dir)
endif
endforeach

gglfw = shared_module('guile_glfw', ['guile_glfw.cpp', 'guile.hpp'],
  install : true,
  cpp_args : lib_args,
  dependencies : [guile_dep,glfw_dep],
  install_dir : guile_extension_dir
)

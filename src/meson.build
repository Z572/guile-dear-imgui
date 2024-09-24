shlib = shared_module('guile_dear_imgui', ['guile_dear_imgui.cpp', 'guile.hpp'],
  install : true,
  cpp_args : lib_args,
  dependencies : [guile_dep,dearimgui_dep,sdl2_dep],
  install_dir : guile_extension_dir
)

gglfw = shared_module('guile_glfw', ['guile_glfw.cpp', 'guile.hpp'],
  install : true,
  cpp_args : lib_args,
  dependencies : [guile_dep,glfw_dep],
  install_dir : guile_extension_dir
)

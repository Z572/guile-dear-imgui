project('guile-dear-imgui', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++14'])

# These arguments are only used to build the shared library
# not the executables that use the library.
lib_args = ['-DBUILDING_GUILE_DEAR_IMGUI']



imgui_options = [
  'default_library=static',
  'werror=false',
  # use 'auto_features=disabled' once available: https://github.com/mesonbuild/meson/issues/5320
  'dx9=disabled',
  'dx10=disabled',
  'dx11=disabled',
  'dx12=disabled',
  'metal=disabled',
  'osx=disabled',
  'win=disabled',
  'allegro5=disabled',
  'webgpu=disabled',
]


sdl2_dep=dependency('sdl2')
glfw_dep=dependency('glfw3')
guile_dep=dependency('guile-3.0')
dearimgui_dep = dependency('imgui',required: false)
if (not dearimgui_dep.found())
  dearimgui_dep=subproject('imgui', default_options: imgui_options).get_variable('imgui_dep')
endif

custom_guile_xd=get_option('guile-extension-dir')
if custom_guile_xd == ''
  guile_extension_dir = guile_dep.get_variable(pkgconfig: 'extensiondir')
else
  guile_extension_dir = custom_guile_xd
endif


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


# Make this library usable as a Meson subproject.
guile_dear_imgui_dep = declare_dependency(
  include_directories: include_directories('.'),
  link_with : shlib)

# Make this library usable from the system's
# package manager.
# install_headers('guile_dear_imgui.hpp', subdir : 'guile_dear_imgui')

pkg_mod = import('pkgconfig')
pkg_mod.generate(
  name : 'guile-dear-imgui',
  filebase : 'guile_dear_imgui',
  description : 'Meson sample project.',
  subdirs : 'guile_dear_imgui',
  libraries : shlib,
  version : '0.1',
)

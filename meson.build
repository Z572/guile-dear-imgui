project('guile-dear-imgui', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++14'])

# These arguments are only used to build the shared library
# not the executables that use the library.
lib_args = ['-DBUILDING_GUILE_DEAR_IMGUI']

prefix = get_option('prefix')
bindir = get_option('bindir')
libdir = get_option('libdir')
datadir = get_option('datadir')

guile_compiled_module_dir = 'guile' / '3.0' / 'site-ccache'

imgui_options = [
  'default_library=shared',
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



guild = guile_dep.get_pkgconfig_variable('guild')
guile = guile_dep.get_pkgconfig_variable('guile')

guile_warnings = [
  '-Warity-mismatch',
  '-Wbad-case-datum',
  '-Wduplicate-case-datum',
  '-Wformat',
  '-Wunbound-variable',
  '-Wunsupported-warning',
  '-Wunused-toplevel',
  '-Wunused-variable',
]

guile_compile = [
  guild,
  'compile',
  guile_warnings,
  '-o',
  '@OUTPUT@',
  '@INPUT@',
]


runner_data = configuration_data()
runner_data.set('GUILE', guile)
runner_data.set('SRC_ROOT', meson.source_root())
runner_data.set('BUILD_ROOT', meson.build_root())


pre_inst_env = configure_file(input: 'pre-inst-env.in',
                         output: 'pre-inst-env',
                         configuration: runner_data)

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

subdir('modules')
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

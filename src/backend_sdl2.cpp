#include <imgui.h>
#include <imgui/backends/imgui_impl_sdl2.h>
#include <imgui/misc/cpp/imgui_stdlib.h>
#include <libguile.h>
#include <guile.hpp>
#if defined(IMGUI_IMPL_OPENGL_ES2)
#include <SDL_opengles2.h>
#else
#include <SDL_opengl.h>
#endif
#include "SDL_video.h"
namespace im {
  using guile::value;
  namespace sdl2 {

    value InitForVulkan(value window){
      SDL_Window* w=static_cast<SDL_Window*>(scm_to_pointer(window));
      return ImGui_ImplSDL2_InitForVulkan(w);
    }
    value InitForOpenGl(value window, value gl_context){
      SDL_Window *w = static_cast<SDL_Window *>(scm_to_pointer(window));
      SDL_GLContext cont=static_cast<SDL_GLContext>(scm_to_pointer(gl_context));
      return ImGui_ImplSDL2_InitForOpenGL(w,cont);
    }
    value NewFrame(){
      ImGui_ImplSDL2_NewFrame();
      return SCM_UNSPECIFIED;
    }
    value Shutdown(){
      ImGui_ImplSDL2_Shutdown();
      return SCM_UNSPECIFIED;
    }
    value ProcessEvent(value event) {
      const SDL_Event* e=static_cast<SDL_Event*>(scm_to_pointer(event));
      return ImGui_ImplSDL2_ProcessEvent(e);
    }
  } // namespace sdl2
} // namespace impl

extern "C" {
  void init_imgui_sdl() {
    scm_c_define_gsubr("impl:sdl2:init-vulkan", 0, 0, 0,
                       (scm_t_subr)im::sdl2::InitForVulkan);
    scm_c_define_gsubr("impl:sdl2:init-opengl", 2, 0, 0,
                       (scm_t_subr)im::sdl2::InitForOpenGl);

    scm_c_define_gsubr("impl:sdl2:shutdown", 0, 0, 0,
                       (scm_t_subr)im::sdl2::Shutdown);
    scm_c_define_gsubr("impl:sdl2:process-event", 1, 0, 0,
                       (scm_t_subr)im::sdl2::ProcessEvent);
    scm_c_define_gsubr("impl:sdl2:new-frame", 0, 0, 0,
                       (scm_t_subr)im::sdl2::NewFrame);

  }
}

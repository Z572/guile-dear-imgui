#include <imgui.h>
#include <imgui/backends/imgui_impl_glfw.h>
#include <libguile.h>
#include <guile.hpp>
namespace im {
  using guile::value;
    namespace glfw {

      value InitForOpenGl(value window, value install_callbacks){
        auto w = static_cast<GLFWwindow *>(scm_to_pointer(window));
        return ImGui_ImplGlfw_InitForOpenGL(w,install_callbacks);
      }
      value NewFrame(){
        ImGui_ImplGlfw_NewFrame();
        return SCM_UNSPECIFIED;
      }
      value Shutdown(){
        ImGui_ImplGlfw_Shutdown();
        return SCM_UNSPECIFIED;
      }
    }
} // namespace impl

extern "C" {
  void init_imgui_glfw() {
    scm_c_define_gsubr("init-opengl", 2, 0, 0,
                       (scm_t_subr)im::glfw::InitForOpenGl);

    scm_c_define_gsubr("shutdown", 0, 0, 0,
                       (scm_t_subr)im::glfw::Shutdown);
    scm_c_define_gsubr("new-frame", 0, 0, 0,
                       (scm_t_subr)im::glfw::NewFrame);
  }
}

#include <imgui.h>
#include <imgui/backends/imgui_impl_opengl2.h>
#include <imgui/backends/imgui_impl_opengl3.h>
#include <libguile.h>
#include <guile.hpp>
namespace im {
  using guile::value;
    namespace opengl2 {
      value NewFrame(){
        ImGui_ImplOpenGL2_NewFrame();
        return SCM_UNSPECIFIED;
      }
    }
    namespace opengl3 {
      value init(value glsl_version){
        if (glsl_version.unboundp())
          return ImGui_ImplOpenGL3_Init();
        else {
          //std::cout << "glsl_version: " << glsl_version << std::endl;
          return ImGui_ImplOpenGL3_Init(LABEL(glsl_version));
        }
      }
      value Shutdown(){
        ImGui_ImplOpenGL3_Shutdown();
        return SCM_UNSPECIFIED;
      }
      value NewFrame(){
        ImGui_ImplOpenGL3_NewFrame();
        return SCM_UNSPECIFIED;
      }
      value RenderDrawData() {
        ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
        return SCM_UNSPECIFIED;
      }
    }
} // namespace impl

extern "C" {
  void init_imgui_gl() {
    scm_c_define_gsubr("impl:opengl3:init", 0, 1, 0,
                       (scm_t_subr)im::opengl3::init);

    scm_c_define_gsubr("impl:opengl3:shutdown", 0, 0, 0,
                       (scm_t_subr)im::opengl3::Shutdown);
    scm_c_define_gsubr("impl:opengl3:new-frame", 0, 0, 0,
                       (scm_t_subr)im::opengl3::NewFrame);
        scm_c_define_gsubr("impl:opengl3:render-draw-data", 0, 0, 0,
                       (scm_t_subr)im::opengl3::RenderDrawData);

  }
}

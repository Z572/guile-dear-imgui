#include <imgui.h>
#include <imgui/backends/imgui_impl_opengl2.h>
#include <imgui/backends/imgui_impl_opengl3.h>
#include <libguile.h>
#include <guile.hpp>
#if defined (HAVE_STB_IMAGE)
#include <GL/gl.h>
#define STB_IMAGE_IMPLEMENTATION
#define STBI_MALLOC(sz) scm_gc_malloc(sz,"stbi")
#define STBI_FREE(p) scm_gc_free(p,sizeof(p),"stbi")
#define STBI_REALLOC(p,newsz) scm_gc_realloc(p,sizeof(p),newsz,"stbi")
#include <stb_image.h>
#endif


namespace im {
  using guile::value;
#if defined (HAVE_STB_IMAGE)
  value loadimage(value filename) {
    int image_width = 0;
    int image_height = 0;
    int channels_in_file;
    auto image_data =
      ::stbi_load(LABEL(filename), &image_width, &image_height, &channels_in_file, 4);
    if (image_data == nullptr)
      return scm_values_3(SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F);
    GLuint image_texture;
    glGenTextures(1, &image_texture);
    glBindTexture(GL_TEXTURE_2D, image_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image_width, image_height, 0,
                 GL_RGBA, GL_UNSIGNED_BYTE, image_data);
    stbi_image_free(image_data);
    return scm_values_3(scm_from_unsigned_integer(image_texture), value(image_width), value(image_height));
  }
#endif
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
      value RenderDrawData(value sdata) {
        auto data=static_cast<ImDrawData*>(scm_to_pointer(sdata));
        ImGui_ImplOpenGL3_RenderDrawData(data);
        return SCM_UNSPECIFIED;
      }
    }
} // namespace impl

extern "C" {
  void init_imgui_gl() {
    scm_c_define_gsubr("init", 0, 1, 0,
                       (scm_t_subr)im::opengl3::init);

    scm_c_define_gsubr("shutdown", 0, 0, 0,
                       (scm_t_subr)im::opengl3::Shutdown);
    scm_c_define_gsubr("new-frame", 0, 0, 0,
                       (scm_t_subr)im::opengl3::NewFrame);
    guile::define("%render-draw-data", 1,
                  (scm_t_subr)im::opengl3::RenderDrawData);

#if defined (HAVE_STB_IMAGE)
    guile::define("load-image", 1, (scm_t_subr)im::loadimage);
    guile::exports("load-image");
#endif
  }
}

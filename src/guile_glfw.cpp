#include <guile.hpp>
#include <GLFW/glfw3.h>
namespace GGLFW {
using guile::value;
  value init() { return (bool)glfwInit(); }
  value Terminate() {
    glfwTerminate();
    return SCM_UNSPECIFIED;
  }
  value CreateWindow(value width, value height, value title) {
    std::string t=title;
    auto o = glfwCreateWindow(width, height, t.c_str(), nullptr, nullptr);
    if (o)
      return scm_from_pointer(o, nullptr);
    return SCM_BOOL_F;
  }
  value DestroyWindow(value window) {
  auto o = static_cast<GLFWwindow *>(scm_to_pointer(window));
  glfwDestroyWindow(o);
  return SCM_UNSPECIFIED;
  }
  value MakeContextCurrent(value window) {
    auto o=static_cast<GLFWwindow*>(scm_to_pointer(window));
    glfwMakeContextCurrent(o);
    return SCM_UNSPECIFIED;
  }
  value WindowHint(value hint,value val) {
    glfwWindowHint(hint, val);
    return SCM_UNSPECIFIED;
  }
value SwapInterval(value interval) {
  glfwSwapInterval(interval);
  return SCM_UNSPECIFIED;
}
  value WindowShouldClose(value window) {
    auto o = static_cast<GLFWwindow *>(scm_to_pointer(window));
    return (bool)glfwWindowShouldClose(o);
  }
  value GetWindowSize(value window) {
    auto o = static_cast<GLFWwindow *>(scm_to_pointer(window));
    int width,height;
    glfwGetWindowSize(o,&width,&height);
    return scm_list_2(value(width), value(height));
  }
    value GetCursorPos(value window) {
    auto o = static_cast<GLFWwindow *>(scm_to_pointer(window));
    double x,y;
    glfwGetCursorPos(o,&x,&y);
    return scm_cons(value(x), value(y));
  }
  value PollEvents() {
    glfwPollEvents();
    return SCM_UNSPECIFIED;
  }
  value SwapBuffers(value window) {
    auto o = static_cast<GLFWwindow *>(scm_to_pointer(window));
    glfwSwapBuffers(o);
    return SCM_UNSPECIFIED;
  }
  } // namespace GGLFW

extern "C" {

void init_glfw() {
  defconst(GLFW_TRANSPARENT_FRAMEBUFFER);
  defconst(GLFW_CONTEXT_VERSION_MAJOR);
  defconst(GLFW_CONTEXT_CREATION_API);
  defconst(GLFW_CONTEXT_NO_ERROR);
  defconst(GLFW_CONTEXT_VERSION_MINOR);
  defconst(GLFW_VISIBLE);
  // defconst(GLFW_DECORATED);
  // defconst(GLFW_AUTO_ICONIFY);
  // defconst(GLFW_MAXIMIZED);
  // defconst(GLFW_FLOATING);
  // defconst(GLFW_HOVERED);

  // defconst(GLFW_WAYLAND_PREFER_LIBDECOR);
  // defconst(GLFW_WAYLAND_DISABLE_LIBDECOR);
  // defconst(GLFW_CENTER_CURSOR);
  scm_c_define_gsubr("glfwinit", 0, 0, 0, (scm_t_subr)GGLFW::init);
  scm_c_define_gsubr("terminate", 0, 0, 0, (scm_t_subr)GGLFW::Terminate);
  scm_c_define_gsubr("glfwcreatewindow", 3, 0, 0,
                       (scm_t_subr)GGLFW::CreateWindow);
  scm_c_define_gsubr("glfwdestroywindow", 1, 0, 0,
                       (scm_t_subr)GGLFW::DestroyWindow);
    scm_c_define_gsubr("glfwmakecontextcurrent", 1, 0, 0,
                       (scm_t_subr)GGLFW::MakeContextCurrent);
    scm_c_define_gsubr("swap-interval", 1, 0, 0,
                       (scm_t_subr)GGLFW::SwapInterval);
    scm_c_define_gsubr("glfwwindowhint", 2, 0, 0,
                       (scm_t_subr)GGLFW::WindowHint);
    scm_c_define_gsubr("glfwwindowshouldclose", 1, 0, 0,
                       (scm_t_subr)GGLFW::WindowShouldClose);
    scm_c_define_gsubr("%get-window-size", 1, 0, 0,
                       (scm_t_subr)GGLFW::GetWindowSize);
    scm_c_define_gsubr("%get-cursor-pos", 1, 0, 0,
                       (scm_t_subr)GGLFW::GetCursorPos);
    scm_c_define_gsubr("poll-events", 0, 0, 0,
                       (scm_t_subr)GGLFW::PollEvents);
    scm_c_define_gsubr("glfwswapbuffers", 1, 0, 0,
                       (scm_t_subr)GGLFW::SwapBuffers);
  }
}

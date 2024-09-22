(define-module (glfw)
  #:export (glfwinit
            glfwwindowhint
            GLFW_CONTEXT_VERSION_MAJOR
            GLFW_CONTEXT_VERSION_MINOR
            glfwcreatewindow
            glfwmakecontextcurrent
            glfwswapinterval
            glfwwindowshouldclose
            glfwpollevents
            glfwswapbuffers
            glfwdestroywindow
            glfwterminate))

(load-extension "build/libguile_glfw.so" "init_glfw")

#pragma once
#if defined _WIN32 || defined __CYGWIN__
  #ifdef BUILDING_GUILE_DEAR_IMGUI
    #define GUILE_DEAR_IMGUI_PUBLIC __declspec(dllexport)
  #else
    #define GUILE_DEAR_IMGUI_PUBLIC __declspec(dllimport)
  #endif
#else
  #ifdef BUILDING_GUILE_DEAR_IMGUI
      #define GUILE_DEAR_IMGUI_PUBLIC __attribute__ ((visibility ("default")))
  #else
      #define GUILE_DEAR_IMGUI_PUBLIC
  #endif
#endif

namespace guile_dear_imgui {

class GUILE_DEAR_IMGUI_PUBLIC Guile_dear_imgui {

public:
  Guile_dear_imgui();
  int get_number() const;

private:

  int number;

};

}


#include "SDL_video.h"
#include <climits>
#include <cstdint>
#include <exception>
#include <guile_dear_imgui.hpp>
#include <imgui.h>
#include <imgui/backends/imgui_impl_sdl2.h>
#include <imgui/backends/imgui_impl_opengl2.h>
#include <imgui/backends/imgui_impl_opengl3.h>
#include <libguile.h>
#include <functional>
#include <memory>
#include <ostream>
#include <string>
#include <iostream>

#include <typeinfo>

#if defined(IMGUI_IMPL_OPENGL_ES2)
#include <SDL_opengles2.h>
#else
#include <SDL_opengl.h>
#endif

namespace guile {
  class value {
  public:
    value() = default;
    ~value() = default;
    value(SCM o) : value_{o} { };
    value(int arg) { value_ = scm_from_int(arg); };
    /* value(int32_t arg) { value_ = scm_from_int32(arg); }; */
    value(int16_t arg) { value_ = scm_from_int16(arg); };
    value(int8_t arg) { value_ = scm_from_int8(arg); };
    value(uint32_t arg) { value_ = scm_from_uint32(arg); };
    value(uint16_t arg) { value_ = scm_from_uint16(arg); };
    value(uint8_t arg) { value_ = scm_from_uint8(arg); };
    value(bool arg) { value_ = scm_from_bool(arg); };
    value(float arg) { value_ = scm_from_double(arg); };
    value(double arg) { value_ = scm_from_double(arg); };
    value(const char* arg) { value_ = scm_from_locale_string(arg);};
    value(std::string str) { value_ = scm_from_locale_string(str.c_str()); };
    SCM get() const {return value_;};
    bool unboundp() const {return SCM_UNBNDP(value_);};
    bool boundp() const { return !unboundp(); };
    bool string_p() const {return scm_to_bool(scm_string_p(value_));};
    operator SCM () const {return value_;}
    operator int32_t () const {return scm_to_int32(value_);}
    operator int16_t() const { return scm_to_int16(value_); }
    operator int8_t() const { return scm_to_int8(value_); }
    operator uint32_t () const {return scm_to_uint32(value_);}
    operator uint16_t () const {return scm_to_uint16(value_);}
    operator uint8_t () const {return scm_to_uint8(value_);}
    operator bool () const {return scm_to_bool(value_);}

    operator double () const {return scm_to_double(value_);}
    operator float () const {

      return scm_to_double(value_);
    }

    operator std::string () const {
      auto c_str=scm_to_locale_string(value_);
      auto str = std::string(c_str);
      free(c_str);
      return str;
    }
    operator char* () const {
      auto c_str=scm_to_locale_string(value_);
      return c_str;
    }
    friend std::ostream &operator<<(std::ostream &os, guile::value v){
      auto c = scm_to_locale_string(scm_simple_format(
          SCM_BOOL_F, scm_from_locale_string("~a"), scm_list_1(v.get())));
      os << c;
      free(c);
      return os;
    };
    value operator[](uint32_t a0) const {
      auto k=scm_from_uint32(a0);
      if (scm_to_bool(scm_list_p(value_)))
        return value{scm_list_ref(value_, k)};
      return value{scm_string_ref(value_, k)};

    };
    value operator() () const
    { return value{scm_call_0(value_)}; }
    value operator() (value a0) const
    { return value{scm_call_1(value_, a0)}; }
    value operator() (value a0, value a1) const
    { return value{scm_call_2(value_, a0, a1)}; }
    value operator() (value a0, value a1, value a2) const
    { return value{scm_call_3(value_, a0, a1, a2)}; }
    value operator() (value a0, value a1, value a2, value a3) const
    { return value{scm_call_4(value_, a0, a1, a2, a3)}; }
    value operator() (value a0, value a1, value a2, value a3, value a4) const
    { return value{scm_call_5(value_, a0, a1, a2, a3, a4)}; }
    value operator() (value a0, value a1, value a2, value a3, value a4, value a5) const
    { return value{scm_call_6(value_, a0, a1, a2, a3, a4, a5)}; }
    value operator() (value a0, value a1, value a2, value a3, value a4, value a5, value a6) const
    { return value{scm_call_7(value_, a0, a1, a2, a3, a4, a5, a6)}; }
    value operator() (value a0, value a1, value a2, value a3, value a4, value a5, value a6,value a7) const
    { return value{scm_call_8(value_, a0, a1, a2, a3, a4, a5, a6,a7)}; }
    value operator() (value a0, value a1, value a2, value a3, value a4, value a5, value a6,value a7, value a8) const
    { return value{scm_call_9(value_, a0, a1, a2, a3, a4, a5, a6,a7,a8)}; }

    template<typename...T, typename = std::enable_if_t<(sizeof...(T) > 9), void>>
    value operator() (T...arg) const{
      return value{scm_call(value_,arg...,SCM_UNDEFINED)};
    }

  protected:
    SCM value_ = SCM_UNSPECIFIED;
  };
} // namespace guile

namespace im {
/* template <typename F, typename Arg> */
/* auto func{ */

/* } */
  using guile::value;
  value create_context() {
    auto c = ImGui::CreateContext();
    return scm_from_pointer(
        c, // [](void *c) {
        //  if (c)
        //    ImGui::DestroyContext(static_cast<ImGuiContext *>(c));
        // }
        nullptr
      );
  }
  value destroy_context(value v) {
    if (v.unboundp()){
        ImGui::DestroyContext();
    } else {
        ImGuiContext *c = static_cast<ImGuiContext *>(scm_to_pointer(v));
        ImGui::DestroyContext(c);
    };
    return SCM_UNSPECIFIED;
  }
  value getio() {
    ImGuiIO &c = ImGui::GetIO();
    // c.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
    // c.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;
    return scm_from_pointer(&c, nullptr);
  }
  value GetStyle() {
    return scm_from_pointer(&ImGui::GetStyle(), nullptr);
  }
  value ScaleAllSizes(value o,value scale) {
    auto c = static_cast<ImGuiStyle*>(scm_to_pointer(o));
    std::cout << scale << std::endl;
    c->ScaleAllSizes(scale);
    return SCM_UNSPECIFIED;
  }
  value GetVersion(){
    return ImGui::GetVersion();
  }

  value setup_font(value io) {
    ImGuiIO *c = static_cast<ImGuiIO *>(scm_to_pointer(io));
    unsigned char* tex_pixels = nullptr;
    int tex_w, tex_h;
    c->Fonts->GetTexDataAsRGBA32(&tex_pixels,&tex_w,&tex_h);
    return scm_list_2(value{tex_w},value{tex_h});
  }

  value set_io_display_size(value io,value vec2) {
    ImGuiIO* c = static_cast<ImGuiIO*>(scm_to_pointer(io));
    ImVec2 *vec = static_cast<ImVec2*>(scm_to_pointer(vec2));
    c->DisplaySize=*vec;
    return SCM_UNSPECIFIED;
  }
  value io_display_size(value io) {
    ImGuiIO* cio = static_cast<ImGuiIO*>(scm_to_pointer(io));
    //ImVec2 *vec = static_cast<ImVec2*>(scm_to_pointer(vec2));
    auto c=cio->DisplaySize;
    return scm_cons(scm_from_int(c.x), scm_from_int(c.y));
  }
  value SetNextWindowSize(value x,value y) {
    auto vec=ImVec2(x ,y);
    ImGui::SetNextWindowSize(vec);
    return SCM_UNSPECIFIED;
  }
  value SetNextWindowPos(value x,value y, value cond, value px,value py) {
    auto vec = ImVec2 (x,y);
    auto vec2 = ImVec2(px, py);
    ImGui::SetNextWindowPos(vec,cond,vec2);
    return SCM_UNSPECIFIED;
  };
value set_io_config_flags(value io,value flag) {
    ImGuiIO* c = static_cast<ImGuiIO*>(scm_to_pointer(io));
    c->ConfigFlags |= scm_to_int(flag);
    return SCM_UNSPECIFIED;
  }
  value vec2(value x,value y) {
    auto c = new ImVec2(x,y);
    return scm_from_pointer(c, [](void* v){
      ImVec2 *vec = static_cast<ImVec2*>(v);
      //std::cout << "x: " << vec->x << " y: " << vec->y << std::endl;
      delete vec;
    });
  }
  value new_frame() {
    ImGui::NewFrame();
    return SCM_UNSPECIFIED;
  }
  value Begin(value v) { return ImGui::Begin(v,nullptr,ImGuiWindowFlags_AlwaysAutoResize); }
  value Render() {
    ImGui::Render();
  return SCM_UNSPECIFIED;
  }
  value End() {
    ImGui::End();
    return SCM_UNSPECIFIED;
  }
  value BeginChild(value id// , value size, value child_flags,
                   // value window_flags
                   ) {
    if (id.string_p()) {
      const char* str_id=id;
      return ImGui::BeginChild(str_id, ImVec2(0, 0),
                               ImGuiChildFlags_AutoResizeX |
                                   ImGuiChildFlags_AutoResizeY |
                                   ImGuiChildFlags_Border);

    } else {
      ImGuiID iid=id;
      return ImGui::BeginChild(iid, ImVec2(0, 0),
                               ImGuiChildFlags_AutoResizeX |
                                   ImGuiChildFlags_AutoResizeY |
                                   ImGuiChildFlags_Border);
    }
  }
  value EndChild() {
    ImGui::EndChild();
    return SCM_UNSPECIFIED;
  }

  value BeginMainMenuBar() { return ImGui::BeginMainMenuBar(); }
  value EndMainMenuBar() {
    ImGui::EndMainMenuBar();
    return SCM_UNSPECIFIED;
  }
  value BeginTooltip() { return ImGui::BeginTooltip(); }
  value EndTooltip() {
    ImGui::EndTooltip();
    return SCM_UNSPECIFIED;
  }

  value BeginCombo(value label, value preview_value) {
    return ImGui::BeginCombo(label, preview_value);
  }
  value EndCombo() {
    ImGui::EndCombo();
    return SCM_UNSPECIFIED;
  }

  value BeginGroup() {
    ImGui::BeginGroup();
    return SCM_UNSPECIFIED;}
  value EndGroup() {
    ImGui::EndGroup();
    return SCM_UNSPECIFIED;
  }
  value BeginListBox(value label, value width,value height) {
    return ImGui::BeginListBox(label,ImVec2(width, height));
  }
  value EndListBox() {
    ImGui::EndListBox();
    return SCM_UNSPECIFIED;
  }

  value BeginMenu(value label,value enabled) {
    return ImGui::BeginMenu(label, enabled);
  }
  value EndMenu() {
    ImGui::EndMenu();
    return SCM_UNSPECIFIED;
  }
  value GetFontSize() {
    return ImGui::GetFontSize();
  }

  value Separator() {
    ImGui::Separator();
    return SCM_UNSPECIFIED;
  }
  value Spacing() {
    ImGui::Spacing();
    return SCM_UNSPECIFIED;
  }

  value BeginItemTooltip() { return ImGui::BeginItemTooltip(); }
  value MenuItem(value label, value shortcut, value selected, value enabled) {
    return ImGui::MenuItem(label,shortcut,selected,enabled);
  }

  value text(value str) {
    ImGui::TextUnformatted(str);
    return SCM_UNSPECIFIED;
  }
  value TextLink(value label) { return ImGui::TextLink(label); }
  value TextLinkOpenURL(value label,value url) {
    ImGui::TextLinkOpenURL(label, url);
    return SCM_UNSPECIFIED;
  }
  value Indent(value indent_w) {
    if (indent_w.boundp())
      ImGui::Indent(indent_w);
    else {
      ImGui::Indent();
    }
    return SCM_UNSPECIFIED;
  }
  value Unindent(value indent_w) {
    if (indent_w.boundp())
      ImGui::Unindent(indent_w);
    else {
      ImGui::Unindent();
    }
    return SCM_UNSPECIFIED;
  }
  value OpenPopup(value id) {
    if (scm_to_bool(scm_string_p(id)))
      ImGui::OpenPopup(scm_to_locale_string(id));
    else
      ImGui::OpenPopup(scm_to_unsigned_integer(id,0,INT_MAX));
    return SCM_UNSPECIFIED;
  }
  value BeginPopup(value label) { return ImGui::BeginPopup(label); }
  value BeginPopupModal(value label,value p_open) {
    bool open=p_open;
    auto ret=ImGui::BeginPopupModal(label,&open);
    return scm_values_2(value(ret), value(open));
  }


  value EndPopup() {
    ImGui::EndPopup();
    return SCM_UNSPECIFIED;
  }

  value Checkbox(value label,value state) {
    bool v=state;
    auto ret=ImGui::Checkbox(label,&v);
    return scm_values_2(value(ret),
                        value(v));
  }

  value InputFloat(value label,value v,value step, value step_fast) {
    float val=v;
    auto ret=ImGui::InputFloat(label,&val,step,step_fast);
    return scm_values_2(value(ret),
                        value(val));
  }
  value InputInt(value label,value v,value step, value step_fast) {
    int val=v;
    auto ret=ImGui::InputInt(label,&val,step,step_fast);
    return scm_values_2(value(ret),
                        value(val));
  }

  value SameLine(value x,value s) {

    float offset_from_start_x = 0.0f, spacing = -1.0f;
    if (x.boundp())
      offset_from_start_x=x;
    if (s.boundp())
      spacing=s;
    ImGui::SameLine(offset_from_start_x,spacing);
    return SCM_UNSPECIFIED;
  }
  value NewLine() {
    ImGui::NewLine();
    return SCM_UNSPECIFIED;
  }
  value Bullet() {
    ImGui::Bullet();
    return SCM_UNSPECIFIED;
  }
  value Button(value label) { return ImGui::Button(label); }
  value SmallButton(value label) { return ImGui::SmallButton(label); }
  value Selectable(value label, value selected) {
    bool selectedp = selected;
    auto ret=ImGui::Selectable(label, &selectedp);
    return scm_values_2(value(ret), value(selectedp));
  }

  namespace impl {
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
    }
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
          return ImGui_ImplOpenGL3_Init(glsl_version);
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
  }
}
/* template <typename T, typename Fn> */
/* static void define(const std::string &name, Fn fn) { */
/*   scm_c_define_gsubr(name.c_str(), 1, 0, 0, (scm_t_subr)fn); */
/* } */

extern "C" {

  void init_imgui() {
    IMGUI_CHECKVERSION();
    scm_c_define_gsubr("begin-window", 1, 0, 0, (scm_t_subr)im::Begin);
    scm_c_define_gsubr("end-window", 0, 0, 0, (scm_t_subr)im::End);
    scm_c_define_gsubr("begin-child", 1, 0, 0, (scm_t_subr)im::BeginChild);
    scm_c_define_gsubr("end-child", 0, 0, 0, (scm_t_subr)im::EndChild);
    scm_c_define_gsubr("begin-main-menu-bar", 0, 0, 0,
                       (scm_t_subr)im::BeginMainMenuBar);

    scm_c_define_gsubr("begin-group", 0, 0, 0, (scm_t_subr)im::BeginGroup);
    scm_c_define_gsubr("end-group", 0, 0, 0, (scm_t_subr)im::EndGroup);

    scm_c_define_gsubr("begin-list-box", 3, 0, 0, (scm_t_subr)im::BeginListBox);
    scm_c_define_gsubr("end-list-box", 0, 0, 0, (scm_t_subr)im::EndListBox);

    scm_c_define_gsubr("begin-menu", 2, 0, 0, (scm_t_subr)im::BeginMenu);
    scm_c_define_gsubr("end-menu", 0, 0, 0, (scm_t_subr)im::EndMenu);

    scm_c_define_gsubr("end-main-menu-bar", 0, 0, 0,
                       (scm_t_subr)im::EndMainMenuBar);
    scm_c_define_gsubr("begin-tooltip", 0, 0, 0,
                       (scm_t_subr)im::BeginTooltip);
    scm_c_define_gsubr("begin-item-tooltip", 0, 0, 0,
                       (scm_t_subr)im::BeginItemTooltip);
    scm_c_define_gsubr("end-tooltip", 0, 0, 0,
                       (scm_t_subr)im::EndTooltip);
    scm_c_define_gsubr("begin-combo", 2, 0, 0,
                       (scm_t_subr)im::BeginCombo);
    scm_c_define_gsubr("end-combo", 0, 0, 0,
                       (scm_t_subr)im::EndCombo);
    scm_c_define_gsubr("menu-item", 4, 0, 0,
                       (scm_t_subr)im::MenuItem);
    scm_c_define_gsubr("set-next-window-size", 2, 0, 0,
                       (scm_t_subr)im::SetNextWindowSize);
    scm_c_define_gsubr("set-next-window-pos", 5, 0, 0, (scm_t_subr)im::SetNextWindowPos);
    scm_c_define_gsubr("render", 0, 0, 0, (scm_t_subr)im::Render);
    scm_c_define_gsubr("new-frame", 0, 0, 0, (scm_t_subr)im::new_frame);
    scm_c_define_gsubr("text", 1, 0, 0, (scm_t_subr)im::text);
    scm_c_define_gsubr("indent", 0, 1, 0, (scm_t_subr)im::Indent);
    scm_c_define_gsubr("unindent", 0, 1, 0, (scm_t_subr)im::Unindent);
    scm_c_define_gsubr("checkbox", 2, 0, 0, (scm_t_subr)im::Checkbox);
    scm_c_define_gsubr("input-int", 4, 0, 0, (scm_t_subr)im::InputInt);
    scm_c_define_gsubr("input-float", 4, 0, 0, (scm_t_subr)im::InputFloat);
    scm_c_define_gsubr("textlink", 1, 0, 0, (scm_t_subr)im::TextLink);
    scm_c_define_gsubr("textlink-open-url", 2, 0, 0, (scm_t_subr)im::TextLinkOpenURL);
    scm_c_define_gsubr("sameline", 0, 2, 0, (scm_t_subr)im::SameLine);
    scm_c_define_gsubr("open-popup", 1, 0, 0, (scm_t_subr)im::OpenPopup);
    scm_c_define_gsubr("begin-popup", 1, 0, 0, (scm_t_subr)im::BeginPopup);
    scm_c_define_gsubr("begin-popup-modal", 2, 0, 0, (scm_t_subr)im::BeginPopupModal);
    scm_c_define_gsubr("end-popup", 0, 0, 0, (scm_t_subr)im::EndPopup);

    scm_c_define_gsubr("get-font-size", 0, 0, 0, (scm_t_subr)im::GetFontSize);
    scm_c_define_gsubr("separator", 0, 0, 0, (scm_t_subr)im::Separator);
    scm_c_define_gsubr("spacing", 0, 0, 0, (scm_t_subr)im::Spacing);
    scm_c_define_gsubr("newline", 0, 0, 0, (scm_t_subr)im::NewLine);
    scm_c_define_gsubr("bullet", 0, 0, 0, (scm_t_subr)im::Bullet);
    scm_c_define_gsubr("button", 1, 0, 0, (scm_t_subr)im::Button);
    scm_c_define_gsubr("small-button", 1, 0, 0, (scm_t_subr)im::SmallButton);
    scm_c_define_gsubr("selectable", 2, 0, 0, (scm_t_subr)im::Selectable);
    scm_c_define_gsubr("create-context", 0, 0, 0,
                       (scm_t_subr)im::create_context);
    scm_c_define_gsubr("destroy-context", 0, 1, 0,
                       (scm_t_subr)im::destroy_context);
    scm_c_define_gsubr("get-io", 0, 0, 0, (scm_t_subr)im::getio);
    scm_c_define_gsubr("get-style", 0, 0, 0, (scm_t_subr)im::GetStyle);
    scm_c_define_gsubr("style-scaleallsizes", 2, 0, 0, (scm_t_subr)im::ScaleAllSizes);
    scm_c_define_gsubr("get-version", 0, 0, 0, (scm_t_subr)im::GetVersion);
    scm_c_define_gsubr("setup-font", 1, 0, 0, (scm_t_subr)im::setup_font);
    scm_c_define_gsubr("set-io-display-size", 2, 0, 0,
                       (scm_t_subr)im::set_io_display_size);
    scm_c_define_gsubr("io-display-size", 1, 0, 0,
                       (scm_t_subr)im::io_display_size);
    scm_c_define_gsubr("vec2", 2, 0, 0, (scm_t_subr) im::vec2 );
    //    scm_c_define_gsubr("vec2.x", 1, 0, 0, (scm_t_subr) [](value vec){} );
    scm_c_define_gsubr("impl:opengl3:init",0,1,0, (scm_t_subr)im::impl::opengl3::init);
    scm_c_define_gsubr("impl:sdl2:init-vulkan", 0, 0, 0,
                       (scm_t_subr)im::impl::sdl2::InitForVulkan);
    scm_c_define_gsubr("impl:sdl2:init-opengl", 2, 0, 0,
                       (scm_t_subr)im::impl::sdl2::InitForOpenGl);

    scm_c_define_gsubr("impl:sdl2:shutdown", 0, 0, 0,
                       (scm_t_subr)im::impl::sdl2::Shutdown);
    scm_c_define_gsubr("impl:sdl2:process-event", 1, 0, 0,
                       (scm_t_subr)im::impl::sdl2::ProcessEvent);
    scm_c_define_gsubr("impl:opengl3:shutdown", 0, 0, 0,
                       (scm_t_subr)im::impl::opengl3::Shutdown);
    scm_c_define_gsubr("impl:sdl2:new-frame", 0, 0, 0,
                       (scm_t_subr)im::impl::sdl2::NewFrame);
    scm_c_define_gsubr("impl:opengl3:new-frame", 0, 0, 0,
                       (scm_t_subr)im::impl::opengl3::NewFrame);
    scm_c_define_gsubr("impl:opengl3:render-draw-data", 0, 0, 0,
                       (scm_t_subr)im::impl::opengl3::RenderDrawData);


}
}

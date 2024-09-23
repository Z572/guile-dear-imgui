#include "SDL_video.h"
#include <climits>
#include <cstdint>
#include <exception>
#include <guile.hpp>
#include <imgui.h>
#include <imgui/misc/cpp/imgui_stdlib.h>
#include <imgui/backends/imgui_impl_sdl2.h>
#include <imgui/backends/imgui_impl_glfw.h>
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

#define maybe_set(v,val) if (v.unboundp()) {v=val;}
#define LABEL(v) std::string(v).c_str()
namespace im {
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
  value Begin(value label, value p_open, value flags) {
    bool n;
    int flag=0;
    auto p_open_boundp= p_open.boundp() and !(scm_is_false(p_open.get()));
    if (p_open_boundp)
      n = p_open();
    if (flags.boundp())
      flag=flags;
    auto ret = ImGui::Begin(LABEL(label), nullptr, flag);
    if (p_open_boundp)
      p_open(n);
    return ret;
  }

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
      std::string str_id=id;
      return ImGui::BeginChild(LABEL(id), ImVec2(0, 0),
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

  value BeginTabBar(value id, value flags) {
    if (flags.unboundp())
      flags=0;
    return ImGui::BeginTabBar(LABEL(id),flags);
  }
  value EndTabBar() {
    ImGui::EndTabBar();
    return SCM_UNSPECIFIED;
  }

  value BeginTabItem(value id// ,value p_open, value flags
                     ) {
    return ImGui::BeginTabItem(LABEL(id));
  }
  value EndTabItem() {
    ImGui::EndTabItem();
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
    return ImGui::BeginCombo(LABEL(label), LABEL(preview_value));
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
    return ImGui::BeginListBox(LABEL(label),ImVec2(width, height));
  }
  value EndListBox() {
    ImGui::EndListBox();
    return SCM_UNSPECIFIED;
  }

  value BeginMenu(value label,value enabled) {
    return ImGui::BeginMenu(LABEL(label), enabled);
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
  value InputText(value label,value buf,value flags) {
    std::string buff = buf();
    auto ret=ImGui::InputText(LABEL(label),&buff,flags);
    buf(buff);
    return ret;
  }
  value InputTextWithHint(value label,value hint,value buf,value flags) {
    std::string buff = buf();
    auto ret = ImGui::InputTextWithHint(LABEL(label),
                                        LABEL(hint),
                                        &buff,flags);
    buf(buff);
    return ret;
  }
  value SliderInt(value label, value v, value v_min, value v_max, value flags) {
    maybe_set(flags, 0);
    int n = v();
    auto ret = ImGui::SliderInt(LABEL(label), &n, v_min, v_max, "%d", flags);
    v(n);
    return ret;
  }
  value DragInt(value label,value v, value v_speed, value v_min, value v_max,
                value flags) {
    int n = v();
    maybe_set(v_speed,1.0f);
    maybe_set(v_min, 0);
    maybe_set(v_max, 0);
    maybe_set(flags, 0);
    auto ret = ImGui::DragInt(LABEL(label), &n, v_speed, v_min, v_max, "%d", flags);
    v(n);
    return ret;
  }
  value BeginItemTooltip() { return ImGui::BeginItemTooltip(); }
  value MenuItem(value label, value shortcut, value selected, value enabled) {
    return ImGui::MenuItem(LABEL(label),LABEL(shortcut),selected,enabled);
  }

  value text(value str) {
    ImGui::TextUnformatted(LABEL(str));
    return SCM_UNSPECIFIED;
  }
  value TextColored(value col, value str) {
    ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(col[0],col[1],col[2],col[3]));
    text(str);
    ImGui::PopStyleColor();
    return SCM_UNSPECIFIED;
  }
  value ProgressBar(value fraction, value size_arg, value overlay) {
    auto size_arg_boundp=size_arg.boundp();
    if (size_arg_boundp) {
      auto overlay_boundp = overlay.boundp();

      if (overlay_boundp) {
        ImGui::ProgressBar(fraction, ImVec2(size_arg.car(), size_arg.cdr()),
                           LABEL(overlay));
      } else {
        ImGui::ProgressBar(fraction,ImVec2(size_arg.car(),size_arg.cdr()));
      }
    }
    else {
      ImGui::ProgressBar(fraction);
    }
    return SCM_UNSPECIFIED;
  }
  value TextLink(value label) { return ImGui::TextLink(LABEL(label)); }
  value TextLinkOpenURL(value label,value url) {
    ImGui::TextLinkOpenURL(LABEL(label), LABEL(url));
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
  value BeginPopup(value label) {
    return ImGui::BeginPopup(LABEL(label));
}
  value BeginPopupModal(value label,value p_open) {
    bool open=p_open();
    auto ret = ImGui::BeginPopupModal(LABEL(label), &open);
    p_open(open);
    return ret;
  }


  value EndPopup() {
    ImGui::EndPopup();
    return SCM_UNSPECIFIED;
  }

  value Checkbox(value label,value state) {
    bool v = state();
    auto ret = ImGui::Checkbox(LABEL(label), &v);
    state(v);
    return ret;
  }

  value InputFloat(value label,value v,value step, value step_fast) {
    float val=v();
    auto ret = ImGui::InputFloat(LABEL(label), &val, step, step_fast);
    v(val);
    return value(ret);
  }
  value InputInt(value label,value v,value step, value step_fast) {
    int val=v();
    auto ret = ImGui::InputInt(LABEL(label), &val, step, step_fast);
    v(val);
    return value(ret);
  }

  value ColorPicker3(value label, value col, value flags) {
    maybe_set(flags, 0);
    auto v=col();
    float colors[3] = {v[0],v[1],v[2]};
    auto ret = ImGui::ColorPicker3(LABEL(label), colors, flags);
    col(scm_list_3(value(colors[0]),value(colors[1]),value(colors[2])));
    return ret;
  }
  value ColorPicker4(value label, value col, value flags) {
    maybe_set(flags, 0);
    auto v=col();
    float colors[4] = {v[0],v[1],v[2],v[3]};
    auto ret = ImGui::ColorPicker4(LABEL(label), colors, flags);
    col(scm_list_4(value(colors[0]),value(colors[1]),value(colors[2]),value(colors[3])));
    return ret;
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
  value Button(value label) { return ImGui::Button(LABEL(label)); }
  value SmallButton(value label) { return ImGui::SmallButton(LABEL(label)); }
  value Selectable(value label, value selected) {
    bool selectedp=false;
    if (selected.is_procedure_p())
      selectedp = selected();
    auto ret = ImGui::Selectable(LABEL(label), &selectedp);
    if (selected.is_procedure_p())
      selected(selectedp);
    return value(ret);
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
      } // namespace sdl2
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
  }
  } // namespace im
#undef maybe_set

extern "C" {

  void init_imgui() {
    IMGUI_CHECKVERSION();
    defconst(ImGuiWindowFlags_None);
    defconst(ImGuiWindowFlags_NoTitleBar);
    defconst(ImGuiWindowFlags_NoResize);
    defconst(ImGuiWindowFlags_NoMove);
    defconst(ImGuiWindowFlags_NoScrollbar);
    defconst(ImGuiWindowFlags_NoScrollWithMouse);
    defconst(ImGuiWindowFlags_NoCollapse);
    defconst(ImGuiWindowFlags_AlwaysAutoResize);
    defconst(ImGuiWindowFlags_NoBackground);
    defconst(ImGuiWindowFlags_NoSavedSettings);
    defconst(ImGuiWindowFlags_NoMouseInputs);
    defconst(ImGuiWindowFlags_MenuBar);
    defconst(ImGuiWindowFlags_HorizontalScrollbar);
    defconst(ImGuiWindowFlags_NoFocusOnAppearing);
    defconst(ImGuiWindowFlags_NoBringToFrontOnFocus);
    defconst(ImGuiWindowFlags_AlwaysVerticalScrollbar);
    defconst(ImGuiWindowFlags_AlwaysHorizontalScrollbar);
    defconst(ImGuiWindowFlags_NoNavInputs);
    defconst(ImGuiWindowFlags_NoNavFocus);
    defconst(ImGuiWindowFlags_UnsavedDocument);
    defconst(ImGuiWindowFlags_NoNav);
    defconst(ImGuiWindowFlags_NoDecoration);
    defconst(ImGuiWindowFlags_NoInputs);

    defconst(ImGuiConfigFlags_None);
    defconst(ImGuiConfigFlags_NavEnableGamepad);
    defconst(ImGuiConfigFlags_NavEnableGamepad);
    defconst(ImGuiConfigFlags_NavEnableSetMousePos);
    defconst(ImGuiConfigFlags_NavNoCaptureKeyboard);
    defconst(ImGuiConfigFlags_NoMouse);
    defconst(ImGuiConfigFlags_NoMouseCursorChange);
    defconst(ImGuiConfigFlags_NoKeyboard);
    defconst(ImGuiConfigFlags_IsSRGB);
    defconst(ImGuiConfigFlags_IsTouchScreen);
    scm_c_define_gsubr("begin-window", 1, 2, 0, (scm_t_subr)im::Begin);
    scm_c_define_gsubr("end-window", 0, 0, 0, (scm_t_subr)im::End);
    scm_c_define_gsubr("begin-child", 1, 0, 0, (scm_t_subr)im::BeginChild);
    scm_c_define_gsubr("end-child", 0, 0, 0, (scm_t_subr)im::EndChild);
    scm_c_define_gsubr("begin-main-menu-bar", 0, 0, 0,
                       (scm_t_subr)im::BeginMainMenuBar);

    scm_c_define_gsubr("begin-tab-bar", 1, 1, 0, (scm_t_subr)im::BeginTabBar);
    scm_c_define_gsubr("end-tab-bar", 0, 0, 0, (scm_t_subr)im::EndTabBar);

    scm_c_define_gsubr("begin-tab-item", 1, 0, 0, (scm_t_subr)im::BeginTabItem);
    scm_c_define_gsubr("end-tab-item", 0, 0, 0, (scm_t_subr)im::EndTabItem);

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
    scm_c_define_gsubr("text-colored", 2, 0, 0, (scm_t_subr)im::TextColored);
    scm_c_define_gsubr("indent", 0, 1, 0, (scm_t_subr)im::Indent);
    scm_c_define_gsubr("unindent", 0, 1, 0, (scm_t_subr)im::Unindent);
    scm_c_define_gsubr("checkbox", 2, 0, 0, (scm_t_subr)im::Checkbox);
    scm_c_define_gsubr("input-int", 4, 0, 0, (scm_t_subr)im::InputInt);
    scm_c_define_gsubr("input-float", 4, 0, 0, (scm_t_subr)im::InputFloat);
    scm_c_define_gsubr("progress-bar", 1, 2, 0, (scm_t_subr)im::ProgressBar);
    scm_c_define_gsubr("textlink", 1, 0, 0, (scm_t_subr)im::TextLink);
    scm_c_define_gsubr("textlink-open-url", 2, 0, 0, (scm_t_subr)im::TextLinkOpenURL);
    scm_c_define_gsubr("color-picker3", 2, 1, 0, (scm_t_subr)im::ColorPicker3);
    scm_c_define_gsubr("color-picker4", 2, 1, 0, (scm_t_subr)im::ColorPicker4);
    scm_c_define_gsubr("sameline", 0, 2, 0, (scm_t_subr)im::SameLine);
    scm_c_define_gsubr("slider-int", 4, 1, 0, (scm_t_subr)im::SliderInt);
    scm_c_define_gsubr("drag-int", 2, 4, 0, (scm_t_subr)im::DragInt);
    scm_c_define_gsubr("open-popup", 1, 0, 0, (scm_t_subr)im::OpenPopup);
    scm_c_define_gsubr("begin-popup", 1, 0, 0, (scm_t_subr)im::BeginPopup);
    scm_c_define_gsubr("begin-popup-modal", 2, 0, 0, (scm_t_subr)im::BeginPopupModal);
    scm_c_define_gsubr("end-popup", 0, 0, 0, (scm_t_subr)im::EndPopup);

    scm_c_define_gsubr("get-font-size", 0, 0, 0, (scm_t_subr)im::GetFontSize);
    scm_c_define_gsubr("separator", 0, 0, 0, (scm_t_subr)im::Separator);
    scm_c_define_gsubr("spacing", 0, 0, 0, (scm_t_subr)im::Spacing);
    scm_c_define_gsubr("%input-text", 3, 0, 0, (scm_t_subr)im::InputText);
    scm_c_define_gsubr("input-text-with-hint", 4, 0, 0, (scm_t_subr)im::InputTextWithHint);
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
    scm_c_define_gsubr("impl:glfw:shutdown", 0, 0, 0,
                       (scm_t_subr)im::impl::glfw::Shutdown);
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

    scm_c_define_gsubr("impl:glfw:init-opengl", 2, 0, 0,
                       (scm_t_subr)im::impl::glfw::InitForOpenGl);
    scm_c_define_gsubr("impl:glfw:new-frame", 0, 0, 0,
                           (scm_t_subr)im::impl::glfw::NewFrame);


}
}

#include <algorithm>
#include <climits>
#include <magic_enum.hpp>
#include <cstdint>
#include <exception>
#include <guile.hpp>
#include <imgui.h>
#include <imgui/misc/cpp/imgui_stdlib.h>
#include <libguile.h>
#include <functional>
#include <memory>
#include <ostream>
#include <string>
#include <iostream>

#include <typeinfo>

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
#define CASE(str, func) if (s==str) return fontAtlas->func;

  const ImWchar* GetGlyphRanges(::ImFontAtlas* fontAtlas,value symbol){
    if (scm_is_false(symbol.get()))
      return nullptr;
    std::string s = value(scm_symbol_to_string(symbol));
    CASE("chinese-simplified-common", GetGlyphRangesChineseSimplifiedCommon());
    CASE("chinese-full", GetGlyphRangesChineseFull());
    CASE("default", GetGlyphRangesDefault());
    CASE("greek", GetGlyphRangesGreek());
    CASE("korean", GetGlyphRangesKorean());
    CASE("thai", GetGlyphRangesThai());
    return nullptr;
  }
#undef CASE
  value ImFontAtlasAddFontFromFileTTF(value o ,value filename, value size_pixels,value conf,value ranges){
      auto fontAtlas = static_cast<struct ::ImFontAtlas*>(scm_to_pointer(o));
      auto fontconfig=static_cast<ImFontConfig*>(scm_to_pointer(conf));
      auto rang=GetGlyphRanges(fontAtlas,ranges);
      auto font = fontAtlas->AddFontFromFileTTF(LABEL(filename), size_pixels,
                                                fontconfig,rang);
      return scm_from_pointer(font,nullptr);
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
  value ShowFontSelector(value label){
    ImGui::ShowFontSelector(LABEL(label));
    return SCM_UNSPECIFIED;
  }
  value ShowStyleSelector(value label){
    ImGui::ShowStyleSelector(LABEL(label));
    return SCM_UNSPECIFIED;
  }
  value setup_font(value io) {
    ImGuiIO *c = static_cast<ImGuiIO *>(scm_to_pointer(io));
    unsigned char* tex_pixels = nullptr;
    int tex_w, tex_h;
    c->Fonts->GetTexDataAsRGBA32(&tex_pixels,&tex_w,&tex_h);
    return scm_list_2(value{tex_w},value{tex_h});
  }

  value set_io_display_size(value io,value w,value h) {
    ImGuiIO* c = static_cast<ImGuiIO*>(scm_to_pointer(io));
    auto vec = ImVec2(w,h);
    c->DisplaySize=vec;
    return SCM_UNSPECIFIED;
  }
  value io_display_size(value io) {
    ImGuiIO* cio = static_cast<ImGuiIO*>(scm_to_pointer(io));
    auto c=cio->DisplaySize;
    return guile::list(c.x, c.y);
  }
    value io_fonts(value io) {
    ImGuiIO* cio = static_cast<ImGuiIO*>(scm_to_pointer(io));
    //ImVec2 *vec = static_cast<ImVec2*>(scm_to_pointer(vec2));
    auto c=cio->Fonts;
    return scm_from_pointer(c,nullptr);
  }
  value GetTexDataAsRGBA32(value o,value b, value width, value height) {
    auto font = static_cast<ImFontAtlas*>(scm_to_pointer(o));
    unsigned char* tex_pixels = nullptr;
    int n,v;
    font->GetTexDataAsRGBA32(&tex_pixels,&n,&v);
    return SCM_UNSPECIFIED;
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
  value SetItemDefaultFocus() {
    ImGui::SetItemDefaultFocus();
    return SCM_UNSPECIFIED;
  }
  value SetKeyboardFocusHere(value offset) {
    ImGui::SetKeyboardFocusHere(offset);
    return SCM_UNSPECIFIED;
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
    value ColorEdit3(value label, value col, value flags) {
    maybe_set(flags, 0);
    auto v=col();
    float colors[3] = {v[0],v[1],v[2]};
    auto ret = ImGui::ColorEdit3(LABEL(label), colors, flags);
    col(scm_list_3(value(colors[0]),value(colors[1]),value(colors[2])));
    return ret;
  }
  value ColorEdit4(value label, value col, value flags) {
    maybe_set(flags, 0);
    auto v=col();
    float colors[4] = {v[0],v[1],v[2],v[3]};
    auto ret = ImGui::ColorEdit4(LABEL(label), colors, flags);
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
  value InvisibleButton(value id, value size) {
    return ImGui::InvisibleButton(LABEL(id), ImVec2(size[0], size[1]));
  }

  value Selectable(value label, value selected) {
    bool selectedp=false;
    if (selected.is_procedure_p())
      selectedp = selected();
    auto ret = ImGui::Selectable(LABEL(label), &selectedp);
    if (selected.is_procedure_p())
      selected(selectedp);
    return value(ret);
  }
  value BeginTable(value id, value column) {
    return ImGui::BeginTable(LABEL(id), column,
                             ImGuiTableFlags_BordersInnerH |
                             ImGuiTableFlags_BordersInnerV);

  }
  value EndTable() {
    ImGui::EndTable();
    return SCM_UNSPECIFIED;
  }
  value TableNextRow(value row_flags, value min_row_heighet) {
    ImGui::TableNextRow();
    return SCM_UNSPECIFIED;
  }
  value TableNextColumn() {
    return ImGui::TableNextColumn();
  }
  value TableSetColumnIndex(value n) {
    return ImGui::TableSetColumnIndex(n);
  }
  value TableGetColumnIndex(){
    return ImGui::TableGetColumnIndex();
  }
  value TableSetupColumn(value label,value flags,value init_width_or_weight,value user_id){
    maybe_set(flags,0);
    maybe_set(init_width_or_weight,0.0f);
    maybe_set(user_id,0);
    ImGui::TableSetupColumn(LABEL(label),flags,init_width_or_weight,user_id);
    return SCM_UNSPECIFIED;
}
  value TableHeadersRow(){
    ImGui::TableHeadersRow();
    return SCM_UNSPECIFIED;
}
  value TableHeader(value label){
    ImGui::TableHeader(LABEL(label));
    return SCM_UNSPECIFIED;
}
value PushStyleColor(value idx, value col) {
  std::string str="ImGuiCol_" + std::string(idx);
    auto is_p = magic_enum::enum_cast<ImGuiCol_>(str);
    if (is_p.has_value()) {
      ImGui::PushStyleColor(is_p.value(), ImVec4(col[0],col[1],col[2],col[3]));
    return true;
    }
    return false;
}
  value PopStyleColor(value count){
    maybe_set(count, 1);
    ImGui::PopStyleColor(count);
    return SCM_UNSPECIFIED;
  }

  value PushStyleVar(value idx, value col) {
  std::string str="ImGuiStyleVar_" + std::string(idx);
    auto is_p = magic_enum::enum_cast<ImGuiStyleVar_>(str);
    if (is_p.has_value()) {
      auto v = is_p.value();
      switch (v) {
      case ImGuiStyleVar_WindowPadding:
      case ImGuiStyleVar_WindowMinSize:
      case ImGuiStyleVar_WindowTitleAlign:
      case ImGuiStyleVar_FramePadding:
      case ImGuiStyleVar_ItemSpacing:
      case ImGuiStyleVar_ItemInnerSpacing:
      case ImGuiStyleVar_CellPadding:
      case ImGuiStyleVar_ButtonTextAlign:
      case ImGuiStyleVar_SelectableTextAlign:
      case ImGuiStyleVar_SeparatorTextAlign:
      case ImGuiStyleVar_SeparatorTextPadding:
        ImGui::PushStyleVar(v, ImVec2(col[0], col[1]));
        break;
      default:
        ImGui::PushStyleVar(v, (float)col);
        break;
      }
    return true;
    }
    return false;
}
  value PopStyleVar(value count){
    maybe_set(count, 1);
    ImGui::PopStyleVar(count);
    return SCM_UNSPECIFIED;
  }
  value AlignTextToFramePadding() {
    ImGui::AlignTextToFramePadding();
    return SCM_UNSPECIFIED;
  }
  value GetTextLineHeight() { return ImGui::GetTextLineHeight(); }
  value GetTextLineHeightWithSpacing() {
    return ImGui::GetTextLineHeightWithSpacing();
  }
  value GetFrameHeight() { return ImGui::GetFrameHeight(); }
  value GetFrameHeightWithSpacing() {
    return ImGui::GetFrameHeightWithSpacing();
  }
  value GetTime(){return ImGui::GetTime();}

  value TreeNode(value label, value flags) {
    if (flags.unboundp())
      return ImGui::TreeNode(LABEL(label));
    else
      return ImGui::TreeNodeEx(LABEL(label),flags);
  }
  value TreePush(value label) {
    ImGui::TreePush(LABEL(label));
    return SCM_UNSPECIFIED;
  }
  value TreePop() {
    ImGui::TreePop();
    return SCM_UNSPECIFIED;
  }

  value CollapsingHeader(value label, value flags) {
    if (flags.unboundp())
      return ImGui::CollapsingHeader(LABEL(label));
    else
      return ImGui::CollapsingHeader(LABEL(label),flags);
  }

  value IsWindowAppearing() { return ImGui ::IsWindowAppearing(); };
  value IsWindowCollapsed() { return ImGui ::IsWindowCollapsed(); };
  value IsWindowFocused(value flags) {
    maybe_set(flags, 0)
    return ImGui ::IsWindowFocused(flags);
  };
  value IsWindowHovered(value flags) {
    maybe_set(flags, 0)
    return ImGui ::IsWindowHovered(flags);
  };

} // namespace im

#define export_enum(e)                                      \
  {                                                         \
    constexpr auto enums = magic_enum::enum_entries<e>();   \
    std::for_each(enums.begin(), enums.end(), [](auto o) {  \
      const char *str = o.second.data();                    \
      scm_c_define(str, guile::value(o.first));             \
      scm_c_export(str,nullptr);                            \
    });                                                     \
  }

extern "C" {

  void init_imgui() {
    IMGUI_CHECKVERSION();
    ImGui::SetAllocatorFunctions([](size_t sz,[[maybe_unused]] void* user_data){
      return scm_gc_malloc(sz,"imgui");
    },
      [](void* ptr,[[maybe_unused]] void* user_data){
        scm_gc_free(ptr,0,"imgui:free");
      });
    export_enum(ImGuiWindowFlags_);
    export_enum(ImGuiConfigFlags_);
    export_enum(ImGuiViewportFlags_);
    export_enum(ImGuiComboFlags_);
    export_enum(ImGuiTreeNodeFlags_);
    export_enum(ImGuiStyleVar_);
    export_enum(ImGuiButtonFlags_);
    // export_enum(ImGuiColorEditFlags_);

    scm_c_define_gsubr("ImFontAtlasAddFontFromFileTTF", 5, 0, 0, (scm_t_subr)im::ImFontAtlasAddFontFromFileTTF);
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

    scm_c_define_gsubr("begin-table", 2, 0, 0, (scm_t_subr)im::BeginTable);
    scm_c_define_gsubr("end-table", 0, 0, 0, (scm_t_subr)im::EndTable);
    scm_c_define_gsubr("table-next-row", 0, 2, 0, (scm_t_subr)im::TableNextRow);
    scm_c_define_gsubr("table-next-column", 0, 2, 0, (scm_t_subr)im::TableNextColumn);
    scm_c_define_gsubr("set-table-column-index!",1,0,0,(scm_t_subr)im::TableSetColumnIndex);
    scm_c_define_gsubr("get-table-column-index",0,0,0,(scm_t_subr)im::TableGetColumnIndex);
    scm_c_define_gsubr("table-setup-column",1,3,0,(scm_t_subr)im::TableSetupColumn);
    scm_c_define_gsubr("table-headers-row",0,0,0,(scm_t_subr)im::TableHeadersRow);
    scm_c_define_gsubr("table-header",1,0,0,(scm_t_subr)im::TableHeader);

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
    scm_c_define_gsubr("color-edit3", 2, 1, 0, (scm_t_subr)im::ColorEdit3);
    scm_c_define_gsubr("color-edit4", 2, 1, 0, (scm_t_subr)im::ColorEdit4);
    scm_c_define_gsubr("color-picker3", 2, 1, 0, (scm_t_subr)im::ColorPicker3);
    scm_c_define_gsubr("color-picker4", 2, 1, 0, (scm_t_subr)im::ColorPicker4);
    scm_c_define_gsubr("sameline", 0, 2, 0, (scm_t_subr)im::SameLine);
    scm_c_define_gsubr("slider-int", 4, 1, 0, (scm_t_subr)im::SliderInt);
    scm_c_define_gsubr("drag-int", 2, 4, 0, (scm_t_subr)im::DragInt);
    scm_c_define_gsubr("open-popup", 1, 0, 0, (scm_t_subr)im::OpenPopup);
    scm_c_define_gsubr("begin-popup", 1, 0, 0, (scm_t_subr)im::BeginPopup);
    scm_c_define_gsubr("begin-popup-modal", 2, 0, 0, (scm_t_subr)im::BeginPopupModal);
    scm_c_define_gsubr("end-popup", 0, 0, 0, (scm_t_subr)im::EndPopup);

    scm_c_define_gsubr("default-focus", 0, 0, 0, (scm_t_subr)im::SetItemDefaultFocus);
    scm_c_define_gsubr("keyboard-focus-here!", 1, 0, 0, (scm_t_subr)im::SetKeyboardFocusHere);
    scm_c_define_gsubr("get-font-size", 0, 0, 0, (scm_t_subr)im::GetFontSize);
    scm_c_define_gsubr("separator", 0, 0, 0, (scm_t_subr)im::Separator);
    scm_c_define_gsubr("spacing", 0, 0, 0, (scm_t_subr)im::Spacing);
    scm_c_define_gsubr("%input-text", 3, 0, 0, (scm_t_subr)im::InputText);
    scm_c_define_gsubr("input-text-with-hint", 4, 0, 0, (scm_t_subr)im::InputTextWithHint);
    scm_c_define_gsubr("newline", 0, 0, 0, (scm_t_subr)im::NewLine);
    scm_c_define_gsubr("bullet", 0, 0, 0, (scm_t_subr)im::Bullet);
    scm_c_define_gsubr("button", 1, 0, 0, (scm_t_subr)im::Button);
    scm_c_define_gsubr("small-button", 1, 0, 0, (scm_t_subr)im::SmallButton);
    guile::define("invisible-button", 2,
               (scm_t_subr)im::InvisibleButton,
               "flexible button behavior without the visuals, frequently "
               "useful to build custom behaviors using the public api (along "
               "with IsItemActive, IsItemHovered, etc.)");
    scm_c_define_gsubr("selectable", 2, 0, 0, (scm_t_subr)im::Selectable);
    scm_c_define_gsubr("%create-context", 0, 0, 0,
                       (scm_t_subr)im::create_context);
    scm_c_define_gsubr("%destroy-context", 0, 1, 0,
                       (scm_t_subr)im::destroy_context);
    scm_c_define_gsubr("get-io", 0, 0, 0, (scm_t_subr)im::getio);
    scm_c_define_gsubr("get-style", 0, 0, 0, (scm_t_subr)im::GetStyle);
    scm_c_define_gsubr("style-scale-all-sizes", 2, 0, 0, (scm_t_subr)im::ScaleAllSizes);
    scm_c_define_gsubr("show-font-selector", 1, 0, 0, (scm_t_subr)im::ShowFontSelector);
    scm_c_define_gsubr("show-style-selector", 1, 0, 0, (scm_t_subr)im::ShowStyleSelector);
    scm_c_define_gsubr("get-version", 0, 0, 0, (scm_t_subr)im::GetVersion);
    scm_c_define_gsubr("setup-font", 1, 0, 0, (scm_t_subr)im::setup_font);
    scm_c_define_gsubr("set-io-display-size", 3, 0, 0,
                       (scm_t_subr)im::set_io_display_size);
    scm_c_define_gsubr("io-display-size", 1, 0, 0,
                       (scm_t_subr)im::io_display_size);
    scm_c_define_gsubr("io-fonts", 1, 0, 0,
                       (scm_t_subr)im::io_fonts);
    scm_c_define_gsubr("io-fonts-get-texdata-as-rgba32", 4, 0, 0,
                       (scm_t_subr)im::GetTexDataAsRGBA32);
    scm_c_define_gsubr("PopStyleColor", 0, 1, 0, (scm_t_subr)im::PopStyleColor);
    scm_c_define_gsubr("PushStyleColor", 2, 0, 0,
                       (scm_t_subr)im::PushStyleColor);
    scm_c_define_gsubr("PopStyleVar", 0, 1, 0, (scm_t_subr)im::PopStyleVar);
    scm_c_define_gsubr("PushStyleVar", 2, 0, 0, (scm_t_subr)im::PushStyleVar);

    scm_c_define_gsubr("align-text-to-frame-padding", 0, 0, 0,
                       (scm_t_subr)im::AlignTextToFramePadding);

    scm_c_define_gsubr("get-text-line-height", 0, 0, 0,
                       (scm_t_subr)im::GetTextLineHeight);

    scm_c_define_gsubr("get-text-line-height-with-spacing", 0, 0, 0,
                       (scm_t_subr)im::GetTextLineHeightWithSpacing);

    scm_c_define_gsubr("get-frame-height", 0, 0, 0,
                       (scm_t_subr)im::GetFrameHeight);

    scm_c_define_gsubr("get-frame-height-with-spacing", 0, 0, 0,
                       (scm_t_subr)im::GetFrameHeightWithSpacing);
    scm_c_define_gsubr("get-time", 0, 0, 0,
                       (scm_t_subr)im::GetTime);

    scm_c_define_gsubr("tree-node", 1, 1, 0, (scm_t_subr)im::TreeNode);
    scm_c_define_gsubr("tree-push", 1, 0, 0, (scm_t_subr)im::TreePush);
    scm_c_define_gsubr("tree-pop", 1, 0, 0, (scm_t_subr)im::TreePop);
    scm_c_define_gsubr("collapsing-header", 1, 1, 0,
                       (scm_t_subr)im::CollapsingHeader);
    guile::define("window-appearing?", (scm_t_subr)im::IsWindowAppearing);
    guile::define("window-collapsed?", (scm_t_subr)im::IsWindowCollapsed);
    guile::define("window-focused?",0,1, (scm_t_subr)im::IsWindowFocused);
    guile::define("window-hovered?", 0, 1, (scm_t_subr)im::IsWindowHovered);

}
}

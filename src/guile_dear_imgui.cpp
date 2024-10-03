#include <algorithm>
#include <climits>
#include <magic_enum.hpp>
#include <cstdint>
#include <exception>
#include <guile.hpp>
#include <imgui.h>
#include <imgui_internal.h>
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
  value GetCurrentContext() {
    auto ctx=ImGui::GetCurrentContext();
    return (ctx) ? scm_from_pointer(ctx, nullptr) : SCM_BOOL_F;
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

  value Separator(value label) {
    if (label.boundp())
      ImGui::SeparatorText(LABEL(label));
    else
      ImGui::Separator();
    return SCM_UNSPECIFIED;
  }
  value Spacing() {
    ImGui::Spacing();
    return SCM_UNSPECIFIED;
  }
  struct InputTextCallback_UserData
  {
    std::string*            Str;
    ImGuiInputTextCallback  ChainCallback;
    void*                   ChainCallbackUserData;
  };

  static int InputTextCallback(ImGuiInputTextCallbackData* data)
  {
    InputTextCallback_UserData* user_data = (InputTextCallback_UserData*)data->UserData;
    if (data->EventFlag == ImGuiInputTextFlags_CallbackResize)
      {
        // Resize string callback
        // If for some reason we refuse the new length (BufTextLen) and/or capacity (BufSize) we need to set them back to what we want.
        std::string* str = user_data->Str;
        IM_ASSERT(data->Buf == str->c_str());
        str->resize(data->BufTextLen);
        data->Buf = (char*)str->c_str();
    } else if (user_data->ChainCallback) {
        // Forward to user callback, if any
        data->UserData = user_data->ChainCallbackUserData;
        return user_data->ChainCallback(data);
      }
    return 0;
  }

  static int InputTextSCMCallback(ImGuiInputTextCallbackData* data)
  {
    value scm_callback = (SCM)data->UserData;
    return scm_to_int(scm_callback(scm_from_utf8_keyword("flags"), value(data->Flags),
                                   scm_from_utf8_keyword("cursor-position"), value(data->CursorPos)));
  }

  value InputTextEx(value label, value str,value hint, value sflags ,value multiline_p,value callback)
  {
    ImGuiInputTextFlags flags=sflags;
    bool multiline= scm_is_true(multiline_p.get());
    IM_ASSERT((flags & ImGuiInputTextFlags_CallbackResize) == 0);
    flags |= ImGuiInputTextFlags_CallbackResize;
    if (multiline)
      flags |= ImGuiInputTextFlags_Multiline;
    std::string cpp_str=str();
    InputTextCallback_UserData cb_user_data;
    cb_user_data.Str = &cpp_str;
    if (callback.is_procedure_p()) {
      cb_user_data.ChainCallback = InputTextSCMCallback;
      cb_user_data.ChainCallbackUserData = callback.get();
    }
    auto ret = ImGui::InputTextEx(
        LABEL(label), hint.string_p() ? LABEL(hint) : nullptr,
        (char *)cpp_str.c_str(), cpp_str.capacity() + 1,
        multiline ? ImVec2(multiline_p[0], multiline_p[1]) : ImVec2(0, 0),
        flags, InputTextCallback, &cb_user_data);
    str(cpp_str);
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
  value LabelText(value label,value str) {
    ImGui::LabelText(LABEL(label), "%s", LABEL(str));
    return SCM_UNSPECIFIED;
  }
  value TextColored(value col, value str) {
    ImGui::TextColored(ImVec4(col[0], col[1], col[2], col[3]), "%s",
                       LABEL(str));
    return SCM_UNSPECIFIED;
  }
  value Image(value image, value x, value y) {
    auto id=scm_to_unsigned_integer(image,0,10000);
    ImGui::Image((void*)(intptr_t)id,ImVec2(x,y));
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
  value ShowMetricsWindow() {
      ImGui::ShowMetricsWindow();
    return SCM_UNSPECIFIED;
  }
  value ShowDemoWindow() {
      ImGui::ShowDemoWindow();
    return SCM_UNSPECIFIED;
  }
  value ShowStyleEditor() {
    ImGui::ShowStyleEditor();
    return SCM_UNSPECIFIED;
  }
  value ShowUserGuide() {
    ImGui::ShowUserGuide();
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
  value dummy(value size) {
    ImGui::Dummy(ImVec2(size[0],size[1]));
    return SCM_UNSPECIFIED;
  }

  value CalcTextSize(value text){
    auto c=ImGui::CalcTextSize(LABEL(text));
    return guile::list(c.x,c.y);
  }
  value CalcItemWidth(){
    return ImGui::CalcItemWidth();
  }
  value Selectable(value label, value selected) {
    bool selectedp=false;
    if (selected.is_procedure_p()) {
      selectedp = selected();
      auto ret = ImGui::Selectable(LABEL(label), &selectedp);
      selected(selectedp);
      return value(ret);
    }
    return ImGui::Selectable(LABEL(label), selected);
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

  value GetScrollX() { return ImGui ::GetScrollX(); };
  value GetScrollY() { return ImGui ::GetScrollY(); };
  value GetScrollMaxX() { return ImGui ::GetScrollMaxX(); };
  value GetScrollMaxY() { return ImGui ::GetScrollMaxY(); };
  value SetScrollHereX() {
    ImGui::SetScrollHereX();
    return SCM_UNSPECIFIED;
  }
  value SetScrollHereY() {
    ImGui::SetScrollHereY();
    return SCM_UNSPECIFIED;
  }


  value GetCursorStartPos() {
    auto v = ImGui::GetCursorStartPos();
    return guile::list(v.x,v.y);
  }
  value GetCursorScreenPos() {
    auto v = ImGui::GetCursorScreenPos();
    return guile::list(v.x,v.y);
  };
  value GetContentRegionAvail() {
    auto v = ImGui::GetContentRegionAvail();
    return guile::list(v.x,v.y);
  };
  value GetCursorPos() {
    auto v = ImGui::GetCursorPos();
    return guile::list(v.x,v.y);
  };
  value SetCursorScreenPos(value x,value y) {
    auto v = ImVec2(x,y);
    ImGui::SetCursorScreenPos(v);
    return SCM_UNSPECIFIED;
  };
  value SetCursorPos(value x,value y) {
    auto v = ImVec2(x,y);
    ImGui::SetCursorPos(v);
    return SCM_UNSPECIFIED;
  };

  value IsKeyDown(value key) {
    auto v = ImGuiKey(scm_to_int(key));
    return ImGui::IsKeyDown(v);
  };
  value IsKeyPressed(value key, value repeat) {
    int flags = (scm_to_bool(repeat)) ? ImGuiInputFlags_Repeat : ImGuiInputFlags_None;
    auto v = ImGuiKey(scm_to_int(key));
    return ImGui::IsKeyPressed(v,flags);
  };
  value IsKeyReleased(value key) {
    auto v = ImGuiKey(scm_to_int(key));
    return ImGui::IsKeyReleased(v);
  };
  value GetKeyName(value key) {
    auto v = ImGuiKey(scm_to_int(key));
    return ImGui::GetKeyName(v);
  };

  value SetItemKeyOwner(value key) {
    ImGuiKey k=ImGuiKey(scm_to_int(key));
    ImGui::SetItemKeyOwner(k);
    return SCM_UNSPECIFIED;
  }

  value IsMouseDown(value key) {
    return ImGui::IsMouseDown(key);
  };
  value IsMouseClicked(value button, value repeat) {
    int flags = (scm_to_bool(repeat)) ? ImGuiInputFlags_Repeat : ImGuiInputFlags_None;
    return ImGui::IsMouseClicked(button,flags);
  };
  value IsMouseReleased(value button) {
    return ImGui::IsMouseReleased(button);
  };
  value IsMouseDoubleClicked(value button) {
    return ImGui::IsMouseDoubleClicked(button);
  };
  value SetNextItemShortcut(value key_chord,value flags) {
    maybe_set(flags, 0);
    ImGui::SetNextItemShortcut(key_chord, flags);
    return SCM_UNSPECIFIED;
  }
  value Shortcut(value key_chord,value flags) {
    maybe_set(flags, 0);
    return ImGui::Shortcut(key_chord, flags);;
  }
  value IsItemHovered(value flags) {
    maybe_set(flags, 0);
    return ImGui::IsItemHovered(flags);
  };
  value IsItemActive() { return ImGui::IsItemActive(); };
  value IsItemFocused() { return ImGui::IsItemFocused(); };
  value IsItemClicked(value flags) {
    maybe_set(flags, 0);
    return ImGui::IsItemClicked(flags);
  };
  value IsItemVisible() { return ImGui::IsItemVisible(); };
  value IsItemEdited() { return ImGui::IsItemEdited(); };
  value IsItemActivated() { return ImGui::IsItemActivated(); };
  value IsItemDeactivated() { return ImGui::IsItemDeactivated(); };
  value IsItemDeactivatedAfterEdit() {
    return ImGui::IsItemDeactivatedAfterEdit();
  };

  value IsItemToggledOpen() { return ImGui::IsItemToggledOpen(); };
  value IsAnyItemHovered() { return ImGui::IsAnyItemHovered(); };
  value IsAnyItemActive() { return ImGui::IsAnyItemActive(); };
  value IsAnyItemFocused() { return ImGui::IsAnyItemFocused(); };
  value GetItemID() { return ImGui::GetItemID(); };
  value GetItemRectMin() {
    auto v = ImGui::GetItemRectMin();
    return guile::list(v.x,v.y);
  };

  value GetItemRectMax() {
    auto v = ImGui::GetItemRectMax();
    return guile::list(v.x,v.y);
  };

  value GetItemRectSize() {
    auto v = ImGui::GetItemRectSize();
    return guile::list(v.x,v.y);
  };
  value GetMainViewport() {
    return scm_from_pointer(ImGui::GetMainViewport(),nullptr);
  }
  value listClipper() {
    auto v = new ImGuiListClipper();
    return scm_from_pointer(v, [](void *v) {
      ImGuiListClipper *c = static_cast<ImGuiListClipper *>(v);
      delete c;
    });
  }
  value clipper_begin(value clipper,value count,value item_height) {
    scm_gc_protect_object(clipper);
    maybe_set(item_height, -1.0f);
    ImGuiListClipper *c =
        static_cast<ImGuiListClipper *>(scm_to_pointer(clipper));
    c->Begin(count, item_height);
    scm_gc_unprotect_object(clipper);
    return SCM_UNSPECIFIED;
  }
    value clipper_step(value clipper) {
    scm_gc_protect_object(clipper);
    ImGuiListClipper *c =
        static_cast<ImGuiListClipper *>(scm_to_pointer(clipper));
    auto ret=c->Step();
    scm_gc_unprotect_object(clipper);
    return ret;
    }
  value clipper_display_start(value clipper) {
    scm_gc_protect_object(clipper);
    ImGuiListClipper *c =
        static_cast<ImGuiListClipper *>(scm_to_pointer(clipper));
    auto ret=c->DisplayStart;
    scm_gc_unprotect_object(clipper);
    return ret;
  }
  value clipper_display_end(value clipper) {
    scm_gc_protect_object(clipper);
    ImGuiListClipper *c =
        static_cast<ImGuiListClipper *>(scm_to_pointer(clipper));
    auto ret=c->DisplayEnd;
    scm_gc_unprotect_object(clipper);
    return ret;
  }

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
  void init_enums() {
    defconst(ImGuiKey_0);
    defconst(ImGuiKey_A);
    defconst(ImGuiKey_Apostrophe);
    defconst(ImGuiKey_AppBack);
    defconst(ImGuiKey_AppForward);
    defconst(ImGuiKey_Backslash);
    defconst(ImGuiKey_Backspace);
    defconst(ImGuiKey_CapsLock);
    defconst(ImGuiKey_Comma);
    defconst(ImGuiKey_Delete);
    defconst(ImGuiKey_DownArrow);
    defconst(ImGuiKey_End);
    defconst(ImGuiKey_Enter);
    defconst(ImGuiKey_Equal);
    defconst(ImGuiKey_Escape);
    defconst(ImGuiKey_F1);
    defconst(ImGuiKey_F13);
    defconst(ImGuiKey_F19);
    defconst(ImGuiKey_F7);
    defconst(ImGuiKey_GamepadBack);
    defconst(ImGuiKey_GamepadDpadDown);
    defconst(ImGuiKey_GamepadDpadLeft);
    defconst(ImGuiKey_GamepadDpadRight);
    defconst(ImGuiKey_GamepadDpadUp);
    defconst(ImGuiKey_GamepadFaceDown);
    defconst(ImGuiKey_GamepadFaceLeft);
    defconst(ImGuiKey_GamepadFaceRight);
    defconst(ImGuiKey_GamepadFaceUp);
    defconst(ImGuiKey_GamepadL1);
    defconst(ImGuiKey_GamepadL2);
    defconst(ImGuiKey_GamepadL3);
    defconst(ImGuiKey_GamepadLStickDown);
    defconst(ImGuiKey_GamepadLStickLeft);
    defconst(ImGuiKey_GamepadLStickRight);
    defconst(ImGuiKey_GamepadLStickUp);
    defconst(ImGuiKey_GamepadR1);
    defconst(ImGuiKey_GamepadR2);
    defconst(ImGuiKey_GamepadR3);
    defconst(ImGuiKey_GamepadRStickDown);
    defconst(ImGuiKey_GamepadRStickLeft);
    defconst(ImGuiKey_GamepadRStickRight);
    defconst(ImGuiKey_GamepadRStickUp);
    defconst(ImGuiKey_GamepadStart);
    defconst(ImGuiKey_GraveAccent);
    defconst(ImGuiKey_Home);
    defconst(ImGuiKey_Insert);
    defconst(ImGuiKey_K);
    defconst(ImGuiKey_Keypad0);
    defconst(ImGuiKey_Keypad5);
    defconst(ImGuiKey_KeypadAdd);
    defconst(ImGuiKey_KeypadDecimal);
    defconst(ImGuiKey_KeypadDivide);
    defconst(ImGuiKey_KeypadEnter);
    defconst(ImGuiKey_KeypadEqual);
    defconst(ImGuiKey_KeypadMultiply);
    defconst(ImGuiKey_KeypadSubtract);
    defconst(ImGuiKey_LeftArrow);
    defconst(ImGuiKey_LeftBracket);
    defconst(ImGuiKey_LeftCtrl);
    defconst(ImGuiKey_Menu);
    defconst(ImGuiKey_Minus);
    defconst(ImGuiKey_MouseLeft);
    defconst(ImGuiKey_NumLock);
    defconst(ImGuiKey_PageDown);
    defconst(ImGuiKey_PageUp);
    defconst(ImGuiKey_Pause);
    defconst(ImGuiKey_Period);
    defconst(ImGuiKey_PrintScreen);
    defconst(ImGuiKey_RightArrow);
    defconst(ImGuiKey_RightBracket);
    defconst(ImGuiKey_RightCtrl);
    defconst(ImGuiKey_ScrollLock);
    defconst(ImGuiKey_Semicolon);
    defconst(ImGuiKey_Slash);
    defconst(ImGuiKey_Space);
    defconst(ImGuiKey_Tab);
    defconst(ImGuiKey_U);
    defconst(ImGuiKey_UpArrow);
    defconst(ImGuiKey_1);
    defconst(ImGuiKey_2);
    defconst(ImGuiKey_3);
    defconst(ImGuiKey_4);
    defconst(ImGuiKey_5);
    defconst(ImGuiKey_6);
    defconst(ImGuiKey_7);
    defconst(ImGuiKey_8);
    defconst(ImGuiKey_9);
    defconst(ImGuiKey_B);
    defconst(ImGuiKey_C);
    defconst(ImGuiKey_D);
    defconst(ImGuiKey_E);
    defconst(ImGuiKey_F);
    defconst(ImGuiKey_F10);
    defconst(ImGuiKey_F11);
    defconst(ImGuiKey_F12);
    defconst(ImGuiKey_F14);
    defconst(ImGuiKey_F15);
    defconst(ImGuiKey_F16);
    defconst(ImGuiKey_F17);
    defconst(ImGuiKey_F18);
    defconst(ImGuiKey_F2);
    defconst(ImGuiKey_F20);
    defconst(ImGuiKey_F21);
    defconst(ImGuiKey_F22);
    defconst(ImGuiKey_F23);
    defconst(ImGuiKey_F24);
    defconst(ImGuiKey_F3);
    defconst(ImGuiKey_F4);
    defconst(ImGuiKey_F5);
    defconst(ImGuiKey_F6);
    defconst(ImGuiKey_F8);
    defconst(ImGuiKey_F9);
    defconst(ImGuiKey_G);
    defconst(ImGuiKey_H);
    defconst(ImGuiKey_I);
    defconst(ImGuiKey_J);
    defconst(ImGuiKey_Keypad1);
    defconst(ImGuiKey_Keypad2);
    defconst(ImGuiKey_Keypad3);
    defconst(ImGuiKey_Keypad4);
    defconst(ImGuiKey_Keypad6);
    defconst(ImGuiKey_Keypad7);
    defconst(ImGuiKey_Keypad8);
    defconst(ImGuiKey_Keypad9);
    defconst(ImGuiKey_L);
    defconst(ImGuiKey_LeftAlt);
    defconst(ImGuiKey_LeftShift);
    defconst(ImGuiKey_LeftSuper);
    defconst(ImGuiKey_M);
    defconst(ImGuiKey_MouseMiddle);
    defconst(ImGuiKey_MouseRight);
    defconst(ImGuiKey_MouseWheelX);
    defconst(ImGuiKey_MouseWheelY);
    defconst(ImGuiKey_MouseX1);
    defconst(ImGuiKey_MouseX2);
    defconst(ImGuiKey_N);
    defconst(ImGuiKey_O);
    defconst(ImGuiKey_P);
    defconst(ImGuiKey_Q);
    defconst(ImGuiKey_R);
    defconst(ImGuiKey_RightAlt);
    defconst(ImGuiKey_RightShift);
    defconst(ImGuiKey_RightSuper);
    defconst(ImGuiKey_S);
    defconst(ImGuiKey_T);
    defconst(ImGuiKey_V);
    defconst(ImGuiKey_W);
    defconst(ImGuiKey_X);
    defconst(ImGuiKey_Y);
    defconst(ImGuiKey_Z);
    export_enum(ImGuiWindowFlags_);
    export_enum(ImGuiConfigFlags_);
    export_enum(ImGuiViewportFlags_);
    export_enum(ImGuiComboFlags_);
    export_enum(ImGuiTreeNodeFlags_);
    export_enum(ImGuiStyleVar_);
    export_enum(ImGuiButtonFlags_);
  }
  void init_imgui() {
    IMGUI_CHECKVERSION();
    ImGui::SetAllocatorFunctions([](size_t sz,[[maybe_unused]] void* user_data){
      return scm_gc_malloc(sz,"imgui");
    },
      [](void* ptr,[[maybe_unused]] void* user_data){
        scm_gc_free(ptr,0,"imgui:free");
      });

    init_enums();

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
    scm_c_define_gsubr("render", 0, 0, 0, (scm_t_subr)im::Render);
    scm_c_define_gsubr("new-frame", 0, 0, 0, (scm_t_subr)im::new_frame);
    scm_c_define_gsubr("text", 1, 0, 0, (scm_t_subr)im::text);
    scm_c_define_gsubr("label-text", 2, 0, 0, (scm_t_subr)im::LabelText);
    scm_c_define_gsubr("text-colored", 2, 0, 0, (scm_t_subr)im::TextColored);
    scm_c_define_gsubr("%image", 3, 0, 0, (scm_t_subr)im::Image);
    scm_c_define_gsubr("indent", 0, 1, 0, (scm_t_subr)im::Indent);
    scm_c_define_gsubr("unindent", 0, 1, 0, (scm_t_subr)im::Unindent);
    guile::define("show-metrics-window", (scm_t_subr)im::ShowMetricsWindow);
    guile::define("show-demo-window", (scm_t_subr)im::ShowDemoWindow);
    guile::define("show-style-editor", (scm_t_subr)im::ShowStyleEditor);
    guile::define("show-user-guide", (scm_t_subr)im::ShowUserGuide);
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
    guile::define("dummy", 1, (scm_t_subr)im::dummy);
    scm_c_define_gsubr("slider-int", 4, 1, 0, (scm_t_subr)im::SliderInt);
    scm_c_define_gsubr("drag-int", 2, 4, 0, (scm_t_subr)im::DragInt);
    scm_c_define_gsubr("open-popup", 1, 0, 0, (scm_t_subr)im::OpenPopup);
    scm_c_define_gsubr("begin-popup", 1, 0, 0, (scm_t_subr)im::BeginPopup);
    scm_c_define_gsubr("begin-popup-modal", 2, 0, 0, (scm_t_subr)im::BeginPopupModal);
    scm_c_define_gsubr("end-popup", 0, 0, 0, (scm_t_subr)im::EndPopup);

    scm_c_define_gsubr("default-focus", 0, 0, 0, (scm_t_subr)im::SetItemDefaultFocus);
    scm_c_define_gsubr("keyboard-focus-here!", 1, 0, 0, (scm_t_subr)im::SetKeyboardFocusHere);
    scm_c_define_gsubr("get-font-size", 0, 0, 0, (scm_t_subr)im::GetFontSize);
    guile::define("separator", 0, 1, (scm_t_subr)im::Separator);
    scm_c_define_gsubr("spacing", 0, 0, 0, (scm_t_subr)im::Spacing);
    scm_c_define_gsubr("%inputex", 6, 0, 0, (scm_t_subr)im::InputTextEx);
    scm_c_define_gsubr("newline", 0, 0, 0, (scm_t_subr)im::NewLine);
    scm_c_define_gsubr("bullet", 0, 0, 0, (scm_t_subr)im::Bullet);
    scm_c_define_gsubr("button", 1, 0, 0, (scm_t_subr)im::Button);
    scm_c_define_gsubr("small-button", 1, 0, 0, (scm_t_subr)im::SmallButton);
    guile::define("invisible-button", 2,
               (scm_t_subr)im::InvisibleButton,
               "flexible button behavior without the visuals, frequently "
               "useful to build custom behaviors using the public api (along "
               "with IsItemActive, IsItemHovered, etc.)");
    guile::define("selectable", 2,(scm_t_subr)im::Selectable);
    scm_c_define_gsubr("%create-context", 0, 0, 0,
                       (scm_t_subr)im::create_context);
    scm_c_define_gsubr("%destroy-context", 0, 1, 0,
                       (scm_t_subr)im::destroy_context);
    guile::define("%current-context", (scm_t_subr)im::GetCurrentContext);
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

    guile::define("scroll-x", 0, 0, (scm_t_subr)im::GetScrollX);
    guile::define("scroll-y", 0, 0, (scm_t_subr)im::GetScrollY);
    guile::define("scroll-max-x", 0, 0, (scm_t_subr)im::GetScrollMaxX);
    guile::define("scroll-max-y", 0, 0, (scm_t_subr)im::GetScrollMaxY);
    guile::define("set-scroll-x!", 0, 0, (scm_t_subr)im::SetScrollHereX);
    guile::define("set-scroll-y!", 0, 0, (scm_t_subr)im::SetScrollHereY);

    guile::define("cursor-start-position", 0, 0,
                  (scm_t_subr)im::GetCursorStartPos);
    guile::define("%cursor-screen-position", 0, 0,
                  (scm_t_subr)im::GetCursorScreenPos);
    guile::define("content-regin-avail", 0, 0,
                  (scm_t_subr)im::GetContentRegionAvail);
    guile::define("%cursor-position", 0, 0, (scm_t_subr)im::GetCursorPos);
    guile::define("%set-cursor-screen-position!", 2, 0,
                  (scm_t_subr)im::SetCursorScreenPos);
    guile::define("%set-cursor-position!", 2, 0, (scm_t_subr)im::SetCursorPos);
    guile::define("calc-text-size", 1, 0, (scm_t_subr)im::CalcTextSize);
    guile::define("calc-item-width", (scm_t_subr)im::CalcItemWidth);
    guile::define("list-clipper", (scm_t_subr)im::listClipper);
    guile::define("list-clipper-begin", 2, 1, (scm_t_subr)im::clipper_begin);
    guile::define("list-clipper-step", 1, (scm_t_subr)im::clipper_step);
    guile::define("list-clipper-display-start", 1,
                  (scm_t_subr)im::clipper_display_start);
    guile::define("list-clipper-display-end", 1,
                  (scm_t_subr)im::clipper_display_end);
  }
  void init_imgui_inputs() {
    guile::define("%key-down?", 1, (scm_t_subr)im::IsKeyDown);
    guile::define("%key-pressed?", 1, 1, (scm_t_subr)im::IsKeyPressed);
    guile::define("%key-released?", 1, (scm_t_subr)im::IsKeyReleased);
    guile::define("%key-name", 1, (scm_t_subr)im::GetKeyName);
    guile::define("set-item-key-owner!", 1, (scm_t_subr)im::SetItemKeyOwner);

    guile::define("%mouse-down?", 1, (scm_t_subr)im::IsMouseDown);
    guile::define("%mouse-released?", 1, (scm_t_subr)im::IsMouseReleased);
    guile::define("%mouse-clicked?", 1, 1, (scm_t_subr)im::IsMouseClicked);
    guile::define("%mouse-double-clicked?", 1,
                  (scm_t_subr)im::IsMouseDoubleClicked);
    guile::define("set-next-item-shortcut!", 1, 1,
                  (scm_t_subr)im::SetNextItemShortcut);
    guile::define("shortcut", 1, 1,
                  (scm_t_subr)im::Shortcut);
  }
  void init_imgui_window() {
    guile::define("%set-next-window-size!", 2, (scm_t_subr)im::SetNextWindowSize);
    guile::define("set-next-window-position!", 5,(scm_t_subr)im::SetNextWindowPos);

    guile::define("window-appearing?", (scm_t_subr)im::IsWindowAppearing);
    guile::define("window-collapsed?", (scm_t_subr)im::IsWindowCollapsed);
    guile::define("window-focused?",0,1, (scm_t_subr)im::IsWindowFocused);
    guile::define("window-hovered?", 0, 1, (scm_t_subr)im::IsWindowHovered);
  }
  void init_imgui_item() {
    guile::define("item-hovered?", 0, 1, (scm_t_subr)im::IsItemHovered);
    guile::define("item-active?", 0, (scm_t_subr)im::IsItemActive);
    guile::define("item-focused?", 0, (scm_t_subr)im::IsItemFocused);
    guile::define("item-clicked?", 0, 1, (scm_t_subr)im::IsItemClicked);
    guile::define("item-visible?", 0, (scm_t_subr)im::IsItemVisible);
    guile::define("item-edited?", 0,  (scm_t_subr)im::IsItemEdited);
    guile::define("item-activated?", 0,  (scm_t_subr)im::IsItemActivated);
    guile::define("item-deactivated?", 0,  (scm_t_subr)im::IsItemDeactivated);
    guile::define("item-deactivated-after-edit?", 0,
                  (scm_t_subr)im::IsItemDeactivatedAfterEdit);
    guile::define("item-toggle-open?", 0,  (scm_t_subr)im::IsItemToggledOpen);
    guile::define("any-item-hovered?", 0,  (scm_t_subr)im::IsAnyItemActive);
    guile::define("any-item-active?", 0, (scm_t_subr)im::IsAnyItemActive);
    guile::define("any-item-focused?", 0, (scm_t_subr)im::IsAnyItemFocused);
    guile::define("item-id", 0, (scm_t_subr)im::GetItemID);
    guile::define("item-rect-min", 0, (scm_t_subr)im::GetItemRectMin);
    guile::define("item-rect-max", 0, (scm_t_subr)im::GetItemRectMax);
    guile::define("item-rect-size", 0, (scm_t_subr)im::GetItemRectSize);
  }
  void init_imgui_viewport() {
        guile::define("%get-main-viewport", (scm_t_subr)im::GetMainViewport);
  }
}

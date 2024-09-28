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
#include <libguile.h>
#include <cstdint>
#include <string>
#include <iostream>

#if defined(DEBUG)
#define defconst(v)                                                            \
  std::cout << "define " #v;                                                   \
  scm_c_define(#v, scm_from_int(v));                                           \
  scm_c_export(#v);                                                            \
  std::cout << " done" << std::endl
#else
#define defconst(v) scm_c_define(#v,scm_from_int(v)); scm_c_export(#v)
#endif
#define maybe_set(v, val)                                                      \
  if (v.unboundp()) {                                                          \
    v = val;                                                                   \
  }
#define LABEL(v) std::string(v).c_str()
namespace guile {
  class value {
  public:
    value() = default;
    ~value() = default;
    value(SCM o) : value_{o} { };
    value(int arg) { value_ = scm_from_int(arg); };
    /* value(int32_t arg) { value_ = scm_from_int32(arg); }; */
    value(int16_t arg) { value_ = scm_from_int16(arg); };
    value(size_t arg) { value_ = scm_from_size_t(arg); };
    value(int8_t arg) { value_ = scm_from_int8(arg); };
    value(uint32_t arg) { value_ = scm_from_uint32(arg); };
    value(uint16_t arg) { value_ = scm_from_uint16(arg); };
    value(uint8_t arg) { value_ = scm_from_uint8(arg); };
    value(bool arg) { value_ = scm_from_bool(arg); };
    value(float arg) { value_ = scm_from_double(arg); };
    value(double arg) { value_ = scm_from_double(arg); };
    value(const char* arg) { value_ = scm_from_utf8_string(arg);};
    value(std::string str) { value_ = scm_from_utf8_string(str.c_str()); };
    SCM get() const {return value_;};
    bool unboundp() const {return SCM_UNBNDP(value_);};
    bool boundp() const { return !unboundp(); };
    bool string_p() const {return scm_to_bool(scm_string_p(value_));};
    bool is_bool_p() const {return scm_is_bool(value_);};
    value car() const {return scm_car(value_);};
    value cdr() const {return scm_cdr(value_);};
    bool is_procedure_p() const {return scm_is_true(scm_procedure_p(value_));};
    operator SCM () const {return value_;}
    operator int32_t () const {return scm_to_int32(value_);}
    operator int16_t() const { return scm_to_int16(value_); }
    operator size_t() const { return scm_to_size_t(value_); }
    operator int8_t() const { return scm_to_int8(value_); }
    operator uint32_t () const {return scm_to_uint32(value_);}
    operator uint16_t () const {return scm_to_uint16(value_);}
    operator uint8_t () const {return scm_to_uint8(value_);}
    operator bool () const {return scm_to_bool(value_);}

    operator double () const {return scm_to_double(value_);}
    operator float () const {

      return scm_to_double(value_);
    }

    operator std::string() const {
      auto c_str=scm_to_utf8_string(value_);
      auto str = std::string(c_str);
      free(c_str);
      return str;
    }
    // operator char* () const {
    //   auto c_str=scm_to_utf8_string(value_);
    //   return c_str;
    // }
    friend std::ostream &operator<<(std::ostream &os, guile::value v){
      auto c = scm_to_utf8_string(scm_simple_format(
          SCM_BOOL_F, scm_from_utf8_string("~a"), scm_list_1(v.get())));
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
    SCM value_ = SCM_UNDEFINED;
  };
  inline value cons(value a, value b) { return value{scm_cons(a, b)}; };
  inline value list(value a) { return value{scm_list_1(a)}; }
  inline value list(value a, value b) { return value{scm_list_2(a, b)}; }
  inline value list(value a, value b, value c) {
    return value{scm_list_3(a, b, c)};
  }
  inline value list(value a, value b, value c,value d) {
    return value{scm_list_4(a, b, c,d)};
  }
  inline value list(value a, value b, value c,value d,value e) {
    return value{scm_list_5(a, b, c,d,e)};
  }
  template <typename... T,
            typename = std::enable_if_t<(sizeof...(T) > 5), void>>
    inline value list(T...arg) {
    return value{scm_list_n(arg...,SCM_UNDEFINED)};
  }

} // namespace guile

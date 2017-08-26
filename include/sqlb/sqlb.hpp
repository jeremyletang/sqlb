// Copyright 2017 Jeremy Letang.
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#include <string>
#include <memory>
#include <array>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>

namespace sqlb {

  struct table_metas {
    std::string table_name;

    table_metas() = default;

    table_metas(const std::string& table_name)
      : table_name(table_name) {}
  };

  namespace {
    struct scalar_type {};
  }

  template <typename T,
            typename = std::enable_if_t<
              std::is_integral<T>::value>>
  struct integer : public scalar_type {
    T t;

    integer() = delete;
    integer(T t): t(t) {}
  };

  template <typename T,
            typename = std::enable_if_t<
              std::is_floating_point<T>::value>>
  struct real : public scalar_type {
    T t;

    real() = delete;
    real(T t): t(t) {}
  };

  struct null : public scalar_type {
    null() = default;
  };

  struct text : public scalar_type {
    std::string t;

    text() = delete;
    text(const std::string& t) : t(t) {};
    text(const char* t) : t(std::string{t}) {};
  };

  template <typename T>
  struct is_string : std::false_type {};

  template <>
  struct is_string<const char *> : std::true_type {};
  template <>
  struct is_string<char *> : std::true_type {};
  template <>
  struct is_string<std::string> : std::true_type {};

  template< class T >
  struct is_scalar : std::integral_constant<bool,
                                            is_string<T>::value ||
                                            std::is_arithmetic<T>::value     ||
                                            std::is_null_pointer<T>::value> {};

  template <typename T,
            typename = std::enable_if_t<std::is_integral<T>::value>>
  auto make_scalar(T t) -> integer<T> {
    return integer<T>{t};
  }

  template <typename T,
            typename = std::enable_if_t<std::is_floating_point<T>::value>>
  auto make_scalar(T t) -> real<T> {
    return real<T>{t};
  }

  template <typename T,
            typename = std::enable_if_t<std::is_convertible<T, text>::value>>
  auto make_scalar(T t) -> text {
    return text{t};
  }

  template <typename T,
            typename = std::enable_if_t<std::is_null_pointer<T>::value>>
  auto make_scalar(T t) -> null {
    (void)t;
    return null{};
  }

  struct table {
    virtual std::string table_name() const = 0;
  };

  struct clause {
    virtual std::string build(const table_metas& m) const = 0;
    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const = 0;
  };

  struct expr {
    virtual std::string build(const table_metas& m) const = 0;
    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const = 0;
  };

  struct stmt {
    virtual std::string build(const table_metas& m) const = 0;
    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const = 0;
  };

  inline std::string open_parent() {
    return std::string{"("};
  }

  inline std::string close_parent() {
    return std::string{")"};
  }

  template <typename L, typename R>
  struct or_expr: public expr {
    L left;
    R right;

    or_expr(L left, R right)
      : left(left),
        right(right) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << open_parent() << left.build(m) << " OR " << right.build(m) << close_parent();
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << open_parent();
      left.debug(m, ss) << " OR ";
      right.debug(m, ss) << close_parent();
      return ss;
    }

  };

  template <typename L, typename R>
  struct and_expr: public expr {

    and_expr(L left, R right)
      : left(left),
        right(right) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << open_parent() << left.build(m) << " AND " << right.build(m) << close_parent();
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << open_parent();
      left.debug(m, ss) << " AND ";
      right.debug(m, ss) << close_parent();
      return ss;
    }

  private:
    L left;
    R right;

  };

  template <typename T>
  struct eq_expr: public expr {

    eq_expr(const std::string& field, T t)
      : field(field),
        t(t) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << open_parent() << m.table_name << "." << field << " " << sym << " '?'" << close_parent();
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << open_parent() << m.table_name << "." << field << " " << sym << " '" << t << "'" << close_parent();
      return ss;
    }

  private:
    const std::string sym = "!=";
    std::string field;
    T t;

  };

  template <typename T>
  struct neq_expr: public expr {

    neq_expr(const std::string& field, T t)
      : field(field),
        t(t) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << open_parent() << m.table_name << "." << field << " " << sym << " '?'" << close_parent();
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << open_parent() << m.table_name << "." << field << " " << sym <<  " '" << t << "'" << close_parent();
      return ss;
    }

  private:
    const std::string sym = "!=";
    std::string field;
    T t;

  };


  template <typename T>
  struct between_expr: public expr {

    between_expr(const std::string& field, T t, T u)
      : field(field),
        t(t),
        u(u) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << open_parent() << m.table_name << "." << field <<
        " BETWEEN '?' AND '?'" << close_parent();
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << open_parent() << m.table_name << "." << field <<
        " BETWEEN '" << t << "' AND '" << u << "'" << close_parent();
      return ss;
    }

  private:
    const std::string sym = "!=";
    std::string field;
    T t;
    T u;

  };

  template <typename T>
  struct in_expr: public expr {
    in_expr(const std::string& field, const std::vector<T>& t)
      : field(field),
        t(t) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << open_parent() << m.table_name << "." << field << " IN (";
      auto i = t.size()-1;
      while (i > 0) {
        ss << "'?'";
        i -= 1;
        if (i > 0) { ss << ", "; }
      }
      ss << ")" << close_parent();
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << open_parent() << m.table_name << "." << field << " IN (";
      auto i = t.size()-1;
      for (const auto& v : t) {
        ss << "'" << v << "'";
        if (i > 0) { ss << ", "; i -= 1; }
      }
      ss << ")" << close_parent();
      return ss;
    }

  private:
    const std::string sym = "!=";
    std::string field;
    std::vector<T> t;

  };


  template <typename E>
  struct where_clause: public clause {
    E expr;

    where_clause(E expr)
      : expr(expr) {}

    virtual std::string build(const table_metas& m) const override {
      auto ss = std::ostringstream{};
      ss << " WHERE " << expr.build(m);
      return ss.str();
    }

    virtual std::stringstream& debug(const table_metas& m, std::stringstream& ss) const override {
      ss << " WHERE ";
      expr.debug(m, ss);
      return ss;
    }
  };


  template<class F, class...Ts, std::size_t...Is>
  void for_each(const std::tuple<Ts...> & tuple, F func, std::index_sequence<Is...>){
    using expander = int[];
    (void)expander { 0, ((void)func(std::get<Is>(tuple)), 0)... };
  }

  template<class F, class...Ts>
  void for_each(const std::tuple<Ts...> & tuple, F func){
    for_each(tuple, func, std::make_index_sequence<sizeof...(Ts)>());
  }

  template <typename... Clauses>
  struct select_stmt {
    std::string select_expr;
    std::tuple<Clauses...> clauses;

    select_stmt(const std::string& select_expr, Clauses... clauses)
      : select_expr(select_expr), clauses(clauses...) {}

    template<typename T>
    std::string build() const {
      auto metas = table_metas{T{}.table_name()};
      auto _select_expr = this->select_expr;
      if (_select_expr == "*") {
        _select_expr = metas.table_name+".*";
      }

      auto ss = std::stringstream{};
      ss << open_parent() << "SELECT " << _select_expr << " FROM " << metas.table_name;
      for_each(this->clauses, [&ss, &metas](auto t){
          ss << t.build(metas);
      });
      ss << close_parent();
      return ss.str();
    }

    template<typename T>
    std::string debug() const {
      auto metas = table_metas{T{}.table_name()};
      auto _select_expr = this->select_expr;
      if (_select_expr == "*") {
        _select_expr = metas.table_name+".*";
      }

      auto ss = std::stringstream{};
      ss << open_parent() << "SELECT " << _select_expr << " FROM " << metas.table_name;
      for_each(this->clauses, [&ss, &metas](auto t){
         t.debug(metas, ss);
      });
      ss << close_parent();
      return ss.str();
    }
  };

  template <typename T,
            typename = std::enable_if_t<is_scalar<T>::value>>
  auto eq(const std::string& field, T p) -> eq_expr<decltype(make_scalar(p))> {
    return eq_expr<decltype(make_scalar(p))>{field, make_scalar(p)};
  }

  template <typename T,
            typename = std::enable_if_t<is_scalar<T>::value>>
  auto neq(const std::string& field, T p) -> neq_expr<decltype(make_scalar(p))> {
    return neq_expr<decltype(make_scalar(p))>{field, make_scalar(p)};
  }

  template <typename T,
            typename = std::enable_if_t<is_scalar<T>::value>>
  auto between(const std::string& field, T p, T q) -> between_expr<decltype(make_scalar(p))> {
    return between_expr<decltype(make_scalar(p))>{field, make_scalar(p), make_scalar(q)};
  }

  template <template<typename, typename> class C,
            typename T,
            typename Allocator,
            typename = std::enable_if_t<is_scalar<T>::value>>
  auto in(const std::string& field, const C<T, Allocator>& p) {
  // -> in_expr<decltype(make_scalar(std::declval<T>()))> {
    std::vector<decltype(make_scalar(std::declval<T>()))> v;
    for (const auto& _v : p) {
      v.push_back(make_scalar(_v));
    }
    return in_expr<decltype(make_scalar(std::declval<T>()))>{field, v};
  }

  template <typename L,
            typename R,
            typename = std::enable_if_t<
              std::is_base_of<expr, L>::value && std::is_base_of<expr, R>::value>>
  auto or_(L left, R right) -> or_expr<L, R> {
    return or_expr<L, R>(left, right);
  }

  template <typename L,
            typename R,
            typename = std::enable_if_t<
              std::is_base_of<expr, L>::value && std::is_base_of<expr, R>::value>>
  auto and_(L left, R right) -> and_expr<L, R> {
    return and_expr<L, R>(left, right);
  }

  template <typename T>
  auto where(T expr) -> where_clause<T> {
    return where_clause<T>{expr};
  }


  template <typename... Clauses>
  auto select(const char* select_expr, Clauses... clauses) -> select_stmt<Clauses...> {
    return select_stmt<Clauses...>{std::string{select_expr}, clauses...};
  }

  template <typename... Clauses>
  auto select(Clauses... clauses) -> select_stmt<Clauses...> {
    return select("*", clauses...);
  }


  template <typename L,
            typename R,
            typename = std::enable_if_t<
              std::is_base_of<expr, L>::value && std::is_base_of<expr, R>::value>>
  auto operator||(L left, R right) -> or_expr<L, R> {
    return or_(left, right);
  }


  template <typename L,
            typename R,
            typename = std::enable_if_t<
              std::is_base_of<expr, L>::value && std::is_base_of<expr, R>::value>>
  auto operator&&(L left, R right) -> and_expr<L, R> {
    return and_(left, right);
  }

  template <typename CharT, typename Traits, typename T>
  std::basic_ostream<CharT, Traits>&
  operator<<(std::basic_ostream<CharT, Traits>&os, const real<T> r) {
    os << r.t;
    return os;
  }

  template <typename CharT, typename Traits, typename T>
  std::basic_ostream<CharT, Traits>&
  operator<<(std::basic_ostream<CharT, Traits> &os, const integer<T> i) {
    os << i.t;
    return os;
  }

  template <typename CharT, typename Traits>
  std::basic_ostream<CharT, Traits>&
  operator<<(std::basic_ostream<CharT, Traits> &os, const text t) {
    os << t.t;
    return os;
  }

  template <typename CharT, typename Traits>
  std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits> &os, const null) {
    os << "NULL";
    return os;
  }

}

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

  enum class op_kind {
    or_,
    and_,
    eq
  };

  enum class stmt_kind {
    select,
    update,
    delete_
  };

  struct table_metas {
    std::string table_name;

    table_metas() = default;

    table_metas(const std::string& table_name)
      : table_name(table_name) {}
  };

  struct table {
    virtual std::string table_name() const = 0;
  };

  struct clause {
    virtual std::string build(const table_metas& m) const = 0;
  };

  struct expr {
    virtual enum op_kind get_op_kind() const = 0;
    virtual std::string build(const table_metas& m) const = 0;
  };

  struct stmt {
    virtual stmt_kind get_stmt_kind() const = 0;
    virtual std::string build(const table_metas& m) const = 0;
  };

  void open_parent(std::ostringstream& os) {
    os << "(";
  }

  void close_parent(std::ostringstream& os) {
    os << ")";
  }

  template <typename L, typename R>
  struct or_expr: public expr {
    L left;
    R right;

    or_expr(L left, R right)
      : left(left),
        right(right) {}

    enum op_kind get_op_kind() const {
      return op_kind::or_;
    }

    std::string build(const table_metas& m) const {
      auto s = std::ostringstream{};
      open_parent(s);
      s << left.build(m);
      s << " OR ";
      s << right.build(m);
      close_parent(s);
      return s.str();
    }
  };

  template <typename L, typename R>
  struct and_expr: public expr {
    L left;
    R right;

    and_expr(L left, R right)
      : left(left),
        right(right) {}

    enum op_kind get_op_kind() const {
      return op_kind::and_;
    }

    std::string build(const table_metas& m) const {
      auto s = std::ostringstream{};
      open_parent(s);
      s << left.build(m);
      s << " AND ";
      s << right.build(m);
      close_parent(s);
      return s.str();
    }
  };

  template <typename T>
  struct eq_expr: public expr {
    eq_expr(const std::string& fmt, T t)
      : fmt(fmt),
        t(t) {}

    virtual enum op_kind get_op_kind() const override {
      return op_kind::eq;
    }

    virtual std::string build(const table_metas& m) const override {
      auto s = std::ostringstream{};
      open_parent(s);
      s << m.table_name << ".";
      s << fmt;
      s << " = '?'";
      close_parent(s);
      return s.str();
    }

  private:
    std::string fmt;
    T t;

  };


  template <typename E>
  struct where_clause: public clause {
    E expr;

    where_clause(E expr)
      : expr(expr) {}

    virtual std::string build(const table_metas& m) const {
      auto s = std::ostringstream{};
      s << " WHERE ";
      s << expr.build(m);
      return s.str();
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

      auto s = std::stringstream{};
      s << "SELECT " << _select_expr << " FROM " << metas.table_name;
      for_each(this->clauses, [&s, &metas](auto t){
          s << t.build(metas);
      });
      s << ";";
      return s.str();
    }
  };

  template <typename T>
  auto eq(const std::string& fmt, T p) -> eq_expr<T> {
    return eq_expr<T>{fmt, p};
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
}

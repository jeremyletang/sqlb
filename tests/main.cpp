// Copyright 2017 Jeremy Letang.
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#include <iostream>
#include <list>
#include <memory>
#include <string>

#include <sqlb/sqlb.hpp>
#include <sqlite3.h>

using namespace sqlb;

struct users: public sqlb::table {
  virtual std::string table_name() const override {
    static std::string name = "users";
    return name;
  }
};

int main() {
  auto q =
    select(
           where(
                 (eq("id", 42) or eq("name",  "hello world")) and
                 eq("age", 84) or
                 eq("thug", 84.42) and
                 neq("id", "hello") and
                 between("age", 42, 54) and
                 in("name", std::list<std::string>{"a", "b"}) and
                 in("is_lol", std::list<bool>{true, false}))
                 ).debug<users>();
  std::cout << q << std::endl;
  return 0;
}

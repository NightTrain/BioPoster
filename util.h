#ifndef UTIL_H
#define UTIL_H

#include <functional>
#include <string>

void exit_with_error(std::string e_msg);

std::string make_str();

template<class T, class... Ts>
std::string make_str(T x, Ts... xs);

// Adapted from <stackoverflow.com/questions/11902840/binding-member-functions-in-a-variadic-fashion>
template<class T, class R, class... Args>
std::function<R(Args...)> method_closure(T &t, R (T::*f)(Args...));

#include "util.tpp"

#endif /* UTIL_H */

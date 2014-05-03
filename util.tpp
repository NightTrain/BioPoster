#include <sstream>

template<class T, class... Ts>
std::string make_str(T x, Ts... xs)
{
	std::stringstream s;
	s << x;
	s << make_str(xs...);
	return s.str();
}

template<class T, class R, class... Args>
std::function<R(Args...)> method_closure(T &t, R (T::*f)(Args...))
{
	return [&t,f](Args... args){ std::mem_fn(f)(&t,args...); };
}

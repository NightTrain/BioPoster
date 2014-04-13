#ifndef ECOLOGY_SIMULATION_TEMPLATES_H
#define ECOLOGY_SIMULATION_TEMPLATES_H

#include <iostream>
#include <sstream>
#include <string>

std::string make_str() { return ""; }
template <class T, class... Ts> std::string make_str(T x, Ts... xs)
{
	std::stringstream s;
	s << x;
	s << make_str(xs...);
	return s.str();
}

#endif

#include "ecology_simulation.h"

template<class T>
RAII_Wrapper<T>::RAII_Wrapper(std::function<T*()> initFunc, std::function<void(T*)> df, std::string err_msg):
	destFunc(df)
{
	if (!(_ptr = initFunc()))
		throw SDL_Exception(err_msg);
}

template<class T>
RAII_Wrapper<T>::~RAII_Wrapper()
{
	destFunc(_ptr);
}

template<class T>
T *RAII_Wrapper<T>::get() const
{
	return _ptr;
}

template<int r, int g, int b, int a>
void clear_screen(SDL_Renderer *rend)
{
	draw_sdl_rectangle(rend, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT,
					   std::make_tuple(r, g, b), a);
}

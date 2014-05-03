#ifndef SDL_UTIL_H
#define SDL_UTIL_H

#include <SDL2/SDL.h>
#include <functional>
#include <map>
#include <string>
#include <tuple>
#include <vector>

class SDL_Exception
{
public:
	SDL_Exception(std::string str);
	std::string message() const;
private:
	const std::string _msg;
};

class SDL_Init_RAII
{
public:
	SDL_Init_RAII(Uint32 flags);
	~SDL_Init_RAII();
};

template<class T>
class RAII_Wrapper
{
public:
	RAII_Wrapper(std::function<T*()> initFunc, std::function<void(T*)> df, std::string err_msg);
	~RAII_Wrapper();
	T *get() const;
	operator T* () { return _ptr; }
private:
	T *_ptr;
	std::function<void(T*)> destFunc;
};

Uint32 timerFunction(Uint32 interval, void* param);

class event_loop
{
public:
	void setRedraw();
	event_loop(SDL_Window* window, SDL_Renderer* renderer);
	void run();
	void addHandler(Uint32 event_type, std::function<void(SDL_Event*)> handler);
	void addHandler(std::function<void(SDL_Event*)> handler);
	void addDrawer(std::function<void(SDL_Renderer*)> drawer);
	void addTimerEvent(Uint32 msInterval, std::function<void()>* handler);
private:
	std::vector<std::function<void(SDL_Event*)>> called_on_all_events;
	std::map<Uint32,std::vector<std::function<void(SDL_Event*)>>> event_funcs;
	std::vector<std::function<void(SDL_Renderer*)>> drawfuncs;
	SDL_Window* _wnd;
	SDL_Renderer* _rend;
	bool _exit;
	void call_all_draw_funcs();
	bool _redraw;
};

typedef std::tuple<int, int, int> RGBColor;

void set_sdl_draw_color(SDL_Renderer *r, RGBColor c, int alpha=0xFF);

void draw_sdl_rectangle(SDL_Renderer* r, int x, int y, int w, int h, RGBColor col, int alpha=0xFF);

template<int r, int g, int b, int a>
void clear_screen(SDL_Renderer *rend);

#include "sdl_util.tpp"

#endif /* SDL_UTIL_H */

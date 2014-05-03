#include "sdl_util.h"
#include "util.h"

using namespace std;

SDL_Exception::SDL_Exception(string str):
	_msg(str)
{}

string SDL_Exception::message() const
{
	return make_str(_msg, SDL_GetError());
}

SDL_Init_RAII::SDL_Init_RAII(Uint32 flags)
{
	if (SDL_Init(flags))
		throw SDL_Exception("Unable to initialize SDL: ");
}

SDL_Init_RAII::~SDL_Init_RAII()
{
	SDL_Quit();
}

Uint32 timerFunction(Uint32 interval, void* param)
{
	function<void()> fn = *reinterpret_cast<function<void()>*>(param);
	fn();
	return interval;
}

event_loop::event_loop(SDL_Window *window, SDL_Renderer *renderer):
	_wnd(window), _rend(renderer), _exit(false), _redraw(false)
{}

void event_loop::setRedraw()
{
	_redraw = true;
}

void event_loop::run()
{
	while (!_exit)
	{
		SDL_PumpEvents();
		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			if (event.type == SDL_QUIT)
			{
				_exit = true;
				break;
			}
			for (auto f: called_on_all_events)
				f(&event);
			for (auto f: event_funcs[event.type])
				f(&event);
		}
		if (_redraw)
		{
			call_all_draw_funcs();
		}
	}
}

void event_loop::addHandler(Uint32 event_type, function<void(SDL_Event*)> handler)
{
	event_funcs[event_type].push_back(handler);
}

void event_loop::addHandler(function<void(SDL_Event*)> handler)
{
	called_on_all_events.push_back(handler);
}

void event_loop::addDrawer(function<void(SDL_Renderer*)> drawer)
{
	drawfuncs.push_back(drawer);
}

void event_loop::addTimerEvent(Uint32 msInterval, function<void()>* handler)
{
	SDL_AddTimer(msInterval, &timerFunction, reinterpret_cast<void*>(handler));
}

void event_loop::call_all_draw_funcs()
{
	SDL_RenderClear(_rend);
	for (auto f: drawfuncs)
		f(_rend);
	SDL_RenderPresent(_rend);
}

void set_sdl_draw_color(SDL_Renderer *r, RGBColor c, int alpha)
{
	SDL_SetRenderDrawColor(r, get<0>(c), get<1>(c), get<2>(c), alpha);
}

void draw_sdl_rectangle(SDL_Renderer* r, int x, int y, int w, int h, RGBColor col, int alpha)
{
	SDL_Rect tmp = {x, y, w, h};
	set_sdl_draw_color(r,col,alpha);
	SDL_RenderFillRect(r,&tmp);
}

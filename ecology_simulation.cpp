#include <iostream>
#include <string>
#include <cstdlib>
#include <functional>
#include <map>
#include <tuple>
#include <vector>
#include <SDL2/SDL.h>
#include "ecology_simulation_templates.h"

const int WINDOW_WIDTH = 640;
const int WINDOW_HEIGHT = 480;

using namespace std;

void exit_with_error(string e_msg)
{
	cout << e_msg << endl;
	exit(1);
}

class SDL_Exception
{
	string _msg;
	public:
	SDL_Exception(string str) : _msg(str) {}
	string message() const { return make_str(_msg,SDL_GetError()); }
};

class SDL_Init_RAII
{
	public:
	SDL_Init_RAII(Uint32 flags)
	{
		if(SDL_Init(flags))throw SDL_Exception("Unable to initialize SDL: ");
	}
	~SDL_Init_RAII()
	{
		SDL_Quit();
	}
};

template <class T> class RAII_Wrapper
{
	T* _ptr;
	function<void(T*)> destFunc;
	public:
	RAII_Wrapper(function<T*()> initFunc, function<void(T*)> df, string err_msg)
	{
		if(!(_ptr = initFunc()))throw new SDL_Exception(err_msg);
		destFunc = df;
	}
	~RAII_Wrapper()
	{
		destFunc(_ptr);
	}
	T* get() const { return _ptr; }
	operator T* () { return _ptr; }
};

struct SDL_Rect make_rect(int x, int y, int w, int h)
{
	struct SDL_Rect tmp;
	tmp.x = x;
	tmp.y = y;
	tmp.w = w;
	tmp.h = h;
	return tmp;
}

int random(int max) { return rand() % max; }

class event_loop
{
	vector<function<void(SDL_Event*)>> called_on_all_events;
	map<Uint32,vector<function<void(SDL_Event*)>>> event_funcs;
	vector<function<void(SDL_Renderer*)>> drawfuncs;
	SDL_Window* _wnd;
	SDL_Renderer* _rend;
	bool _exit;
	void call_all_draw_funcs()
	{
		SDL_RenderClear(_rend);
		for(auto f : drawfuncs)f(_rend);
		SDL_RenderPresent(_rend);
	}
	public:
	event_loop(SDL_Window* window, SDL_Renderer* renderer)
	{
		_wnd = window;
		_rend = renderer;
		_exit = false;
	}
	void run()
	{
		while(!_exit)
		{
			SDL_PumpEvents();
			SDL_Event event;
			while(SDL_PollEvent(&event))
			{
				if(event.type == SDL_QUIT)
				{
					_exit = true;
					break;
				}
				for(auto f : called_on_all_events)f(&event);
				for(auto f : event_funcs[event.type])f(&event);
			}
			call_all_draw_funcs();
		}
	}
	void addHandler(Uint32 event_type, function<void(SDL_Event*)> handler)
	{
		event_funcs[event_type].push_back(handler);
	}
	void addHandler(function<void(SDL_Event*)> handler)
	{
		called_on_all_events.push_back(handler);
	}
	void addDrawer(function<void(SDL_Renderer*)> drawer)
	{
		drawfuncs.push_back(drawer);
	}
};

// SDL initialization code adapted from wiki.libsdl.org
int main(int argc, char* argv[])
{
	try
	{
		SDL_Init_RAII raii1(SDL_INIT_EVERYTHING);
		RAII_Wrapper<SDL_Window> main_window(bind(SDL_CreateWindow,"Ecology simulation",0,0,WINDOW_WIDTH,WINDOW_HEIGHT,0),SDL_DestroyWindow,"Error creating the window: ");
		RAII_Wrapper<SDL_Renderer> main_window_renderer(bind(SDL_CreateRenderer,main_window.get(),-1,0),SDL_DestroyRenderer,"Error getting the window's renderer: ");
		event_loop el(main_window,main_window_renderer);
		//el.addHandler([](SDL_Event* e){cout << "Received an event of type " << e->type << ".\n";});
		tuple<int,int> most_recent_mouse_pos;
		tuple<int,int,int> color(0,0,0);
		el.addHandler(SDL_MOUSEMOTION,[&](SDL_Event* e)
		{
			SDL_MouseMotionEvent mme = e->motion;
			most_recent_mouse_pos = make_tuple(mme.x,mme.y);
			color = make_tuple(random(255),random(255),random(255));
			//cout << mme.x << "," << mme.y << "\n";
		});
		el.addDrawer([](SDL_Renderer* r)
		{
			SDL_Rect tmp = make_rect(0,0,WINDOW_WIDTH,WINDOW_HEIGHT);
			SDL_SetRenderDrawColor(r,255,255,255,255);
			SDL_RenderFillRect(r,&tmp);
		});
		el.addDrawer([&](SDL_Renderer* r)
		{
			SDL_Rect tmp = make_rect(get<0>(most_recent_mouse_pos)-16,get<1>(most_recent_mouse_pos)-16,32,32);
			SDL_SetRenderDrawColor(r,get<0>(color),get<1>(color),get<2>(color),255);
			SDL_RenderFillRect(r,&tmp);
		});
		SDL_Rect tmp;
		tmp = make_rect(0,0,WINDOW_WIDTH,WINDOW_HEIGHT);
		SDL_RenderSetViewport(main_window_renderer,&tmp);
		el.run();
		return 0;
	}
	catch(SDL_Exception e) {exit_with_error(e.message());}
}

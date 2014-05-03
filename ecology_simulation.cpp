#include <iostream>
#include <string>
#include <cstdlib>
#include <functional>
#include <map>
#include <tuple>
#include <vector>
#include <SDL2/SDL.h>
#include "ecology_simulation_templates.h"
#include "model.h"

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

Uint32 timerFunction(Uint32 interval, void* param)
{
	function<void()> fn = *reinterpret_cast<function<void()>*>(param);
	fn();
	return interval;
}

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
	bool _redraw;
	public:
	void setRedraw() {_redraw = true;}
	event_loop(SDL_Window* window, SDL_Renderer* renderer)
	{
		_wnd = window;
		_rend = renderer;
		_exit = false;
		_redraw = false;
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
			if(_redraw)
			{
				call_all_draw_funcs();
			}
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
	void addTimerEvent(Uint32 msInterval, function<void()>* handler)
	{
		SDL_AddTimer(msInterval,&timerFunction,reinterpret_cast<void*>(handler));
	}
};

typedef tuple<int,int,int> RGBColor;

void set_sdl_draw_color(SDL_Renderer* r, RGBColor c, int alpha = 0xFF)
{
	SDL_SetRenderDrawColor(r,get<0>(c),get<1>(c),get<2>(c),alpha);
}

class mouse_drawing
{
	int x,y;
	RGBColor color;
	public:
	void receive_mouse_input(SDL_Event* e)
	{
		SDL_MouseMotionEvent mme = e->motion;
		//most_recent_mouse_pos = make_tuple(mme.x,mme.y);
		x = mme.x; y = mme.y;
		color = make_tuple(random(0xFF),random(0xFF),random(0xFF));
		//cout << mme.x << "," << mme.y << "\n";
	}
	void draw(SDL_Renderer* r)
	{
		SDL_Rect tmp = make_rect(x-16,y-16,32,32);
		set_sdl_draw_color(r,color);
		SDL_RenderFillRect(r,&tmp);
	}
};

void draw_sdl_rectangle(SDL_Renderer* r, int x, int y, int w, int h, RGBColor col, int alpha = 0xFF)
{
	SDL_Rect tmp = make_rect(x,y,w,h);
	set_sdl_draw_color(r,col,alpha);
	SDL_RenderFillRect(r,&tmp);
}

template <int r, int g, int b, int a> void clear_screen(SDL_Renderer* rend)
{
	draw_sdl_rectangle(rend,0,0,WINDOW_WIDTH,WINDOW_HEIGHT,make_tuple(r,g,b),a);
}

//adapted from "http://stackoverflow.com/questions/11902840/binding-member-functions-in-a-variadic-fashion"
template <class T, class R, class... Args> function<R(Args...)> method_closure(T& t, R(T::* f)(Args...))
{
	return [&t,f](Args... args){mem_fn(f)(&t,args...);};
}

template <class A, class B> function<B(A)> map_to_function(map<A,B>& m)
{
	function<B(A)> mapfn = [&](A key) -> B
	{
		typename map<A,B>::iterator it = m.find(key);
		B retval;
		if(it != m.end())retval = it->second;
		return retval;
	};
	return mapfn;
}

template <class EnumType> void draw_cells(SDL_Renderer* renderer, map<EnumType,RGBColor> colormap, vector<vector<EnumType>> cells)
{
	int total_height = cells.size();
	double cell_height = double(WINDOW_HEIGHT)/total_height;
	int x=0; int y=0;
	for(vector<EnumType> row : cells)
	{
		int total_width = row.size();
		double cell_width = double(WINDOW_WIDTH)/total_width;
		for(EnumType cell : row)
		{
			int x1 = x*cell_width;
			int y1 = y*cell_height;
			int x2 = (x+1)*cell_width;
			int y2 = (y+1)*cell_height;
			draw_sdl_rectangle(renderer,x1,y1,x2,y2,colormap.at(cell));
			x++;
		}
		x=0; y++;
	}
}

template <class EnumType> void draw_cells(SDL_Renderer* renderer, function<RGBColor(EnumType)> colormap, vector<vector<EnumType>> cells)
{
	//int total_width = cells[0].size(); //changed to be row-dependent
	int total_height = cells.size();
	//cout << "totaldimensions(" << total_width << "," << total_height << ")\n";
	//double cell_width = double(WINDOW_WIDTH)/total_width;
	double cell_height = double(WINDOW_HEIGHT)/total_height;
	//cout << "cellsize(" << cell_width << "," << cell_height << ")\n";
	int x=0; int y=0;
	for(vector<EnumType> row : cells)
	{
		int total_width = row.size();
		double cell_width = double(WINDOW_WIDTH)/total_width;
		for(EnumType cell : row)
		{
			//cout << "cellposition(" << x << "," << y << ")\n";
			//cout << "color(" << get<0>(colormap.at(cell)) << "," << get<1>(colormap.at(cell)) << "," << get<2>(colormap.at(cell)) << ")\n";
			int x1 = x*cell_width;
			int y1 = y*cell_height;
			int x2 = (x+1)*cell_width;
			int y2 = (y+1)*cell_height;
			//cout << "rectangle(" << x1 << "," << y1 << "," << x2 << "," << y2 << ")\n";
			draw_sdl_rectangle(renderer,x1,y1,x2,y2,colormap(cell));
			x++;
		}
		x=0; y++;
	}
}

class sample_grid_drawer
{
	vector<vector<int>> grid;
	map<int,RGBColor> gridcolors;
	function<RGBColor(int)> colorfunction;
	public:
	sample_grid_drawer()
	{
		grid.push_back(vector<int>({0,1,0}));
		grid.push_back(vector<int>({1,2,1}));
		grid.push_back(vector<int>({5,4,3,2,1,0}));
		gridcolors[0] = make_tuple(0xFF,0,0);
		gridcolors[1] = make_tuple(0,0xFF,0);
		gridcolors[2] = make_tuple(0,0,0xFF);
		gridcolors[3] = make_tuple(0xFF,0,0xFF);
		gridcolors[4] = make_tuple(0,0xFF,0xFF);
		gridcolors[5] = make_tuple(0xFF,0xFF,0);
		colorfunction = map_to_function(gridcolors);
	}
	void draw(SDL_Renderer* r)
	{
		draw_cells(r,colorfunction,grid);
	}
};

class sample_grid_drawer2
{
	vector<vector<RGBColor>> grid;
	function<RGBColor(RGBColor)> identity;
	public:
	sample_grid_drawer2(int numrows, int numcolumns)
	{
		for(int y=0;y < numcolumns;y++)
		{
			vector<RGBColor> tmp;
			for(int x=0;x<numrows;x++)
			{
				tmp.push_back(make_tuple(random(0xFF),random(0xFF),random(0xFF)));
			}
			grid.push_back(tmp);
		}
		identity = [](RGBColor col){return col;};
	}
	void draw(SDL_Renderer* r)
	{
		draw_cells(r,identity,grid);
	}
};

class simulation_drawer
{
	Model m;
	map<Model::Cell,RGBColor> colormap;
	//function<RGBColor(Model::Cell)> colorfn;
	public:
	simulation_drawer()	: m(128,1.0,1.0/5.0,1.0/5.0,1.0/25.0)
	{
		colormap[Model::NONE] = make_tuple(0xFF,0xFF,0xFF);
		colormap[Model::PREY] = make_tuple(0,0xFF,0);
		colormap[Model::PRED] = make_tuple(0xFF,0,0);
		//colorfn = map_to_function(colormap);
	}
	void timerCallback()
	{
		m.tick();
	}
	void draw(SDL_Renderer* r)
	{
		draw_cells(r,colormap,m.world);
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
		mouse_drawing md;
		//function<void()> helloWorldCallback = [](){ cout << "Hello, world\n";};
		//el.addTimerEvent(1000,&helloWorldCallback);
		el.addHandler(SDL_MOUSEMOTION,method_closure(md,&mouse_drawing::receive_mouse_input));
		el.addDrawer(&clear_screen<0xFF,0xFF,0xFF,0xFF>);
		el.addDrawer(method_closure(md,&mouse_drawing::draw));
		simulation_drawer sd;
		el.addDrawer(method_closure(sd,&simulation_drawer::draw));
		//function<void()> tickTimerCallback = method_closure(sd,&simulation_drawer::timerCallback);
		function<void()> tickTimerCallback = [&]()
		{
			sd.timerCallback();
			el.setRedraw();
		};
		el.addTimerEvent(100,&tickTimerCallback);
		SDL_Rect tmp;
		tmp = make_rect(0,0,WINDOW_WIDTH,WINDOW_HEIGHT);
		SDL_RenderSetViewport(main_window_renderer,&tmp);
		el.run();
		return 0;
	}
	catch(SDL_Exception e) {exit_with_error(e.message());}
}

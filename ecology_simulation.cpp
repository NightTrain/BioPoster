#include "ecology_simulation.h"
#include "model.h"
#include "sdl_util.h"
#include "util.h"

using namespace std;

const int WINDOW_WIDTH = 640;
const int WINDOW_HEIGHT = 480;

class simulation_drawer
{
public:
	simulation_drawer();
	void timerCallback();
	void draw(SDL_Renderer* r);
private:
	Model m;
	map<Model::Cell, RGBColor> colormap;
};

simulation_drawer::simulation_drawer():
	m(128, 0.04, 0.7, 0.15, 0.8)
{
	colormap[Model::NONE] = make_tuple(0xFF,0xFF,0xFF);
	colormap[Model::PREY] = make_tuple(0x00,0xFF,0x00);
	colormap[Model::PRED] = make_tuple(0xFF,0x00,0x00);
}

void simulation_drawer::timerCallback()
{
	m.tick();
}

void simulation_drawer::draw(SDL_Renderer *r)
{
	int total_height = m.world.size();
	double cell_height = double(WINDOW_HEIGHT)/total_height;
	int x=0; int y=0;
	for (auto row: m.world)
	{
		int total_width = row.size();
		double cell_width = double(WINDOW_WIDTH)/total_width;
		for (auto cell: row)
		{
			int x1 = x*cell_width;
			int y1 = y*cell_height;
			int x2 = (x+1)*cell_width;
			int y2 = (y+1)*cell_height;
			draw_sdl_rectangle(r, x1,y1,x2,y2,colormap.at(cell));
			x++;
		}
		x=0; y++;
	}
}

// Adapted from <wiki.libsdl.org>
int main() try
{
	SDL_Init_RAII sdl(SDL_INIT_EVERYTHING);
	RAII_Wrapper<SDL_Window> main_window(bind(SDL_CreateWindow, "Ecology simulation", 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT, 0),
										 SDL_DestroyWindow, "Error creating the window: ");
	RAII_Wrapper<SDL_Renderer> main_window_renderer(bind(SDL_CreateRenderer, main_window.get(), -1, 0),
													SDL_DestroyRenderer, "Error getting the window's renderer: ");
	event_loop el(main_window, main_window_renderer);

	el.addDrawer(&clear_screen<0xFF,0xFF,0xFF,0xFF>);
	simulation_drawer sd;
	el.addDrawer(method_closure(sd,&simulation_drawer::draw));
	function<void()> tickTimerCallback = [&]()
	{
		sd.timerCallback();
		el.setRedraw();
	};
	el.addTimerEvent(100,&tickTimerCallback);
	el.run();

	return 0;
}
catch(SDL_Exception e)
{
	exit_with_error(e.message());
}

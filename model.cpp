#include <cmath>
#include <ctime>
#include <limits>
#include <iostream>
#include <utility>

#include "model.h"

using namespace std;

static_assert(numeric_limits<Model::Row::size_type>::max()
              >= numeric_limits<Model::size_type>::max() &&
              numeric_limits<Model::Grid::size_type>::max()
              >= numeric_limits<Model::size_type>::max(),
              "insufficient vector size type");

const Model::interactor Model::INTERACT[CELL_NUM][CELL_NUM] = {
	{
		&Model::interact_null,
		&Model::interact_none_preypred,
		&Model::interact_none_preypred
	},
	{
		&Model::interact_prey_none,
		&Model::interact_null,
		&Model::interact_prey_pred
	},
	{
		&Model::interact_pred_nonepred,
		&Model::interact_pred_prey,
		&Model::interact_pred_nonepred
	}
};

Model::Model(size_type size, float alpha, float beta, float gamma,
             float delta):
        world(size, Row(size, NONE)),
        alpha(alpha), beta(beta), gamma(gamma), delta(delta),
        gen(time(nullptr)), neighbor(0, 7), event(0.0, 1.0),
        num_prey(0), num_pred(0), dt(0)
{
        // Randomly add prey and predators. TODO: Replace this with
        // something more reasonable.
        for (size_type y = 0; y < size; ++y)
                for (size_type x = 0; x < size; ++x) {
                        float add = event(gen);
                        if (add < 0.2) {
                                world[y][x] = PREY;
                                ++num_prey;
                        } else if (add < 0.22) {
                                world[y][x] = PRED;
                                ++num_pred;
                        }
                }
}

Model::Cell &Model::choose_neighbor(size_type x, size_type y)
{
        unsigned int t = neighbor(gen);
        size_type nx = (size_type)(x + round(cos(t * M_PI / 4.0))) % world.size();
        size_type ny = (size_type)(y + round(sin(t * M_PI / 4.0))) % world.size();
        return world[ny][nx];
}

// Ticks the model, causing each cell to interact with a randomly selected
// Moore neighborhood cell.
void Model::tick()
{
        ++dt;
        for (size_type y = 0; y < world.size(); ++y)
                for (size_type x = 0; x < world.size(); ++x) {
                        Cell &current = world[y][x], &target = choose_neighbor(x, y);
                        (this->*INTERACT[current][target])(current, target);
                }
        cout << dt << ' ' << num_prey << ' ' << num_pred << endl;
}

// Two cells interact according to the rules in the NOTES file. Every cell
// in the world is the current cell once and only once per tick.

void Model::interact_null(Cell &current, Cell &neighbor)
{
        (void)current, (void)neighbor;
}

void Model::interact_none_preypred(Cell &current, Cell &neighbor)
{
        swap(current, neighbor);
}

void Model::interact_prey_none(Cell &current, Cell &neighbor)
{
        (void)current;
        if (event(gen) < alpha) {
                neighbor = PREY;
                ++num_prey;
        }
}

void Model::interact_prey_pred(Cell &current, Cell &neighbor)
{
        if (event(gen) < beta) {
                current = NONE;
                --num_prey;
                if (event(gen) < delta) {
                        current = PRED;
                        ++num_pred;
                }
        }
        if (event(gen) < gamma) {
                neighbor = NONE;
                --num_pred;
        }
}

void Model::interact_pred_nonepred(Cell &current, Cell &neighbor)
{
        (void)neighbor;
        if (event(gen) < gamma) {
                current = NONE;
                --num_pred;
        }
}

void Model::interact_pred_prey(Cell &current, Cell &neighbor)
{
        interact_prey_pred(neighbor, current);
}

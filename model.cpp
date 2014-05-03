#include <cmath>
#include <ctime>
#include <limits>
#include <utility>

#include "model.h"

static_assert(std::numeric_limits<Model::Row::size_type>::max()
              >= std::numeric_limits<Model::size_type>::max() &&
              std::numeric_limits<Model::Grid::size_type>::max()
              >= std::numeric_limits<Model::size_type>::max(),
              "insufficient vector size type");

Model::Model(size_type size, float alpha, float beta, float gamma, float delta):
        world(size, Row(size, NONE)),
        interact{{&Model::interact_null, &Model::interact_none, &Model::interact_none},
        {&Model::interact_prey_none, &Model::interact_null, &Model::interact_prey_pred},
        {&Model::interact_pred_both, &Model::interact_pred_prey, &Model::interact_pred_both}},
        alpha(alpha), beta(beta), gamma(gamma), delta(delta),
        gen(std::time(NULL)), neighbor(0, 7), event(0.0, 1.0)
{
        // Randomly add prey and predators. TODO: Replace this with
        // something more reasonable.
        for (size_type y = 0; y < size; ++y)
                for (size_type x = 0; x < size; ++x) {
                        float add = event(gen);
                        if (add < 0.2)
                                world[y][x] = PREY;
                        else if (add < 0.22)
                                world[y][x] = PRED;
                }
}

Model::Cell &Model::choose_neighbor(size_type x, size_type y)
{
        unsigned int t = neighbor(gen);
        size_type nx = (size_type)(x + std::round(std::cos(t * M_PI / 4.0))) % world.size();
        size_type ny = (size_type)(y + std::round(std::sin(t * M_PI / 4.0))) % world.size();
        return world[ny][nx];
}

// Ticks the model, causing each cell to interact with a randomly selected
// Moore neighborhood cell.
void Model::tick()
{
        for (size_type y = 0; y < world.size(); ++y)
                for (size_type x = 0; x < world.size(); ++x) {
                        Cell &current = world[y][x], &target = choose_neighbor(x, y);
                        (this->*interact[current][target])(current, target);
                }
}

// Two cells interact according to the rules in the NOTES file. Every cell
// in the world is the current cell once and only once per tick.

// Nothing happens . . .
void Model::interact_null(Cell &current, Cell &target)
{
        (void)current, (void)target;
}

// Prey/predator moves.
void Model::interact_none(Cell &current, Cell &target)
{
        std::swap(current, target);
}

// Prey might reproduce.
void Model::interact_prey_none(Cell &current, Cell &target)
{
        (void)current;
        if (event(gen) < alpha)
                target = PREY;
}

// Prey might be eaten. Predator might die. If prey is eaten, predator
// might reproduce.
void Model::interact_prey_pred(Cell &current, Cell &target)
{
        if (event(gen) < beta) {
                current = NONE;
                if (event(gen) < delta)
                        current = PRED;
        }
        if (event(gen) < gamma)
                target = NONE;
}

// Predator might die.
void Model::interact_pred_both(Cell &current, Cell &target)
{
        (void)target;
        if (event(gen) < gamma)
                current = NONE;
}

// Prey might be eaten. Predator might die. If prey is eaten, predator
// might reproduce.
void Model::interact_pred_prey(Cell &current, Cell &target)
{
        interact_prey_pred(target, current);
}

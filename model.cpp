#include <cmath>
#include <limits>
#include <utility>

#include "model.h"

static_assert(std::numeric_limits<Model::Row::size_type>::max()
              >= std::numeric_limits<Model::size_type>::max() &&
              std::numeric_limits<Model::Grid::size_type>::max()
              >= std::numeric_limits<Model::size_type>::max(),
              "insufficient vector size type");

Model::Model(size_type size, float alpha, float beta, float gamma,
             float delta):
        world(size, Row(size, NONE)),
        alpha(alpha), beta(beta), gamma(gamma), delta(delta),
        neighbor(0, 7), event(0.0, 1.0)
{
        // Randomly add prey and predators. TODO: Replace this with
        // something more reasonable.
        for (size_type y = 0; y < size; ++y)
                for (size_type x = 0; x < size; ++x) {
                        unsigned int add = event(gen);
                        if (add < 0.25)
                                world[y][x] = PREY;
                        else if (add < 0.5)
                                world[y][x] = PRED;
                }
}

Model::Cell &Model::choose_neighbor(size_type x, size_type y)
{
        unsigned int t = neighbor(gen);
        size_type nx = (x + (size_type)std::round(std::cos(t * M_PI / 4.0))) % world.size();
        size_type ny = (y + (size_type)std::round(std::sin(t * M_PI / 4.0))) % world.size();
        return world[ny][nx];
}

// Ticks the model, causing each cell to interact with a randomly selected
// Moore neighborhood cell.
void Model::tick()
{
        for (size_type y = 0; y < world.size(); ++y)
                for (size_type x = 0; x < world.size(); ++x)
                        interact(world[y][x], choose_neighbor(x, y));
}

// Current cell interacts with target cell according to the rules in the
// NOTES file.
void Model::interact(Cell &current, Cell &target)
{
        switch (current) {
        case NONE:
                // Prey/predator moves.
                std::swap(current, target);
                break;
        case PREY:
                switch (target) {
                case NONE:
                        // Prey might reproduce.
                        if (event(gen) < alpha)
                                target = PREY;
                        break;
                case PRED:
                        // Prey might be eaten.
                        if (event(gen) < beta) {
                                current = NONE;
                                // Predator might reproduce.
                                if (event(gen) < delta)
                                        current = PRED;
                        }
                        break;
                }
                break;
        case PRED:
                if (target == PREY) {
                        // Prey might be eaten.
                        if (event(gen) < beta) {
                                target = NONE;
                                // Predator might reproduce.
                                if (event(gen) < delta)
                                        target = PRED;
                        }
                }
                // Predator might die.
                if (event(gen) < gamma)
                        current = NONE;
                break;
        }
}

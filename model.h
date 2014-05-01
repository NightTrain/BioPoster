#ifndef MODEL_H
#define MODEL_H

#include <random>
#include <vector>

class Model {
public:
        enum Cell {
                NONE,
                PREY,
                PRED
        };

        typedef std::vector<Cell> Row;
        typedef std::vector<Row> Grid;
        typedef unsigned int size_type;

        Grid world;

        Model(size_type size, float alpha, float beta, float gamma,
              float delta);
        void tick();
private:
        // alpha = natural prey growth rate
        // beta = predation rate
        // gamma = natural predator death rate
        // delta = predator growth rate
        float alpha, beta, gamma, delta;

        std::default_random_engine gen;
        std::uniform_int_distribution<unsigned int> neighbor;
        std::uniform_real_distribution<float> event;

        Cell &choose_neighbor(size_type x, size_type y);
        void interact(Cell &current, Cell &target);
};

#endif /* MODEL_H */

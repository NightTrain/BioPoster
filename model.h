#ifndef MODEL_H
#define MODEL_H

#include <random>
#include <vector>

class Model
{
	public:
	enum Cell
	{
		NONE,
		PREY,
		PRED,
		CELL_NUM
	};

	typedef std::vector<Cell> Row;
	typedef std::vector<Row> Grid;
	typedef unsigned int size_type;

	Grid world;

	Model(size_type size, float alpha, float beta, float gamma, float delta);
	void tick();
	private:
	typedef void(Model::*interactor)(Cell&, Cell&);

	interactor interact[CELL_NUM][CELL_NUM];

	// alpha = natural prey growth rate
	// beta = predation rate
	// gamma = natural predator death rate
	// delta = predator growth rate
	const float alpha, beta, gamma, delta;

	std::default_random_engine gen;
	std::uniform_int_distribution<unsigned int> neighbor;
	std::uniform_real_distribution<float> event;

	Cell &choose_neighbor(size_type x, size_type y);

	void interact_null(Cell &current, Cell &target);
	void interact_none(Cell &current, Cell &target);
	void interact_prey_none(Cell &current, Cell &target);
	void interact_prey_pred(Cell &current, Cell &target);
	void interact_pred_both(Cell &current, Cell &target);
	void interact_pred_prey(Cell &current, Cell &target);
};

#endif /* MODEL_H */

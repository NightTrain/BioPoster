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

	Model(size_type side_length, float alpha, float beta, float gamma, float delta);
	void tick();
private:
	typedef void (Model::*interactor)(Cell&, Cell&);

	//
	static const interactor INTERACT[CELL_NUM][CELL_NUM];

	// alpha = natural prey growth rate
	// beta = predation rate
	// gamma = natural predator death rate
	// delta = predator growth rate
	float alpha, beta, gamma, delta;

	std::default_random_engine gen;
	std::uniform_int_distribution<unsigned int> neighbor;
	std::uniform_real_distribution<float> event;

	unsigned long num_prey, num_pred, dt;

	Cell &choose_neighbor(size_type x, size_type y);

	// Nothing happens . . .
	void interact_null(Cell &current, Cell &neighbor);
	// Prey/predator moves.
	void interact_none_preypred(Cell &current, Cell &neighbor);
	// Prey might reproduce.
	void interact_prey_none(Cell &current, Cell &neighbor);
	// Prey might be eaten. Predator might die. If prey is eaten, predator
	// might reproduce.
	void interact_prey_pred(Cell &current, Cell &neighbor);
	// Predator might die.
	void interact_pred_nonepred(Cell &current, Cell &neighbor);
	// Prey might be eaten. Predator might die. If prey is eaten, predator
	// might reproduce.
	void interact_pred_prey(Cell &current, Cell &neighbor);
};

#endif /* MODEL_H */

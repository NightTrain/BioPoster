#!/bin/bash
g++ ecology_simulation.cpp model.cpp $(sdl2-config --libs) --std=c++0x -o ecology_simulation

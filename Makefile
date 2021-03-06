CXX = g++
CXXFLAGS = -g
CXXFLAGS += -Wall -Wextra -pedantic -std=c++0x
LDFLAGS = -lSDL2
OBJECTS = ecology_simulation.o model.o sdl_util.o util.o

.PHONY: all
all: eco

eco: $(OBJECTS)
	$(CXX) $(CXXFLAGS) $^ $(LDFLAGS) -o $@

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

%.d: %.cpp
	$(CXX) $(CXXFLAGS) -MM -MP $< -MF $@
	sed -i 's/$(@:.d=.o)/$@ &/' $@

-include $(OBJECTS:.o=.d)

.PHONY: clean
clean:
	rm -f eco $(OBJECTS) $(OBJECTS:.o=.d)


COMPILER = g++
OPS = -Wall -std=c++2a

test : obj/vec_test obj/stats_test obj/ecs_test
run : obj/run
	./obj/run

.PHONY: test run

obj/vec_test : vec.h vec_test.cpp test.h
	${COMPILER} ${OPS} vec_test.cpp -o obj/vec_test
	./obj/vec_test

obj/ecs_test : ecs.h ecs_test.cpp test.h util.h
	${COMPILER} ${OPS} ecs_test.cpp -o obj/ecs_test
	./obj/ecs_test

obj/graphics.o : graphics.h graphics.cpp util.h
	${COMPILER} ${OPS} -c graphics.cpp -o obj/graphics.o

obj/math.o : math.h math.cpp
	${COMPILER} ${OPS} -c math.cpp -o obj/math.o

obj/main.o : main.cpp obj/graphics.o obj/math.o glpp.h
	${COMPILER} ${OPS} -c main.cpp -o obj/main.o

obj/run : obj/main.o 
	${COMPILER} ${OPS} obj/*.o -lSDL2 -lGL -lGLEW -lGLU -o obj/run

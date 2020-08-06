
COMPILER = g++
OPS = -Wall -std=c++2a

test : obj/vec_test obj/stats_test obj/ecs_test
run : obj/run
	./obj/run

.PHONY: test run

obj/random.o : random.h random.cpp
	${COMPILER} ${OPS} -c random.cpp -o obj/random.o

obj/vec_test : vec.h vec_test.cpp test.h
	${COMPILER} ${OPS} vec_test.cpp -o obj/vec_test
	./obj/vec_test

obj/ecs_test : ecs.h ecs_test.cpp test.h util.h
	${COMPILER} ${OPS} ecs_test.cpp -o obj/ecs_test
	./obj/ecs_test

obj/graphics.o : graphics.h graphics.cpp glpp.h util.h
	${COMPILER} ${OPS} -c graphics.cpp -o obj/graphics.o

obj/line_breaker_shader.o : line_breaker_shader.h line_breaker_shader.cpp \
														util.h graphics.h
	${COMPILER} ${OPS} -c line_breaker_shader.cpp -o obj/line_breaker_shader.o

obj/math.o : math.h math.cpp
	${COMPILER} ${OPS} -c math.cpp -o obj/math.o

obj/line_breaker_track.o : line_breaker_track.h line_breaker_track.cpp \
													 line_breaker_components.h line_breaker_shader.h \
													 math.h random.h
	${COMPILER} ${OPS} -c line_breaker_track.cpp -o obj/line_breaker_track.o


obj/main.o : main.cpp *.h
	${COMPILER} ${OPS} -c main.cpp -o obj/main.o

obj/run : obj/main.o obj/math.o obj/graphics.o obj/random.o \
					obj/line_breaker_shader.o obj/line_breaker_track.o
	${COMPILER} ${OPS} obj/*.o -lSDL2 -lGL -lGLEW -lGLU -o obj/run

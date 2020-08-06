# Line Breaker

Line Breaker is a silly little game where you play as a ship and you have to
fly along a track and stay on it as long as possible. Before getting on, you
can control your ship with the arrow keys, UP and DOWN to adjust the throttle.
After entering the track, you may no longer control your throttle.

A limited number of planks make up the track. When the player runs over a
plank, it splits in two and a new plank appears at the end of the track. Miss
enough planks and the track will have no more planks in it. Additionally, the
player's throttle will decay over time so staying off the track for too long
will cause to lose speed and not have the ability to keep playing.

Press 'r' to restart.
Press 'q' to exit or just close the window.

# Compiling [UNTESTED]

On Ubuntu Linux, install the necessary packages like so:
```bash
sudo apt install libsdl2-dev libglm-dev libglew-dev
```
Then compile and run the game:
```bash
mkdir obj
make -j run
```
The game window should appear and the output binary lands in `obj/run`.

# Source Code

With the exception of `main.cpp`, the headers and source files largely contain
generic functions not specific to this game. Headers specific to this game
should begin with `line_breaker_`.

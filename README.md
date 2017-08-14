# Heavenly Bodies

A space debris shooting game created in Haskell, as a programming exercise. And
to relax a bit from C++.

## How to play

Use the keyboard to control the game:

* Up/down arrow: accelerate,
* Left/right arrow: turn the ship,
* Space: shooting,
* Escape: end the game,
* R: restart the game.

## Dependencies and Builiding the game

We use [Haskell Stack][Stack] to build the game and manage dependencies. Apart
from Stack, you'll also need SDL2 installed.

To build or run the game, use Stack as usual:

    stack setup
    stack build

You can run the game directly through Stack using:

    stack exec heavenly-bodies

[Stack]: https://www.haskellstack.org/

## Things yet to be done

This is an informal list of things that would be nice to have before calling the
game finished:

* Add score, player death.
* Make asteroids bounce from each other, make new asteroids appear.
* Better player bouncing:
    * change the angle only if the player is turned more or less in the
      direction in which she is flying, and has a good speed.
    * alternative idea: only change the angle if the player hits the edge of the
      screen under a low angle.
* Add full-screen mode; make it the default.
* Use 21st century OpenGL for rendering. Profiling has shown that about 20% of
  CPU time used by the game is spent in glPushAttrib/glPopAttrib, which is not
  the best use of processing power.
* Add more visual effects.
* Tune the shades of grey.
* Balance the difficulty.
* Add heat management as an additional game mechanic to prevent the players from
  shooting all the time.

## License

The game and all its files are available under the [Apache 2.0 License][Apache].

Copyright 2017 Ondrej Sykora

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[Apache]: http://www.apache.org/licenses/LICENSE-2.0

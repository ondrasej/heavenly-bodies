# Heavenly Bodies

A space debris shooting game created in Haskell, as a programming exercise. Or
maybe to relax a bit from C++, it's hard to say.

## How to play

Left and right arrows turn the ship. Up arrow accelerates forward, down arrow
accelerates backwards.

## Dependencies and Builiding the game

The game and its dependencies are managed using Cabal. To build the game, simply
run the following commands in the game directory.

    cabal configure
    cabal run

The game is built using OpenGL and SDL, and the following Hackage libraries are
necessary to build it:

* `hspec` (for running tests)
* `OpenGL`
* `sdl2`
* `text`

To build `sdl2` and `OpenGL`, you might also need to install the development
versions of the native libraries. It is highly advisible to build the game
inside a Cabal sandbox.

## Things to do before release

* Make the asteroids interactive
* Shooting at asteroids
* Better bouncing:
    * change the angle only if the player is turned more or less in the
      direction in which she is flying, and has a good speed
    * alternative idea: only change the angle if the player hits the edge of the
      screen under a low angle
* Tune the shades of grey
* Full-screen mode
* Try using vertex buffers to optimize rendering (no glVertex calls, yay!)
* Try building and running it on Windows
* Create an OS X bundle to make running it easier

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

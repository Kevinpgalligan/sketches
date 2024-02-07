### Description
These are my experiments with the [Sketch](https://github.com/vydd/sketch) library for Common Lisp, which provides a Processing-like interface for creative programming.

### Requirements
* [Sketch](https://github.com/vydd/sketch) (which comes with an extra SDL2-related installation step)
* random-state

### How to run
Setup: Add soft link to this directory to quicklisp's local-projects folder, then from the REPL do `(ql:quickload 'sketches)`, then switch to the `sketches` package (`(in-package sketches)`).

To run a particular sketch from the list below, like `stars`, enter this at the REPL: `(run-sketch 'stars)`.

### Sketches
* `stars`: a star field based on The Coding Train's first coding challenge.
* `heightmap`: testing out my implementation of value noise (for correlated randomness).
* `rain`: rain, wind and fog. Uses 1d value noise for the wind effect, and 3d value noise for the fog (the 3 dimensions being x, y and time).
* `unknown`: recreation of the Unknown Pleasures album cover, uses 3d value noise to make the lines wobble over time.
* `flowfield`: flow field sketch based on The Coding Train's video.
* `dots`: particles move around a flow field and displace dots.
* `snow`: mountains and snow.
* `growth`: colourful blobs growing randomly.
* `groove2`: recreation of a piece I saw at the Institute of Contemporary Art in Boston.
* `mandelbrot`: the Mandelbrot set.
* `squarez`: a grid of white quadrilaterals on a black background, inspired by a t-shirt I saw.
* `xmas`: recursive Christmas trees, in the colours of Palestine.
* `invaders`: a skeleton implementation of Space Invaders that I did for my Recurse Center application.
* `toast`: a piece of toast that bounces and always lands on the wrong side, contains a crappy/non-general implementation of Verlet physics.
* `reaction-diffusion`: visualisation of the reaction diffusion model, based on a Coding Train video.
* `palette`: for testing out colour palettes.
* `trees.recursive`: random trees drawn using recursion.
* `trees.oo`: random trees drawn using object-oriented modelling (with leaves!).
* `trees.lsystem`: random trees drawn using an L-system.
* `swirl`: particles spiraling towards the center of the canvas and leaving a trail in their wake.

In case this list is incomplete, you can run `(print-all-sketches)` to show all the available sketches.

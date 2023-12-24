### Description
These are my experiments with the `sketch` library for Common Lisp, which provides a Processing-like interface for creative programming.

### Requirements
* sketch (which comes with an extra SDL2-related installation step)
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

In case this list is incomplete, you can run `(print-all-sketches)` to show all the available sketches.

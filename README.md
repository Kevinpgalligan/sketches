### Description
These are my experiments with the [Sketch](https://github.com/vydd/sketch) library for Common Lisp, which provides a Processing-like interface for creative programming.

![Clips of some example sketches.](https://github.com/Kevinpgalligan/sketches/blob/master/examples.mp4)

### How to run
* Add a soft link to this directory to quicklisp's local-projects folder (`ln -s /path/to/sketches/ /quicklisp/local-projects/`).
* Clone my fork of Sketch into your local-projects folder, from [here](https://github.com/Kevinpgalligan/sketch/tree/dev).
* Follow the normal installation instructions for Sketch (includes an SDL2-specific step).
* From the REPL, load the system: `(ql:quickload 'sketches)`.
* Switch to the `sketches` package: `(in-package sketches)`.
* To run a particular sketch from the list below, like `stars`: `(start-sketch 'stars)`.

### Sketches
In case this list is incomplete, you can run `(print-all-sketches)` to show all the available sketches.

* `stars`: a star field based on The Coding Train's first coding challenge.
* `heightmap`: testing out my implementation of correlated noise.
* `rain`: rain, wind and fog.
* `unknown`: recreation of the Unknown Pleasures album cover, uses noise to make the lines wobble over time.
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
* `reuleaux`: [reuleaux triangle](https://en.wikipedia.org/wiki/Reuleaux_triangle) rotating in a square.

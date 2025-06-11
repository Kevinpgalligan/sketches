### Description
These are my experiments with the [Sketch](https://github.com/vydd/sketch) library for Common Lisp, which provides a Processing-like interface for creative programming.

https://github.com/user-attachments/assets/25d78de7-db0d-4d43-920b-6be973a1091a

### How to run
* Add a soft link to this directory to quicklisp's local-projects folder (`ln -s /path/to/sketches/ /quicklisp/local-projects/`).
* Clone my fork of Sketch into your local-projects folder, from [here](https://github.com/Kevinpgalligan/sketch/tree/dev).
* Follow the normal installation instructions for Sketch (includes an SDL2-specific step).
* From the REPL, load the system: `(ql:quickload 'sketches)`.
* Switch to the `sketches` package: `(in-package sketches)`.
* To run a particular sketch from the list below, like `stars`: `(load-sketch 'stars)`.

### Sketches
In case this list is incomplete, you can run `(print-all-sketches)` to show all the available sketches.

* `stars`: A star field based on The Coding Train's first coding challenge.
* `heightmap`: Testing out my implementation of correlated noise.
* `rain`: Rain, wind and fog.
* `unknown`: Recreation of the Unknown Pleasures album cover, uses noise to make the lines wobble over time.
* `flowfield`: Flow field sketch based on The Coding Train's video.
* `dots`: Particles move around a flow field and displace dots.
* `snow`: Mountains and snow.
* `growth`: Colourful blobs growing randomly.
* `groove2`: Recreation of a piece I saw at the Institute of Contemporary Art in Boston.
* `mandelbrot`: The Mandelbrot set.
* `squarez`: A grid of white quadrilaterals on a black background, inspired by a t-shirt I saw.
* `xmas`: Recursive Christmas trees, in the colours of Palestine.
* `invaders`: A skeleton implementation of Space Invaders that I did for my Recurse Center application.
* `toast`: A piece of toast that bounces and always lands on the wrong side, contains a crappy/non-general implementation of Verlet physics.
* `reaction-diffusion`: Visualisation of the reaction diffusion model, based on a Coding Train video.
* `palette`: For testing out colour palettes.
* `trees.recursive`: Random trees drawn using recursion.
* `trees.oo`: Random trees drawn using object-oriented modelling (with leaves!).
* `trees.lsystem`: Random trees drawn using an L-system.
* `swirl`: Particles spiraling towards the center of the canvas and leaving a trail in their wake.
* `reuleaux`: [Reuleaux triangle](https://en.wikipedia.org/wiki/Reuleaux_triangle) rotating in a square.
* `lsystems.koch`: A variant of the Koch curve.
* `lsystems.sierspinski`: The Sierspinski triangle.
* `terrain`: Using noise to generate islands and mountains.
* `nested-squares`: Squares inside squares.
* `dandy`: A medusa-like flower thing.
* `boids`: An implementation of the [boids algorithm](https://en.wikipedia.org/wiki/Boids) for simulating flocking behaviour.
* `nadasurf`: An animated version of the album cover from Nada Surf's *Let Go*.
* `barricelli`: A simple version of Barricelli's cellular automata, as described in [this blog post](https://akkartik.name/post/2024-08-30-devlog).
* `physics-test`: Testing my implementation of Verlet physics. Particles, gravity, bounds, springs, sticks.
* `keffiyeh`: Randomly generated patterns in the style of the Palestinian keffiyeh.
* `renderer`: Rendering a 3000ish-triangle model of the Utah teapot. Includes camera model, z-sorting, etc. Completely unoptimised, render takes about 45ms.
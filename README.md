### Description
These are my experiments with the `sketch` library for Common Lisp, which provides a Processing-like interface for creative programming.

### Requirements
* sketch (which comes with an extra SDL2-related installation step)
* random-state

### How to run
Add soft link to this directory to quicklisp's local-projects folder, then from the REPL do `(ql:quickload 'sketches)`, then switch to the `sketches` package (`(in-package sketches)`), then `(make-instance 'rain)`.

### Sketches
* `stars`: a star field based on The Coding Train's first coding challenge.
* `heightmap`: testing out my implementation of value noise (for correlated randomness).
* `rain`: rain, wind and fog. Uses 1d value noise for the wind effect, and 3d value noise for the fog (the 3 dimensions being x, y and time).
* `unknown`: recreation of the Unknown Pleasures album cover, uses 3d value noise to make the lines wobble over time.
* `scene-flow`: flow field sketch based on The Coding Train's video.
* `dots`: particles move around a flow field and displace dots.
* `snowscene`: mountains and snow (currently doesn't work due to a bug in sketch).
* `growth`: colourful blobs growing randomly.
* `groove2`: recreation of a piece I saw at the Institute of Contemporary Art in Boston.
# Description
These are my experiments with the `sketch` library for Common Lisp, which provides a Processing-like interface for creative programming.

# Requirements
* sketch (which comes with an extra SDL2-related installation step)
* random-state

# How to run
Add soft link to this directory to quicklisp's local-projects folder, then
from the REPL do `(ql:quickload 'sketches)`, then switch to the `sketches` package (`(in-package sketches)`), then `(make-instance 'rain)`.

# Sketches
* `stars.lisp`: a star field based on The Coding Train's first coding challenge.
* `heightmap.lisp`: testing out my implementation of value noise (for correlated randomness).
* `rain.lisp`: rain, wind and fog. Uses 1d value noise for the wind effect, and 3d value noise for the fog (the 3 dimensions being x, y and time).
* `unknown-pleasures.lisp`: recreation of the Unknown Pleasures album cover, uses 3d value noise to make the lines wobble over time.
PFP Project
==========

Notes
----------

[Reference Issue](https://github.com/Twinside/Rasterific/issues/32). Some notes:

 *  The `combineEdgeSamples` function is pointed out as a easily parallelizable function (the `sub` function is basically a parallel map).
    [Link](https://github.com/Twinside/Rasterific/blob/87c4c39a7062457c6bea44cf79a981ccdf432dbf/src/Graphics/Rasterific/Rasterize.hs#L31).
    
    As a reference, for the snowflake benchmark the function is called 8500 times with a mean vector length of 1076.98. 
 *  The "C Implementation" referenced as parallel can be found in [this](https://github.com/damelang/gezira) repo. It is a machine written
    C implementation using the runtime of some data streaming programming language ([Nile](https://github.com/damelang/nile)). Inspecting
    that runtime is necessary to see where the parallelism happens.
 *  The most recent commit in the Rasterific repo defines a Snowflake benchmark -- very convienient.

TODO List
----------

In no particular order:

 *  Consider writing some more benchmarks ala the Snowflake example.
 *  Try slamming some parallelism into `combineEdgeSamples`.
 *  Try to identify what parallelism could be optained wrt. primitives (see the issue).

Rasterific
==========

![Rasterific logo](https://raw.github.com/Twinside/Rasterific/master/img/logo.png)

[![Hackage](https://img.shields.io/hackage/v/Rasterific.svg)](http://hackage.haskell.org/package/Rasterific)

Rasterific is a Haskell rasterization engine (a vectorial renderer)
implemented on top of [JuicyPixels](https://github.com/Twinside/Juicy.Pixels).
Rasterific bases its text rendering on [FontyFruity](https://github.com/Twinside/FontyFruity).

Main capability
---------------

 * Draw vector graphics to an image.
 * Export graphics to PDF (since 0.6).

Design
------
The renderer design is based on the
[Nile](https://github.com/damelang/nile) /
[Gezira](https://github.com/damelang/gezira) renderer from the STEP
project from the [VPRI](http://www.vpri.org/index.html) institute. The
interesting thing about this renderer is the conciseness of it's
implementation, providing antialiased rendering in the way.


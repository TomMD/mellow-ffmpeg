# Introduction

Mellow-ffmpeg is a Haskell library providing a layer over
[ffmpeg-light](https://github.com/acowley/ffmpeg-light),
[friday](https://github.com/RaphaelJ/friday), and
[gloss](http://gloss.ouroborus.net/).  That is, Mellow acquires RGBA frames from
the web camera, applies the user-provided image transformation, and renders the
image to screen.

The main routine is `mellow state0 update render eventHandler` where `state0` is
the initial state, update will update the state with a new frame, `render` will
rasterize a given state for display to screen, and `eventHandler` is useful for
key presses and other inputs.

# Example

In under 20 lines we can get a pretty little OpenGL colored rendering of the
image edges.  The basic steps are:

1. Import Mellow and 'friday'.  Import 'gloss' as needed.
2. Call 'mellow' with desired call backs for receiving a new frame into your
   state, rendering the state, and handling keyboard events (see 'gloss'
   documentation for keyboard events).
3. Define your callback that transforms RGBA images into RGBA images
   using whatever techniques you desire (see the 'friday' library).


For example, see the [canny demo](examples/EdgeDetect.hs).

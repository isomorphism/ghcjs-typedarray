ghcjs-typedarray
================

Some slightly over-complicated bindings hacked together for working with
JavaScript Typed Arrays. This is intended only for use with FFI bindings
that make heavy use of Typed Arrays, primarily WebGL. For anything else,
you're better off using standard Haskell types.

The eventual goal is to reformulate this to interoperate nicely with e.g.
`Data.Vector.Storable` but there was a bug with vectors of `Float` which 
was a show-stopper for WebGL purposes and I'm not sure how to correctly wrap 
a TypedArray created externally into a Vector...

Contributions are welcome (especially if you want to do the Vector stuff for me).

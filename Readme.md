# PureScript Module Graph

WIP.

Simple project that demonstrates the following things:
- building a server using `purescript-warp`
- handling client-server routing via `purescript-routing-duplex`
- handling JSON serialization via `purescript-codec`
- parsing text files via `purescript-string-parsers`
- running a CLI program on the server via simple FFI to the `child-process` module
- using string interpolation via `purescript-interpolate`
- using newtypes for type safety and readability

Run `npm run both:run` to compile the client code to `dist/app.js` and start a server on `localhost:3000`.

# om-state-stream
_Disclaimer: This is a work in progress and isn't well documented._

Port of react-state-stream to Om.

# Build
```
lein cljsbuild auto om-state-stream
```

# Why is this cool?
The point behind state-stream (React or Om) is to be able to describe the state of a component over time. There are many other ways to do this without streams (or lazy-sequences), but working with streams enables us to leverage the existing map, reduce, filter, concat functions that can work on those.
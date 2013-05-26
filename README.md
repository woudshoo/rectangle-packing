## Simple Bin Packing / Rectangle Packing

This code implements a simple algorithm for a complicated problem,
given a set of rectangles, pack them into a square/rectangle.

I wrote this because I am playing with OpenGL and want to pack
multiple graphics into a texture.

### Interface

The main interface consists of two functions `pack-rectangles` and
`pack-rectangles-tree`.
Both functions take two arguments,

1. A list of rectangles
2. A keyword argument `:size` specifying the size of the target rectangle
   (as a rectangle)

The only difference is the return value.
The function `pack-rectangles-tree` return a tree representation
of the packing.  This tree is mostly usefull as an intermediate representation.

The `pack-rectangles` returns a list of placements, each placement is
of the form `(x y orientation rectangle)` where `rectangle` is one of
the input rectangles, the `x` and `y` are the placement coordinates, and
`orientation` indicates if the rectangle is rotated.

At the moment `orientation` is always `:0` indicating no rotation.

### Rectangles

A rectangle is any list as along as the first two values of the list indicate
the width and height of the rectangle.  So for example
`(200 300 "This is a rectangle")` is a rectangle, and so is `(200 300)` or `(200 300 (lambda (x) x))`

### Supporting functions

1. `tree-utilized-size` takes one argument, a tree, and returns the packed size.
2. `rectangle-tree-to-rectangle-list` takes a tree as an argument and returns the same output as `pack-rectangles`.
3. `write-html` takes a tree and a file name and write a html file which will show the packing.

### Example

Simple packing.

```common-lisp
> (pack-rectangles (list '(100 200 "Hallo") '(300 400 "Nr 2")  '(50 100 "Tall") '(200 30 "Wide")))
=>
((400 0 :|0| 200 30 "Wide") (100 0 :|0| 300 400 "Nr 2")
 (0 200 :|0| 50 100 "Tall") (0 0 :|0| 100 200 "Hallo"))
```

Or using the tree

```common-lisp
> (pack-rectangles-tree (list '(100 200 "Hallo") '(300 400 "Nr 2")  '(50 100 "Tall") '(200 30 "Wide")))
=>
#<NODE {100CCF8473}>
> (write-html * "/tmp/pack.html")
=>
NIL
```

The resulting html shows a picture like this:

<svg xmlns="http://www.w3.org/2000/svg" version="1.1">
   <rect rx="10" ry="10" 
         x="0" y="0" width="100" height="200" 
         style="fill:red;stroke:black;opacity=0.1" />
  <!-- low = (0 0) high = (100 200)   (rect: (100 200 Hallo))-->
  <rect rx="10" ry="10" 
         x="0" y="200" width="50" height="100" 
         style="fill:red;stroke:black;opacity=0.1" />
  <!-- low = (0 200) high = (50 300)   (rect: (50 100 Tall))-->
  <rect rx="10" ry="10" 
         x="0" y="300" width="50" height="100" 
         style="fill:blue;stroke:black;opacity=0.1" />
  <!-- low = (0 300) high = (50 NIL)   (rect: NIL)-->
  <rect rx="10" ry="10" 
         x="50" y="200" width="50" height="200" 
         style="fill:blue;stroke:black;opacity=0.1" />
  <!-- low = (50 200) high = (100 NIL)   (rect: NIL)-->
  <rect rx="10" ry="10" 
         x="100" y="0" width="300" height="400" 
         style="fill:red;stroke:black;opacity=0.1" />
  <!-- low = (100 0) high = (400 400)   (rect: (300 400 Nr 2))-->
  <rect rx="10" ry="10" 
         x="400" y="0" width="200" height="30" 
         style="fill:red;stroke:black;opacity=0.1" />
  <!-- low = (400 0) high = (600 30)   (rect: (200 30 Wide))-->
  <rect rx="10" ry="10" 
         x="600" y="0" width="0" height="30" 
         style="fill:blue;stroke:black;opacity=0.1" />
  <!-- low = (600 0) high = (NIL 30)   (rect: NIL)-->
  <rect rx="10" ry="10" 
         x="400" y="30" width="200" height="370" 
         style="fill:blue;stroke:black;opacity=0.1" />
  <!-- low = (400 30) high = (NIL 400)   (rect: NIL)-->
  <rect rx="10" ry="10" 
         x="100" y="400" width="500" height="0" 
         style="fill:blue;stroke:black;opacity=0.1" />
  <!-- low = (100 400) high = (NIL NIL)   (rect: NIL)-->
</svg>



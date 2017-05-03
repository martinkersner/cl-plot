# cl-plot
Plotting library for Common Lisp utilizing [gnuplot](http://www.gnuplot.info/).
Martin Kersner, <m.kersner@gmail.com>

*cl-plot* was developed for Common Lisp Machine Learning library [cl-ml](https://github.com/martinkersner/cl-ml.git). Input data follow rules of Common Lisp Mathematical Library [cl-math](https://github.com/martinkersner/cl-math.git).

# Plot types
* Scatter plot
* Histogram (TODO)

# Plot elements
## Figure
## Point
## Arrow

## Labels
Labels (`xlabel` and `ylabel`) require instance of figure as a first parameter. The second parameter represent text to display.

### Example
```lisp
(xlabel  *fig* "x-label")
(ylabel  *fig* "y-label")
```

# Examples
1. A basic example
 * With explanation

First, create matrix with data to plot.
```lisp
(defparameter *dataframe*
  '((1 1)
    (2 2)
    (3 1.5)
    (4 3)
    (5 2.5)
    (6 4)))
```

Then, create a figure.
```lisp
(defparameter *fig* (make-instance 'figure))
```

Make figure of scatter plot type and connect it to generated data.
```lisp
(scatter *fig* *dataframe*)
```

Set description of *x* and *y* labels of plot.
```lisp
(xlabel  *fig* "x-label")
(ylabel  *fig* "y-label")
```

Finally, display plot.
```lisp
(show *fig*)
```

 * Full example
```lisp
(defparameter *dataframe*
  '((1 1)
    (2 2)
    (3 1.5)
    (4 3)
    (5 2.5)
    (6 4)))

(defparameter *fig* (make-instance 'figure))

(scatter *fig* *dataframe*)

(xlabel  *fig* "x-label")
(ylabel  *fig* "y-label")

(show *fig*)
```

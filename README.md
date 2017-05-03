# cl-plot
Plotting library for Common Lisp utilizing [gnuplot](http://www.gnuplot.info/).

Martin Kersner, <m.kersner@gmail.com>, 2017

1. [Info](https://github.com/martinkersner/cl-plot#info)
2. [Getting started](https://github.com/martinkersner/cl-plot#getting-started)
3. [Documentation](https://github.com/martinkersner/cl-plot#documentation)
4. [Examples](https://github.com/martinkersner/cl-plot#examples)

## Info
*cl-plot* was developed for Common Lisp Machine Learning library [cl-ml](https://github.com/martinkersner/cl-ml.git). Input data should follow rules of matrix data structure introduced in Common Lisp Mathematical library [cl-math](https://github.com/martinkersner/cl-math.git).

## Getting started
### Prerequisites
[asdf](https://gitlab.common-lisp.net/asdf/asdf) is required for initialization of *cl-plot*.

### Installation
Clone *cl-plot* repository.
```bash
git clone https://github.com/martinkersner/cl-plot.git
```
### Initialization
Before running any code from *cl-plot* library, you must initialize plotting library with following code.
```lisp
(load "init")
```
> It is expected that [asdf](https://gitlab.common-lisp.net/asdf/asdf) is already loaded when `(load "init")` is executed.

## Documentation
Generating plots with *cl-plot* can be described with five steps.

1. Create a figure
2. Set figure properties
3. Select a plot type
4. Set plot properties
5. Visualize a plot

### Create a figure
### Set figure properties
#### `xlabel` and `ylabel`
Labels (`xlabel` and `ylabel`) require instance of figure as the first parameter. The second parameter represent text to display.

##### Example
```lisp
(xlabel *fig* "x-label")
(ylabel *fig* "y-label")
```

### Select a plot type
### Set plot properties

### Visualize a plot
Plot can be either displayed (`show`) or exported to image (`save`).

#### `show`
`show` takes only one argument; figure.
After `show` command is executed, plot is displayed with gnuplot window which lets you zoom-in, zoom-out and export plot to various formats.

##### Example
```lisp
(show *fig*)
```

#### `save`
`save` takes two mandatory arguments; figure and path for generated image. If you want to modify size of generated plot, you can supply width and height as two optional arguments.

> Currently, only PNG format is supported.

##### Example
```lisp
(save *fig* "fig.png")
(save *fig* "fig2.png" 600 400)
```

## Examples
There are several more examples within *examples* directory.

Following section explains how to create a scatter plot figure, give it a title and label both x and y axes of plot. Eventually, it shows a generated plot.

### Basic example (with explanation)
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

Set a description of *x* and *y* labels of plot. 
> `xlabel` and `ylabel` are both optional figure properties.
```lisp
(xlabel *fig* "x-label")
(ylabel *fig* "y-label")
```

Set a title of plot.
> `title` is an optional figure property.
```lisp
(title *fig* "Basic example")
```

Make figure of scatter plot type and connect it to generated data.
```lisp
(scatter *fig* *dataframe*)
```

Finally, display a plot.
```lisp
(show *fig*)
```

<p align="center">
<img src="http://i.imgur.com/04YOTQA.png" />
</p>

### Basic example (full code)
```lisp
(defparameter *dataframe*
  '((1 1)
    (2 2)
    (3 1.5)
    (4 3)
    (5 2.5)
    (6 4)))

(defparameter *fig* (make-instance 'figure))
(xlabel *fig* "x-label")
(ylabel *fig* "y-label")
(title *fig* "Basic example")

(scatter *fig* *dataframe*)

(show *fig*)
```

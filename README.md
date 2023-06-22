# Bin Pack

## eb-afit (in-progress)

eb stands for the paper's author Erhan Baltacioglu, and afit stands for Air
Force Institude of Technology.

`lotuc.binpack.eb-afit` implements the algorithm described in paper
[The Distributer's Three-Dimensional Pallet-Packing Problem: A Human Intelligence-Based Heuristic Approach](https://scholar.afit.edu/etd/4563/).

The reference implemention described in the paper can be found here at
https://github.com/wknechtel/3d-bin-pack .

## visualizer

The [visualizer](./visualizer/README.md) (which is deployed at
https://lotuc.org/bin-pack/) take the input text, find the best packing strategy
and visualize the packing result.

![](./visualizer/doc/resources/eb-afit-find-best-dpp06.gif)

The input text format is defined in the original paper, and you can find more
sample data [here](https://github.com/wknechtel/3d-bin-pack/tree/master/test).

> In the first line, the three numbers are the x, y, and z dimensions of the
> pallet. All subsequent lines contain box information. In each line,the first
> number is the box label. The second, third, and fourth numbers are the x, y,
> and z dimensions of each box type, respectively. The fifth number represents
> the number of boxes of the sametype.

Since the algorithm tries all orientations, all of the dimensions' order has no
importance.

Here is a sample input text:

```text
104, 96, 84
1. 70, 104, 24, 4
2. 14, 104, 48, 2
3. 40, 52, 36, 3
```

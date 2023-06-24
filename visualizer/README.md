# bin-pack visualizer

The visualizer is mainly built with
[@react-three/fiber](https://github.com/pmndrs/react-three-fiber) and
[@react-three/drei](https://github.com/pmndrs/drei).

You can find a deployed version of this visualizer at
https://lotuc.org/bin-pack/.

![](./visualizer/doc/resources/eb-afit-find-best-dpp06.gif)

It supports two kind of input, `eb-afit input` and `eb-afit visualdot`. You can
change the `packedBoxes` number (-1 shows all packed, 0 shows none, 1, shows
first packed, and so on) to replay the packing process.

## `eb-afit input` text

The input text format is defined in the original paper, and you can find more
sample data [here](https://github.com/wknechtel/3d-bin-pack/tree/master/test).

> In the first line, the three numbers are the x, y, and z dimensions of the
> pallet. All subsequent lines contain box information. In each line,the first
> number is the box label. The second, third, and fourth numbers are the x, y,
> and z dimensions of each box type, respectively. The fifth number represents
> the number of boxes of the sametype.

Since the algorithm tries all orientations, all of the dimensions' order has no
importance.

Here is a sample:

```text
104, 96, 84
1. 70, 104, 24, 4
2. 14, 104, 48, 2
3. 40, 52, 36, 3
```

## `eb-afit visualdot` text

The first line is the pallet dimensions, the rest lines are box's coordinates
and their dimensions.

Here is a sample:

```text
104   96   84
  0    0    0  104   24   70
  0    0   70  104   48   14
  0   24    0  104   24   70
  0   48    0  104   24   70
  0   48   70  104   48   14
  0   72    0  104   24   70
```

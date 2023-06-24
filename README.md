# Bin Pack

## eb-afit

eb stands for the paper's author Erhan Baltacioglu, and afit stands for Air
Force Institude of Technology.

`lotuc.binpack.eb-afit` implements the algorithm described in paper
[The Distributer's Three-Dimensional Pallet-Packing Problem: A Human Intelligence-Based Heuristic Approach](https://scholar.afit.edu/etd/4563/),
some of the implementation detail differences can be found
[here](./doc/eb-afit-explain.md).

The reference implemention described in the paper can be found here at
https://github.com/wknechtel/3d-bin-pack .

```clojure
(require '[lotuc.binpack.eb-afit :as eb-afit])

(eb-afit/find-best-pack
 {:pallet-volume 1000,
  :pallet-dims [10 10 10],
  :boxes [{:dims [10 10 10], :vol 1000, :n 1}],
  :box-volume 1000})

;; ->

{:packed-volume 1000,
 :packed-number 1,
 :pallet-variant [10 10 10],
 :percentage-used 100.0,
 :first-layer-thickness 10,
 :pack                                  ; ordered by packing order
 [{:pack-dims [10 10 10],
   :pack-coord [0 0 0],
   :dims [10 10 10],
   :vol 1000,
   :n 1}],
 :unpacked []}
```

## visualizer

The [visualizer](./visualizer/README.md) (which is deployed at
https://lotuc.org/bin-pack/) take the input text, find the best packing strategy
and visualize the packing result.

![](./visualizer/doc/resources/eb-afit-find-best-dpp06.gif)

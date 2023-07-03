[![Clojars Project](https://img.shields.io/clojars/v/org.lotuc/bin-pack.svg)](https://clojars.org/org.lotuc/bin-pack)

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

(eb-afit/find-best-pack {:pallet-dims [10 5 5],
                         :boxes [{:dims [5 5 5], :n 3}]})

;; ->

{:pallet-volume 250,
 :box-volume 375,
 :packed-volume 250,
 :packed-number 2,
 :pallet-variant [10 5 5],
 :percentage-used 100.0,
 :first-layer-thickness 5,
 :pack
 [{:pack-dims [5 5 5], :pack-coord [0 0 0], :dims [5 5 5], :vol 125, :n 3}
  {:pack-dims [5 5 5], :pack-coord [5 0 0], :dims [5 5 5], :vol 125, :n 3}],
 :unpacked {:dims [5 5 5], :vol 125, :n 3, :unpacked-n 1}}
```

## visualizer

The [visualizer](./visualizer/README.md) (which is deployed at
https://lotuc.org/bin-pack/) take the input text, find the best packing strategy
and visualize the packing result.

![](./visualizer/doc/resources/eb-afit-find-best-dpp06.gif)

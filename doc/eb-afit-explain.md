# eb-afit explained

Some of the dimension names are copied from the original paper, and are
explained well in the paper. Here are two graph for clarification:

<img src="./resources/eb-afit-01.svg" alt="eb-afit-01.svg" class="org-svg" width="400">

<img src="./resources/eb-afit-02.svg" alt="eb-afit-01.svg" class="org-svg" width="300">

Notice a major data structure difference (not that much difference) between this
one and the original one is the scrappad which describes the topology of the
edge of the current layer (in essense recording the points in the second graph),
the original implementation use a doubled link list and here I use a vector, and
thus the `smallest-z` becomes the index of the vector.

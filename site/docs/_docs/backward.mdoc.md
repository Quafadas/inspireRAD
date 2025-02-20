---
title: Backward Mode
---

So we got forward mode, essentially free. Sweet.

However, if you look into the code for `Jet`, what you'll see, is that it's tracking the gradient of every partial derivative, through every calculation. If you were to have a large number of input dimensions (e.g. training a neural net) this calculation gets expensive. I believe it to be O(n).

The motivation for reverse mode differentiation, is that it is O(1). 



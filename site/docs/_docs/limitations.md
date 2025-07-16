---
title: Limitations
---

Please note that this is an experimental project investigating a hard problem. If you choose to use this outside of places it's already tested, you should expect pain.

# Dimensions

Currently, a maximum of two dimensions. There's no inherent limitation to the design, but I don't currently have a viable higher dimensional datastructure available. I know what it looks like, but lack time. Let me know if you want to help :-). 

# Performance

I am not aware of a GPU integration layer for scala I can take advantage of. Therefore, this will be limited to small networks at curiosity scale.

On CPU, [vecxt](https://github.com/Quafadas/vecxt) itself is the essentially the backend, and should have pretty good performance characteristcs (note: inside the limitations of the JVM, vecxt uses the Vector API wherever possible, but it will probably get crushed by native code). inspireRAD has not been profiled. It is certain, that it is recklessly allocating memory, and so there is plenty of optimisation potential. it will get crushed by `Torch`, `JAX` or whatever professionally optimised, native code whizz-bangery is going on in python these days.

# Complexity

There may be weird bugs. InspireRAD essentially embeds a little mathematical compiler that is not-that-typesafe inside your calculation. At points, it does some `.asInstanceOf[]`. There is all sorts of funny type resolution taking place. Expect pain. 






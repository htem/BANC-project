#!/usr/bin/env python3
"""Approach 2 of the neck-as-bottleneck investigation: predict
inter-region connectivity from region properties and compare against
observed brain ↔ VNC connectivity.

The idea: fit a model of within-brain and within-VNC connectivity as a
function of region size, intrinsic-neuron count, and inter-region
distance. The fitted model is then asked to predict what brain ↔ VNC
connectivity would be if the neck connective had zero length. The gap
between predicted and observed quantifies the bottleneck.

Placeholder: implementation pending — see ``README.md``.
"""

raise NotImplementedError

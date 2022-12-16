"""Plot 3 columns (x, y, z) of data from a file. Currently hardcoded
to plot a file /tmp/data.txt."""

import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = plt.axes(projection="3d")

data = np.loadtxt("/tmp/data.txt", delimiter=" ")
ax.scatter(data[:,0], data[:,1], data[:,2], c=data[:,2])
plt.show()


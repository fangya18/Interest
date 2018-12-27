**Python Day 2: Christmas Tree**

Last time I went to pick up a real Christmas tree was with Little Charles. It was a really big tree, I have to hold it on the way back.

This year I don't have a real tree, so I made a Christmas tree with Python.

![image](http://upload-images.jianshu.io/upload_images/8699364-9ffdc2ccd77a315c.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

**Code:**

```
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt

theta = np.linspace(-8 * np.pi, 8 * np.pi, 300) 
z = np.linspace(-3, 0, 300)
r = 8
x = r * np.sin(theta)*z
y = r * np.cos(theta)*z

fig = plt.figure() # Create figure
ax = fig.gca(projection='3d') # It's a 3D Xmas tree!
ax.view_init(15, 0) # Set a nice view angle
ax._axis3don = False # Hide the 3d axes

ax.plot(x, y, z,
        c='green', linewidth=2.5)

# Every Xmas tree needs a star
ax.scatter(0, 0, 0.2,
           c='red', s=300, marker='*')

# Type here your best whishes
ax.set_title("Merry Christmas !")
plt.show()
```

The python code is inspired by https://gist.github.com/franktoffel/aea4329b760eb3e72f4d

#### **Happy Studying! 🎁**

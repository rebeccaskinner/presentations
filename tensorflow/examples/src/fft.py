import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec

import tensorflow as tf
from PIL import Image
import numpy

sess = tf.Session()
img = Image.open('george-small.jpg')
img = img.convert('L', (0.2989, 0.5870, 0.1140, 0))  # convert to gray scale
img = numpy.asarray(img, dtype='complex64')
mat = tf.constant(img)
fft = tf.fft2d(mat)
results = sess.run(fft)
# fftImg = Image.fromarray(results * 255).astype(numpy.unit8)
# fftImg.save('george-fft.jpg')

import tensorflow as tf
x = [[1,0,0],
     [1,1,0],
     [1,1,1]]
sess = tf.Session()
a = tf.constant(x)
b = tf.transpose(a)
print(sess.run(a + b))

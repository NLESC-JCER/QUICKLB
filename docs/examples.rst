Example usage
=============

To use QUICKLB it has to be imported first, if this fails please go to the
:ref:`installation` section.

.. code::

  import quicklb


Loadbalancer object
-------------------

The easies and quickest way to get started is with the Loadbalancer object. 

.. code::
  
  from quicklb import Loadbalancer


First we need to create some data which we want the loadbalancer to work on,
take note that we omit any distribution over multiple processors here, if this
is desired the mpi4py can be used to create an initial distribution.

.. code::

  import numpy as np

  A_concentration = np.zeros(1000, dtype=np.float64)
  B_concentration = np.ones(1000, dtype=np.float64)
  pressure        = np.zeros(1000, dtype=np.float64)
  
  

This gives us 1000 datapoints on each processor

Secondly we need a structure to encapsulate these points such that the
loadbalancer knows which data to communicate, how to encapsulate, and how to compute an interation 

.. code::

  class Cell():
    # Pointers to the actual information
    A_concentration: np.array = np.array([1.],dtype=np.float64)
    B_concentration: np.array = np.array([1.],dtype=np.float64)
    pressure       : np.array = np.array([0.],dtype=np.float64)

    def __init__(self,A,B,P)
      A_concentration = A
      B_concentration = B
      pressure        = pressure

    def compute(self):
      self.pressure[0] = self.A_concentration[0] * 0.25 + self.B_concentration[0] * 0.5 + 0.25

    def serialize(self):
      return numpy.concatenate( A.tobytes(), B.tobytes(), pressure.tobytes() )

    def deserialize(self, buffer):
      A_concentration[0] = buffer[0:4]
      B_concentration[0] = buffer[4:8]
      pressure[0]        = buffer[8:12]


This Cell class can either point to data when local, or store data when
offloaded, furthermore a compute function is provided

We must create a list of cells that we want loadbalanced

.. code::

  cells = [Cell(A_concentration[i:i+1], B_concentration[i:i+1],pressure[i:i+1]) for i in range(1000)]

Then we can create the Loadbalancer:

.. code::
  
  lb = Loadbalancer(cells, "SORT", 0.01, 0.01, 10)


And with this loadbalancer we can perform (re-)partitions and iterations to our
harts content:

.. code::
  number_of_iterations = 42
  for i in range(number_of_iterations):
    if i%10 == 0:
      lb.partition()
    lb.iterate()


We could also have separate physical processes which need to be calculated, some of which are very prone to
loadimbalance and are handled by the loadbalancers, while others are well
behaved and don't exhibit load imbalance:

.. code::
  number_of_iterations = 42
  for i in range(number_of_iterations):
    if i%10 == 0:
      lb.partition()
    # Calulate load imbalance prone stuff
    lb.iterate()

    # Calculate stuff without loadbalancing
    A_concentration = A_concentration * pressure
    B_concentration = B_concentration * pressure



..

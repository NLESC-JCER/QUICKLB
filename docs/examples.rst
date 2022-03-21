Example usage
=============

To use QUICKLB it has to be imported first, if this fails please go to the
:ref:`installation` section.

.. code::

  import quicklb


Unbalanced reaction diffusion with QUICKLB
------------------------------------------

In this example we will create a reaction diffusion simulation which uses a
loadbalancer for the chemistry calculation part (which is local), and a normal
non-loadbalanced process for the diffusion process. To unbalance the chemistry
process we will solve it at each timestep as a pair of stiff ode's


The easies and quickest way to get started is with the Loadbalancer object. 

.. code::
  
  from quicklb import Loadbalancer


We have to create our domain, and work arrays and divide them over the number
of processors, to reduce the amount of communication code necessary for the
non-loadbalanced part we use shared-memory array (which are a bit more complex
than normal numpy arrays). It also means that the example will only work if
all processors have acces to the same memoryspace.

.. code::

  import numpy as np
  from mpi4py import MPI
  comm = MPI.COMM_WORLD

  def allocate_shared_array(size):
    nbytes = np.prod(size) * MPI.DOUBLE.Get_size() 
    if comm.Get_rank() != 0:
      nbytes = 0
    win = MPI.Win.Allocate_shared(nbytes, MPI.DOUBLE.Get_size(),comm=comm)
    buf, _ = win.Shared_query(0)
    return np.ndarray(size,dtype=np.float64,buffer=buf)

  # Using shared array we cheat a bit to  make it simpler
  A = allocate_shared_array((1000,1000))
  B = allocate_shared_array((1000,1000))

  # Calculate which part of the shared array is "local" to this processor
  remainder = 1000%comm.Get_rank()
  start = 1000/comm.Get_rank() + min(comm.Get_rank(), remainder)
  end   = 1000/(comm.Get_rank() + 1) + min(comm.Get_rank()+1, remainder)

  A_local = A[:,start:end]
  B_local = B[:,start:end]

This gives us a slice on each processor

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

  cells = [Cell(A_local[i:i+1,j], B_local[i:i+1,j])
              for i in range(1000) for j in range(start,stop)]

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

This takes care of the loadbalancing part of this example, but only calculates
the chemistry on single cells, we still need to implement the diffusion to get a
working reaction diffusion system. We do that here:

.. code::

  import scipy as sp
  
  def diffuse(field,D):
    # diffusion with laplace
    laplace_kernel = np.array([[ .05, .2, 0.5],
                               [  .2, -1,  .2],
                               [ .05, .2, .05]])
    # Perform diffusion over the local field + boundaries (this is where we cheat with shared memory arrays
    newfield = sp.signal.convolve2d(field[:,start-1:stop+1],laplace_kernel, boundary='symm', mode='same')

    comm.Barrier()

    field[:,start:stop] = field[:,start:stop] + D*newfield[:,1,-1]

Then we have to change our main loop to include diffusion as well

.. code::

  number_of_iterations = 42
  for i in range(number_of_iterations):
    diffuse(A,XXX)
    diffuse(B,XXX)
    if i%10 == 0:
      lb.partition()
    lb.iterate()


Lastly we can create visualise this quite easily with matplotlib!

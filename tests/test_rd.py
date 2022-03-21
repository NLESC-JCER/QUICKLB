from quicklb import Loadbalancer
import numpy as np
import scipy.integrate
import scipy.signal
from mpi4py import MPI
import matplotlib.pyplot as plt
comm = MPI.COMM_WORLD

def allocate_shared_array(size):
  nbytes = np.prod(size) * MPI.DOUBLE.Get_size() 
  if comm.Get_rank() != 0:
    nbytes = 0
  win = MPI.Win.Allocate_shared(nbytes, MPI.DOUBLE.Get_size(),comm=comm)
  buf, _ = win.Shared_query(0)
  return np.ndarray(size,dtype=np.float64,buffer=buf), win

# Using shared array we cheat a bit to  make it simpler
size = 30
A, winA = allocate_shared_array((size,size))
B, winB = allocate_shared_array((size,size))

# Calculate which part of the shared array is "local" to this processor
remainder = size%comm.Get_size()
slice_size = size//comm.Get_size()
start = int(slice_size*comm.Get_rank() + min(comm.Get_rank(), remainder))
end   = int(slice_size*(comm.Get_rank() + 1) + min(comm.Get_rank()+1, remainder))

A_local = A[start:end,:]
B_local = B[start:end,:]

if comm.Get_rank() == 0:
  A[:,:] = 1.
  B[:,:] = 0.
  A[size//2-3:size//2+3,size//2-5:size//2+5] = 0.50
  B[size//2-3:size//2+3,size//2-5:size//2+5] = 0.25

class Cell():
  # Pointers to the actual information
  # A: np.array = np.array([float('nan')],dtype=np.float64)
  # B: np.array = np.array([float('nan')],dtype=np.float64)

  def __init__(self,AA=None,BB=None):
    self.A = np.array([float('nan')],dtype=np.float64)
    self.B = np.array([float('nan')],dtype=np.float64)
    if AA != None:
      self.A = AA
    if BB != None:
      self.B = BB

  def compute(self):
#    soln =scipy.integrate.solve_ivp(Cell.deriv,(0,1),(self.A[0],self.B[0]), method='Radau')
#    self.A[0] = self.A[0] + soln.y[0][-1]
#    self.B[0] = self.B[0] + soln.y[1][-1]
    AA = self.A[0]
    BB = self.B[0]
    self.A[0] = AA - AA*BB*BB + 0.0545*(1-AA)
    self.B[0] = BB + AA*BB*BB - (0.0545+0.062)*BB

  def deriv(t,y):
    a, b = y
    adot = a - a*b*b + 0.04 * ( 1 - a ) 
    bdot = b + a*b*b - ( 0.04 + 0.06 ) * b
    return adot, bdot

  def serialize(self):
    ans = np.concatenate( (self.A.view(dtype=np.byte),
                           self.B.view(dtype=np.byte)))
    return ans

  def deserialize(self, buffer):
      
    self.A[0] = buffer[0:8].view(dtype=np.float64)
    self.B[0] = buffer[8:16].view(dtype=np.float64)

cells = [Cell(A_local[i,j:j+1], B_local[i,j:j+1])
         for j in range(size) for i in range(A_local.shape[0])]

lb = Loadbalancer(cells, "SORT", 0., 0., 10)


A_diff = np.empty(shape=(A_local.shape[0]+2,A_local.shape[1]+2))
B_diff = np.empty(shape=(B_local.shape[0]+2,B_local.shape[1]+2))
def diffuse():
  A_diff[1:-1,1:-1] = A_local
  # local periodic boundaries
  A_diff[0,1:-1] = A[start-1,:]
  A_diff[-1,1:-1] =A[end%A.shape[0],:]
  # "communicate" boundaries,
  A_diff[1:-1,0] = A_local[:,-1]
  A_diff[1:-1,-1] =A_local[:,0]

  B_diff[1:-1,1:-1] = B_local
  # local periodic boundaries
  B_diff[0,1:-1] = B[start-1,:]
  B_diff[-1,1:-1] =B[end%B.shape[0],:]
  # "communicate" boundaries,
  B_diff[1:-1,0] = B_local[:,-1]
  B_diff[1:-1,-1] =B_local[:,0]

  LA       =                       A_diff[ :-2, 1:-1] + \
             A_diff[1:-1, :-2] - 4*A_diff[1:-1, 1:-1] + A_diff[1:-1, 2:] + \
                               +   A_diff[2:  , 1:-1] 
  LB       =                       B_diff[ :-2, 1:-1] + \
             B_diff[1:-1, :-2] - 4*B_diff[1:-1, 1:-1] + B_diff[1:-1, 2:] + \
                               +   B_diff[2:  , 1:-1] 
  return LA,LB


number_of_iterations = 100
for i in range(number_of_iterations):
  # Perform diffusion over the local field + boundaries (this is where we cheat with shared memory arrays
  winA.Sync()
  winB.Sync()
  LA,LB = diffuse()

  # Chemistry
  if i%10 == 0:
    lb.partition()
    lb.partitioning_info()
  lb.iterate()

  A_local[:,:] = A_local + 0.1*LA
  B_local[:,:] = B_local + 0.05*LB

  if i%40 == 0:
  
    if comm.Get_rank() == 0:
      plt.imshow(B)
      plt.show(block=False)
      plt.pause(0.5)

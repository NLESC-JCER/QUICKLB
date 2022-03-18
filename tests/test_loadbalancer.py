from quicklb import Loadbalancer
from time import sleep
import random
import struct
from mpi4py import MPI
rank = MPI.COMM_WORLD.Get_rank()

class Cell():
  """Cell class which represents and contains our data"""
  difficulty: int = 1
  processor: int = rank
  id: int = -1
  step: int = 1

  def difficulty_init(self):
    rand = random.randint(0,100)
    if (rand > 100-rank*4):
      self.difficulty = rand

  def compute(self):
    sleep(self.difficulty/1000.) #sleep in miliseconds/10
    self.step +=1

  def serialize(self):
    return struct.pack('llll',self.difficulty,self.step,self.id, self.processor)

  def deserialize(self, buffer):
    self.difficulty, self.step, self.id, self.processor = struct.unpack('llll',buffer.tobytes())


# Set up our data
cells = [Cell() for _ in range(40)]
for i in range(len(cells)):
  cells[i].difficulty_init()
  cells[i].id = i


# Create a loadbalancer four our data
# Partitioner: Greedy
# Max_abs_li: 0.0
# Max_rel_li: 0.0 (not used for GREEDY partitioner
# Max_it: maximum number of iterations taken by partitioner
lb = Loadbalancer(cells,"GREEDY",0.0,0.0,10)

# Our main time iterating loop
for i in range(10):
  if rank == 0:
    print("Iteration ", i)
  lb.partition()
  lb.partitioning_info()
  lb.iterate()

#check resulting values
for i in range(len(cells)):
  if cells[i].processor != MPI.COMM_WORLD.Get_rank():
    print("Processor Value: ", cells[i].processor, " Expected: ", MPI.COMM_WORLD.Get_rank())
    print("Failed")
    break
  if cells[i].id != i:
    print("ID Value: ", cells[i].id, " Expected: ", i)
    print("Failed")
    break

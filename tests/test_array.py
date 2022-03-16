import quicklb
import numpy as np
from mpi4py import MPI
from dataclasses import dataclass
from time import sleep
from sys import getsizeof
import random
import argparse
import struct
random.seed(MPI.COMM_WORLD.Get_rank())

@dataclass
class Cell():
  """Cell that needs some work to be done"""
  difficulty: int = 1
  step: int = 1
  id: int = -1
  processor: int = MPI.COMM_WORLD.Get_rank()
  offloaded: bool = False

  def difficulty_init(self):
    rand = random.randint(0,100)
    if (rand > 100-MPI.COMM_WORLD.Get_rank()*2):
      self.difficulty = rand

  def compute(self):
    sleep(self.difficulty/10000.) #sleep in miliseconds/10
    self.step +=1

  def serialize(self):
    self.offloaded = True
    return np.frombuffer(
        struct.pack('llll',self.difficulty,self.step,self.id,self.processor)
        , dtype=np.byte)

  def deserialize(self, buffer):
    self.difficulty, self.step, self.id, self.processor = struct.unpack('llll',buffer.tobytes())

class Grid():
  def __init__(self, size):
    self.cells = [ Cell() for x in range(size)]
    for i in range(len(self.cells)):
      self.cells[i].difficulty_init()
      self.cells[i].id = i
    self.remote_cells = []

  def serialize(self,id):
    return self.cells[id].serialize()

  def deserialize(self,buffer,id):
    self.cells[id].deserialize(buffer)

  def add_remote_cell(self, cell):
    self.remote_cells.append(cell)

  def update_difficulty(self):
    for cell in self.cells:
      what = random.random()
      if what < 0.25:
        cell.difficulty = max(cell.difficulty - 5,1)
      elif what < 0.5:
        cell.difficulty = min(200,cell.difficulty+5)
      elif what < 1:
        pass

  def compute(self):
    for cell in self.cells:
      if not cell.offloaded:
        cell.compute()
    for cell in self.remote_cells:
      cell.compute()

  def localize(self):
    for cell in self.cells:
      cell.offloaded = False
    self.remote_cells = []
    

# option to not use loadbalancer
parser = argparse.ArgumentParser(description="Python test of QUICKLB")
parser.add_argument('--nolb', default=False, action='store_true',
                    help="option to disable loadbalancing")

args = parser.parse_args()
loadbalance = not args.nolb

#Lets have 500 cells per processor
numcells = 500
grid = Grid(numcells)

#create our loadbalancer
cell_size = len( Cell().serialize())
lb = quicklb.create(cell_size,cell_size,numcells,quicklb.init())

#set the desired partitioning algorithm
quicklb.set_partition_algorithm(lb, 'SORT', .0,.0,10)


#Callback function needs to be unique for grid instance
def serialize_data_cell(buffer, ids, buffer_size, ids_size):
  for i in range(ids_size):
    buffer[:,i] = grid.serialize(ids[i]-1)
  return buffer

def deserialize_data_cell(buffer, ids, buffer_size, ids_size):
  for i in range(ids_size):
    cell = Cell()
    cell.deserialize(buffer[:,i])
    grid.add_remote_cell(cell)

def serialize_result_cell(buffer, ids, buffer_size, ids_size):
  for i in range(ids_size):
     buffer[:,i] = grid.remote_cells[i].serialize()
  return buffer

def deserialize_result_cell(buffer, ids, buffer_size, ids_size):
  for i in range(ids_size):
    grid.deserialize(buffer[:,i], ids[i]-1)

#print info
quicklb.info(lb)

for iteration in range(10):
  if MPI.COMM_WORLD.Get_rank() == 0:
    print("Iteration ", iteration)
  # Update difficulty randomly
  grid.update_difficulty()

  if loadbalance:
    # Update weights and run partitioning algorithm, and output it to loadbalance.info
    weights = np.array([ cell.difficulty for cell in grid.cells],dtype=np.float32)
    quicklb.set_weights(lb,weights)
    quicklb.partition(lb)
    quicklb.partitioning_info(lb,False)

    quicklb.communicate_data(lb, serialize_data_cell, deserialize_data_cell)

  grid.compute()

  if loadbalance:
    quicklb.communicate_result(lb, serialize_result_cell, deserialize_result_cell)
    grid.localize()

  MPI.COMM_WORLD.Barrier()

#check values
for i in range(len(grid.cells)):
  if grid.cells[i].processor != MPI.COMM_WORLD.Get_rank():
    print("Processor Value: ", grid.cells[i].processor, " Expected: ", MPI.COMM_WORLD.Get_rank())
    print("Failed")
    break
  if grid.cells[i].id != i:
    print("ID Value: ", grid.cells[i].id, " Expected: ", i)
    print("Failed")
    break

import quicklb
import numpy as np
from mpi4py import MPI
from dataclasses import dataclass
import pickle
from time import sleep
from sys import getsizeof
import random
import argparse
random.seed(MPI.COMM_WORLD.Get_rank())

@dataclass
class Cell():
  """Cell that needs some work to be done"""
  difficulty: int = 1
  step: int = 1
  offloaded: bool = False

  def compute(self):
    sleep(self.difficulty/1000.) #sleep in miliseconds/10
    self.step +=1

  def serialize(self):
    self.offloaded = True
    return np.frombuffer(pickle.dumps(self),dtype=np.byte)

class Grid():
  def __init__(self, size):
    self.cells = [ Cell() for x in range(size)]
    self.remote_cells = []

  def serialize(self,id):
    return self.cells[id].serialize()

  def deserialize(self,buffer,id):
    self.cells[id] = pickle.loads(buffer)

  def add_remote_cell(self, cell):
    self.remote_cells.append(cell)

  def update_difficulty(self):
    for cell in self.cells:
      rand = random.randint(0,100)
      if rand > 95:
        cell.difficulty = min(100,cell.difficulty+rand)
      else:
        cell.difficulty = max(cell.difficulty - 5,1)

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

#Lets have 1000 cells divided by the number of processors
numcells = int(1000/MPI.COMM_WORLD.Get_size())
grid = Grid(numcells)

#create our loadbalancer
cell_size = len( pickle.dumps(Cell()))
lb = quicklb.create(cell_size,cell_size,numcells,quicklb.init())

#set the desired partitioning algorithm
quicklb.set_partition_algorithm(lb, 'GREEDY', .0,.0,10)


#Callback function needs to be unique for grid instance
def serialize_data_cell(buffer, id, buffer_size):
  buffer[:] = grid.serialize(id-1)
  return buffer

def deserialize_data_cell(buffer, id, buffer_size):
  grid.add_remote_cell(pickle.loads(buffer))

def serialize_result_cell(buffer, id):
  buffer[:] = grid.remote_cells[id-1].serialize()
  return buffer

def deserialize_result_cell(buffer, id):
  grid.deserialize(buffer, id-1)

#print info
quicklb.info(lb)

for iteration in range(10):
  if MPI.COMM_WORLD.Get_rank() == 0:
    print("Iteration ", iteration)
  # Update difficulty randomly
  grid.update_difficulty()
  # Update weights and run partitioning algorithm, and output it to loadbalance.info
  weights = np.array([ cell.difficulty for cell in grid.cells],dtype=np.float32)

  if loadbalance:
    quicklb.set_weights(lb,weights)
    quicklb.partition(lb)
    quicklb.partitioning_info(lb,True)

    quicklb.communicate_data(lb, serialize_data_cell, deserialize_data_cell)

  grid.compute()

  if loadbalance:
    quicklb.communicate_result(lb, serialize_result_cell, deserialize_result_cell)
    grid.localize()

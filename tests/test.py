import quicklb
import numpy as np
from mpi4py import MPI
lb = quicklb.create(8,4,5,1,quicklb.init())
quicklb.set_partition_algorithm(lb, 'GREEDY', .0,.0,10)

weights = np.array([5.,4.,3.,2.,1.],dtype=np.float32)
weights = weights*(MPI.COMM_WORLD.Get_rank()+1)
quicklb.set_weights(lb,weights)
quicklb.partition(lb)
quicklb.info(lb)
quicklb.partitioning_info(lb,True)

def callback(buffer, id):
  print(buffer, id)
  buffer[:] = 43
quicklb.set_export_import_routines(lb,callback,callback,callback,callback)

#quicklb.communicate_data(lb)


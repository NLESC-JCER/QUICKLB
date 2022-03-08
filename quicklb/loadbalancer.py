import quicklb
import pickle
import numpy as np
import time

class Loadbalancer():
  def __init__(self,objects, algorithm, max_abs_li, max_rel_li, max_it):
    self.comm = quicklb.init()

    #Check if objects is list-like
    #Check if its all the same object

    self.cell_class = objects[0].__class__

    self.objects = objects
   
    self.weights = np.array([1.]*len(objects),dtype=np.float32)
    self.offloaded  = [False for _ in range(len(objects))]

    self.object_size = len( objects[0].serialize())
    self.weight_size = len( self.weights[0].tobytes())
    self.remote_objects = []


    self.lb = quicklb.create(self.object_size, self.object_size 
        + self.weight_size, len(objects), self.comm)

    quicklb.set_partition_algorithm(self.lb, algorithm, max_abs_li, max_rel_li, max_it)
    quicklb.info(self.lb)

  def serialize_data_object(self,buffer, ids, buffer_size, ids_size):
    for i in range(len(ids)):
      id = ids[i]-1
      buffer[:,i] = np.frombuffer(self.objects[id].serialize(),dtype=np.byte)
      self.offloaded[id] = True
    return buffer

  def deserialize_data_object(self,buffer, ids, buffer_size, ids_size):
    for i in range(ids_size):
      cell = self.cell_class()
      cell.deserialize(buffer[:,i])
      self.remote_objects.append(cell)

  def serialize_result_object(self,buffer, ids, buffer_size, ids_size):
    for i in range(ids_size):
       id = i
       buffer[:self.object_size,i] = np.frombuffer(self.remote_objects[id].serialize(),dtype=np.byte)
       buffer[self.object_size:,i] = np.frombuffer(self.remote_weights[id],dtype=np.byte)
    return buffer

  def deserialize_result_object(self,buffer, ids, buffer_size, ids_size):
    for i in range(ids_size):
      id = ids[i]-1
      self.objects[id].deserialize(buffer[:self.object_size,i])
      self.weights[id] = np.frombuffer(buffer[self.object_size:,i],dtype=np.float32)

  def iterate(self):
    quicklb.communicate_data(self.lb
        , lambda buffer,ids,buffer_size,ids_size:
            Loadbalancer.serialize_data_object(self,buffer,ids,buffer_size,ids_size) 
        , lambda buffer,ids,buffer_size,ids_size:
            Loadbalancer.deserialize_data_object(self,buffer,ids,buffer_size,ids_size) 
    )

    self.remote_weights = np.empty(len(self.objects),dtype=np.float32)

    for i in range(len(self.objects)):
      if self.offloaded[i]:
        continue
      start = time.monotonic()
      self.objects[i].compute()
      self.weights[i] = time.monotonic() - start
    for i in range(len(self.remote_objects)):
      start = time.monotonic()
      self.remote_objects[i].compute()
      self.remote_weights[i] = time.monotonic() - start

    quicklb.communicate_result(self.lb
        , lambda buffer,ids,buffer_size,ids_size:
            Loadbalancer.serialize_result_object(self,buffer,ids,buffer_size,ids_size) 
        , lambda buffer,ids,buffer_size,ids_size:
            Loadbalancer.deserialize_result_object(self,buffer,ids,buffer_size,ids_size) 
    )

    # Restore lists
    self.remote_objects = []
    self.offloaded  = [False for _ in range(len(self.objects))]

  def partition(self):
    quicklb.set_weights(self.lb,self.weights)
    quicklb.partition(self.lb)

  def partitioning_info(self,detailed=False):
    quicklb.partitioning_info(self.lb,detailed)

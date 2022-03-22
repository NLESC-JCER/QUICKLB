import quicklb
import numpy as np
import time
import copy

class Loadbalancer():
  def __init__(self,objects, algorithm, max_abs_li, max_rel_li, max_it):
    """
    Create a Loadbalancer object

    Parameters
    ----------
    objects: list<Cell>
        A list of objects which implement all functions specified in the
        mock **Cell** object.
    algorithm: string
        Specifies the loadbalancing algorithm, valid values are:
        - `"GREEDY"`
        - `"SORT"`
        - `"SORT2"`
    max_abs_li: float
        specify the maximum absolute load-imbalance for the partitioning
        algorithm. When this threshold is reached the partitioning concludes
        `0.0` would be no load imbalance, `1.` would be a load imbalance of
        100%
    max_rel_li: float
        specify the maximum relative load-imbalance for the partitioning
        algorithm. Is not used in the GREEDY partitioning scheme. When this
        threshold is reached the partitioning concludes.
    max_it: int
        Maximum number of iterations for the loadbalancing algorithm. Set this
        somewhere between 1 and the number of processors used (or larger, it is
        limited to the maximum number of processors anyway)

    Returns
    -------
    Loadbalancer:
      A very fresh loadbalancing object !
    """
    self.cell_class = type(objects[0])

    self.objects = objects
   
    self.weights = np.array([1.]*len(objects),dtype=np.float32)
    self.offloaded  = [False for _ in range(len(objects))]

    self.object_size = len( objects[0].serialize())
    self.weight_size = len( self.weights[0].tobytes())
    self.remote_objects = []


    self.lb = quicklb.create(self.object_size, self.object_size 
        + self.weight_size, len(objects), quicklb.init())

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
      self.remote_objects.append(copy.deepcopy(self.cell_class()))
      self.remote_objects[-1].deserialize(buffer[:,i])

  def serialize_result_object(self,buffer, ids, buffer_size, ids_size):
    for i in range(ids_size):
       buffer[:self.object_size,i] = np.frombuffer(self.remote_objects[i].serialize(),dtype=np.byte)
       buffer[self.object_size:,i] = np.frombuffer(self.remote_weights[i],dtype=np.byte)
    return buffer

  def deserialize_result_object(self,buffer, ids, buffer_size, ids_size):
    for i in range(ids_size):
      id = ids[i]-1
      self.objects[id].deserialize(buffer[:self.object_size,i])
      self.weights[id] = np.frombuffer(buffer[self.object_size:,i],dtype=np.float32)

  def iterate(self):
    """
    Perform a single iteration, with computation offloading.
    this eventually calls **compute** on every single cell

    Returns
    -------
    None

    """
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
    """
    Call this function to (re)-partition the cells over the processors, it is
    recommended to call this once before **iterate()**

    Returns
    -------

    None
    """
    quicklb.set_weights(self.lb,self.weights)
    quicklb.partition(self.lb)

  def partitioning_info(self,detailed=False):
    """
    Writes out partitioning info to the loadbalance.info file

    Parameters
    ----------
    detailed: bool
              When set to `True` write detailed information about every cell to
              loadbalance.info

    Returns
    -------
    None
    """
    quicklb.partitioning_info(self.lb,detailed)

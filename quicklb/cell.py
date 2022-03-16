import struct

class Cell():
  """Example Cell for the Loadbalancer class that needs some work to be done"""
  data: float = 42.

  def compute(self):
    """
    Function which is called whenever work needs to be done, as this cell can be
    offloaded at the time of computation, it is recommended to only use
    (de)serialized data for the computation, otherwise results will be undefined
    """
    self.data = 1./data

  def serialize(self):
    """
    Function which serializes the object.

    Returns
    -------
    Bytes-like object
    """

    return struct.pack('d',self.data)

  def deserialize(self, buffer):
    """
    Function which deserializes the object.

    Parameters
    ----------
    buffer: 1D numpy bytes array

    Returns
    -------
    None
    """

    self.data = struct.unpack('d',buffer.tobytes())

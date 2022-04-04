.. _examples:

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
non-loadbalanced process for the diffusion process.

The easies and quickest way to get started is with the Loadbalancer object. 

.. literalinclude:: ../tests/test_rd.py
  :lines: 1

We have to create our domain, and work arrays and divide them over the number
of processors, to reduce the amount of communication code necessary for the
non-loadbalanced part we use shared-memory array (which are a bit more complex
than normal numpy arrays). It also means that the example will only work if
all processors have acces to the same memoryspace.

.. literalinclude:: ../tests/test_rd.py
  :lines: 2-27

This gives us a slice on each processor which we fill up with an initial
condition:

.. literalinclude:: ../tests/test_rd.py
  :lines: 29-33


Secondly we need a structure to encapsulate these points such that the
loadbalancer knows which data to communicate, how to encapsulate, and how to compute an interation 

.. literalinclude:: ../tests/test_rd.py
  :lines: 35-67

This Cell class can either point to data when local, or store data when
offloaded, furthermore a compute function is provided

We must create a list of cells that we want loadbalanced

.. literalinclude:: ../tests/test_rd.py
  :lines: 70-71

Then we can create the Loadbalancer:

.. literalinclude:: ../tests/test_rd.py
  :lines: 73


As an intermezzo we also need a diffusion operator which is separate from the
loadbalancer:

.. literalinclude:: ../tests/test_rd.py
  :lines: 76-102

And with this loadbalancer + diffusion operator we can iterate to our
heart's content:

.. literalinclude:: ../tests/test_rd.py
  :lines: 105-125

Lastly we can create visualise this quite easily with matplotlib!

.. literalinclude:: ../tests/test_rd.py
  :lines: 127-

.. image:: ../images/gray_scott_rd.png

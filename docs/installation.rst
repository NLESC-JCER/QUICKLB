.. _installation:

Installation
============

Quick and easy installation can be done with conda by creating an environment from the included conda_env.yml file and installing quicklb into this environment:

.. code:: bash

  $ conda create -c conda-forge -n quicklb --file conda_env.yml
  $ conda activate quicklb
  $ python setup.py install

Manual installation
-------------------

Make sure you have the folling packages available:

- cmake **>= 3.22**
- gfortran **>= 11.2** or ifort **>= 2022.1**
- openmpi **>= 4.1.2**

For python functionality:

- python **>= 3.9**
- numpy **>= 1.20**
- mpi4py **>= 3.1.1**
- matplotlib **>= 3.5.1**
- scipy **>= 1.8**

The library can be created through cmake directly:

.. code:: bash
  
  $ mkdir build
  $ cd build
  $ cmake ../ -DFortran_COMPILER=mpif90 -DCMAKE_BUILD_TYPE={RELEASE,DEBUG}  -DPYTHON_INTERFACE={off,on}

Or it can be created throught the python setuptools:

.. code:: bash
  
  $ python setup.py install

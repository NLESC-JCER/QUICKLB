---
title: 'QUICKLB: a quick loadbalancer for unbalanced HPC applications'
tags:
  - Python
  - Fortran
  - Load balancing
  - Fluid dynamics
  - High performance computing
authors:
  - name: Victor Azizi
    orcid: 
    affiliation: "1"
  - name: Gijs van den oord
    orcid: 
    affiliation: "1"
  - name: Mohamad Fathi
    orcid: 
    affiliation: "2"
  - name: Stefan Hickel
    orcid: 
    affiliation: "2"
affiliations:
 - name: Netherlands Escience Center, Amsterdam, The Netherlands
   index: 1
 - name: Aerodynamics Group, TU Delft, The Netherlands
   index: 2
date: 31 May 2022
bibliography: paper.bib
---

# Summary

`QUICKLB` is a quick loadbalancer for unbalanced HPC applications. QUICKLB is
a very efficient library that sacrifices optimality for speed. It shines in 
distributed applications that have unpredictable unbalanced computational peaks
in their domains. The loadbalancer is written in Fortran with a Python wrapper
to call it. Thus it is suitable for both Python and Fortran HPC applications.
The loadbalancer can be "bolted on" already existing applications, by pointing it 
to the data and compute functions, QUICKLB will then take care of the computation 
offloading. 

# Statement of need


# Acknowledgements


# References

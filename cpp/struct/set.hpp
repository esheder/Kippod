// << set.hpp >>
// This header is for a XSSet, which is a container of microscopic nuclear data information for isotopes in a specific condition.
// All isotopes that are not followed are dumped into the residual isotope, which is always at concentration 1.0

#ifndef _SET_H_
#define _SET_H_

// #include <isotopes.hpp>
// #include <sparse.hpp>

#include <map>
#include <vector>

class Isotope ; // Forward declaration

class SparseMat ; // Forward declaration

class MicroSet {
public:
  MicroSet() ; //Todo: decide on a constructor for isotopic cross section sets
  //Todo: Write ways to get read only versions of the cross sections that should be protected in the implementation.

};

class XSSet {
public:
  std::map<Isotope, MicroSet> isosets ;
  XSSet() ; //Todo: decide on a constructor for cross section sets
  
};
#endif

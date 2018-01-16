// << set.hpp >>
// This header is for a XSSet, which is a container of microscopic nuclear data information for isotopes in a specific condition.
// All isotopes that are not followed are dumped into the residual isotope, which is always at concentration 1.0

#ifndef _SET_H_
#define _SET_H_

// #include "isotope.hpp"
// #include <sparse.hpp>

#include <map>
#include <vector>
#include <memory>

using namespace std ;

class Isotope ; // Forward declaration
class sparseV ;

typedef vector<sparseV> SparseMat ;
typedef double DMat[3][3] ;

class MicroSet {
private: //This data is currently private because no using application should be able to change it
    vector<double>  totalXS_ ;
    SparseMat sM_ ;
    vector<double> chi_ ;
    vector<double> nF_ ;
    vector<double>  absXS_ ;
public:
    MicroSet(const vector<double>& tXS, const vector<double>& chi, const vector<double>& nfXS,
	     const SparseMat& sM) ; //Todo: decide on a constructor for isotopic cross section sets
    //Todo: Write ways to get read only versions of the cross sections that should be protected in the implementation.
    
};

class XSSet {
public:
    map<Isotope, MicroSet> isosets ;
    XSSet(const vector<Isotope>& isos, const vector<MicroSet>& sets) ; //Todo: decide on a constructor for cross section sets
    
};
#endif

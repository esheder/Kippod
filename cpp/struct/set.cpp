// << set.cpp >>
// Implementation of the set.hpp header.
//

#include "set.hpp"
#include "sparsevec.hpp"
//#include "sparse.hpp"
#include <memory>
#include <cassert>

MicroSet :: MicroSet(const vector<double>& tXS, const vector<double>& chi, const vector<double>& nfXS, const SparseMat& sM) : totalXS_(tXS),sM_(sM),chi_(chi),nF_(nfXS),absXS_()
{
    assert(tXS.size() == chi.size() && tXS.size() == nfXS.size() &&
	   tXS.size() == sM.size()) ;
    absXS_.reserve(tXS.size());
    for (int i=0; i<tXS.size(); i++)
    {
	absXS_[i] = tXS[i] - sM[i][i] ;
    }
}

XSSet :: XSSet(const vector<Isotope>& isos, const vector<MicroSet>& sets)
{
    assert(isos.size() == sets.size());
    for (int i=0; i<isos.size(); i++)
    {
	isosets[isos[i]] = sets[i] ;
    }
}

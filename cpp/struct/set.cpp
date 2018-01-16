// << set.cpp >>
// Implementation of the set.hpp header.
//

#include "set.hpp"
//#include "sparse.hpp"
#include <memory>
#include <cassert>

MicroSet :: MicroSet(const vector<double>& tXS, const vector<double>& chi, const vector<double>& nfXS, const SparseMat& sM) : totalXS_(tXS),sM_(sM),chi_(chi),nF_(nfXS),absXS_()
{
    assert(tXS.size() != chi.size() || tXS.size() != nfXS.size() ||
	   tXS.size() != sM.size() || tXS.size() != D.size()) ;
    absXS_.reserve(tXS.size())
    for (int i=0; i<res.size(); i++)
    {
	absXS_[i] = tXS[i] - sM[i][i]
    }
}

vector<double> totToAbs (const vector<double>& tXS, const SparseMat& sM)
{
    //Assumes vectors are of the same length. Should be assured by caller.
    vector<double> res(tXS.size());
    return res;
}

XSSet :: XSSet(const vector<Isotope>& isos, const vector<MicroSet>& sets)
{
    assert(
    for (int i=0; i<size(isos); i++)
    {
	isosets[isos[i]] = sets[i] ;
    }
}

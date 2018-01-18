// << sparsevec.cpp >>
// Implementation file for sparse vector containers. Might someday be replaced by libraries.
//

#include "sparsevec.hpp"
#include <cassert>
#include <stdexcept>
//#include <numeric>
//#include <algorithm>
using std::begin ;
using std::end ;

#include <iostream>
using namespace std ;

sparseV :: sparseV() : v_(), start_(0) {}
sparseV :: sparseV(const vector<double>& vals, const int& first) : v_(vals),start_(first) {}
int sparseV :: length() const {return v_.size();}
int sparseV :: begin() const {return start_;}
int sparseV :: end() const {return v_.size() + start_;}
int sparseV :: size() const {return v_.size() + start_;}
double sparseV :: operator[] (const int& i) const
{
    return (i < start_ || i >= start_+v_.size() ? 0. : v_[i-start_]);
}

double operator*(const sparseV& v1, const vector<double>& v2)
{
    if (v1.size() > v2.size()) throw out_of_range("Dot-product only defined for equal length vec") ;

    //TODO: Needs to be done using std::inner_product but I can't figure out how to make sparseV a proper C++ iterator. It demands so much!
    double res=0. ;
    for (int i=v1.begin(); i<v1.size(); i++)
    {
	res += v1[i] * v2[i] ;
    }
    return res ;
}

double operator*(const vector<double>& v1, const sparseV& v2) {return v2*v1;}
double operator*(const sparseV& v1, const sparseV& v2)
{
    int s = std::max(v1.begin(), v2.begin());
    int e = std::min(v1.end(), v2.end());
    double res=0. ;
    for (int i=s; i<e; i++)
    {
	res += v1[i]*v2[i] ;
    }
    return res ;
}
    

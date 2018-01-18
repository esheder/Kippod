// << sparsevec.hpp >>
// A header for sparse vector containers. These are going to be wildly useful. It might be best at some point to use a library for these, but right now I'm keeping everything close to home.
//

#ifndef _SPARSEVEC_H_
#define _SPARSEVEC_H_

#include <vector>
using namespace std ;

class sparseV
{
private:
    std::vector<double> v_ ;
    int start_ ;
public:
    sparseV(const vector<double>& vals, const int& first=0);
    sparseV();   
    int size() const; // The last index
    int length() const; //Actual values in vector
    int begin() const; // first nonzero index
    int end() const; // alias to size
    double operator[] (const int& i) const;
    
};
double operator*(const sparseV& v1, const vector<double>& v2) ;
double operator*(const vector<double>& v2, const sparseV& v1) ;
double operator*(const sparseV& v1, const sparseV& v2) ;

#endif

// <<sparsevtest.hpp>>
// Test suite for sparse vectors

#define CXXTEST_HAVE_STD
#include <cxxtest/TestSuite.h>
#include "../sparsevec.hpp"
#include <vector>
#include <stdexcept>
#include <iostream>

using namespace std ;

static const double arr1[4] = {1.0 ,2.0 ,3.0 ,4.0 } ;
static const double arr2[10] = {1., 1., 1., 1., 1., 1., 1., 1., 1., 1.} ;
class testSparseVec : public CxxTest::TestSuite
{
public:
    void testCreation(void)
	{
	    vector<double> v (arr1, arr1+sizeof(arr1)/sizeof(arr1[0])) ;
	    sparseV s(v, 5) ;
	    TS_ASSERT_EQUALS(s[3], 0.) ;
	    TS_ASSERT_EQUALS(s[6], 2.) ;
	    TS_ASSERT_EQUALS(s.size(), 9) ;
	    TS_ASSERT_EQUALS(s.length(), 4) ;
	    TS_ASSERT_EQUALS(s.begin(), 5) ;
	    TS_ASSERT_EQUALS(s.size(), s.end()) ;
	}
    void testProduct(void)
	{
	    vector<double> v2 (arr2, arr2+sizeof(arr2)/sizeof(arr2[0])) ; 
	    vector<double> v (arr1, arr1+sizeof(arr1)/sizeof(arr1[0])) ;
	    sparseV s(v, 5) ;
	    TS_ASSERT_EQUALS(s * s, 30.) ;
	    TS_ASSERT_THROWS(s * v, out_of_range) ;
	    TS_ASSERT_EQUALS(s * v2, 10.) ;
	    TS_ASSERT_EQUALS(s * v2, v2 * s) ;
	}
};
    

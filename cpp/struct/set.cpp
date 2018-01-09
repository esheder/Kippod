// << set.cpp >>
// Implementation of the set.hpp header.
//

#include <set.hpp>

class Microset {
private: //This data is currently private because no using application should be able to change it
  std::shared_ptr< double[3][3] > DiffusionCoeff_ ; //3x3 tensor for every energy
  std::shared_ptr<double> totalXS_ ;
  std::shared_ptr<double> absXS_ ;
  sparseMat sM_ ;
  std::shared_ptr<double> chi_ ;
  std::shared_ptr<double> nF_ ;
}

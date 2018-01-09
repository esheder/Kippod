// << rod.hpp >>
// This header is for a rod object, which would behave as a nuclear fuel assembly rod.
// Style is header-should-be-small

#include <components.hpp>
#include <vector>

class Rod {
protected:
  std::vector<Component> components ;
  std::vector<double> comp_widths ;
  double height ;
  
public:
  Rod(double h) ;
  assemble(Component* cs, double* widths) ;
  
};

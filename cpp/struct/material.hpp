// << material.hpp >>
// This header is for a material object, which is a homogenically smeared material in an assembly.
// Basically, think of this as a pack of fuel smeared with water and the cladding or a spring inside the cladding that is smeared with the water and so on.
// Unlike a component, this object is not physical, and does not contain actual concentrations.
// The component object contains the material it is made out of, but has other uses.
// Style is header-should-be-small

#include <set.hpp>
#include <condition.hpp>
#include <map>
#include <string>

class Material {
protected:
  std::map<Condition,XSSet> sets_ ;
  std::string name_ ;

public:
  Material(std::string name) ; //Todo:Define how to initiate a material object. Depends on library format.
  getXSSet(Condition C) ;

};

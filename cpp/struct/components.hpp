// << components.hpp >>
// This header is for a component objects. Components are physical things that make up nuclear assembly rods. They are made of a given material, and are at a specific condition.
// Style is header-should-be-small

#include <material.hpp>
#include <condition.hpp>

class Component {
protected:
  Material mat_ ;
  Condition cond_ ;
public:
  Component(Material mat, Condition cond) ;
  getMacXSSet() ;
  change_Condition(Condition cond) ;
};

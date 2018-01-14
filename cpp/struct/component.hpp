// << component.hpp >>
// This header is for a component objects. Components are physical things that make up nuclear assembly rods. They are made of a given material, and are at a specific condition.
// Style is header-should-be-small

#ifndef _COMPONENT_H_
#define _COMPONENT_H_

#include <memory>
//#include <material.hpp>
//#include <condition.hpp>

class Material ; // Forward declarations
class Condition ; // Forward declarations

class Component {
protected:
  std::shared_ptr<Material> mat_ ;
  std::shared_ptr<Condition> cond_ ;
public:
  Component(std::shared_ptr<Material> mat, std::shared_ptr<Condition> cond) ;
  getMacXSSet() ;
  change_Condition(std::shared_ptr<Condition> cond) ;
};

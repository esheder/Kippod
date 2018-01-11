// << condition.hpp >>
// This header is for a condition object. This is a set of conditions that defines a state a component/core is in. Some things are local (temperature might be local) and some things are global. Deal with it.
// Style is header-should-be-small
//

#ifndef _CONDITION_H_
#define _CONDITION_H_

#include <map>
#include <string>
#include <shared_ptr>
#include <unique_ptr>
#include <boost/any.hpp>

class Condition {
  // The correct way to do this is by creating maps of variables (global and local) and values.
  // Alas, there is no such map given in C++. We can create a de-facto one with internet help,
protected:
  std::map< std::string, boost::any > global_ ;
  std::map< std::string, boost::any > local_ ;
public:
  Condition() ; //A condition is initially empty, and then we add things to the maps.
  void setGlobalVar(std::string key, boost:any val) ;
  void setLocalVar(std::string key, boost:any val) ;
  bool operator==(const Condition& c) ;
  
};

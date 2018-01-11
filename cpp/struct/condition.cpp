// << condition.cpp >>
// This is the implementation of a condition object. See condition.hpp for details on what a condition is.
// This should only have full implementations.
//

#include "condition.hpp"

void Condition :: setGlobalVar(std::string key, boost::any val)
{
global_[key] = val ;
}

void Condition :: setLocalVar(std::string key, boost::any val)
{
local_[key] = val ;
}

bool Condition :: operator==(const Condition& c)
{
// Conditions are the same if all their condition variables are the same. I think.
try {
for (auto& kv : global_)
  {
if (kv.second != c.global_.at(kv.first).second)
  {
return false ;
} 
}
}
}

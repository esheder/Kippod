// << condition.cpp >>
// This is the implementation of a condition object. See condition.hpp for details on what a condition is.
// This should only have full implementations.
//

#include "condition.hpp" 

Condition :: Condition() {}
void Condition :: setGlobalVar(std::string key, VarVal val){global_[key] = val ;}

void Condition :: setLocalVar(std::string key, VarVal val){local_[key] = val ;}

bool Condition :: operator==(const Condition& c)
{
    //Conditions are the same if all their condition variables are the same
    try {
	for (const auto& kv : this->global_) {
	    if (kv.second != c.global_.at(kv.first)) {return false ;}
	}
	for (const auto& kv : c.global_) {
	    if (kv.second != this->global_.at(kv.first)) {return false ;}
	}
	return true ; //If they're all the same, they must be true.
    }
    catch (...) {return false ;} // Any missing key means they are not the same
}


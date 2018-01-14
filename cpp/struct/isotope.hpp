// << isotope.hpp >>
// A header file for the idea of an isotope. Isotopes are named and contain decay information.
//

#ifndef ISOTOPE_H
#define ISOTOPE_H

#include <string>
#include <unique_ptr>

class Transmutation ; //Forward declaration so transmutations can have isotopes and vice-versa.

class Isotope {
public:
    std::string name ;
    Isotope(std::string name); //Isotopes are first generated, then data about their decay is added
    addDecayMode(Transmutation T);
    bool operator<(const Isotope& lhs, const Isotope& rhs);
  
protected:
  std::unique_ptr<Transmutation> dmodes ; //Todo: Needs a better name
  
};

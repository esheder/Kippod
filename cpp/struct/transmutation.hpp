// << transmutation.hpp >>
// A header file for the idea of an isotopic transmutation. This is the base class for derived classes Absorption, Fission-Product and Radioactive Decay, which is further derived if needed.
//

#ifndef TRANS_H
#define TRANS_H

#include <memory>

class Isotope ; //Forward declaration so transmutations can have isotopes and vice-versa.

class Transmuation {
protected:
  std::shared_ptr<Isotope> target ;
};

class RadioactiveDecay : protected Transmuation {
protected:
  double halflife_ ;
  double Q_ ; //Energy release in MeV
  double yield_ ;
public:
  RadioactiveDecay(Isotope target, double hl, double Q) ;
};

class Absorption : protected Transmutation {
protected:
  double Egamma_; //Energy released in (n,gamma)
public:
  Absorption(Isotope target, double E);
}

class FissionProduction : protected Transmutation {
protected:
  double yield_ ;
public:
  FissionProduction(double yield) ;

}

/* Radioactive types that might exist. I don't know if this detail is necessary
class AlphaDecay : protected RadioactiveDecay {
public:
  AlphaDecay(Isotope target, double hl, double Q) ;

};

class BetaDecay : protected RadioactiveDecay
*/

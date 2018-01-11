// << core.hpp >>
// This header is for a core object, which should have similar tendencies as an actual reactor core
// Style is header-should-be-small

#ifndef _CORE_H_
#define _CORE_H_

//#include <rod.hpp> Forward declaration instead?
#include <unique_ptr>
#include <string>
typedef std::unique_ptr< std::unique_ptr<Rod> > GridType

class Rod ; //Forward Declaration

class Core {
protected:
  GridType grid ;
  std::string name ;

public:
  Core(int rows, int cols) ;
  insert_rod(std::unique_ptr<Rod> r) ;
  extract_rod(std::unique_ptr<Rod> r) ;
};

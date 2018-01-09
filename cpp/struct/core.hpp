// << core.hpp >>
// This header is for a core object, which should have similar tendencies as an actual reactor core
// Style is header-should-be-small

#include <rod.hpp>
#include <unique_ptr>
#include <string>
typedef std::unique_ptr< std::unique_ptr<Rod> > GridType

class Core {
protected:
GridType grid ;
std::string name ;

public:
Core(int rows, int cols) ;
insert_rod(Rod) ;
extract_rod(Rod) ;
};

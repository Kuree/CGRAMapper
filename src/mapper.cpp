#include "coreir.h"

#include "coreir/libs/cgralib.h"
#include "coreir/libs/commonlib.h"
#include "coreir/libs/float.h"
#include "coreir/passes/analysis/coreirjson.h"
#include "lakelib.h"

#include <fstream>

using namespace std;

#include "passes/verifycanmap.h"

#include "definitions/cgralib_def.h"
#include "passes/verifytechmapping.h"
#include "passes/memconst.h"
#include "passes/constdup.h"
//#include "passes/constregduplication.h"



using namespace CoreIR;

typedef struct {
  vector<SelectPath> IO16;
  vector<SelectPath> IO16in;
  vector<SelectPath> IO1;
  vector<SelectPath> IO1in;
} IOpaths;

void getAllIOPaths(Wireable* w, IOpaths& paths) {
  Type* t = w->getType();
  if (auto at = dyn_cast<ArrayType>(t)) {
    if (at->getLen()==16 && isa<BitType>(at->getElemType())) {
      paths.IO16.push_back(w->getSelectPath());
    }
    else if (at->getLen() == 16 && isa<BitInType>(at->getElemType())) {
      paths.IO16in.push_back(w->getSelectPath());
    }
    else {
      for (auto selstr : t->getSelects()) {
        getAllIOPaths(w->sel(selstr),paths);
      }
    }
  }
  else if (isa<BitType>(t)) {
    paths.IO1.push_back(w->getSelectPath());
  }
  else if (isa<BitInType>(t)) {
    paths.IO1in.push_back(w->getSelectPath());
  }
  else {
    for (auto sw : w->getSelects()) {
      getAllIOPaths(sw.second,paths);
    }
  }
  
}

void addIOs(Context* c, Module* top) {
  ModuleDef* mdef = top->getDef();

  Values aWidth({{"width",Const::make(c,16)}});
  IOpaths iopaths;
  getAllIOPaths(mdef->getInterface(), iopaths);
  Instance* pt = addPassthrough(mdef->getInterface(),"_self");
  for (auto path : iopaths.IO16) {
    string ioname = "io16in_" + join(++path.begin(),path.end(),string("_"));
    mdef->addInstance(ioname,"cgralib.IO",aWidth,{{"mode",Const::make(c,"in")}});
    path[0] = "in";
    path.insert(path.begin(),"_self");
    mdef->connect({ioname,"out"},path);
  }
  for (auto path : iopaths.IO16in) {
    string ioname = "io16_" + join(++path.begin(),path.end(),string("_"));
    mdef->addInstance(ioname,"cgralib.IO",aWidth,{{"mode",Const::make(c,"out")}});
    path[0] = "in";
    path.insert(path.begin(),"_self");
    mdef->connect({ioname,"in"},path);
  }
  for (auto path : iopaths.IO1) {
    string ioname = "io1in_" + join(++path.begin(),path.end(),string("_"));
    mdef->addInstance(ioname,"cgralib.BitIO",{{"mode",Const::make(c,"in")}});
    path[0] = "in";
    path.insert(path.begin(),"_self");
    mdef->connect({ioname,"out"},path);
  }
  for (auto path : iopaths.IO1in) {
    string ioname = "io1_" + join(++path.begin(),path.end(),string("_"));
    mdef->addInstance(ioname,"cgralib.BitIO",{{"mode",Const::make(c,"out")}});
    path[0] = "in";
    path.insert(path.begin(),"_self");
    mdef->connect({ioname,"in"},path);
  }
  mdef->disconnect(mdef->getInterface());
  inlineInstance(pt);
}


int main(int argc, char *argv[]){
  Context* c = newContext();
  
  CoreIRLoadLibrary_cgralib(c);
  CoreIRLoadLibrary_commonlib(c);
  CoreIRLoadLibrary_float(c);
  CoreIRLoadLibrary_lakelib(c);

  string premap;
  string postmap;
  if (argc == 3) {
    premap = argv[1];
    postmap = argv[2];
  }
  else {
    cout << "usage: mapper premapped.json mapped.json" << endl;
    return 1;
  }

  //cout << "Loading " << premap << endl;
  //deleteContext(c); exit(0);
  
  Module* top = nullptr;
  if (!loadFromFile(c,premap,&top)) {
    c->die();
  }
  ASSERT(top,"Could not load top:");

  //SLight hack. Add a default width for all of coreir
  for (auto cgenmap : c->getNamespace("coreir")->getGenerators()) {
    if (cgenmap.second->getGenParams().count("width")) {
      cgenmap.second->addDefaultGenArgs({{"width",Const::make(c,16)}});
    }
  }
  c->getGenerator("commonlib.lutN")->addDefaultGenArgs({{"N",Const::make(c,3)}});

  LoadDefinition_cgralib(c); //Load the definitions first
  c->runPasses({"rungenerators","verifyconnectivity --onlyinputs --noclkrst","removebulkconnections","removesinglemuxes"});


  //load last verification
  c->addPass(new MapperPasses::VerifyCanMap);
  c->runPasses({"verifycanmap"});

  //DO any normal optimizations
  c->runPasses({"deletedeadinstances"});
  c->runPasses({"removewires"});
  addIOs(c,top);
  c->runPasses({"cullgraph"}); 
  c->addPass(new MapperPasses::VerifyTechMapping);
  c->runPasses({"verifytechmapping"});
  

  ////Fold constants and registers into PEs
  //c->addPass(new MapperPasses::ConstDuplication);
  //c->runPasses({"constduplication"});

  //Flatten
  c->runPasses({"flatten"});
  cout << "after flatten" << endl;
  c->addPass(new MapperPasses::ConstDuplication);
  c->runPasses({"constduplication"});
  c->addPass(new MapperPasses::MemConst);
  c->runPasses({"memconst"});
  c->runPasses({"cullgraph"});
  c->getPassManager()->printLog();
  cout << "Trying to save" << endl;
  c->runPasses({"coreirjson"},{"global","commonlib","mantle"});
  auto jpass = static_cast<Passes::CoreIRJson*>(c->getPassManager()->getAnalysisPass("coreirjson"));
  //Create file here.
  std::ofstream file(postmap);
  jpass->writeToStream(file,top->getRefName());
 

  deleteContext(c);
  return 0;
}

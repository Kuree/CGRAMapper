
using namespace CoreIR;

namespace MapperPasses {
class TechMapping : public InstanceVisitorPass {
  public :
    static std::string ID;
    TechMapping() : InstanceVisitorPass(ID,"Does substituions like LT -> GTE + Not") {}
    void setVisitorInfo() override;
};

}

namespace {

//Replaces all Data (Add,Sub,RSHIFT,LSHIFT,Mult,OR,AND,XOR)
bool binaryOpReplacement(Instance* inst) {
  Context* c = inst->getContext();
  ModuleDef* def = inst->getContainer();
  string iname = inst->getInstname();
  //For now just use the coreir lib name as the op
  string opstr = inst->getModuleRef()->getName();
  Values dataPEArgs({{"alu_op",Const::make(c,opstr)},{"signed",Const::make(c,false)},{"flag_sel",});
  Instance* dataPE = def->addInstance(iname+"_PE","cgralib.PE",{{"op_kind",Const::make(c,"alu")}},dataPEArgs);

  
  //Isolate the instance
  Instance* pt = addPassthrough(inst,"_pt"+c->getUnique());
  def->disconnect(pt->sel("in"),inst);
  //Some of these connections might be lost while inlining passthrough
  //But this is what we want because some inputs are unconnected
  def->connect(pt->sel({"in","in0"}),dataPE->sel({"data","in","0"}));
  def->connect(pt->sel({"in","in1"}),dataPE->sel({"data","in","1"}));
  def->connect(pt->sel({"in","out"}),dataPE->sel({"data","out"}));
  
  //Remove instance and inline passthrough
  def->removeInstance(inst);
  inlineInstance(pt);
  return true;
}

//Replaces  (GTE,LTE) with PE
bool compOpReplacement(Instance* inst) {
  Context* c = inst->getContext();
  ModuleDef* def = inst->getContainer();
  string iname = inst->getInstname();
  //For now just use the coreir lib name as the op
  string opstr = inst->getModuleRef()->getName();
  Values PEArgs({{"alu_op",Const::make(c,opstr)}});
  Instance* PE = def->addInstance(iname+"_PE","cgralib.PE",{{"op_kind",Const::make(c,"combined")}},PEArgs);

  
  //Isolate the instance
  Instance* pt = addPassthrough(inst,"_pt"+c->getUnique());
  def->disconnect(pt->sel("in"),inst);
  //Some of these connections might be lost while inlining passthrough
  //But this is what we want because some inputs are unconnected
  def->connect(pt->sel({"in","in0"}),PE->sel({"data","in","0"}));
  def->connect(pt->sel({"in","in1"}),PE->sel({"data","in","1"}));
  def->connect(pt->sel({"in","out"}),PE->sel({"bit","out"}));
  
  //Remove instance and inline passthrough
  def->removeInstance(inst);
  inlineInstance(pt);
  return true;
}

//Replaces mux with PE
bool muxOpReplacement(Instance* inst) {
  Context* c = inst->getContext();
  ModuleDef* def = inst->getContainer();
  string iname = inst->getInstname();
  //For now just use the coreir lib name as the op
  string opstr = inst->getModuleRef()->getName();
  Values PEArgs({{"alu_op",Const::make(c,"mux")}});
  Instance* PE = def->addInstance(iname+"_PE","cgralib.PE",{{"op_kind",Const::make(c,"combined")}},PEArgs);

  
  //Isolate the instance
  Instance* pt = addPassthrough(inst,"_pt"+c->getUnique());
  def->disconnect(pt->sel("in"),inst);
  //Some of these connections might be lost while inlining passthrough
  //But this is what we want because some inputs are unconnected
  def->connect(pt->sel({"in","in0"}),PE->sel({"data","in","0"}));
  def->connect(pt->sel({"in","in1"}),PE->sel({"data","in","1"}));
  def->connect(pt->sel({"in","sel"}),PE->sel({"bit","in","0"}));
  def->connect(pt->sel({"in","out"}),PE->sel({"data","out"}));
  
  //Remove instance and inline passthrough
  def->removeInstance(inst);
  inlineInstance(pt);
  return true;
}

//This will assume lbMem will have been linked with a cgra def
//bool rungenAndReplace(Instance* inst) {
//  Namespace* ns = inst->getInstantiableRef()->getNamespace();
//  inst->runGenerator();
//  Module* m = inst->getModuleRef();
//  if (!ns->hasModule(m->getName())) {
//    ns->addModule(m);
//  }
//  inlineInstance(inst);
//  return true;
//}

bool removeInstance(Instance* inst) {
  inst->getContainer()->removeInstance(inst);
  return true;
}

}


std::string MapperPasses::TechMapping::ID = "techmapping";

void MapperPasses::TechMapping::setVisitorInfo() {
  Context* c = this->getContext();
  addVisitorFunction(c->getGenerator("commonlib.lutN"),lutReplacement);
  for (auto str : {"uge","ule","eq","neq"}) {
    addVisitorFunction(c->getGenerator("coreir."+string(str)),compOpReplacement);
  }
  
  addVisitorFunction(c->getGenerator("coreir.mux"),muxOpReplacement);
  addVisitorFunction(c->getGenerator("coreir.term"),removeInstance);
  addVisitorFunction(c->getModule("corebit.term"),removeInstance);
  
  //TODO what about dlshl
  for (auto str : {"add","sub","shl","ashr","mul","or","and","xor"}) {
    addVisitorFunction(c->getGenerator("coreir." + string(str)),binaryOpReplacement);
  }


}

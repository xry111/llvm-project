//===--- LoongArch.h - Declare LoongArch target feature support -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares LoongArch TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_LOONGARCH_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_LOONGARCH_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY LoongArchTargetInfo : public TargetInfo {
  void setDataLayout() {
    StringRef Layout;

    if (ABI == "lp32")
      Layout = "m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64";
    else if (ABI == "lpx32")
      Layout = "m:e-p:32:32-i8:8:32-i16:16:32-i64:64-n32:64-S128";
    else if (ABI == "lp64")
      Layout = "m:e-i8:8:32-i16:16:32-i64:64-n32:64-S128";
    else
      llvm_unreachable("Invalid ABI");

    resetDataLayout(("e-" + Layout).str());
  }

  static const Builtin::Info BuiltinInfo[];
  std::string CPU;
  bool IsSingleFloat;
  enum LoongArchFloatABI { HardFloat, SoftFloat } FloatABI;

protected:
  enum FPModeEnum { FP32, FP64 } FPMode;
  std::string ABI;

public:
  LoongArchTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
      : TargetInfo(Triple),
        IsSingleFloat(false),
        FloatABI(HardFloat),
        FPMode(FP64) {
    TheCXXABI.set(TargetCXXABI::GenericLoongArch);

    if (Triple.isLoongArch32())
      setABI("lp32");
    else
      setABI("lp64");

    // Currently, CPU only supports 'la464' in LA.
    if ( ABI == "lp64")
      CPU = "la464";
  }

  bool processorSupportsGPR64() const;

  StringRef getABI() const override { return ABI; }

  bool setABI(const std::string &Name) override {
    if (Name == "lp32") {
      setLP32ABITypes();
      ABI = Name;
      return true;
    }

    if (Name == "lpx32") {
      //setLPX32ABITypes();
      //ABI = Name;
      //return true;
      //TODO: implement
      return false;
    }
    if (Name == "lp64") {
      setLP64ABITypes();
      ABI = Name;
      return true;
    }
    return false;
  }

  void setLP32ABITypes() {
    Int64Type = SignedLongLong;
    IntMaxType = Int64Type;
    LongDoubleFormat = &llvm::APFloat::IEEEdouble();
    LongDoubleWidth = LongDoubleAlign = 64;
    LongWidth = LongAlign = 32;
    MaxAtomicPromoteWidth = MaxAtomicInlineWidth = 32;
    PointerWidth = PointerAlign = 32;
    PtrDiffType = SignedInt;
    SizeType = UnsignedInt;
    SuitableAlign = 64;
  }

  void setLPX32LP64ABITypes() {
    LongDoubleWidth = LongDoubleAlign = 128;
    LongDoubleFormat = &llvm::APFloat::IEEEquad();
    if (getTriple().isOSFreeBSD()) {
      LongDoubleWidth = LongDoubleAlign = 64;
      LongDoubleFormat = &llvm::APFloat::IEEEdouble();
    }
    MaxAtomicPromoteWidth = MaxAtomicInlineWidth = 64;
    SuitableAlign = 128;
  }

  void setLP64ABITypes() {
    setLPX32LP64ABITypes();
    if (getTriple().isOSOpenBSD()) {
      Int64Type = SignedLongLong;
    } else {
      Int64Type = SignedLong;
    }
    IntMaxType = Int64Type;
    LongWidth = LongAlign = 64;
    PointerWidth = PointerAlign = 64;
    PtrDiffType = SignedLong;
    SizeType = UnsignedLong;
  }

  void setLPX32ABITypes() {
    setLPX32LP64ABITypes();
    Int64Type = SignedLongLong;
    IntMaxType = Int64Type;
    LongWidth = LongAlign = 32;
    PointerWidth = PointerAlign = 32;
    PtrDiffType = SignedInt;
    SizeType = UnsignedInt;
  }

  bool isValidCPUName(StringRef Name) const override;
  void fillValidCPUList(SmallVectorImpl<StringRef> &Values) const override;

  bool setCPU(const std::string &Name) override {
    CPU = Name;
    return isValidCPUName(Name);
  }

  const std::string &getCPU() const { return CPU; }
  bool
  initFeatureMap(llvm::StringMap<bool> &Features, DiagnosticsEngine &Diags,
                 StringRef CPU,
                 const std::vector<std::string> &FeaturesVec) const override {
#if 0
    if (CPU.empty())
      CPU = getCPU();
    Features[CPU] = true;
#else
//    if (CPU == "la464")
//      Features["loongarch64"] = true;

//FIXME: we need this?
//    if (CPU == "la464")
//      Features["64bit"] = true;
#endif
    return TargetInfo::initFeatureMap(Features, Diags, CPU, FeaturesVec);
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  ArrayRef<Builtin::Info> getTargetBuiltins() const override;

  bool hasFeature(StringRef Feature) const override;

  BuiltinVaListKind getBuiltinVaListKind() const override {
    return TargetInfo::VoidPtrBuiltinVaList;
  }

  ArrayRef<const char *> getGCCRegNames() const override {
    static const char *const GCCRegNames[] = {
        // CPU register names
        // Must match second column of GCCRegAliases
        "$r0", "$r1", "$r2", "$r3", "$r4", "$r5", "$r6", "$r7", "$r8", "$r9", "$r10",
        "$r11", "$r12", "$r13", "$r14", "$r15", "$r16", "$r17", "$r18", "$r19", "$r20",
        "$r21", "$r22", "$r23", "$r24", "$r25", "$r26", "$r27", "$r28", "$r29", "$r30",
        "$r31",
        // Floating point register names
        "$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7", "$f8", "$f9",
        "$f10", "$f11", "$f12", "$f13", "$f14", "$f15", "$f16", "$f17", "$f18",
        "$f19", "$f20", "$f21", "$f22", "$f23", "$f24", "$f25", "$f26", "$f27",
        "$f28", "$f29", "$f30", "$f31",
        // condition register names
        "$fcc0", "$fcc1", "$fcc2", "$fcc3", "$fcc4", "$fcc5", "$fcc6", "$fcc7"
    };
    return llvm::makeArrayRef(GCCRegNames);
  }

  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &Info) const override {
    switch (*Name) {
    default:
      return false;
    case 'r': // CPU registers.
    case 'f': // floating-point registers.
      Info.setAllowsRegister();
      return true;
    case 'l': // Signed 16-bit constant
    case 'I': // Signed 12-bit constant
    case 'K': // Unsigned 12-bit constant
    case 'J': // Integer 0
    case 'G': // Floating-point 0
      return true;
    case 'm': // Memory address with 12-bit offset
    case 'R': // An address that can be used in a non-macro load or store
      Info.setAllowsMemory();
      return true;
    case 'Z':
      if (Name[1] == 'C'        // Memory address with 16-bit and 4 bytes aligned offset
          || Name[1] == 'B' ) { // Memory address with 0 offset
        Info.setAllowsMemory();
        Name++; // Skip over 'Z'.
        return true;
      }
      return false;
    }
  }

  std::string convertConstraint(const char *&Constraint) const override {
    std::string R;
    switch (*Constraint) {
    case 'Z': // Two-character constraint; add "^" hint for later parsing.
      if (Constraint[1] == 'C' || Constraint[1] == 'B') {
        R = std::string("^") + std::string(Constraint, 2);
        Constraint++;
        return R;
      }
      break;
    }
    return TargetInfo::convertConstraint(Constraint);
  }

  const char *getClobbers() const override {
#if 0
    // In GCC, $1 is not widely used in generated code (it's used only in a few
    // specific situations), so there is no real need for users to add it to
    // the clobbers list if they want to use it in their inline assembly code.
    //
    // In LLVM, $1 is treated as a normal GPR and is always allocatable during
    // code generation, so using it in inline assembly without adding it to the
    // clobbers list can cause conflicts between the inline assembly code and
    // the surrounding generated code.
    //
    // Another problem is that LLVM is allowed to choose $1 for inline assembly
    // operands, which will conflict with the ".set at" assembler option (which
    // we use only for inline assembly, in order to maintain compatibility with
    // GCC) and will also conflict with the user's usage of $1.
    //
    // The easiest way to avoid these conflicts and keep $1 as an allocatable
    // register for generated code is to automatically clobber $1 for all inline
    // assembly code.
    //
    // FIXME: We should automatically clobber $1 only for inline assembly code
    // which actually uses it. This would allow LLVM to use $1 for inline
    // assembly operands if the user's assembly code doesn't use it.
    return "~{$1}";
#endif
    return "";
  }

  bool handleTargetFeatures(std::vector<std::string> &Features,
                            DiagnosticsEngine &Diags) override {
    IsSingleFloat = false;
    FloatABI = HardFloat;
    FPMode = FP64;

    for (const auto &Feature : Features) {
      if (Feature == "+single-float")
        IsSingleFloat = true;
      else if (Feature == "+soft-float")
        FloatABI = SoftFloat;
      else if (Feature == "+fp64")
        FPMode = FP64;
      else if (Feature == "-fp64")
        FPMode = FP32;
    }

    setDataLayout();

    return true;
  }

  int getEHDataRegisterNumber(unsigned RegNo) const override {
    if (RegNo == 0)
      return 4;
    if (RegNo == 1)
      return 5;
    return -1;
  }

  bool isCLZForZeroUndef() const override { return false; }

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override {
    static const TargetInfo::GCCRegAlias GCCRegAliases[] = {
        {{"zero", "$zero", "r0", "$0"}, "$r0"},
        {{"ra", "$ra", "r1", "$1"}, "$r1"},
        {{"tp", "$tp", "r2", "$2"}, "$r2"},
        {{"sp", "$sp", "r3", "$3"}, "$r3"},
        {{"a0", "$a0", "r4", "$4", "v0"}, "$r4"},
        {{"a1", "$a1", "r5", "$5", "v1"}, "$r5"},
        {{"a2", "$a2", "r6", "$6"}, "$r6"},
        {{"a3", "$a3", "r7", "$7"}, "$r7"},
        {{"a4", "$a4", "r8", "$8"}, "$r8"},
        {{"a5", "$a5", "r9", "$9"}, "$r9"},
        {{"a6", "$a6", "r10", "$10"}, "$r10"},
        {{"a7", "$a7", "r11", "$11"}, "$r11"},
        {{"t0", "$t0", "r12", "$12"}, "$r12"},
        {{"t1", "$t1", "r13", "$13"}, "$r13"},
        {{"t2", "$t2", "r14", "$14"}, "$r14"},
        {{"t3", "$t3", "r15", "$15"}, "$r15"},
        {{"t4", "$t4", "r16", "$16"}, "$r16"},
        {{"t5", "$t5", "r17", "$17"}, "$r17"},
        {{"t6", "$t6", "r18", "$18"}, "$r18"},
        {{"t7", "$t7", "r19", "$19"}, "$r19"},
        {{"t8", "$t8", "r20", "$20"}, "$r20"},
        //{{"x", "$x", "r21", "$21"}, "$r21"},
        {{"fp", "$fp", "r22", "$22"}, "$r22"},
        {{"s0", "$s0", "r23", "$23"}, "$r23"},
        {{"s1", "$s1", "r24", "$24"}, "$r24"},
        {{"s2", "$s2", "r25", "$25"}, "$r25"},
        {{"s3", "$s3", "r26", "$26"}, "$r26"},
        {{"s4", "$s4", "r27", "$27"}, "$r27"},
        {{"s5", "$s5", "r28", "$28"}, "$r28"},
        {{"s6", "$s6", "r29", "$29"}, "$r29"},
        {{"s7", "$s7", "r30", "$30"}, "$r30"},
        {{"s8", "$s8", "r31", "$31"}, "$r31"},
        {{"fa0", "$fa0", "f0"}, "$f0"},
        {{"fa1", "$fa1", "f1"}, "$f1"},
        {{"fa2", "$fa2", "f2"}, "$f2"},
        {{"fa3", "$fa3", "f3"}, "$f3"},
        {{"fa4", "$fa4", "f4"}, "$f4"},
        {{"fa5", "$fa5", "f5"}, "$f5"},
        {{"fa6", "$fa6", "f6"}, "$f6"},
        {{"fa7", "$fa7", "f7"}, "$f7"},
        {{"ft0", "$ft0", "f8"}, "$f8"},
        {{"ft1", "$ft1", "f9"}, "$f9"},
        {{"ft2", "$ft2", "f10"}, "$f10"},
        {{"ft3", "$ft3", "f11"}, "$f11"},
        {{"ft4", "$ft4", "f12"}, "$f12"},
        {{"ft5", "$ft5", "f13"}, "$f13"},
        {{"ft6", "$ft6", "f14"}, "$f14"},
        {{"ft7", "$ft7", "f15"}, "$f15"},
        {{"ft8", "$ft8", "f16"}, "$f16"},
        {{"ft9", "$ft9", "f17"}, "$f17"},
        {{"ft10", "$ft10", "f18"}, "$f18"},
        {{"ft11", "$ft11", "f19"}, "$f19"},
        {{"ft12", "$ft12", "f20"}, "$f20"},
        {{"ft13", "$ft13", "f21"}, "$f21"},
        {{"ft14", "$ft14", "f22"}, "$f22"},
        {{"ft15", "$ft15", "f23"}, "$f23"},
        {{"fs0", "$fs0", "f24"}, "$f24"},
        {{"fs1", "$fs1", "f25"}, "$f25"},
        {{"fs2", "$fs2", "f26"}, "$f26"},
        {{"fs3", "$fs3", "f27"}, "$f27"},
        {{"fs4", "$fs4", "f28"}, "$f28"},
        {{"fs5", "$fs5", "f29"}, "$f29"},
        {{"fs6", "$fs6", "f30"}, "$f30"},
        {{"fs7", "$fs7", "f31"}, "$f31"},
    };
    return llvm::makeArrayRef(GCCRegAliases);
  }

  bool hasInt128Type() const override {
    return (ABI == "lpx32" || ABI == "lp64") || getTargetOpts().ForceEnableInt128;
  }

  bool validateTarget(DiagnosticsEngine &Diags) const override;
};
} // namespace targets
} // namespace clang

#endif // LLVM_CLANG_LIB_BASIC_TARGETS_LOONGARCH_H

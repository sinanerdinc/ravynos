//===--- RavynOS.h - RavynOS ToolChain Implementations ------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_RAVYNOS_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_RAVYNOS_H

#include "Cuda.h"
#include "ROCm.h"
#include "clang/Basic/RavynOSSDKInfo.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Driver/Tool.h"
#include "clang/Driver/ToolChain.h"
#include "clang/Driver/XRayArgs.h"

namespace clang {
namespace driver {

namespace toolchains {
class MachO;
} // end namespace toolchains

namespace tools {

namespace ravynOS {
llvm::Triple::ArchType getArchTypeForMachOArchName(StringRef Str);
void setTripleTypeForMachOArchName(llvm::Triple &T, StringRef Str);

class LLVM_LIBRARY_VISIBILITY MachOTool : public Tool {
  virtual void anchor();

protected:
  void AddMachOArch(const llvm::opt::ArgList &Args,
                    llvm::opt::ArgStringList &CmdArgs) const;

  const toolchains::MachO &getMachOToolChain() const {
    return reinterpret_cast<const toolchains::MachO &>(getToolChain());
  }

public:
  MachOTool(const char *Name, const char *ShortName, const ToolChain &TC)
      : Tool(Name, ShortName, TC) {}
};

class LLVM_LIBRARY_VISIBILITY Assembler : public MachOTool {
public:
  Assembler(const ToolChain &TC)
      : MachOTool("ravynOS::Assembler", "assembler", TC) {}

  bool hasIntegratedCPP() const override { return false; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

class LLVM_LIBRARY_VISIBILITY Linker : public MachOTool {
  bool NeedsTempPath(const InputInfoList &Inputs) const;
  void AddLinkArgs(Compilation &C, const llvm::opt::ArgList &Args,
                   llvm::opt::ArgStringList &CmdArgs,
                   const InputInfoList &Inputs, unsigned Version[5],
                   bool LinkerIsLLD) const;

public:
  Linker(const ToolChain &TC) : MachOTool("ravynOS::Linker", "linker", TC) {}

  bool hasIntegratedCPP() const override { return false; }
  bool isLinkJob() const override { return true; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

class LLVM_LIBRARY_VISIBILITY StaticLibTool : public MachOTool {
public:
  StaticLibTool(const ToolChain &TC)
      : MachOTool("ravynOS::StaticLibTool", "static-lib-linker", TC) {}

  bool hasIntegratedCPP() const override { return false; }
  bool isLinkJob() const override { return true; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

class LLVM_LIBRARY_VISIBILITY Lipo : public MachOTool {
public:
  Lipo(const ToolChain &TC) : MachOTool("ravynOS::Lipo", "lipo", TC) {}

  bool hasIntegratedCPP() const override { return false; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

class LLVM_LIBRARY_VISIBILITY Dsymutil : public MachOTool {
public:
  Dsymutil(const ToolChain &TC)
      : MachOTool("ravynOS::Dsymutil", "dsymutil", TC) {}

  bool hasIntegratedCPP() const override { return false; }
  bool isDsymutilJob() const override { return true; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

class LLVM_LIBRARY_VISIBILITY VerifyDebug : public MachOTool {
public:
  VerifyDebug(const ToolChain &TC)
      : MachOTool("ravynOS::VerifyDebug", "dwarfdump", TC) {}

  bool hasIntegratedCPP() const override { return false; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};
} // end namespace ravynOS
} // end namespace tools

namespace toolchains {

class LLVM_LIBRARY_VISIBILITY MachO : public ToolChain {
protected:
  Tool *buildAssembler() const override;
  Tool *buildLinker() const override;
  Tool *buildStaticLibTool() const override;
  Tool *getTool(Action::ActionClass AC) const override;

private:
  mutable std::unique_ptr<tools::ravynOS::Lipo> Lipo;
  mutable std::unique_ptr<tools::ravynOS::Dsymutil> Dsymutil;
  mutable std::unique_ptr<tools::ravynOS::VerifyDebug> VerifyDebug;

public:
  MachO(const Driver &D, const llvm::Triple &Triple,
        const llvm::opt::ArgList &Args);
  ~MachO() override;

  /// @name MachO specific toolchain API
  /// {

  /// Get the "MachO" arch name for a particular compiler invocation. For
  /// example, Apple treats different ARM variations as distinct architectures.
  StringRef getMachOArchName(const llvm::opt::ArgList &Args) const;

  /// Add the linker arguments to link the ARC runtime library.
  virtual void AddLinkARCArgs(const llvm::opt::ArgList &Args,
                              llvm::opt::ArgStringList &CmdArgs) const {}

  /// Add the linker arguments to link the compiler runtime library.
  ///
  /// FIXME: This API is intended for use with embedded libraries only, and is
  /// misleadingly named.
  virtual void AddLinkRuntimeLibArgs(const llvm::opt::ArgList &Args,
                                     llvm::opt::ArgStringList &CmdArgs,
                                     bool ForceLinkBuiltinRT = false) const;

  virtual void addStartObjectFileArgs(const llvm::opt::ArgList &Args,
                                      llvm::opt::ArgStringList &CmdArgs) const {
  }

  virtual void addMinVersionArgs(const llvm::opt::ArgList &Args,
                                 llvm::opt::ArgStringList &CmdArgs) const {}

  virtual void addPlatformVersionArgs(const llvm::opt::ArgList &Args,
                                      llvm::opt::ArgStringList &CmdArgs) const {
  }

  /// On some iOS platforms, kernel and kernel modules were built statically. Is
  /// this such a target?
  virtual bool isKernelStatic() const { return false; }

  /// Is the target either iOS or an iOS simulator?
  bool isTargetIOSBased() const { return false; }

  /// Options to control how a runtime library is linked.
  enum RuntimeLinkOptions : unsigned {
    /// Link the library in even if it can't be found in the VFS.
    RLO_AlwaysLink = 1 << 0,

    /// Use the embedded runtime from the macho_embedded directory.
    RLO_IsEmbedded = 1 << 1,

    /// Emit rpaths for @executable_path as well as the resource directory.
    RLO_AddRPath = 1 << 2,
  };

  /// Add a runtime library to the list of items to link.
  void AddLinkRuntimeLib(const llvm::opt::ArgList &Args,
                         llvm::opt::ArgStringList &CmdArgs, StringRef Component,
                         RuntimeLinkOptions Opts = RuntimeLinkOptions(),
                         bool IsShared = false) const;

  /// Add any profiling runtime libraries that are needed. This is essentially a
  /// MachO specific version of addProfileRT in Tools.cpp.
  void addProfileRTLibs(const llvm::opt::ArgList &Args,
                        llvm::opt::ArgStringList &CmdArgs) const override {
    // There aren't any profiling libs for embedded targets currently.
  }

  /// }
  /// @name ToolChain Implementation
  /// {

  types::ID LookupTypeForExtension(StringRef Ext) const override;

  bool HasNativeLLVMSupport() const override;

  llvm::opt::DerivedArgList *
  TranslateArgs(const llvm::opt::DerivedArgList &Args, StringRef BoundArch,
                Action::OffloadKind DeviceOffloadKind) const override;

  bool IsBlocksDefault() const override {
    // Always allow blocks on Apple; users interested in versioning are
    // expected to use /usr/include/Block.h.
    return true;
  }
  bool IsIntegratedAssemblerDefault() const override {
    // Default integrated assembler to on for Apple's MachO targets.
    return true;
  }

  bool IsMathErrnoDefault() const override { return false; }

  bool IsEncodeExtendedBlockSignatureDefault() const override { return true; }

  bool IsObjCNonFragileABIDefault() const override {
    // Non-fragile ABI is default for everything but i386.
    return getTriple().getArch() != llvm::Triple::x86;
  }

  bool UseObjCMixedDispatch() const override { return true; }

  bool IsUnwindTablesDefault(const llvm::opt::ArgList &Args) const override;

  RuntimeLibType GetDefaultRuntimeLibType() const override {
    return ToolChain::RLT_CompilerRT;
  }

  bool isPICDefault() const override;
  bool isPIEDefault(const llvm::opt::ArgList &Args) const override;
  bool isPICDefaultForced() const override;

  bool SupportsProfiling() const override;

  bool UseDwarfDebugFlags() const override;

  llvm::ExceptionHandling
  GetExceptionModel(const llvm::opt::ArgList &Args) const override {
    return llvm::ExceptionHandling::None;
  }

  virtual StringRef getOSLibraryNameSuffix(bool IgnoreSim = false) const {
    return "";
  }

  // RavynOS toolchain uses legacy thin LTO API, which is not
  // capable of unit splitting.
  bool canSplitThinLTOUnit() const override { return false; }
  /// }
};

/// RavynOS - The base RavynOS tool chain.
class LLVM_LIBRARY_VISIBILITY RavynOS : public MachO {
public:
  /// Whether the information on the target has been initialized.
  //
  // FIXME: This should be eliminated. What we want to do is make this part of
  // the "default target for arguments" selection process, once we get out of
  // the argument translation business.
  mutable bool TargetInitialized;

  enum RavynOSPlatformKind {
    RavynOSOS,
    LastRavynOSPlatform = RavynOS
  };
  enum RavynOSEnvironmentKind {
    NativeEnvironment,
  };

  mutable RavynOSPlatformKind TargetPlatform;
  mutable RavynOSEnvironmentKind TargetEnvironment;

  /// The native OS version we are targeting.
  mutable VersionTuple TargetVersion;
  /// The OS version we are targeting as specified in the triple.
  mutable VersionTuple OSTargetVersion;

  /// The information about the ravynOS SDK that was used.
  mutable Optional<RavynOSSDKInfo> SDKInfo;

  CudaInstallationDetector CudaInstallation;
  RocmInstallationDetector RocmInstallation;

private:
  void AddDeploymentTarget(llvm::opt::DerivedArgList &Args) const;

public:
  RavynOS(const Driver &D, const llvm::Triple &Triple,
         const llvm::opt::ArgList &Args);
  ~RavynOS() override;

  std::string ComputeEffectiveClangTriple(const llvm::opt::ArgList &Args,
                                          types::ID InputType) const override;

  /// @name Apple Specific Toolchain Implementation
  /// {

  void addMinVersionArgs(const llvm::opt::ArgList &Args,
                         llvm::opt::ArgStringList &CmdArgs) const override;

  void addPlatformVersionArgs(const llvm::opt::ArgList &Args,
                              llvm::opt::ArgStringList &CmdArgs) const override;

  void addStartObjectFileArgs(const llvm::opt::ArgList &Args,
                              llvm::opt::ArgStringList &CmdArgs) const override;

  bool isKernelStatic() const override {
    return (!(isTargetIPhoneOS() && !isIPhoneOSVersionLT(6, 0)) &&
            !isTargetWatchOS());
  }

  void addProfileRTLibs(const llvm::opt::ArgList &Args,
                        llvm::opt::ArgStringList &CmdArgs) const override;

protected:
  /// }
  /// @name RavynOS specific Toolchain functions
  /// {

  // FIXME: Eliminate these ...Target functions and derive separate tool chains
  // for these targets and put version in constructor.
  void setTarget(RavynOSPlatformKind Platform, RavynOSEnvironmentKind Environment,
                 unsigned Major, unsigned Minor, unsigned Micro,
                 VersionTuple NativeTargetVersion) const {
    // FIXME: For now, allow reinitialization as long as values don't
    // change. This will go away when we move away from argument translation.
    if (TargetInitialized && TargetPlatform == Platform &&
        TargetEnvironment == Environment &&
        TargetVersion == VersionTuple(Major, Minor, Micro))
      return;

    assert(!TargetInitialized && "Target already initialized!");
    TargetInitialized = true;
    TargetPlatform = Platform;
    TargetEnvironment = Environment;
    TargetVersion = VersionTuple(Major, Minor, Micro);
  }

public:
  bool isTargetIPhoneOS() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetIOSSimulator() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetIOSBased() const {
    assert(TargetInitialized && "Target not initialized!");
    return isTargetIPhoneOS() || isTargetIOSSimulator();
  }

  bool isTargetTvOS() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetTvOSSimulator() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetTvOSBased() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetWatchOS() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetWatchOSSimulator() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetWatchOSBased() const {
    assert(TargetInitialized && "Target not initialized!");
    return false;
  }

  bool isTargetMacCatalyst() const {
    return false;
  }

  bool isTargetRavynOS() const {
    assert(TargetInitialized && "Target not initialized!");
    return true;
  }

  bool isTargetRavynOSBased() const {
    assert(TargetInitialized && "Target not initialized!");
    return true;
  }

  bool isTargetAppleSiliconMac() const {
    assert(TargetInitialized && "Target not initialized!");
    return isTargetRavynOSBased() && getArch() == llvm::Triple::aarch64;
  }

  bool isTargetInitialized() const { return TargetInitialized; }

  /// The version of the OS that's used by the OS specified in the target
  /// triple. It might be different from the actual target OS on which the
  /// program will run, e.g. MacCatalyst code runs on a macOS target, but its
  /// target triple is iOS.
  VersionTuple getTripleTargetVersion() const {
    assert(TargetInitialized && "Target not initialized!");
    return TargetVersion;
  }

protected:
  /// Return true if c++17 aligned allocation/deallocation functions are not
  /// implemented in the c++ standard library of the deployment target we are
  /// targeting.
  bool isAlignedAllocationUnavailable() const;

  void addClangTargetOptions(const llvm::opt::ArgList &DriverArgs,
                             llvm::opt::ArgStringList &CC1Args,
                             Action::OffloadKind DeviceOffloadKind) const override;

  StringRef getPlatformFamily() const;
  StringRef getOSLibraryNameSuffix(bool IgnoreSim = false) const override;

public:
  static StringRef getSDKName(StringRef isysroot);

  /// }
  /// @name ToolChain Implementation
  /// {

  // RavynOS tools support multiple architecture (e.g., i386 and x86_64) and
  // most development is done against SDKs, so compiling for a different
  // architecture should not get any special treatment.
  bool isCrossCompiling() const override { return false; }

  llvm::opt::DerivedArgList *
  TranslateArgs(const llvm::opt::DerivedArgList &Args, StringRef BoundArch,
                Action::OffloadKind DeviceOffloadKind) const override;

  CXXStdlibType GetDefaultCXXStdlibType() const override;
  ObjCRuntime getDefaultObjCRuntime(bool isNonFragile) const override;
  bool hasBlocksRuntime() const override;

  void AddCudaIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                          llvm::opt::ArgStringList &CC1Args) const override;
  void AddHIPIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                         llvm::opt::ArgStringList &CC1Args) const override;

  bool UseObjCMixedDispatch() const override {
    // This is only used with the non-fragile ABI and non-legacy dispatch.

    // Mixed dispatch is used everywhere except OS X before 10.6.
    return true;
  }

  LangOptions::StackProtectorMode
  GetDefaultStackProtectorLevel(bool KernelOrKext) const override {
    // Stack protectors default to on for user code on 10.5,
    // and for everything in 10.6 and beyond
    return LangOptions::SSPOn;
  }

  void CheckObjCARC() const override;

  llvm::ExceptionHandling GetExceptionModel(
      const llvm::opt::ArgList &Args) const override;

  bool SupportsEmbeddedBitcode() const override;

  SanitizerMask getSupportedSanitizers() const override;

  void printVerboseInfo(raw_ostream &OS) const override;
};

/// RavynOSClang - The RavynOS toolchain used by Clang.
class LLVM_LIBRARY_VISIBILITY RavynOSClang : public RavynOS {
public:
  RavynOSClang(const Driver &D, const llvm::Triple &Triple,
              const llvm::opt::ArgList &Args);

  /// @name Apple ToolChain Implementation
  /// {

  RuntimeLibType GetRuntimeLibType(const llvm::opt::ArgList &Args) const override;

  void AddLinkRuntimeLibArgs(const llvm::opt::ArgList &Args,
                             llvm::opt::ArgStringList &CmdArgs,
                             bool ForceLinkBuiltinRT = false) const override;

  void AddClangCXXStdlibIncludeArgs(
      const llvm::opt::ArgList &DriverArgs,
      llvm::opt::ArgStringList &CC1Args) const override;

  void AddClangSystemIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                                 llvm::opt::ArgStringList &CC1Args) const override;

  void AddCXXStdlibLibArgs(const llvm::opt::ArgList &Args,
                           llvm::opt::ArgStringList &CmdArgs) const override;

  void AddCCKextLibArgs(const llvm::opt::ArgList &Args,
                        llvm::opt::ArgStringList &CmdArgs) const override;

  void addClangWarningOptions(llvm::opt::ArgStringList &CC1Args) const override;

  void AddLinkARCArgs(const llvm::opt::ArgList &Args,
                      llvm::opt::ArgStringList &CmdArgs) const override;

  unsigned GetDefaultDwarfVersion() const override;
  // Until dtrace (via CTF) and LLDB can deal with distributed debug info,
  // RavynOS defaults to standalone/full debug info.
  bool GetDefaultStandaloneDebug() const override { return true; }
  llvm::DebuggerKind getDefaultDebuggerTuning() const override {
    return llvm::DebuggerKind::LLDB;
  }

  /// }

private:
  void AddLinkSanitizerLibArgs(const llvm::opt::ArgList &Args,
                               llvm::opt::ArgStringList &CmdArgs,
                               StringRef Sanitizer,
                               bool shared = true) const;

  bool AddGnuCPlusPlusIncludePaths(const llvm::opt::ArgList &DriverArgs,
                                   llvm::opt::ArgStringList &CC1Args,
                                   llvm::SmallString<128> Base,
                                   llvm::StringRef Version,
                                   llvm::StringRef ArchDir,
                                   llvm::StringRef BitDir) const;

  llvm::StringRef GetHeaderSysroot(const llvm::opt::ArgList &DriverArgs) const;
};

} // end namespace toolchains
} // end namespace driver
} // end namespace clang

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_RAVYNOS_H

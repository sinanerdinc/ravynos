//===--- RavynOS.cpp - RavynOS Tool and ToolChain Implementations -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "RavynOS.h"
#include "Arch/AArch64.h"
#include "Arch/ARM.h"
#include "CommonArgs.h"
#include "clang/Basic/AlignedAllocation.h"
#include "clang/Basic/ObjCRuntime.h"
#include "clang/Config/config.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "clang/Driver/SanitizerArgs.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/ArgList.h"
#include "llvm/ProfileData/InstrProf.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/ScopedPrinter.h"
#include "llvm/Support/TargetParser.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <cstdlib> // ::getenv

using namespace clang::driver;
using namespace clang::driver::tools;
using namespace clang::driver::toolchains;
using namespace clang;
using namespace llvm::opt;

static VersionTuple minimumMacCatalystDeploymentTarget() {
  return VersionTuple(13, 1);
}

llvm::Triple::ArchType ravynos::getArchTypeForMachOArchName(StringRef Str) {
  // See arch(3) and llvm-gcc's driver-driver.c. We don't implement support for
  // archs which ravynOS doesn't use.

  // The matching this routine does is fairly pointless, since it is neither the
  // complete architecture list, nor a reasonable subset. The problem is that
  // historically the driver driver accepts this and also ties its -march=
  // handling to the architecture name, so we need to be careful before removing
  // support for it.

  // This code must be kept in sync with Clang's ravynOS specific argument
  // translation.

  return llvm::StringSwitch<llvm::Triple::ArchType>(Str)
      .Cases("ppc", "ppc601", "ppc603", "ppc604", "ppc604e", llvm::Triple::ppc)
      .Cases("ppc750", "ppc7400", "ppc7450", "ppc970", llvm::Triple::ppc)
      .Case("ppc64", llvm::Triple::ppc64)
      .Cases("i386", "i486", "i486SX", "i586", "i686", llvm::Triple::x86)
      .Cases("pentium", "pentpro", "pentIIm3", "pentIIm5", "pentium4",
             llvm::Triple::x86)
      .Cases("x86_64", "x86_64h", llvm::Triple::x86_64)
      // This is derived from the driver driver.
      .Cases("arm", "armv4t", "armv5", "armv6", "armv6m", llvm::Triple::arm)
      .Cases("armv7", "armv7em", "armv7k", "armv7m", llvm::Triple::arm)
      .Cases("armv7s", "xscale", llvm::Triple::arm)
      .Cases("arm64", "arm64e", llvm::Triple::aarch64)
      .Case("arm64_32", llvm::Triple::aarch64_32)
      .Case("r600", llvm::Triple::r600)
      .Case("amdgcn", llvm::Triple::amdgcn)
      .Case("nvptx", llvm::Triple::nvptx)
      .Case("nvptx64", llvm::Triple::nvptx64)
      .Case("amdil", llvm::Triple::amdil)
      .Case("spir", llvm::Triple::spir)
      .Default(llvm::Triple::UnknownArch);
}

void ravynos::setTripleTypeForMachOArchName(llvm::Triple &T, StringRef Str) {
  const llvm::Triple::ArchType Arch = getArchTypeForMachOArchName(Str);
  llvm::ARM::ArchKind ArchKind = llvm::ARM::parseArch(Str);
  T.setArch(Arch);
  if (Arch != llvm::Triple::UnknownArch)
    T.setArchName(Str);

  if (ArchKind == llvm::ARM::ArchKind::ARMV6M ||
      ArchKind == llvm::ARM::ArchKind::ARMV7M ||
      ArchKind == llvm::ARM::ArchKind::ARMV7EM) {
    T.setOS(llvm::Triple::UnknownOS);
    T.setObjectFormat(llvm::Triple::MachO);
  }
}

void ravynos::Assembler::ConstructJob(Compilation &C, const JobAction &JA,
                                     const InputInfo &Output,
                                     const InputInfoList &Inputs,
                                     const ArgList &Args,
                                     const char *LinkingOutput) const {
  const llvm::Triple &T(getToolChain().getTriple());

  ArgStringList CmdArgs;

  assert(Inputs.size() == 1 && "Unexpected number of inputs.");
  const InputInfo &Input = Inputs[0];

  // Determine the original source input.
  const Action *SourceAction = &JA;
  while (SourceAction->getKind() != Action::InputClass) {
    assert(!SourceAction->getInputs().empty() && "unexpected root action!");
    SourceAction = SourceAction->getInputs()[0];
  }

  // Forward -g, assuming we are dealing with an actual assembly file.
  if (SourceAction->getType() == types::TY_Asm ||
      SourceAction->getType() == types::TY_PP_Asm) {
    if (Args.hasArg(options::OPT_gstabs))
      CmdArgs.push_back("--gstabs");
    else if (Args.hasArg(options::OPT_g_Group))
      CmdArgs.push_back("-g");
  }

  // Derived from asm spec.
  AddMachOArch(Args, CmdArgs);

  // Use -force_cpusubtype_ALL on x86 by default.
  if (T.isX86() || Args.hasArg(options::OPT_force__cpusubtype__ALL))
    CmdArgs.push_back("-force_cpusubtype_ALL");

  if (getToolChain().getArch() != llvm::Triple::x86_64 &&
      (((Args.hasArg(options::OPT_mkernel) ||
         Args.hasArg(options::OPT_fapple_kext)) &&
        getMachOToolChain().isKernelStatic()) ||
       Args.hasArg(options::OPT_static)))
    CmdArgs.push_back("-static");

  Args.AddAllArgValues(CmdArgs, options::OPT_Wa_COMMA, options::OPT_Xassembler);

  assert(Output.isFilename() && "Unexpected lipo output.");
  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  assert(Input.isFilename() && "Invalid input.");
  CmdArgs.push_back(Input.getFilename());

  // asm_final spec is empty.

  const char *Exec = Args.MakeArgString(getToolChain().GetProgramPath("as"));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(),
                                         Exec, CmdArgs, Inputs, Output));
}

void ravynos::MachOTool::anchor() {}

void ravynos::MachOTool::AddMachOArch(const ArgList &Args,
                                     ArgStringList &CmdArgs) const {
  StringRef ArchName = getMachOToolChain().getMachOArchName(Args);

  // Derived from ravynOS_arch spec.
  CmdArgs.push_back("-arch");
  CmdArgs.push_back(Args.MakeArgString(ArchName));

  // FIXME: Is this needed anymore?
  if (ArchName == "arm")
    CmdArgs.push_back("-force_cpusubtype_ALL");
}

bool ravynos::Linker::NeedsTempPath(const InputInfoList &Inputs) const {
  // We only need to generate a temp path for LTO if we aren't compiling object
  // files. When compiling source files, we run 'dsymutil' after linking. We
  // don't run 'dsymutil' when compiling object files.
  for (const auto &Input : Inputs)
    if (Input.getType() != types::TY_Object)
      return true;

  return false;
}

/// Pass -no_deduplicate to ld64 under certain conditions:
///
/// - Either -O0 or -O1 is explicitly specified
/// - No -O option is specified *and* this is a compile+link (implicit -O0)
///
/// Also do *not* add -no_deduplicate when no -O option is specified and this
/// is just a link (we can't imply -O0)
static bool shouldLinkerNotDedup(bool IsLinkerOnlyAction, const ArgList &Args) {
  if (Arg *A = Args.getLastArg(options::OPT_O_Group)) {
    if (A->getOption().matches(options::OPT_O0))
      return true;
    if (A->getOption().matches(options::OPT_O))
      return llvm::StringSwitch<bool>(A->getValue())
                    .Case("1", true)
                    .Default(false);
    return false; // OPT_Ofast & OPT_O4
  }

  if (!IsLinkerOnlyAction) // Implicit -O0 for compile+linker only.
    return true;
  return false;
}

void ravynos::Linker::AddLinkArgs(Compilation &C, const ArgList &Args,
                                 ArgStringList &CmdArgs,
                                 const InputInfoList &Inputs,
                                 unsigned Version[5], bool LinkerIsLLD) const {
  const Driver &D = getToolChain().getDriver();
  const toolchains::MachO &MachOTC = getMachOToolChain();

  // Newer linkers support -demangle. Pass it if supported and not disabled by
  // the user.
  if ((Version[0] >= 100 || LinkerIsLLD) &&
      !Args.hasArg(options::OPT_Z_Xlinker__no_demangle))
    CmdArgs.push_back("-demangle");

  // FIXME: Pass most of the flags below that check Version if LinkerIsLLD too.

  if (Args.hasArg(options::OPT_rdynamic) && Version[0] >= 137)
    CmdArgs.push_back("-export_dynamic");

  // If we are using App Extension restrictions, pass a flag to the linker
  // telling it that the compiled code has been audited.
  if (Args.hasFlag(options::OPT_fapplication_extension,
                   options::OPT_fno_application_extension, false))
    CmdArgs.push_back("-application_extension");

  if (D.isUsingLTO() && Version[0] >= 116 && NeedsTempPath(Inputs)) {
    std::string TmpPathName;
    if (D.getLTOMode() == LTOK_Full) {
      // If we are using full LTO, then automatically create a temporary file
      // path for the linker to use, so that it's lifetime will extend past a
      // possible dsymutil step.
      TmpPathName =
          D.GetTemporaryPath("cc", types::getTypeTempSuffix(types::TY_Object));
    } else if (D.getLTOMode() == LTOK_Thin)
      // If we are using thin LTO, then create a directory instead.
      TmpPathName = D.GetTemporaryDirectory("thinlto");

    if (!TmpPathName.empty()) {
      auto *TmpPath = C.getArgs().MakeArgString(TmpPathName);
      C.addTempFile(TmpPath);
      CmdArgs.push_back("-object_path_lto");
      CmdArgs.push_back(TmpPath);
    }
  }

  // Use -lto_library option to specify the libLTO.dylib path. Try to find
  // it in clang installed libraries. ld64 will only look at this argument
  // when it actually uses LTO, so libLTO.dylib only needs to exist at link
  // time if ld64 decides that it needs to use LTO.
  // Since this is passed unconditionally, ld64 will never look for libLTO.dylib
  // next to it. That's ok since ld64 using a libLTO.dylib not matching the
  // clang version won't work anyways.
  // lld is built at the same revision as clang and statically links in
  // LLVM libraries, so it doesn't need libLTO.dylib.
  if (Version[0] >= 133 && !LinkerIsLLD) {
    // Search for libLTO in <InstalledDir>/../lib/libLTO.dylib
    StringRef P = llvm::sys::path::parent_path(D.Dir);
    SmallString<128> LibLTOPath(P);
    llvm::sys::path::append(LibLTOPath, "lib");
    llvm::sys::path::append(LibLTOPath, "libLTO.dylib");
    CmdArgs.push_back("-lto_library");
    CmdArgs.push_back(C.getArgs().MakeArgString(LibLTOPath));
  }

  // ld64 version 262 and above run the deduplicate pass by default.
  if (Version[0] >= 262 && shouldLinkerNotDedup(C.getJobs().empty(), Args))
    CmdArgs.push_back("-no_deduplicate");

  // Derived from the "link" spec.
  Args.AddAllArgs(CmdArgs, options::OPT_static);
  if (!Args.hasArg(options::OPT_static))
    CmdArgs.push_back("-dynamic");
  if (Args.hasArg(options::OPT_fgnu_runtime)) {
    // FIXME: gcc replaces -lobjc in forward args with -lobjc-gnu
    // here. How do we wish to handle such things?
  }

  if (!Args.hasArg(options::OPT_dynamiclib)) {
    AddMachOArch(Args, CmdArgs);
    // FIXME: Why do this only on this path?
    Args.AddLastArg(CmdArgs, options::OPT_force__cpusubtype__ALL);

    Args.AddLastArg(CmdArgs, options::OPT_bundle);
    Args.AddAllArgs(CmdArgs, options::OPT_bundle__loader);
    Args.AddAllArgs(CmdArgs, options::OPT_client__name);

    Arg *A;
    if ((A = Args.getLastArg(options::OPT_compatibility__version)) ||
        (A = Args.getLastArg(options::OPT_current__version)) ||
        (A = Args.getLastArg(options::OPT_install__name)))
      D.Diag(diag::err_drv_argument_only_allowed_with) << A->getAsString(Args)
                                                       << "-dynamiclib";

    Args.AddLastArg(CmdArgs, options::OPT_force__flat__namespace);
    Args.AddLastArg(CmdArgs, options::OPT_keep__private__externs);
    Args.AddLastArg(CmdArgs, options::OPT_private__bundle);
  } else {
    CmdArgs.push_back("-dylib");

    Arg *A;
    if ((A = Args.getLastArg(options::OPT_bundle)) ||
        (A = Args.getLastArg(options::OPT_bundle__loader)) ||
        (A = Args.getLastArg(options::OPT_client__name)) ||
        (A = Args.getLastArg(options::OPT_force__flat__namespace)) ||
        (A = Args.getLastArg(options::OPT_keep__private__externs)) ||
        (A = Args.getLastArg(options::OPT_private__bundle)))
      D.Diag(diag::err_drv_argument_not_allowed_with) << A->getAsString(Args)
                                                      << "-dynamiclib";

    Args.AddAllArgsTranslated(CmdArgs, options::OPT_compatibility__version,
                              "-dylib_compatibility_version");
    Args.AddAllArgsTranslated(CmdArgs, options::OPT_current__version,
                              "-dylib_current_version");

    AddMachOArch(Args, CmdArgs);

    Args.AddAllArgsTranslated(CmdArgs, options::OPT_install__name,
                              "-dylib_install_name");
  }

  Args.AddLastArg(CmdArgs, options::OPT_all__load);
  Args.AddAllArgs(CmdArgs, options::OPT_allowable__client);
  Args.AddLastArg(CmdArgs, options::OPT_bind__at__load);
  if (MachOTC.isTargetIOSBased())
    Args.AddLastArg(CmdArgs, options::OPT_arch__errors__fatal);
  Args.AddLastArg(CmdArgs, options::OPT_dead__strip);
  Args.AddLastArg(CmdArgs, options::OPT_no__dead__strip__inits__and__terms);
  Args.AddAllArgs(CmdArgs, options::OPT_dylib__file);
  Args.AddLastArg(CmdArgs, options::OPT_dynamic);
  Args.AddAllArgs(CmdArgs, options::OPT_exported__symbols__list);
  Args.AddLastArg(CmdArgs, options::OPT_flat__namespace);
  Args.AddAllArgs(CmdArgs, options::OPT_force__load);
  Args.AddAllArgs(CmdArgs, options::OPT_headerpad__max__install__names);
  Args.AddAllArgs(CmdArgs, options::OPT_image__base);
  Args.AddAllArgs(CmdArgs, options::OPT_init);

  // Add the deployment target.
  if (Version[0] >= 520 || LinkerIsLLD)
    MachOTC.addPlatformVersionArgs(Args, CmdArgs);
  else
    MachOTC.addMinVersionArgs(Args, CmdArgs);

  Args.AddLastArg(CmdArgs, options::OPT_nomultidefs);
  Args.AddLastArg(CmdArgs, options::OPT_multi__module);
  Args.AddLastArg(CmdArgs, options::OPT_single__module);
  Args.AddAllArgs(CmdArgs, options::OPT_multiply__defined);
  Args.AddAllArgs(CmdArgs, options::OPT_multiply__defined__unused);

  if (const Arg *A =
          Args.getLastArg(options::OPT_fpie, options::OPT_fPIE,
                          options::OPT_fno_pie, options::OPT_fno_PIE)) {
    if (A->getOption().matches(options::OPT_fpie) ||
        A->getOption().matches(options::OPT_fPIE))
      CmdArgs.push_back("-pie");
    else
      CmdArgs.push_back("-no_pie");
  }

  // for embed-bitcode, use -bitcode_bundle in linker command
  if (C.getDriver().embedBitcodeEnabled()) {
    // Check if the toolchain supports bitcode build flow.
    if (MachOTC.SupportsEmbeddedBitcode()) {
      CmdArgs.push_back("-bitcode_bundle");
      if (C.getDriver().embedBitcodeMarkerOnly() && Version[0] >= 278) {
        CmdArgs.push_back("-bitcode_process_mode");
        CmdArgs.push_back("marker");
      }
    } else
      D.Diag(diag::err_drv_bitcode_unsupported_on_toolchain);
  }

  // If GlobalISel is enabled, pass it through to LLVM.
  if (Arg *A = Args.getLastArg(options::OPT_fglobal_isel,
                               options::OPT_fno_global_isel)) {
    if (A->getOption().matches(options::OPT_fglobal_isel)) {
      CmdArgs.push_back("-mllvm");
      CmdArgs.push_back("-global-isel");
      // Disable abort and fall back to SDAG silently.
      CmdArgs.push_back("-mllvm");
      CmdArgs.push_back("-global-isel-abort=0");
    }
  }

  Args.AddLastArg(CmdArgs, options::OPT_prebind);
  Args.AddLastArg(CmdArgs, options::OPT_noprebind);
  Args.AddLastArg(CmdArgs, options::OPT_nofixprebinding);
  Args.AddLastArg(CmdArgs, options::OPT_prebind__all__twolevel__modules);
  Args.AddLastArg(CmdArgs, options::OPT_read__only__relocs);
  Args.AddAllArgs(CmdArgs, options::OPT_sectcreate);
  Args.AddAllArgs(CmdArgs, options::OPT_sectorder);
  Args.AddAllArgs(CmdArgs, options::OPT_seg1addr);
  Args.AddAllArgs(CmdArgs, options::OPT_segprot);
  Args.AddAllArgs(CmdArgs, options::OPT_segaddr);
  Args.AddAllArgs(CmdArgs, options::OPT_segs__read__only__addr);
  Args.AddAllArgs(CmdArgs, options::OPT_segs__read__write__addr);
  Args.AddAllArgs(CmdArgs, options::OPT_seg__addr__table);
  Args.AddAllArgs(CmdArgs, options::OPT_seg__addr__table__filename);
  Args.AddAllArgs(CmdArgs, options::OPT_sub__library);
  Args.AddAllArgs(CmdArgs, options::OPT_sub__umbrella);

  // Give --sysroot= preference, over the Apple specific behavior to also use
  // --isysroot as the syslibroot.
  StringRef sysroot = C.getSysRoot();
  if (sysroot != "") {
    CmdArgs.push_back("-syslibroot");
    CmdArgs.push_back(C.getArgs().MakeArgString(sysroot));
  } else if (const Arg *A = Args.getLastArg(options::OPT_isysroot)) {
    CmdArgs.push_back("-syslibroot");
    CmdArgs.push_back(A->getValue());
  }

  Args.AddLastArg(CmdArgs, options::OPT_twolevel__namespace);
  Args.AddLastArg(CmdArgs, options::OPT_twolevel__namespace__hints);
  Args.AddAllArgs(CmdArgs, options::OPT_umbrella);
  Args.AddAllArgs(CmdArgs, options::OPT_undefined);
  Args.AddAllArgs(CmdArgs, options::OPT_unexported__symbols__list);
  Args.AddAllArgs(CmdArgs, options::OPT_weak__reference__mismatches);
  Args.AddLastArg(CmdArgs, options::OPT_X_Flag);
  Args.AddAllArgs(CmdArgs, options::OPT_y);
  Args.AddLastArg(CmdArgs, options::OPT_w);
  Args.AddAllArgs(CmdArgs, options::OPT_pagezero__size);
  Args.AddAllArgs(CmdArgs, options::OPT_segs__read__);
  Args.AddLastArg(CmdArgs, options::OPT_seglinkedit);
  Args.AddLastArg(CmdArgs, options::OPT_noseglinkedit);
  Args.AddAllArgs(CmdArgs, options::OPT_sectalign);
  Args.AddAllArgs(CmdArgs, options::OPT_sectobjectsymbols);
  Args.AddAllArgs(CmdArgs, options::OPT_segcreate);
  Args.AddLastArg(CmdArgs, options::OPT_why_load);
  Args.AddLastArg(CmdArgs, options::OPT_whatsloaded);
  Args.AddAllArgs(CmdArgs, options::OPT_dylinker__install__name);
  Args.AddLastArg(CmdArgs, options::OPT_dylinker);
  Args.AddLastArg(CmdArgs, options::OPT_Mach);
}

/// Determine whether we are linking the ObjC runtime.
static bool isObjCRuntimeLinked(const ArgList &Args) {
  if (isObjCAutoRefCount(Args)) {
    Args.ClaimAllArgs(options::OPT_fobjc_link_runtime);
    return true;
  }
  return Args.hasArg(options::OPT_fobjc_link_runtime);
}

static bool checkRemarksOptions(const Driver &D, const ArgList &Args,
                                const llvm::Triple &Triple) {
  // When enabling remarks, we need to error if:
  // * The remark file is specified but we're targeting multiple architectures,
  // which means more than one remark file is being generated.
  bool hasMultipleInvocations =
      Args.getAllArgValues(options::OPT_arch).size() > 1;
  bool hasExplicitOutputFile =
      Args.getLastArg(options::OPT_foptimization_record_file_EQ);
  if (hasMultipleInvocations && hasExplicitOutputFile) {
    D.Diag(diag::err_drv_invalid_output_with_multiple_archs)
        << "-foptimization-record-file";
    return false;
  }
  return true;
}

static void renderRemarksOptions(const ArgList &Args, ArgStringList &CmdArgs,
                                 const llvm::Triple &Triple,
                                 const InputInfo &Output, const JobAction &JA) {
  StringRef Format = "yaml";
  if (const Arg *A = Args.getLastArg(options::OPT_fsave_optimization_record_EQ))
    Format = A->getValue();

  CmdArgs.push_back("-mllvm");
  CmdArgs.push_back("-lto-pass-remarks-output");
  CmdArgs.push_back("-mllvm");

  const Arg *A = Args.getLastArg(options::OPT_foptimization_record_file_EQ);
  if (A) {
    CmdArgs.push_back(A->getValue());
  } else {
    assert(Output.isFilename() && "Unexpected ld output.");
    SmallString<128> F;
    F = Output.getFilename();
    F += ".opt.";
    F += Format;

    CmdArgs.push_back(Args.MakeArgString(F));
  }

  if (const Arg *A =
          Args.getLastArg(options::OPT_foptimization_record_passes_EQ)) {
    CmdArgs.push_back("-mllvm");
    std::string Passes =
        std::string("-lto-pass-remarks-filter=") + A->getValue();
    CmdArgs.push_back(Args.MakeArgString(Passes));
  }

  if (!Format.empty()) {
    CmdArgs.push_back("-mllvm");
    Twine FormatArg = Twine("-lto-pass-remarks-format=") + Format;
    CmdArgs.push_back(Args.MakeArgString(FormatArg));
  }

  if (getLastProfileUseArg(Args)) {
    CmdArgs.push_back("-mllvm");
    CmdArgs.push_back("-lto-pass-remarks-with-hotness");

    if (const Arg *A =
            Args.getLastArg(options::OPT_fdiagnostics_hotness_threshold_EQ)) {
      CmdArgs.push_back("-mllvm");
      std::string Opt =
          std::string("-lto-pass-remarks-hotness-threshold=") + A->getValue();
      CmdArgs.push_back(Args.MakeArgString(Opt));
    }
  }
}

void ravynos::Linker::ConstructJob(Compilation &C, const JobAction &JA,
                                  const InputInfo &Output,
                                  const InputInfoList &Inputs,
                                  const ArgList &Args,
                                  const char *LinkingOutput) const {
  assert(Output.getType() == types::TY_Image && "Invalid linker output type.");

  // If the number of arguments surpasses the system limits, we will encode the
  // input files in a separate file, shortening the command line. To this end,
  // build a list of input file names that can be passed via a file with the
  // -filelist linker option.
  llvm::opt::ArgStringList InputFileList;

  // The logic here is derived from gcc's behavior; most of which
  // comes from specs (starting with link_command). Consult gcc for
  // more information.
  ArgStringList CmdArgs;

  /// Hack(tm) to ignore linking errors when we are doing ARC migration.
  if (Args.hasArg(options::OPT_ccc_arcmt_check,
                  options::OPT_ccc_arcmt_migrate)) {
    for (const auto &Arg : Args)
      Arg->claim();
    const char *Exec =
        Args.MakeArgString(getToolChain().GetProgramPath("touch"));
    CmdArgs.push_back(Output.getFilename());
    C.addCommand(std::make_unique<Command>(
        JA, *this, ResponseFileSupport::None(), Exec, CmdArgs, None, Output));
    return;
  }

  unsigned Version[5] = {0, 0, 0, 0, 0};
  if (Arg *A = Args.getLastArg(options::OPT_mlinker_version_EQ)) {
    if (!Driver::GetReleaseVersion(A->getValue(), Version))
      getToolChain().getDriver().Diag(diag::err_drv_invalid_version_number)
          << A->getAsString(Args);
  }

  bool LinkerIsLLD;
  const char *Exec =
      Args.MakeArgString(getToolChain().GetLinkerPath(&LinkerIsLLD));

  // I'm not sure why this particular decomposition exists in gcc, but
  // we follow suite for ease of comparison.
  AddLinkArgs(C, Args, CmdArgs, Inputs, Version, LinkerIsLLD);

  if (willEmitRemarks(Args) &&
      checkRemarksOptions(getToolChain().getDriver(), Args,
                          getToolChain().getTriple()))
    renderRemarksOptions(Args, CmdArgs, getToolChain().getTriple(), Output, JA);

  // Propagate the -moutline flag to the linker in LTO.
  if (Arg *A =
          Args.getLastArg(options::OPT_moutline, options::OPT_mno_outline)) {
    if (A->getOption().matches(options::OPT_moutline)) {
      if (getMachOToolChain().getMachOArchName(Args) == "arm64") {
        CmdArgs.push_back("-mllvm");
        CmdArgs.push_back("-enable-machine-outliner");

        // Outline from linkonceodr functions by default in LTO.
        CmdArgs.push_back("-mllvm");
        CmdArgs.push_back("-enable-linkonceodr-outlining");
      }
    } else {
      // Disable all outlining behaviour if we have mno-outline. We need to do
      // this explicitly, because targets which support default outlining will
      // try to do work if we don't.
      CmdArgs.push_back("-mllvm");
      CmdArgs.push_back("-enable-machine-outliner=never");
    }
  }

  // Setup statistics file output.
  SmallString<128> StatsFile =
      getStatsFileName(Args, Output, Inputs[0], getToolChain().getDriver());
  if (!StatsFile.empty()) {
    CmdArgs.push_back("-mllvm");
    CmdArgs.push_back(Args.MakeArgString("-lto-stats-file=" + StatsFile.str()));
  }

  // It seems that the 'e' option is completely ignored for dynamic executables
  // (the default), and with static executables, the last one wins, as expected.
  Args.AddAllArgs(CmdArgs, {options::OPT_d_Flag, options::OPT_s, options::OPT_t,
                            options::OPT_Z_Flag, options::OPT_u_Group,
                            options::OPT_e, options::OPT_r});

  // Forward -ObjC when either -ObjC or -ObjC++ is used, to force loading
  // members of static archive libraries which implement Objective-C classes or
  // categories.
  if (Args.hasArg(options::OPT_ObjC) || Args.hasArg(options::OPT_ObjCXX))
    CmdArgs.push_back("-ObjC");

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nostartfiles))
    getMachOToolChain().addStartObjectFileArgs(Args, CmdArgs);

  Args.AddAllArgs(CmdArgs, options::OPT_L);

  AddLinkerInputs(getToolChain(), Inputs, Args, CmdArgs, JA);
  // Build the input file for -filelist (list of linker input files) in case we
  // need it later
  for (const auto &II : Inputs) {
    if (!II.isFilename()) {
      // This is a linker input argument.
      // We cannot mix input arguments and file names in a -filelist input, thus
      // we prematurely stop our list (remaining files shall be passed as
      // arguments).
      if (InputFileList.size() > 0)
        break;

      continue;
    }

    InputFileList.push_back(II.getFilename());
  }

  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs))
    addOpenMPRuntime(CmdArgs, getToolChain(), Args);

  if (isObjCRuntimeLinked(Args) &&
      !Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs)) {
    // We use arclite library for both ARC and subscripting support.
    getMachOToolChain().AddLinkARCArgs(Args, CmdArgs);

    CmdArgs.push_back("-framework");
    CmdArgs.push_back("Foundation");
    // Link libobj.
    CmdArgs.push_back("-lobjc");
  }

  if (LinkingOutput) {
    CmdArgs.push_back("-arch_multiple");
    CmdArgs.push_back("-final_output");
    CmdArgs.push_back(LinkingOutput);
  }

  if (Args.hasArg(options::OPT_fnested_functions))
    CmdArgs.push_back("-allow_stack_execute");

  getMachOToolChain().addProfileRTLibs(Args, CmdArgs);

  StringRef Parallelism = getLTOParallelism(Args, getToolChain().getDriver());
  if (!Parallelism.empty()) {
    CmdArgs.push_back("-mllvm");
    unsigned NumThreads =
        llvm::get_threadpool_strategy(Parallelism)->compute_thread_count();
    CmdArgs.push_back(Args.MakeArgString("-threads=" + Twine(NumThreads)));
  }

  if (getToolChain().ShouldLinkCXXStdlib(Args))
    getToolChain().AddCXXStdlibLibArgs(Args, CmdArgs);

  bool NoStdOrDefaultLibs =
      Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs);
  bool ForceLinkBuiltins = Args.hasArg(options::OPT_fapple_link_rtlib);
  if (!NoStdOrDefaultLibs || ForceLinkBuiltins) {
    // link_ssp spec is empty.

    // If we have both -nostdlib/nodefaultlibs and -fapple-link-rtlib then
    // we just want to link the builtins, not the other libs like libSystem.
    if (NoStdOrDefaultLibs && ForceLinkBuiltins) {
      getMachOToolChain().AddLinkRuntimeLib(Args, CmdArgs, "builtins");
    } else {
      // Let the tool chain choose which runtime library to link.
      getMachOToolChain().AddLinkRuntimeLibArgs(Args, CmdArgs,
                                                ForceLinkBuiltins);

      // No need to do anything for pthreads. Claim argument to avoid warning.
      Args.ClaimAllArgs(options::OPT_pthread);
      Args.ClaimAllArgs(options::OPT_pthreads);
    }
  }

  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nostartfiles)) {
    // endfile_spec is empty.
  }

  Args.AddAllArgs(CmdArgs, options::OPT_T_Group);
  Args.AddAllArgs(CmdArgs, options::OPT_F);

  // -iframework should be forwarded as -F.
  for (const Arg *A : Args.filtered(options::OPT_iframework))
    CmdArgs.push_back(Args.MakeArgString(std::string("-F") + A->getValue()));

  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs)) {
    if (Arg *A = Args.getLastArg(options::OPT_fveclib)) {
      if (A->getValue() == StringRef("Accelerate")) {
        CmdArgs.push_back("-framework");
        CmdArgs.push_back("Accelerate");
      }
    }
  }

  ResponseFileSupport ResponseSupport;
  if (Version[0] >= 705 || LinkerIsLLD) {
    ResponseSupport = ResponseFileSupport::AtFileUTF8();
  } else {
    // For older versions of the linker, use the legacy filelist method instead.
    ResponseSupport = {ResponseFileSupport::RF_FileList, llvm::sys::WEM_UTF8,
                       "-filelist"};
  }

  std::unique_ptr<Command> Cmd = std::make_unique<Command>(
      JA, *this, ResponseSupport, Exec, CmdArgs, Inputs, Output);
  Cmd->setInputFileList(std::move(InputFileList));
  C.addCommand(std::move(Cmd));
}

void ravynos::StaticLibTool::ConstructJob(Compilation &C, const JobAction &JA,
                                         const InputInfo &Output,
                                         const InputInfoList &Inputs,
                                         const ArgList &Args,
                                         const char *LinkingOutput) const {
  const Driver &D = getToolChain().getDriver();

  // Silence warning for "clang -g foo.o -o foo"
  Args.ClaimAllArgs(options::OPT_g_Group);
  // and "clang -emit-llvm foo.o -o foo"
  Args.ClaimAllArgs(options::OPT_emit_llvm);
  // and for "clang -w foo.o -o foo". Other warning options are already
  // handled somewhere else.
  Args.ClaimAllArgs(options::OPT_w);
  // Silence warnings when linking C code with a C++ '-stdlib' argument.
  Args.ClaimAllArgs(options::OPT_stdlib_EQ);

  // libtool <options> <output_file> <input_files>
  ArgStringList CmdArgs;
  // Create and insert file members with a deterministic index.
  CmdArgs.push_back("-static");
  CmdArgs.push_back("-D");
  CmdArgs.push_back("-no_warning_for_no_symbols");
  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  for (const auto &II : Inputs) {
    if (II.isFilename()) {
      CmdArgs.push_back(II.getFilename());
    }
  }

  // Delete old output archive file if it already exists before generating a new
  // archive file.
  const auto *OutputFileName = Output.getFilename();
  if (Output.isFilename() && llvm::sys::fs::exists(OutputFileName)) {
    if (std::error_code EC = llvm::sys::fs::remove(OutputFileName)) {
      D.Diag(diag::err_drv_unable_to_remove_file) << EC.message();
      return;
    }
  }

  const char *Exec = Args.MakeArgString(getToolChain().GetStaticLibToolPath());
  C.addCommand(std::make_unique<Command>(JA, *this,
                                         ResponseFileSupport::AtFileUTF8(),
                                         Exec, CmdArgs, Inputs, Output));
}

void ravynos::Lipo::ConstructJob(Compilation &C, const JobAction &JA,
                                const InputInfo &Output,
                                const InputInfoList &Inputs,
                                const ArgList &Args,
                                const char *LinkingOutput) const {
  ArgStringList CmdArgs;

  CmdArgs.push_back("-create");
  assert(Output.isFilename() && "Unexpected lipo output.");

  CmdArgs.push_back("-output");
  CmdArgs.push_back(Output.getFilename());

  for (const auto &II : Inputs) {
    assert(II.isFilename() && "Unexpected lipo input.");
    CmdArgs.push_back(II.getFilename());
  }

  const char *Exec = Args.MakeArgString(getToolChain().GetProgramPath("lipo"));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(),
                                         Exec, CmdArgs, Inputs, Output));
}

void ravynos::Dsymutil::ConstructJob(Compilation &C, const JobAction &JA,
                                    const InputInfo &Output,
                                    const InputInfoList &Inputs,
                                    const ArgList &Args,
                                    const char *LinkingOutput) const {
  ArgStringList CmdArgs;

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  assert(Inputs.size() == 1 && "Unable to handle multiple inputs.");
  const InputInfo &Input = Inputs[0];
  assert(Input.isFilename() && "Unexpected dsymutil input.");
  CmdArgs.push_back(Input.getFilename());

  const char *Exec =
      Args.MakeArgString(getToolChain().GetProgramPath("dsymutil"));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(),
                                         Exec, CmdArgs, Inputs, Output));
}

void ravynos::VerifyDebug::ConstructJob(Compilation &C, const JobAction &JA,
                                       const InputInfo &Output,
                                       const InputInfoList &Inputs,
                                       const ArgList &Args,
                                       const char *LinkingOutput) const {
  ArgStringList CmdArgs;
  CmdArgs.push_back("--verify");
  CmdArgs.push_back("--debug-info");
  CmdArgs.push_back("--eh-frame");
  CmdArgs.push_back("--quiet");

  assert(Inputs.size() == 1 && "Unable to handle multiple inputs.");
  const InputInfo &Input = Inputs[0];
  assert(Input.isFilename() && "Unexpected verify input");

  // Grabbing the output of the earlier dsymutil run.
  CmdArgs.push_back(Input.getFilename());

  const char *Exec =
      Args.MakeArgString(getToolChain().GetProgramPath("dwarfdump"));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(),
                                         Exec, CmdArgs, Inputs, Output));
}

/// ravynOS tool chain for i386 and x86_64.
RavynOS::RavynOS(const Driver &D, const llvm::Triple &Triple, const ArgList &Args)
    : MachO(D, Triple, Args), TargetInitialized(false),
      CudaInstallation(D, Triple, Args), RocmInstallation(D, Triple, Args) {}

ToolChain::CXXStdlibType RavynOS::GetDefaultCXXStdlibType() const {
    return ToolChain::CST_Libcxx;
}

/// RavynOS provides an ARC runtime starting in MacOS X 10.7 and iOS 5.0.
ObjCRuntime RavynOS::getDefaultObjCRuntime(bool isNonFragile) const {
  if (isNonFragile)
    return ObjCRuntime(ObjCRuntime::MacOSX, TargetVersion);
  return ObjCRuntime(ObjCRuntime::FragileMacOSX, TargetVersion); // FIXME: should be GNUstep?
}

/// RavynOS provides a blocks runtime starting in MacOS X 10.6 and iOS 3.2.
bool RavynOS::hasBlocksRuntime() const {
  return true;
}

void RavynOS::AddCudaIncludeArgs(const ArgList &DriverArgs,
                                ArgStringList &CC1Args) const {
  CudaInstallation.AddCudaIncludeArgs(DriverArgs, CC1Args);
}

void RavynOS::AddHIPIncludeArgs(const ArgList &DriverArgs,
                               ArgStringList &CC1Args) const {
  RocmInstallation.AddHIPIncludeArgs(DriverArgs, CC1Args);
}

// This is just a MachO name translation routine and there's no
// way to join this into ARMTargetParser without breaking all
// other assumptions. Maybe MachO should consider standardising
// their nomenclature.
static const char *ArmMachOArchName(StringRef Arch) {
  return llvm::StringSwitch<const char *>(Arch)
      .Case("armv6k", "armv6")
      .Case("armv6m", "armv6m")
      .Case("armv5tej", "armv5")
      .Case("xscale", "xscale")
      .Case("armv4t", "armv4t")
      .Case("armv7", "armv7")
      .Cases("armv7a", "armv7-a", "armv7")
      .Cases("armv7r", "armv7-r", "armv7")
      .Cases("armv7em", "armv7e-m", "armv7em")
      .Cases("armv7k", "armv7-k", "armv7k")
      .Cases("armv7m", "armv7-m", "armv7m")
      .Cases("armv7s", "armv7-s", "armv7s")
      .Default(nullptr);
}

static const char *ArmMachOArchNameCPU(StringRef CPU) {
  llvm::ARM::ArchKind ArchKind = llvm::ARM::parseCPUArch(CPU);
  if (ArchKind == llvm::ARM::ArchKind::INVALID)
    return nullptr;
  StringRef Arch = llvm::ARM::getArchName(ArchKind);

  // FIXME: Make sure this MachO triple mangling is really necessary.
  // ARMv5* normalises to ARMv5.
  if (Arch.startswith("armv5"))
    Arch = Arch.substr(0, 5);
  // ARMv6*, except ARMv6M, normalises to ARMv6.
  else if (Arch.startswith("armv6") && !Arch.endswith("6m"))
    Arch = Arch.substr(0, 5);
  // ARMv7A normalises to ARMv7.
  else if (Arch.endswith("v7a"))
    Arch = Arch.substr(0, 5);
  return Arch.data();
}

RavynOS::~RavynOS() {}

std::string RavynOS::ComputeEffectiveClangTriple(const ArgList &Args,
                                                types::ID InputType) const {
  llvm::Triple Triple(ComputeLLVMTriple(Args, InputType));

  // If the target isn't initialized (e.g., an unknown RavynOS platform, return
  // the default triple).
  if (!isTargetInitialized())
    return Triple.getTriple();

  SmallString<16> Str;
  Str += "ravynos";
  Str += getTripleTargetVersion().getAsString();
  Triple.setOSName(Str);

  return Triple.getTriple();
}

RavynOSClang::RavynOSClang(const Driver &D, const llvm::Triple &Triple,
                         const ArgList &Args)
    : RavynOS(D, Triple, Args) {}

void RavynOSClang::addClangWarningOptions(ArgStringList &CC1Args) const {
  // Always error about undefined 'TARGET_OS_*' macros.
  CC1Args.push_back("-Wundef-prefix=TARGET_OS_");
  CC1Args.push_back("-Werror=undef-prefix");

#ifdef notyet
  // For modern targets, promote certain warnings to errors.
  if (getTriple().isArch64Bit()) {
    // Always enable -Wdeprecated-objc-isa-usage and promote it
    // to an error.
    CC1Args.push_back("-Wdeprecated-objc-isa-usage");
    CC1Args.push_back("-Werror=deprecated-objc-isa-usage");
  }
#endif
}

/// Take a path that speculatively points into Xcode and return the
/// `XCODE/Contents/Developer` path if it is an Xcode path, or an empty path
/// otherwise.
static StringRef getXcodeDeveloperPath(StringRef PathIntoXcode) {
  return "";
}

void RavynOSClang::AddLinkARCArgs(const ArgList &Args,
                                 ArgStringList &CmdArgs) const {
  return;
}

unsigned RavynOSClang::GetDefaultDwarfVersion() const {
  return 4;
}

StringRef RavynOS::getPlatformFamily() const {
  return "ravynOS";
}

StringRef RavynOS::getSDKName(StringRef isysroot) {
  // Assume SDK has path: SOME_PATH/SDKs/PlatformXX.YY.sdk
  auto BeginSDK = llvm::sys::path::rbegin(isysroot);
  auto EndSDK = llvm::sys::path::rend(isysroot);
  for (auto IT = BeginSDK; IT != EndSDK; ++IT) {
    StringRef SDK = *IT;
    if (SDK.endswith(".sdk"))
      return SDK.slice(0, SDK.size() - 4);
  }
  return "";
}

StringRef RavynOS::getOSLibraryNameSuffix(bool IgnoreSim) const {
  return "";
}

/// Check if the link command contains a symbol export directive.
static bool hasExportSymbolDirective(const ArgList &Args) {
  for (Arg *A : Args) {
    if (A->getOption().matches(options::OPT_exported__symbols__list))
      return true;
    if (!A->getOption().matches(options::OPT_Wl_COMMA) &&
        !A->getOption().matches(options::OPT_Xlinker))
      continue;
    if (A->containsValue("-exported_symbols_list") ||
        A->containsValue("-exported_symbol"))
      return true;
  }
  return false;
}

/// Add an export directive for \p Symbol to the link command.
static void addExportedSymbol(ArgStringList &CmdArgs, const char *Symbol) {
  CmdArgs.push_back("-exported_symbol");
  CmdArgs.push_back(Symbol);
}

/// Add a sectalign directive for \p Segment and \p Section to the maximum
/// expected page size for RavynOS.
///
/// On iPhone 6+ the max supported page size is 16K. On macOS, the max is 4K.
/// Use a common alignment constant (16K) for now, and reduce the alignment on
/// macOS if it proves important.
static void addSectalignToPage(const ArgList &Args, ArgStringList &CmdArgs,
                               StringRef Segment, StringRef Section) {
  for (const char *A : {"-sectalign", Args.MakeArgString(Segment),
                        Args.MakeArgString(Section), "0x4000"})
    CmdArgs.push_back(A);
}

void RavynOS::addProfileRTLibs(const ArgList &Args,
                              ArgStringList &CmdArgs) const {
  if (!needsProfileRT(Args) && !needsGCovInstrumentation(Args))
    return;

  AddLinkRuntimeLib(Args, CmdArgs, "profile",
                    RuntimeLinkOptions(RLO_AlwaysLink));

  bool ForGCOV = needsGCovInstrumentation(Args);

  // If we have a symbol export directive and we're linking in the profile
  // runtime, automatically export symbols necessary to implement some of the
  // runtime's functionality.
  if (hasExportSymbolDirective(Args)) {
    if (ForGCOV) {
      addExportedSymbol(CmdArgs, "___gcov_dump");
      addExportedSymbol(CmdArgs, "___gcov_reset");
      addExportedSymbol(CmdArgs, "_writeout_fn_list");
      addExportedSymbol(CmdArgs, "_reset_fn_list");
    } else {
      addExportedSymbol(CmdArgs, "___llvm_profile_filename");
      addExportedSymbol(CmdArgs, "___llvm_profile_raw_version");
    }
    addExportedSymbol(CmdArgs, "_lprofDirMode");
  }

  // Align __llvm_prf_{cnts,data} sections to the maximum expected page
  // alignment. This allows profile counters to be mmap()'d to disk. Note that
  // it's not enough to just page-align __llvm_prf_cnts: the following section
  // must also be page-aligned so that its data is not clobbered by mmap().
  //
  // The section alignment is only needed when continuous profile sync is
  // enabled, but this is expected to be the default in Xcode. Specifying the
  // extra alignment also allows the same binary to be used with/without sync
  // enabled.
  if (!ForGCOV) {
    for (auto IPSK : {llvm::IPSK_cnts, llvm::IPSK_data}) {
      addSectalignToPage(
          Args, CmdArgs, "__DATA",
          llvm::getInstrProfSectionName(IPSK, llvm::Triple::MachO,
                                        /*AddSegmentInfo=*/false));
    }
  }
}

void RavynOSClang::AddLinkSanitizerLibArgs(const ArgList &Args,
                                          ArgStringList &CmdArgs,
                                          StringRef Sanitizer,
                                          bool Shared) const {
  auto RLO = RuntimeLinkOptions(RLO_AlwaysLink | (Shared ? RLO_AddRPath : 0U));
  AddLinkRuntimeLib(Args, CmdArgs, Sanitizer, RLO, Shared);
}

ToolChain::RuntimeLibType RavynOSClang::GetRuntimeLibType(
    const ArgList &Args) const {
  if (Arg* A = Args.getLastArg(options::OPT_rtlib_EQ)) {
    StringRef Value = A->getValue();
    if (Value != "compiler-rt")
      getDriver().Diag(clang::diag::err_drv_unsupported_rtlib_for_platform)
          << Value << "ravynos";
  }

  return ToolChain::RLT_CompilerRT;
}

void RavynOSClang::AddLinkRuntimeLibArgs(const ArgList &Args,
                                        ArgStringList &CmdArgs,
                                        bool ForceLinkBuiltinRT) const {
  // Call once to ensure diagnostic is printed if wrong value was specified
  GetRuntimeLibType(Args);

  // RavynOS doesn't support real static executables, don't link any runtime
  // libraries with -static.
  if (Args.hasArg(options::OPT_static) ||
      Args.hasArg(options::OPT_fapple_kext) ||
      Args.hasArg(options::OPT_mkernel)) {
    if (ForceLinkBuiltinRT)
      AddLinkRuntimeLib(Args, CmdArgs, "builtins");
    return;
  }

  // Reject -static-libgcc for now, we can deal with this when and if someone
  // cares. This is useful in situations where someone wants to statically link
  // something like libstdc++, and needs its runtime support routines.
  if (const Arg *A = Args.getLastArg(options::OPT_static_libgcc)) {
    getDriver().Diag(diag::err_drv_unsupported_opt) << A->getAsString(Args);
    return;
  }

  const SanitizerArgs &Sanitize = getSanitizerArgs(Args);
  if (Sanitize.needsAsanRt())
    AddLinkSanitizerLibArgs(Args, CmdArgs, "asan");
  if (Sanitize.needsLsanRt())
    AddLinkSanitizerLibArgs(Args, CmdArgs, "lsan");
  if (Sanitize.needsUbsanRt())
    AddLinkSanitizerLibArgs(Args, CmdArgs,
                            Sanitize.requiresMinimalRuntime() ? "ubsan_minimal"
                                                              : "ubsan",
                            Sanitize.needsSharedRt());
  if (Sanitize.needsTsanRt())
    AddLinkSanitizerLibArgs(Args, CmdArgs, "tsan");
  if (Sanitize.needsFuzzer() && !Args.hasArg(options::OPT_dynamiclib)) {
    AddLinkSanitizerLibArgs(Args, CmdArgs, "fuzzer", /*shared=*/false);

    // Libfuzzer is written in C++ and requires libcxx.
    AddCXXStdlibLibArgs(Args, CmdArgs);
  }
  if (Sanitize.needsStatsRt()) {
    AddLinkRuntimeLib(Args, CmdArgs, "stats_client", RLO_AlwaysLink);
    AddLinkSanitizerLibArgs(Args, CmdArgs, "stats");
  }

  const XRayArgs &XRay = getXRayArgs();
  if (XRay.needsXRayRt()) {
    AddLinkRuntimeLib(Args, CmdArgs, "xray");
    AddLinkRuntimeLib(Args, CmdArgs, "xray-basic");
    AddLinkRuntimeLib(Args, CmdArgs, "xray-fdr");
  }

  // Otherwise link libSystem, then the dynamic runtime library, and finally any
  // target specific static runtime library.
  CmdArgs.push_back("-lSystem");

  // Select the dynamic runtime library and the target specific static library.
  AddLinkRuntimeLib(Args, CmdArgs, "builtins");
}

/// Returns the most appropriate macOS target version for the current process.
///
/// If the macOS SDK version is the same or earlier than the system version,
/// then the SDK version is returned. Otherwise the system version is returned.
/// This is mostly faked since ravynOS does not yet have actual SDKs
static std::string getSystemOrSDKMacOSVersion(StringRef MacOSSDKVersion) {
  unsigned Major, Minor, Micro;
  llvm::Triple SystemTriple(llvm::sys::getProcessTriple());
  if (!SystemTriple.isOSRavynOS())
    return std::string(MacOSSDKVersion);
  VersionTuple SystemVersion;
  SystemTriple.getMacOSXVersion(SystemVersion);
  bool HadExtra;
  if (!Driver::GetReleaseVersion(MacOSSDKVersion, Major, Minor, Micro,
                                 HadExtra))
    return std::string(MacOSSDKVersion);
  VersionTuple SDKVersion(Major, Minor, Micro);
  if (SDKVersion > SystemVersion)
    return SystemVersion.getAsString();
  return std::string(MacOSSDKVersion);
}


namespace {

/// The RavynOS OS that was selected or inferred from arguments / environment.
struct RavynOSPlatform {
  enum SourceKind {
    /// The OS was specified using the -target argument.
    TargetArg,
    /// The OS was specified using the -mtargetos= argument.
    MTargetOSArg,
    /// The OS was specified using the -m<os>-version-min argument.
    OSVersionArg,
    /// The OS was specified using the OS_DEPLOYMENT_TARGET environment.
    DeploymentTargetEnv,
    /// The OS was inferred from the SDK.
    InferredFromSDK,
    /// The OS was inferred from the -arch.
    InferredFromArch
  };

  using RavynOSPlatformKind = RavynOS::RavynOSPlatformKind;
  using RavynOSEnvironmentKind = RavynOS::RavynOSEnvironmentKind;

  RavynOSPlatformKind getPlatform() const { return Platform; }

  RavynOSEnvironmentKind getEnvironment() const { return Environment; }

  void setEnvironment(RavynOSEnvironmentKind Kind) {
    Environment = Kind;
    InferSimulatorFromArch = false;
  }

  StringRef getOSVersion() const {
    if (Kind == OSVersionArg)
      return Argument->getValue();
    return OSVersion;
  }

  void setOSVersion(StringRef S) {
    assert(Kind == TargetArg && "Unexpected kind!");
    OSVersion = std::string(S);
  }

  bool hasOSVersion() const { return HasOSVersion; }

  VersionTuple getNativeTargetVersion() const {
    return NativeTargetVersion;
  }

  /// Returns true if the target OS was explicitly specified.
  bool isExplicitlySpecified() const { return Kind <= DeploymentTargetEnv; }

  /// Returns true if the simulator environment can be inferred from the arch.
  bool canInferSimulatorFromArch() const { return InferSimulatorFromArch; }

  /// Adds the -m<os>-version-min argument to the compiler invocation.
  void addOSVersionMinArgument(DerivedArgList &Args, const OptTable &Opts) {
    if (Argument)
      return;
    assert(Kind != TargetArg && Kind != MTargetOSArg && Kind != OSVersionArg &&
           "Invalid kind");
    options::ID Opt;
    Opt = options::OPT_mmacosx_version_min_EQ;
    Argument = Args.MakeJoinedArg(nullptr, Opts.getOption(Opt), VersionTuple(10, 6, 0).getAsString());
    Args.append(Argument);
  }

  /// Returns the OS version with the argument / environment variable that
  /// specified it.
  std::string getAsString(DerivedArgList &Args, const OptTable &Opts) {
    switch (Kind) {
    case TargetArg:
    case MTargetOSArg:
    case OSVersionArg:
    case InferredFromSDK:
    case InferredFromArch:
      assert(Argument && "OS version argument not yet inferred");
      return Argument->getAsString(Args);
    case DeploymentTargetEnv:
      return (llvm::Twine(EnvVarName) + "=" + OSVersion).str();
    }
    llvm_unreachable("Unsupported ravynOS Source Kind");
  }

  void setEnvironment(llvm::Triple::EnvironmentType EnvType,
                      const VersionTuple &OSVersion,
                      const Optional<DarwinSDKInfo> &SDKInfo) {
    // nop
  }

  static RavynOSPlatform
  createFromTarget(const llvm::Triple &TT, StringRef OSVersion, Arg *A,
                   const Optional<DarwinSDKInfo> &SDKInfo) {
    RavynOSPlatform Result(TargetArg, getPlatformFromOS(TT.getOS()), OSVersion,
                          A);
    VersionTuple OsVersion = TT.getOSVersion();
    if (OsVersion.getMajor() == 0)
      Result.HasOSVersion = false;
    Result.setEnvironment(TT.getEnvironment(), OsVersion, SDKInfo);
    return Result;
  }
  static RavynOSPlatform
  createFromMTargetOS(llvm::Triple::OSType OS, VersionTuple OSVersion,
                      llvm::Triple::EnvironmentType Environment, Arg *A,
                      const Optional<DarwinSDKInfo> &SDKInfo) {
    RavynOSPlatform Result(MTargetOSArg, getPlatformFromOS(OS),
                          OSVersion.getAsString(), A);
    Result.InferSimulatorFromArch = false;
    Result.setEnvironment(Environment, OSVersion, SDKInfo);
    return Result;
  }
  static RavynOSPlatform createOSVersionArg(RavynOSPlatformKind Platform,
                                           Arg *A) {
    return RavynOSPlatform(OSVersionArg, Platform, A);
  }
  static RavynOSPlatform createDeploymentTargetEnv(RavynOSPlatformKind Platform,
                                                  StringRef EnvVarName,
                                                  StringRef Value) {
    RavynOSPlatform Result(DeploymentTargetEnv, Platform, Value);
    Result.EnvVarName = EnvVarName;
    return Result;
  }
  static RavynOSPlatform createFromSDK(RavynOSPlatformKind Platform,
                                      StringRef Value,
                                      bool IsSimulator = false) {
    RavynOSPlatform Result(InferredFromSDK, Platform, Value);
    Result.InferSimulatorFromArch = false;
    return Result;
  }
  static RavynOSPlatform createFromArch(llvm::Triple::OSType OS,
                                       StringRef Value) {
    return RavynOSPlatform(InferredFromArch, getPlatformFromOS(OS), Value);
  }

  /// Constructs an inferred SDKInfo value based on the version inferred from
  /// the SDK path itself. Only works for values that were created by inferring
  /// the platform from the SDKPath.
  DarwinSDKInfo inferSDKInfo() {
    assert(Kind == InferredFromSDK && "can infer SDK info only");
    llvm::VersionTuple Version;
    bool IsValid = !Version.tryParse(OSVersion);
    (void)IsValid;
    assert(IsValid && "invalid SDK version");
    return DarwinSDKInfo(
        Version,
        /*MaximumDeploymentTarget=*/VersionTuple(Version.getMajor(), 0, 99));
  }

private:
  RavynOSPlatform(SourceKind Kind, RavynOSPlatformKind Platform, Arg *Argument)
      : Kind(Kind), Platform(Platform), Argument(Argument) {}
  RavynOSPlatform(SourceKind Kind, RavynOSPlatformKind Platform, StringRef Value,
                 Arg *Argument = nullptr)
      : Kind(Kind), Platform(Platform), OSVersion(Value), Argument(Argument) {}

  static RavynOSPlatformKind getPlatformFromOS(llvm::Triple::OSType OS) {
    return RavynOSPlatformKind::ravynOS;
  }

  SourceKind Kind;
  RavynOSPlatformKind Platform;
  RavynOSEnvironmentKind Environment = RavynOSEnvironmentKind::NativeEnvironment;
  VersionTuple NativeTargetVersion;
  std::string OSVersion;
  bool HasOSVersion = true, InferSimulatorFromArch = true;
  Arg *Argument;
  StringRef EnvVarName;
};

/// Returns the deployment target that's specified using the -m<os>-version-min
/// argument.
Optional<RavynOSPlatform>
getDeploymentTargetFromOSVersionArg(DerivedArgList &Args,
                                    const Driver &TheDriver) {
  Arg *OSXVersion = Args.getLastArg(options::OPT_mmacosx_version_min_EQ);
  if (OSXVersion) {
    return RavynOSPlatform::createOSVersionArg(RavynOS::ravynOS, OSXVersion);
  }
  return None;
}

/// Returns the deployment target that's specified using the
/// OS_DEPLOYMENT_TARGET environment variable.
// FIXME: do translation here from Darwin platforms?
Optional<RavynOSPlatform>
getDeploymentTargetFromEnvironmentVariables(const Driver &TheDriver,
                                            const llvm::Triple &Triple) {
  std::string Targets[RavynOS::LastRavynOSPlatform + 1];
  const char *EnvVars[] = {
      "MACOSX_DEPLOYMENT_TARGET"
  };
  static_assert(llvm::array_lengthof(EnvVars) == RavynOS::LastRavynOSPlatform + 1,
                "Missing platform");
  for (const auto &I : llvm::enumerate(llvm::makeArrayRef(EnvVars))) {
    if (char *Env = ::getenv(I.value()))
      Targets[I.index()] = Env;
  }

  for (const auto &Target : llvm::enumerate(llvm::makeArrayRef(Targets))) {
    if (!Target.value().empty())
      return RavynOSPlatform::createDeploymentTargetEnv(
          (RavynOS::RavynOSPlatformKind)Target.index(), EnvVars[Target.index()],
          Target.value());
  }
  return None;
}

/// Returns the SDK name without the optional prefix that ends with a '.' or an
/// empty string otherwise.
static StringRef dropSDKNamePrefix(StringRef SDKName) {
  size_t PrefixPos = SDKName.find('.');
  if (PrefixPos == StringRef::npos)
    return "";
  return SDKName.substr(PrefixPos + 1);
}

/// Tries to infer the deployment target from the SDK specified by -isysroot
/// (or SDKROOT). Uses the version specified in the SDKSettings.json file if
/// it's available.
Optional<RavynOSPlatform>
inferDeploymentTargetFromSDK(DerivedArgList &Args,
                             const Optional<DarwinSDKInfo> &SDKInfo) {
  const Arg *A = Args.getLastArg(options::OPT_isysroot);
  if (!A)
    return None;
  StringRef isysroot = A->getValue();
  StringRef SDK = RavynOS::getSDKName(isysroot);
  if (!SDK.size())
    return None;

  std::string Version;
  if (SDKInfo) {
    // Get the version from the SDKSettings.json if it's available.
    Version = SDKInfo->getVersion().getAsString();
  } else {
    // Slice the version number out.
    // Version number is between the first and the last number.
    size_t StartVer = SDK.find_first_of("0123456789");
    size_t EndVer = SDK.find_last_of("0123456789");
    if (StartVer != StringRef::npos && EndVer > StartVer)
      Version = std::string(SDK.slice(StartVer, EndVer + 1));
  }
  if (Version.empty())
    return None;

  auto CreatePlatformFromSDKName =
      [&](StringRef SDK) -> Optional<RavynOSPlatform> {
    return RavynOSPlatform::createFromSDK(RavynOS::ravynOS,
                                           getSystemOrSDKMacOSVersion(Version));
  };
  if (auto Result = CreatePlatformFromSDKName(SDK))
    return Result;
  // The SDK can be an SDK variant with a name like `<prefix>.<platform>`.
  return CreatePlatformFromSDKName(dropSDKNamePrefix(SDK));
}

std::string getOSVersion(llvm::Triple::OSType OS, const llvm::Triple &Triple,
                         const Driver &TheDriver) {
  VersionTuple OsVersion;
  llvm::Triple SystemTriple(llvm::sys::getProcessTriple());
  switch (OS) {
  case llvm::Triple::RavynOS:
    // If there is no version specified on triple, and both host and target are
    // macos, use the host triple to infer OS version.
    if (Triple.isOSRavynOS() && SystemTriple.isOSRavynOS() &&
        !Triple.getOSMajorVersion())
      OsVersion = SystemTriple.getOSVersion();
    else
      OsVersion = Triple.getOSVersion();
    break;
  default:
    llvm_unreachable("Unexpected OS type");
    break;
  }

  std::string OSVersion;
  llvm::raw_string_ostream(OSVersion)
      << OsVersion.getMajor() << '.' << OsVersion.getMinor().getValueOr(0)
      << '.' << OsVersion.getSubminor().getValueOr(0);
  return OSVersion;
}

/// Tries to infer the target OS from the -arch.
Optional<RavynOSPlatform>
inferDeploymentTargetFromArch(DerivedArgList &Args, const RavynOS &Toolchain,
                              const llvm::Triple &Triple,
                              const Driver &TheDriver) {
  llvm::Triple::OSType OSTy = llvm::Triple::UnknownOS;

  StringRef MachOArchName = Toolchain.getMachOArchName(Args);
  OSTy = llvm::Triple::RavynOS;
  return RavynOSPlatform::createFromArch(OSTy,
                                        getOSVersion(OSTy, Triple, TheDriver));
}

/// Returns the deployment target that's specified using the -target option.
Optional<RavynOSPlatform> getDeploymentTargetFromTargetArg(
    DerivedArgList &Args, const llvm::Triple &Triple, const Driver &TheDriver,
    const Optional<DarwinSDKInfo> &SDKInfo) {
  if (!Args.hasArg(options::OPT_target))
    return None;
  if (Triple.getOS() == llvm::Triple::RavynOS ||
      Triple.getOS() == llvm::Triple::UnknownOS)
    return None;
  std::string OSVersion = getOSVersion(Triple.getOS(), Triple, TheDriver);
  return RavynOSPlatform::createFromTarget(
      Triple, OSVersion, Args.getLastArg(options::OPT_target), SDKInfo);
}

/// Returns the deployment target that's specified using the -mtargetos option.
Optional<RavynOSPlatform>
getDeploymentTargetFromMTargetOSArg(DerivedArgList &Args,
                                    const Driver &TheDriver,
                                    const Optional<DarwinSDKInfo> &SDKInfo) {
  auto *A = Args.getLastArg(options::OPT_mtargetos_EQ);
  if(!A)
    return None;
  llvm::Triple TT(llvm::Twine("unknown-ravynsoft-") + A->getValue());
  switch (TT.getOS()) {
  case llvm::Triple::RavynOS:
    break;
  default:
    TheDriver.Diag(diag::err_drv_invalid_os_in_arg)
        << TT.getOSName() << A->getAsString(Args);
    return None;
  }

  VersionTuple Version = TT.getOSVersion();
  if (!Version.getMajor()) {
    TheDriver.Diag(diag::err_drv_invalid_version_number)
        << A->getAsString(Args);
    return None;
  }
  return RavynOSPlatform::createFromMTargetOS(TT.getOS(), Version,
                                             TT.getEnvironment(), A, SDKInfo);
}

Optional<DarwinSDKInfo> parseSDKSettings(llvm::vfs::FileSystem &VFS,
                                         const ArgList &Args,
                                         const Driver &TheDriver) {
  const Arg *A = Args.getLastArg(options::OPT_isysroot);
  if (!A)
    return None;
  StringRef isysroot = A->getValue();
  auto SDKInfoOrErr = parseDarwinSDKInfo(VFS, isysroot);
  if (!SDKInfoOrErr) {
    llvm::consumeError(SDKInfoOrErr.takeError());
    TheDriver.Diag(diag::warn_drv_darwin_sdk_invalid_settings);
    return None;
  }
  return *SDKInfoOrErr;
}

} // namespace

void RavynOS::AddDeploymentTarget(DerivedArgList &Args) const {
  const OptTable &Opts = getDriver().getOpts();

  // Support allowing the SDKROOT environment variable used by xcrun and other
  // Xcode tools to define the default sysroot, by making it the default for
  // isysroot.
  if (const Arg *A = Args.getLastArg(options::OPT_isysroot)) {
    // Warn if the path does not exist.
    if (!getVFS().exists(A->getValue()))
      getDriver().Diag(clang::diag::warn_missing_sysroot) << A->getValue();
  } else {
    if (char *env = ::getenv("SDKROOT")) {
      // We only use this value as the default if it is an absolute path,
      // exists, and it is not the root path.
      if (llvm::sys::path::is_absolute(env) && getVFS().exists(env) &&
          StringRef(env) != "/") {
        Args.append(Args.MakeSeparateArg(
            nullptr, Opts.getOption(options::OPT_isysroot), env));
      }
    }
  }

  // Read the SDKSettings.json file for more information, like the SDK version
  // that we can pass down to the compiler.
  SDKInfo = parseSDKSettings(getVFS(), Args, getDriver());

  // The OS and the version can be specified using the -target argument.
  Optional<RavynOSPlatform> OSTarget =
      getDeploymentTargetFromTargetArg(Args, getTriple(), getDriver(), SDKInfo);
  if (OSTarget) {
    // Disallow mixing -target and -mtargetos=.
    if (const auto *MTargetOSArg = Args.getLastArg(options::OPT_mtargetos_EQ)) {
      std::string TargetArgStr = OSTarget->getAsString(Args, Opts);
      std::string MTargetOSArgStr = MTargetOSArg->getAsString(Args);
      getDriver().Diag(diag::err_drv_cannot_mix_options)
          << TargetArgStr << MTargetOSArgStr;
    }
    Optional<RavynOSPlatform> OSVersionArgTarget =
        getDeploymentTargetFromOSVersionArg(Args, getDriver());
    if (OSVersionArgTarget) {
      unsigned TargetMajor, TargetMinor, TargetMicro;
      bool TargetExtra;
      unsigned ArgMajor, ArgMinor, ArgMicro;
      bool ArgExtra;
      if (OSTarget->getPlatform() != OSVersionArgTarget->getPlatform() ||
          (Driver::GetReleaseVersion(OSTarget->getOSVersion(), TargetMajor,
                                     TargetMinor, TargetMicro, TargetExtra) &&
           Driver::GetReleaseVersion(OSVersionArgTarget->getOSVersion(),
                                     ArgMajor, ArgMinor, ArgMicro, ArgExtra) &&
           (VersionTuple(TargetMajor, TargetMinor, TargetMicro) !=
                VersionTuple(ArgMajor, ArgMinor, ArgMicro) ||
            TargetExtra != ArgExtra))) {
        // Select the OS version from the -m<os>-version-min argument when
        // the -target does not include an OS version.
        if (OSTarget->getPlatform() == OSVersionArgTarget->getPlatform() &&
            !OSTarget->hasOSVersion()) {
          OSTarget->setOSVersion(OSVersionArgTarget->getOSVersion());
        } else {
          // Warn about -m<os>-version-min that doesn't match the OS version
          // that's specified in the target.
          std::string OSVersionArg =
              OSVersionArgTarget->getAsString(Args, Opts);
          std::string TargetArg = OSTarget->getAsString(Args, Opts);
          getDriver().Diag(clang::diag::warn_drv_overriding_flag_option)
              << OSVersionArg << TargetArg;
        }
      }
    }
  } else if ((OSTarget = getDeploymentTargetFromMTargetOSArg(Args, getDriver(),
                                                             SDKInfo))) {
    // The OS target can be specified using the -mtargetos= argument.
    // Disallow mixing -mtargetos= and -m<os>version-min=.
    Optional<RavynOSPlatform> OSVersionArgTarget =
        getDeploymentTargetFromOSVersionArg(Args, getDriver());
    if (OSVersionArgTarget) {
      std::string MTargetOSArgStr = OSTarget->getAsString(Args, Opts);
      std::string OSVersionArgStr = OSVersionArgTarget->getAsString(Args, Opts);
      getDriver().Diag(diag::err_drv_cannot_mix_options)
          << MTargetOSArgStr << OSVersionArgStr;
    }
  } else {
    // The OS target can be specified using the -m<os>version-min argument.
    OSTarget = getDeploymentTargetFromOSVersionArg(Args, getDriver());
    // If no deployment target was specified on the command line, check for
    // environment defines.
    if (!OSTarget) {
      OSTarget =
          getDeploymentTargetFromEnvironmentVariables(getDriver(), getTriple());
      if (OSTarget) {
        // Don't infer simulator from the arch when the SDK is also specified.
        Optional<RavynOSPlatform> SDKTarget =
            inferDeploymentTargetFromSDK(Args, SDKInfo);
        if (SDKTarget)
          OSTarget->setEnvironment(SDKTarget->getEnvironment());
      }
    }
    // If there is no command-line argument to specify the Target version and
    // no environment variable defined, see if we can set the default based
    // on -isysroot using SDKSettings.json if it exists.
    if (!OSTarget) {
      OSTarget = inferDeploymentTargetFromSDK(Args, SDKInfo);
      /// If the target was successfully constructed from the SDK path, try to
      /// infer the SDK info if the SDK doesn't have it.
      if (OSTarget && !SDKInfo)
        SDKInfo = OSTarget->inferSDKInfo();
    }
    // If no OS targets have been specified, try to guess platform from -target
    // or arch name and compute the version from the triple.
    if (!OSTarget)
      OSTarget =
          inferDeploymentTargetFromArch(Args, *this, getTriple(), getDriver());
  }

  assert(OSTarget && "Unable to infer ravynOS variant");
  OSTarget->addOSVersionMinArgument(Args, Opts);
  RavynOSPlatformKind Platform = OSTarget->getPlatform();

  unsigned Major, Minor, Micro;
  bool HadExtra;
  // Set the tool chain target information.
  if (Platform == ravynOS) {
    if (!Driver::GetReleaseVersion(OSTarget->getOSVersion(), Major, Minor,
                                   Micro, HadExtra))
      getDriver().Diag(diag::err_drv_invalid_version_number)
          << OSTarget->getAsString(Args, Opts);
  } else
    llvm_unreachable("unknown kind of ravynOS platform");

  RavynOSEnvironmentKind Environment = OSTarget->getEnvironment();
  VersionTuple NativeTargetVersion;
  setTarget(Platform, Environment, Major, Minor, Micro, NativeTargetVersion);

  if (const Arg *A = Args.getLastArg(options::OPT_isysroot)) {
    StringRef SDK = getSDKName(A->getValue());
    if (SDK.size() > 0) {
      size_t StartVer = SDK.find_first_of("0123456789");
      StringRef SDKName = SDK.slice(0, StartVer);
      if (!SDKName.startswith(getPlatformFamily()) &&
          !dropSDKNamePrefix(SDKName).startswith(getPlatformFamily()))
        getDriver().Diag(diag::warn_incompatible_sysroot)
            << SDKName << getPlatformFamily();
    }
  }
}

// Returns the effective header sysroot path to use. This comes either from
// -isysroot or --sysroot.
llvm::StringRef RavynOSClang::GetHeaderSysroot(const llvm::opt::ArgList &DriverArgs) const {
  if(DriverArgs.hasArg(options::OPT_isysroot))
    return DriverArgs.getLastArgValue(options::OPT_isysroot);
  if (!getDriver().SysRoot.empty())
    return getDriver().SysRoot;
  return "/";
}

void RavynOSClang::AddClangSystemIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                                            llvm::opt::ArgStringList &CC1Args) const {
  const Driver &D = getDriver();

  llvm::StringRef Sysroot = GetHeaderSysroot(DriverArgs);

  bool NoStdInc = DriverArgs.hasArg(options::OPT_nostdinc);
  bool NoStdlibInc = DriverArgs.hasArg(options::OPT_nostdlibinc);
  bool NoBuiltinInc = DriverArgs.hasFlag(
      options::OPT_nobuiltininc, options::OPT_ibuiltininc, /*Default=*/false);
  bool ForceBuiltinInc = DriverArgs.hasFlag(
      options::OPT_ibuiltininc, options::OPT_nobuiltininc, /*Default=*/false);

  // Add <sysroot>/usr/local/include
  if (!NoStdInc && !NoStdlibInc) {
      SmallString<128> P(Sysroot);
      llvm::sys::path::append(P, "usr", "local", "include");
      addSystemInclude(DriverArgs, CC1Args, P);
  }

  // Add the Clang builtin headers (<resource>/include)
  if (!(NoStdInc && !ForceBuiltinInc) && !NoBuiltinInc) {
    SmallString<128> P(D.ResourceDir);
    llvm::sys::path::append(P, "include");
    addSystemInclude(DriverArgs, CC1Args, P);
  }

  if (NoStdInc || NoStdlibInc)
    return;

  // Check for configure-time C include directories.
  llvm::StringRef CIncludeDirs(C_INCLUDE_DIRS);
  if (!CIncludeDirs.empty()) {
    llvm::SmallVector<llvm::StringRef, 5> dirs;
    CIncludeDirs.split(dirs, ":");
    for (llvm::StringRef dir : dirs) {
      llvm::StringRef Prefix =
          llvm::sys::path::is_absolute(dir) ? "" : llvm::StringRef(Sysroot);
      addExternCSystemInclude(DriverArgs, CC1Args, Prefix + dir);
    }
  } else {
    // Otherwise, add <sysroot>/usr/include.
    SmallString<128> P(Sysroot);
    llvm::sys::path::append(P, "usr", "include");
    addExternCSystemInclude(DriverArgs, CC1Args, P.str());
  }
}

bool RavynOSClang::AddGnuCPlusPlusIncludePaths(const llvm::opt::ArgList &DriverArgs,
                                              llvm::opt::ArgStringList &CC1Args,
                                              llvm::SmallString<128> Base,
                                              llvm::StringRef Version,
                                              llvm::StringRef ArchDir,
                                              llvm::StringRef BitDir) const {
  llvm::sys::path::append(Base, Version);

  // Add the base dir
  addSystemInclude(DriverArgs, CC1Args, Base);

  // Add the multilib dirs
  {
    llvm::SmallString<128> P = Base;
    if (!ArchDir.empty())
      llvm::sys::path::append(P, ArchDir);
    if (!BitDir.empty())
      llvm::sys::path::append(P, BitDir);
    addSystemInclude(DriverArgs, CC1Args, P);
  }

  // Add the backward dir
  {
    llvm::SmallString<128> P = Base;
    llvm::sys::path::append(P, "backward");
    addSystemInclude(DriverArgs, CC1Args, P);
  }

  return getVFS().exists(Base);
}

void RavynOSClang::AddClangCXXStdlibIncludeArgs(
    const llvm::opt::ArgList &DriverArgs,
    llvm::opt::ArgStringList &CC1Args) const {
  // The implementation from a base class will pass through the -stdlib to
  // CC1Args.
  // FIXME: this should not be necessary, remove usages in the frontend
  //        (e.g. HeaderSearchOptions::UseLibcxx) and don't pipe -stdlib.
  //        Also check whether this is used for setting library search paths.
  ToolChain::AddClangCXXStdlibIncludeArgs(DriverArgs, CC1Args);

  if (DriverArgs.hasArg(options::OPT_nostdlibinc) ||
      DriverArgs.hasArg(options::OPT_nostdincxx))
    return;

  llvm::StringRef Sysroot = GetHeaderSysroot(DriverArgs);

  switch (GetCXXStdlibType(DriverArgs)) {
  case ToolChain::CST_Libcxx: {
    // On RavynOS, libc++ can be installed in one of the following two places:
    // 1. Alongside the compiler in         <install>/include/c++/v1
    // 2. In a SDK (or a custom sysroot) in <sysroot>/usr/include/c++/v1
    //
    // The precendence of paths is as listed above, i.e. we take the first path
    // that exists. Also note that we never include libc++ twice -- we take the
    // first path that exists and don't send the other paths to CC1 (otherwise
    // include_next could break).

    // Check for (1)
    // Get from '<install>/bin' to '<install>/include/c++/v1'.
    // Note that InstallBin can be relative, so we use '..' instead of
    // parent_path.
    llvm::SmallString<128> InstallBin =
        llvm::StringRef(getDriver().getInstalledDir()); // <install>/bin
    llvm::sys::path::append(InstallBin, "..", "include", "c++", "v1");
    if (getVFS().exists(InstallBin)) {
      addSystemInclude(DriverArgs, CC1Args, InstallBin);
      return;
    } else if (DriverArgs.hasArg(options::OPT_v)) {
      llvm::errs() << "ignoring nonexistent directory \"" << InstallBin
                   << "\"\n";
    }

    // Otherwise, check for (2)
    llvm::SmallString<128> SysrootUsr = Sysroot;
    llvm::sys::path::append(SysrootUsr, "usr", "include", "c++", "v1");
    if (getVFS().exists(SysrootUsr)) {
      addSystemInclude(DriverArgs, CC1Args, SysrootUsr);
      return;
    } else if (DriverArgs.hasArg(options::OPT_v)) {
      llvm::errs() << "ignoring nonexistent directory \"" << SysrootUsr
                   << "\"\n";
    }

    // Otherwise, don't add any path.
    break;
  }

  case ToolChain::CST_Libstdcxx:
    llvm::SmallString<128> UsrIncludeCxx = Sysroot;
    llvm::sys::path::append(UsrIncludeCxx, "usr", "include", "c++");

    llvm::Triple::ArchType arch = getTriple().getArch();
    bool IsBaseFound = true;
    switch (arch) {
    default: break;

    case llvm::Triple::ppc:
    case llvm::Triple::ppc64:
      IsBaseFound = AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.2.1",
                                                "powerpc-apple-darwin10",
                                                arch == llvm::Triple::ppc64 ? "ppc64" : "");
      IsBaseFound |= AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.0.0", "powerpc-apple-darwin10",
                                                 arch == llvm::Triple::ppc64 ? "ppc64" : "");
      break;

    case llvm::Triple::x86:
    case llvm::Triple::x86_64:
      IsBaseFound = AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.2.1",
                                                "i686-apple-darwin10",
                                                arch == llvm::Triple::x86_64 ? "x86_64" : "");
      IsBaseFound |= AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.0.0", "i686-apple-darwin8",
                                                 "");
      break;

    case llvm::Triple::arm:
    case llvm::Triple::thumb:
      IsBaseFound = AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.2.1",
                                                "arm-apple-darwin10",
                                                "v7");
      IsBaseFound |= AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.2.1",
                                                "arm-apple-darwin10",
                                                 "v6");
      break;

    case llvm::Triple::aarch64:
      IsBaseFound = AddGnuCPlusPlusIncludePaths(DriverArgs, CC1Args, UsrIncludeCxx,
                                                "4.2.1",
                                                "arm64-apple-darwin10",
                                                "");
      break;
    }

    if (!IsBaseFound) {
      getDriver().Diag(diag::warn_drv_libstdcxx_not_found);
    }

    break;
  }
}
void RavynOSClang::AddCXXStdlibLibArgs(const ArgList &Args,
                                      ArgStringList &CmdArgs) const {
  CXXStdlibType Type = GetCXXStdlibType(Args);

  switch (Type) {
  case ToolChain::CST_Libcxx:
    CmdArgs.push_back("-lc++");
    break;

  case ToolChain::CST_Libstdcxx:
    // Unfortunately, -lstdc++ doesn't always exist in the standard search path;
    // it was previously found in the gcc lib dir. However, for all the RavynOS
    // platforms we care about it was -lstdc++.6, so we search for that
    // explicitly if we can't see an obvious -lstdc++ candidate.

    // Check in the sysroot first.
    if (const Arg *A = Args.getLastArg(options::OPT_isysroot)) {
      SmallString<128> P(A->getValue());
      llvm::sys::path::append(P, "usr", "lib", "libstdc++.dylib");

      if (!getVFS().exists(P)) {
        llvm::sys::path::remove_filename(P);
        llvm::sys::path::append(P, "libstdc++.6.dylib");
        if (getVFS().exists(P)) {
          CmdArgs.push_back(Args.MakeArgString(P));
          return;
        }
      }
    }

    // Otherwise, look in the root.
    // FIXME: This should be removed someday when we don't have to care about
    // 10.6 and earlier, where /usr/lib/libstdc++.dylib does not exist.
    if (!getVFS().exists("/usr/lib/libstdc++.dylib") &&
        getVFS().exists("/usr/lib/libstdc++.6.dylib")) {
      CmdArgs.push_back("/usr/lib/libstdc++.6.dylib");
      return;
    }

    // Otherwise, let the linker search.
    CmdArgs.push_back("-lstdc++");
    break;
  }
}

void RavynOSClang::AddCCKextLibArgs(const ArgList &Args,
                                   ArgStringList &CmdArgs) const {
  // For RavynOS platforms, use the compiler-rt-based support library
  // instead of the gcc-provided one (which is also incidentally
  // only present in the gcc lib dir, which makes it hard to find).

  SmallString<128> P(getDriver().ResourceDir);
  llvm::sys::path::append(P, "lib", "ravynOS");
  llvm::sys::path::append(P, "libclang_rt.a");

  // For now, allow missing resource libraries to support developers who may
  // not have compiler-rt checked out or integrated into their build.
  if (getVFS().exists(P))
    CmdArgs.push_back(Args.MakeArgString(P));
}

bool RavynOS::isAlignedAllocationUnavailable() const {
  return false; // always available
}

void RavynOS::addClangTargetOptions(const llvm::opt::ArgList &DriverArgs,
                                   llvm::opt::ArgStringList &CC1Args,
                                   Action::OffloadKind DeviceOffloadKind) const {
  // Pass "-faligned-alloc-unavailable" only when the user hasn't manually
  // enabled or disabled aligned allocations.
  if (!DriverArgs.hasArgNoClaim(options::OPT_faligned_allocation,
                                options::OPT_fno_aligned_allocation) &&
      isAlignedAllocationUnavailable())
    CC1Args.push_back("-faligned-alloc-unavailable");

  if (SDKInfo) {
    /// Pass the SDK version to the compiler when the SDK information is
    /// available.
    auto EmitTargetSDKVersionArg = [&](const VersionTuple &V) {
      std::string Arg;
      llvm::raw_string_ostream OS(Arg);
      OS << "-target-sdk-version=" << V;
      CC1Args.push_back(DriverArgs.MakeArgString(OS.str()));
    };

    EmitTargetSDKVersionArg(SDKInfo->getVersion());
  }

  // Enable compatibility mode for NSItemProviderCompletionHandler in
  // Foundation/NSItemProvider.h.
  CC1Args.push_back("-fcompatibility-qualified-id-block-type-checking");

  // Give static local variables in inline functions hidden visibility when
  // -fvisibility-inlines-hidden is enabled.
  if (!DriverArgs.getLastArgNoClaim(
          options::OPT_fvisibility_inlines_hidden_static_local_var,
          options::OPT_fno_visibility_inlines_hidden_static_local_var))
    CC1Args.push_back("-fvisibility-inlines-hidden-static-local-var");
}

DerivedArgList *
RavynOS::TranslateArgs(const DerivedArgList &Args, StringRef BoundArch,
                      Action::OffloadKind DeviceOffloadKind) const {
  // First get the generic Apple args, before moving onto RavynOS-specific ones.
  DerivedArgList *DAL =
      MachO::TranslateArgs(Args, BoundArch, DeviceOffloadKind);
  const OptTable &Opts = getDriver().getOpts();

  // If no architecture is bound, none of the translations here are relevant.
  if (BoundArch.empty())
    return DAL;

  // Add an explicit version min argument for the deployment target. We do this
  // after argument translation because -Xarch_ arguments may add a version min
  // argument.
  AddDeploymentTarget(*DAL);

  if (!Args.getLastArg(options::OPT_stdlib_EQ) &&
      GetCXXStdlibType(Args) == ToolChain::CST_Libcxx)
    DAL->AddJoinedArg(nullptr, Opts.getOption(options::OPT_stdlib_EQ),
                      "libc++");

  // Validate the C++ standard library choice.
  CXXStdlibType Type = GetCXXStdlibType(*DAL);
  if (Type == ToolChain::CST_Libcxx) {
    // Check whether the target provides libc++.
    StringRef where;

    if (where != StringRef()) {
      getDriver().Diag(clang::diag::err_drv_invalid_libcxx_deployment) << where;
    }
  }

  auto Arch = tools::ravynos::getArchTypeForMachOArchName(BoundArch);
  if ((Arch == llvm::Triple::arm || Arch == llvm::Triple::thumb)) {
    if (Args.hasFlag(options::OPT_fomit_frame_pointer,
                     options::OPT_fno_omit_frame_pointer, false))
      getDriver().Diag(clang::diag::warn_drv_unsupported_opt_for_target)
          << "-fomit-frame-pointer" << BoundArch;
  }

  return DAL;
}

llvm::ExceptionHandling RavynOS::GetExceptionModel(const ArgList &Args) const {
  // RavynOS uses SjLj exceptions on ARM.
  if (getTriple().getArch() != llvm::Triple::arm &&
      getTriple().getArch() != llvm::Triple::thumb)
    return llvm::ExceptionHandling::None;

  // Only watchOS uses the new DWARF/Compact unwinding method.
  llvm::Triple Triple(ComputeLLVMTriple(Args));
  if (Triple.isWatchABI())
    return llvm::ExceptionHandling::DwarfCFI;

  return llvm::ExceptionHandling::SjLj;
}

bool RavynOS::SupportsEmbeddedBitcode() const {
  assert(TargetInitialized && "Target not initialized!");
  return true;
}

void RavynOS::addMinVersionArgs(const ArgList &Args,
                               ArgStringList &CmdArgs) const {
  VersionTuple TargetVersion = getTripleTargetVersion();

  CmdArgs.push_back("-macosx_version_min");
  VersionTuple MinTgtVers = getEffectiveTriple().getMinimumSupportedOSVersion();
  if (!MinTgtVers.empty() && MinTgtVers > TargetVersion)
    TargetVersion = MinTgtVers;
  CmdArgs.push_back(Args.MakeArgString(TargetVersion.getAsString()));
}

static const char *getPlatformName(RavynOS::RavynOSPlatformKind Platform,
                                   RavynOS::RavynOSEnvironmentKind Environment) {
  return "ravynOS";
}

void RavynOS::addPlatformVersionArgs(const llvm::opt::ArgList &Args,
                                    llvm::opt::ArgStringList &CmdArgs) const {
  // -platform_version <platform> <target_version> <sdk_version>
  // Both the target and SDK version support only up to 3 components.
  CmdArgs.push_back("-platform_version");
  std::string PlatformName = getPlatformName(TargetPlatform, TargetEnvironment);
  CmdArgs.push_back(Args.MakeArgString(PlatformName));
  VersionTuple TargetVersion = getTripleTargetVersion().withoutBuild();
  VersionTuple MinTgtVers = getEffectiveTriple().getMinimumSupportedOSVersion();
  if (!MinTgtVers.empty() && MinTgtVers > TargetVersion)
    TargetVersion = MinTgtVers;
  CmdArgs.push_back(Args.MakeArgString(TargetVersion.getAsString()));

  if (SDKInfo) {
    VersionTuple SDKVersion = SDKInfo->getVersion().withoutBuild();
    CmdArgs.push_back(Args.MakeArgString(SDKVersion.getAsString()));
  } else {
    // Use an SDK version that's matching the deployment target if the SDK
    // version is missing. This is preferred over an empty SDK version (0.0.0)
    // as the system's runtime might expect the linked binary to contain a
    // valid SDK version in order for the binary to work correctly. It's
    // reasonable to use the deployment target version as a proxy for the
    // SDK version because older SDKs don't guarantee support for deployment
    // targets newer than the SDK versions, so that rules out using some
    // predetermined older SDK version, which leaves the deployment target
    // version as the only reasonable choice.
    CmdArgs.push_back(Args.MakeArgString(TargetVersion.getAsString()));
  }
}

// Add additional link args for the -dynamiclib option.
static void addDynamicLibLinkArgs(const RavynOS &D, const ArgList &Args,
                                  ArgStringList &CmdArgs) {
}

// Add additional link args for the -bundle option.
static void addBundleLinkArgs(const RavynOS &D, const ArgList &Args,
                              ArgStringList &CmdArgs) {
  if (Args.hasArg(options::OPT_static))
    return;
}

// Add additional link args for the -pg option.
static void addPgProfilingLinkArgs(const RavynOS &D, const ArgList &Args,
                                   ArgStringList &CmdArgs) {
    CmdArgs.push_back("-lgcrt1.o");
    CmdArgs.push_back("-no_new_main");
}

static void addDefaultCRTLinkArgs(const RavynOS &D, const ArgList &Args,
                                  ArgStringList &CmdArgs) {
  CmdArgs.push_back("-lcrt1.10.6.o");
}

void RavynOS::addStartObjectFileArgs(const ArgList &Args,
                                    ArgStringList &CmdArgs) const {
  // Derived from startfile spec.
  if (Args.hasArg(options::OPT_dynamiclib))
    addDynamicLibLinkArgs(*this, Args, CmdArgs);
  else if (Args.hasArg(options::OPT_bundle))
    addBundleLinkArgs(*this, Args, CmdArgs);
  else if (Args.hasArg(options::OPT_pg) && SupportsProfiling())
    addPgProfilingLinkArgs(*this, Args, CmdArgs);
  else if (Args.hasArg(options::OPT_static) ||
           Args.hasArg(options::OPT_object) ||
           Args.hasArg(options::OPT_preload))
    CmdArgs.push_back("-lcrt0.o");
  else
    addDefaultCRTLinkArgs(*this, Args, CmdArgs);
}

void RavynOS::CheckObjCARC() const {
  return;
}

SanitizerMask RavynOS::getSupportedSanitizers() const {
  const bool IsX86_64 = getTriple().getArch() == llvm::Triple::x86_64;
  const bool IsAArch64 = getTriple().getArch() == llvm::Triple::aarch64;
  SanitizerMask Res = ToolChain::getSupportedSanitizers();
  Res |= SanitizerKind::Address;
  Res |= SanitizerKind::PointerCompare;
  Res |= SanitizerKind::PointerSubtract;
  Res |= SanitizerKind::Leak;
  Res |= SanitizerKind::Fuzzer;
  Res |= SanitizerKind::FuzzerNoLink;
  Res |= SanitizerKind::Function;
  Res |= SanitizerKind::ObjCCast;
  Res |= SanitizerKind::Vptr;

  if ((IsX86_64 || IsAArch64)) {
    Res |= SanitizerKind::Thread;
  }
  return Res;
}

void RavynOS::printVerboseInfo(raw_ostream &OS) const {
  CudaInstallation.print(OS);
  RocmInstallation.print(OS);
}

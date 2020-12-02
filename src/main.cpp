#include "ast.hpp"
#include "ast_printer.hpp"
#include "error.hpp"
#include "escape_analysis.hpp"
#include "generics_pass.hpp"
#include "ir.hpp"
#include "ir_printer.hpp"
#include "llvm_codegen.hpp"
#include "parser.hpp"
#include "resolve_pass.hpp"
#include "tokenizer.hpp"
#include "type_check.hpp"
#include <iostream>
extern "C" {
#include <getopt.h>
}

namespace ovid {

class DriverArgs {
public:
  /* intermediate forms to emit on command line */
  bool dump_ast;
  bool dump_ir;
  bool dump_mono_ir;
  bool dump_escape_analysis;
  bool dump_llvm;

  // object, asm, or llvm output
  ir::CodegenOutputType codegen_out;
  // llvm optimization level
  llvm::PassBuilder::OptimizationLevel opt_level;
  // llvm relocation model
  llvm::Reloc::Model reloc_model;
  // llvm code model
  llvm::CodeModel::Model code_model;

  std::string out_file;
  std::vector<std::string> in_files;
  std::vector<std::string> package_name;
  int64_t package_version;

  bool do_main;

  DriverArgs()
      : dump_ast(false), dump_ir(false), dump_mono_ir(false),
        dump_escape_analysis(false), dump_llvm(false),
        codegen_out(ir::CodegenOutputType::OBJ),
        opt_level(llvm::PassBuilder::OptimizationLevel::O2),
        reloc_model(llvm::Reloc::PIC_), code_model(llvm::CodeModel::Small),
        out_file("ovidc.out"), in_files(), package_name(), package_version(-1),
        do_main(false){};
};

void usage() {
  std::cout
      << "Usage: "
      << "ovidc"
      << " [OPTIONS] file...\n"
         "The Ovid Language Compiler Backend\n"
         "Options:\n"
         "  -h, --help             Display this message\n"
         "  -o, --output FILE      Output compilation results in FILE\n"
         "  -c, --emit-obj         Produce object code output\n"
         "  -S, --emit-asm         Produce assembly output\n"
         "  --emit-llvm            Produce LLVM IR output\n"
         "  -O, --opt-level OPT    Set the optimization level:\n"
         "    0  no optimization\n"
         "    1  optimize without greatly increasing compile time\n"
         "    2  optimize without greatly increasing output size\n"
         "    3  optimize fully for speed\n"
         "    s  optimize for output size\n"
         "    z  optimize fully for output size\n"
         "  --code-model MODEL     Set the code model to use:\n"
         "    tiny, small, kernel, medium, large\n"
         "  --reloc-model MODEL    Set the relocation mode to use:\n"
         "    static, pic, dynamic-no-pic, ropi, rwpi, ropi-rwpi\n"
         "  --dump-ast             Print the abstract syntax tree for "
         "the source code\n"
         "  --dump-ir              Print the intermediate "
         "representation for the source code\n"
         "  --dump-mono-ir         Print the monomorphic intermediate "
         "representation for the source code\n"
         "  --dump-escape          Print escape analysis results\n"
         "  --dump-llvm            Print unoptimized llvm ir\n"
         "  --package NAME[:NAME]...\n"
         "                         Set the package name\n"
         "  --package-version VERSION\n"
         "                         Set the package version (must be an "
         "integer)\n"
         "  --main                 Generate a main function\n"
         "  --no-main              Don't generate a main function (default)\n";

  exit(1);
}

static struct option cli_opts[] = {
    {"dump-ast", no_argument, nullptr, 0},
    {"dump-ir", no_argument, nullptr, 1},
    {"dump-escape", no_argument, nullptr, 2},
    {"emit-obj", no_argument, nullptr, 'c'},
    {"emit-asm", no_argument, nullptr, 'S'},
    {"emit-llvm", no_argument, nullptr, 3},
    {"output", required_argument, nullptr, 'o'},
    {"reloc-model", required_argument, nullptr, 4},
    {"code-model", required_argument, nullptr, 5},
    {"opt-level", required_argument, nullptr, 'O'},
    {"help", no_argument, nullptr, 'h'},
    {"package", required_argument, nullptr, 6},
    {"main", no_argument, nullptr, 7},
    {"no-main", no_argument, nullptr, 8},
    {"dump-llvm", no_argument, nullptr, 9},
    {"package-version", required_argument, nullptr, 10},
    {"dump-mono-ir", no_argument, nullptr, 11},
    {nullptr, 0, nullptr, 0}};

DriverArgs parseCLIArgs(int argc, char **argv) {
  DriverArgs res;

  int opt;
  int opt_index;
  while ((opt = getopt_long(argc, argv, "cSo:O:h", cli_opts, &opt_index)) !=
         -1) {
    switch (opt) {
    case 'o':
      res.out_file = std::string(optarg);
      break;
    case 'c':
      res.codegen_out = ir::CodegenOutputType::OBJ;
      break;
    case 'S':
      res.codegen_out = ir::CodegenOutputType::ASM;
      break;
    case 3:
      res.codegen_out = ir::CodegenOutputType::LLVM_IR;
      break;
    case 'O':
      if (!strcmp(optarg, "0")) {
        res.opt_level = llvm::PassBuilder::OptimizationLevel::O0;
      } else if (!strcmp(optarg, "1")) {
        res.opt_level = llvm::PassBuilder::OptimizationLevel::O1;
      } else if (!strcmp(optarg, "2")) {
        res.opt_level = llvm::PassBuilder::OptimizationLevel::O2;
      } else if (!strcmp(optarg, "3")) {
        res.opt_level = llvm::PassBuilder::OptimizationLevel::O3;
      } else if (!strcmp(optarg, "s")) {
        res.opt_level = llvm::PassBuilder::OptimizationLevel::Os;
      } else if (!strcmp(optarg, "z")) {
        res.opt_level = llvm::PassBuilder::OptimizationLevel::Oz;
      } else {
        std::cerr << "invalid optimization level " << std::string(optarg)
                  << "\n";
      }
      break;
    case 0:
      res.dump_ast = true;
      break;
    case 1:
      res.dump_ir = true;
      break;
    case 2:
      res.dump_escape_analysis = true;
      break;
    case 4:
      if (!strcmp(optarg, "static")) {
        res.reloc_model = llvm::Reloc::Static;
      } else if (!strcmp(optarg, "pic")) {
        res.reloc_model = llvm::Reloc::PIC_;
      } else if (!strcmp(optarg, "dynamic-no-pic")) {
        res.reloc_model = llvm::Reloc::DynamicNoPIC;
      } else if (!strcmp(optarg, "ropi")) {
        res.reloc_model = llvm::Reloc::ROPI;
      } else if (!strcmp(optarg, "rwpi")) {
        res.reloc_model = llvm::Reloc::RWPI;
      } else if (!strcmp(optarg, "ropi-rwpi")) {
        res.reloc_model = llvm::Reloc::ROPI_RWPI;
      } else {
        std::cerr << "invalid relocation model " << std::string(optarg) << "\n";
      }
      break;
    case 5:
      if (!strcmp(optarg, "tiny")) {
        res.code_model = llvm::CodeModel::Tiny;
      } else if (!strcmp(optarg, "small")) {
        res.code_model = llvm::CodeModel::Small;
      } else if (!strcmp(optarg, "kernel")) {
        res.code_model = llvm::CodeModel::Kernel;
      } else if (!strcmp(optarg, "medium")) {
        res.code_model = llvm::CodeModel::Medium;
      } else if (!strcmp(optarg, "large")) {
        res.code_model = llvm::CodeModel::Large;
      } else {
        std::cerr << "invalid code model " << std::string(optarg) << "\n";
      }
      break;
    case 6: {
      std::string package = optarg;
      size_t last = 0;
      size_t next = 0;
      while ((next = package.find(':', last)) != std::string::npos) {
        res.package_name.emplace_back(package.substr(last, next - last));
        last = next + 1;
      }
      res.package_name.emplace_back(package.substr(last));
      break;
    }
    case 7:
      res.do_main = true;
      break;
    case 8:
      res.do_main = false;
      break;
    case 9:
      res.dump_llvm = true;
      break;
    case 10:
      res.package_version = std::strtol(optarg, nullptr, 10);
      break;
    case 11:
      res.dump_mono_ir = true;
      break;
    case 'h':
    default:
      usage();
    }
  }

  for (int index = optind; index < argc; index++) {
    res.in_files.emplace_back(argv[index]);
  }

  return res;
}

class CLIDriver {
  PrintingErrorManager errorMan;
  DriverArgs args;
  ScopesRoot root_scopes;
  ActiveScopes scopes;
  std::vector<std::fstream> files;

  ast::StatementList parseFiles();

public:
  CLIDriver(int argc, char **argv)
      : errorMan(), args(parseCLIArgs(argc, argv)), root_scopes(),
        scopes(args.package_name, args.package_version, root_scopes.names.get(),
               root_scopes.types.get()) {
    files.reserve(args.in_files.size());
  };

  int run();
};

ast::StatementList CLIDriver::parseFiles() {
  ast::StatementList ast;

  for (auto &path : args.in_files) {
    // open file
    files.emplace_back(path);
    auto file = &files[files.size() - 1];
    // feed file to tokenizer
    auto lexer = Tokenizer(&path, file, errorMan);
    // parse file
    auto parser = Parser(lexer, errorMan, scopes, args.package_name);
    auto file_ast = parser.parseProgram();
    parser.removePushedPackageScope();
    // add file's ast to whole program ast
    for (auto &stat : file_ast) {
      ast.push_back(std::move(stat));
    }
  }

  // run resolve pass
  auto resolvePass = ast::ResolvePass(scopes, errorMan, args.package_name);
  resolvePass.visitNodes(ast, ast::ResolvePassState());
  resolvePass.removePushedPackageScope();

  return ast;
}

int CLIDriver::run() {
  ir::reset_id();
  ast::reset_id();

  if (args.in_files.empty()) {
    usage();
    return 1;
  }

  auto ast = parseFiles();

  if (args.dump_ast) {
    std::cout << "---- AST ----\n";
    auto astPrinter = ast::ASTPrinter(std::cout);
    astPrinter.visitNodes(ast, ast::ASTPrinterState());
  }

  auto ir = ast::TypeCheck::produceIR(errorMan, args.package_name, root_scopes,
                                      scopes, ast);

  if (errorMan.criticalErrorOccurred()) {
    std::cout << "\x1b[1;31mtype checking failed";
    return 1;
  }

  if (args.dump_ir) {
    std::cout << "\n---- IR ----\n";
    auto irPrinter = ir::IRPrinter(std::cout);
    irPrinter.visitInstructions(ir, ast::ASTPrinterState());
  }

  // run generics pass
  ir = ir::GenericsPass::produceIR(scopes, errorMan, ir);

  if (args.dump_mono_ir) {
    std::cout << "\n---- MONOMORPHIC IR ----\n";
    auto irPrinter = ir::IRPrinter(std::cout);
    irPrinter.visitInstructions(ir, ast::ASTPrinterState());
  }

  // run escape analysis
  if (args.dump_escape_analysis) {
    std::cout << "\n---- ESCAPE ANALYSIS ----\n";
    ir::runEscapeAnalysis(ir, true, true, true, std::cout);
  } else {
    ir::runEscapeAnalysis(ir, false, false, false, std::cout);
  }

  // generate llvm
  auto codegen = ir::LLVMCodegenPass(args.in_files[0], errorMan);
  codegen.visitInstructions(ir, ir::LLVMCodegenPassState());

  if (args.dump_llvm) {
    std::cout << "\n---- LLVM IR ----\n";
    codegen.llvm_module->print(llvm::outs(), nullptr);
  }

  /* construct main function name */
  std::string main_name = "main";
  /* emit */
  codegen.optAndEmit(args.opt_level, args.out_file, args.codegen_out,
                     args.do_main,
                     root_scopes.names->getScopeTable(args.package_name),
                     &main_name, args.reloc_model, args.code_model);

  if (errorMan.criticalErrorOccurred()) {
    return 1;
  } else {
    return 0;
  }
}

} // namespace ovid

int main(int argc, char **argv) {
  auto driver = ovid::CLIDriver(argc, argv);

  return driver.run();
}
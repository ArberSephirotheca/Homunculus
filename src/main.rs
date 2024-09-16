mod codegen;
mod compiler;
use camino::Utf8Path;
use codegen::context::CodegenCx;
use compiler::parse::parser::parse;
use lazy_static::lazy_static;
use std::io::{self};

use std::env;
use std::fs::File;
use std::io::Read;

static DEFAULT_PROGRAM_FILE: &str = "MCProgram.tla";

fn read_glsl_file(file_path: &str) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

fn compile(glsl_code: &str) {
    let syntax = parse(glsl_code).syntax();
    let mut codegen_ctx = CodegenCx::new();
    let program = codegen_ctx.generate_code(syntax);
    let path = Utf8Path::new("./forward-progress/validation");
    let file = path.join(DEFAULT_PROGRAM_FILE);
    program.write_to_file(&file);
}

fn main() {
    // Get the command-line arguments
    let args: Vec<String> = env::args().collect();

    // Check if the user provided a filename
    if args.len() < 2 {
        eprintln!("Usage: {} <glsl_file>", args[0]);
        return;
    }

    let filename = &args[1];

    // Read the GLSL file
    match read_glsl_file(filename) {
        Ok(glsl_code) => println!("GLSL Code: \n{}", glsl_code),
        Err(e) => eprintln!("Failed to read GLSL file '{}': {}", filename, e),
    }
}

// fn main() -> io::Result<()> {
//     let stdin = io::stdin();
//     let mut stdout = io::stdout();

//     let mut input = String::new();
//     loop {
//         write!(stdout, "→ ")?;
//         stdout.flush()?;

//         stdin.read_line(&mut input)?;

//         let parse = parse(&input);
//         println!("{}", parse.debug_tree());

//         let syntax = parse.syntax();

//         let root = ast::Root::cast(syntax).unwrap();

//         dbg!(root
//             .stmts()
//             .filter_map(|stmt| if let ast::Stmt::VariableDef(var_def) = stmt {
//                 Some(var_def.value())
//             } else {
//                 None
//             })
//             .collect::<Vec<_>>());

//         input.clear();
//     }
// }

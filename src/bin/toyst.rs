use std::{collections::HashMap, ffi, fs::File, io::Read};

use ariadne::Source;
use inkwell::{AddressSpace, OptimizationLevel, context::Context};
use toyst::{
    ast::Spanned,
    codegen::codegen_for_program,
    lexer::Lexer,
    name::{NameResolverContainer, resolve_name_for_program},
    parser::parse_program,
    report::TakeReport,
    semantics::validate_semantics_for_program,
    types::{Type, TypeEnvironment, infer_type_for_program},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<_>>();

    let path = args.get(1).ok_or("source path is needed.")?;

    let mut source = String::new();

    File::open(path)?.read_to_string(&mut source)?;

    let mut lexer = Lexer::new(source.as_str());
    let mut errors = Vec::new();

    let ast = parse_program(&mut lexer, &mut errors, &[]);

    if let Err(reports) = errors.take_report(path) {
        for report in reports {
            report.print((path.clone(), Source::from(&source))).unwrap();
        }
        return Err("This source has error.".into());
    }

    let mut container = NameResolverContainer::new();
    let resolver = container.new_resolver(None);
    let mut errors = Vec::new();
    let mut resolved_map = HashMap::new();

    resolve_name_for_program(
        &ast,
        &mut errors,
        resolver,
        &mut container,
        &mut resolved_map,
    );

    if let Err(reports) = errors.take_report(path) {
        for report in reports {
            report.print((path.clone(), Source::from(&source))).unwrap();
        }
        return Err("This source has error.".into());
    }

    let mut errors = Vec::new();
    validate_semantics_for_program(&ast, &mut errors);

    if let Err(reports) = errors.take_report(path) {
        for report in reports {
            report.print((path.clone(), Source::from(&source))).unwrap();
        }
        return Err("This source has error.".into());
    }

    let mut env = TypeEnvironment::new();
    let mut errors = Vec::new();
    infer_type_for_program(
        &ast,
        &resolved_map,
        &Spanned::new(Type::Void, 0..source.len()),
        &mut env,
        &mut errors,
    );

    if let Err(reports) = errors.take_report(path) {
        for report in reports {
            report.print((path.clone(), Source::from(&source))).unwrap();
        }
        return Err("This source has error.".into());
    }

    let type_map = env.dump();

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("module");

    let print_str_function = module.add_function(
        "print",
        context
            .void_type()
            .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
        None,
    );
    let print_int_function = module.add_function(
        "print_int",
        context
            .void_type()
            .fn_type(&[context.i64_type().into()], false),
        None,
    );

    codegen_for_program(
        &ast,
        &context,
        &builder,
        &module,
        &resolved_map,
        &type_map,
        None,
        &mut HashMap::new(),
        &mut HashMap::new(),
        &mut HashMap::new(),
    );

    unsafe extern "C" fn print_str(char: *const i8) {
        println!("{}", unsafe { ffi::CStr::from_ptr(char).to_str().unwrap() });
    }

    unsafe extern "C" fn print_int(int: i64) {
        println!("{}", int);
    }

    println!("{}", module.to_string());

    let jit_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    jit_engine.add_global_mapping(&print_str_function, print_str as usize);
    jit_engine.add_global_mapping(&print_int_function, print_int as usize);

    let main = unsafe {
        jit_engine
            .get_function::<unsafe extern "C" fn()>("main")
            .unwrap()
    };

    unsafe { main.into_raw()() };

    Ok(())
}

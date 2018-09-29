extern crate clap;
extern crate ecmascript;
extern crate failure;
extern crate serde_json;

use clap::{App, Arg};
use failure::Error;

fn main() -> Result<(), Error> {
    let matches = App::new("ESTree AST exporter")
        .version("0.1")
        .author("Nick Dujay <nickdujay@gmail.com>")
        .about("Exports ESTree AST from the first argument")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input source to parse")
                .required(true)
                .index(1),
        ).arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Sets the verbosity"),
        ).get_matches();
    let source = matches.value_of("INPUT").unwrap();
    if matches.is_present("verbose") {
        println!("source: {:?}", source);
    }
    let ast = ecmascript::parse(&source)?;
    let json_string = serde_json::to_string_pretty(&ast)?;
    println!("{}", json_string);
    Ok(())
}

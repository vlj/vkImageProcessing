#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate clap;

#[macro_use]
extern crate bitflags;

use clap::{Arg, App, SubCommand};


use std::io;
use std::fs::File;
use std::io::prelude::*;
use std::path::*;


mod SPV;
mod HeaderGeneration;
use HeaderGeneration::*;


fn main() -> io::Result<()> {
    

    let matches = App::new("shader2headers")
                          .arg(Arg::with_name("input")
                               .long("input")
                               .required(true)
                               .value_name("FILE")
                               .takes_value(true))
                          .arg(Arg::with_name("output_folder")
                                .value_name("FILE")
                               .required(true)
                               .long("output_folder"))
                          .get_matches();

    let filename = match matches.value_of("input") {
        Some(v) => std::path::Path::new(v),
        None => return Err(io::Error::from(io::ErrorKind::InvalidData)),
    };

    
    let output_folder = match matches.value_of("output_folder") {
        Some(v) => std::path::Path::new(v),
        None => return Err(io::Error::from(io::ErrorKind::InvalidData)),
    };

    let basename = std::path::Path::new(&filename).file_name().map(|x| x.to_os_string().into_string().unwrap()).unwrap();
    

    let shader_name  = basename.replace(".", "_");
    let mut f = File::open(filename)?;

    let mut buf = Vec::new();
    f.read_to_end(&mut buf)?;

    let properModule = SPV::spvReflectCreateShaderModule(buf).expect("failed to do the conversion");


    let header = build_header(&shader_name, properModule);
    let new_path = format!("{}.h", shader_name);
    let mut dest_file = File::create(&output_folder.join(new_path))?;
    dest_file.write_all(header.as_bytes())?;


    Ok(())
}
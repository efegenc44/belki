use std::process::exit;

use crate::token::Location;

pub fn report(loc: Location, msg: &str) {
    println!();
    println!("{}:{} | {}", loc.col, loc.row, msg);
    println!();
    exit(1);
}

pub fn runtime(msg: &str) {
    println!();
    println!("| {}", msg);
    println!();
    exit(1);
}

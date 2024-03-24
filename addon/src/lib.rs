use log::warn;

use std::{
    ffi::{c_void, CStr},
    os::raw::{c_char, c_int},
    ptr::null_mut,
};

mod addon;


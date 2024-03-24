use std::{
  ffi::{c_void, CStr, CString},
  mem::swap,
  ops::{Deref, DerefMut},
  os::raw::{c_char, c_int, c_short},
  sync::{Arc, Mutex},
};
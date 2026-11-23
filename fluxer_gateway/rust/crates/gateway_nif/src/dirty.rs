// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::CStr;

use guild_rt::Scheduler;
use rustler::codegen_runtime::c_int;
use rustler::sys::{
    ERL_NIF_DIRTY_JOB_CPU_BOUND, ERL_NIF_THR_NORMAL_SCHEDULER, ErlNifEnv, ErlNifTerm,
    enif_schedule_nif, enif_thread_type,
};
use rustler::{Encoder, Env, Term};

type Raw = unsafe extern "C" fn(*mut ErlNifEnv, c_int, *const ErlNifTerm) -> ErlNifTerm;

mod atoms {
    rustler::atoms! {
        error,
        needs_dirty,
    }
}

pub fn scheduler() -> Scheduler {
    if unsafe { enif_thread_type() } == ERL_NIF_THR_NORMAL_SCHEDULER {
        Scheduler::Normal
    } else {
        Scheduler::Dirty
    }
}

pub fn raw(name: &CStr, arity: usize) -> Option<Raw> {
    rustler::codegen_runtime::inventory::iter::<rustler::Nif>()
        .find(|nif| {
            usize::try_from(nif.arity).ok() == Some(arity)
                && unsafe { CStr::from_ptr(nif.name) } == name
        })
        .map(|nif| nif.raw_func)
}

pub fn run<'a>(
    env: Env<'a>,
    name: &'static CStr,
    args: &[Term<'a>],
    body: impl FnOnce(Scheduler) -> Option<Term<'a>>,
) -> Term<'a> {
    let on = scheduler();
    match body(on) {
        Some(term) => term,
        None if on == Scheduler::Normal => reschedule(env, name, args),
        None => (atoms::error(), atoms::needs_dirty()).encode(env),
    }
}

fn reschedule<'a>(env: Env<'a>, name: &'static CStr, args: &[Term<'a>]) -> Term<'a> {
    let (Some(fun), Ok(argc)) = (raw(name, args.len()), c_int::try_from(args.len())) else {
        return (atoms::error(), atoms::needs_dirty()).encode(env);
    };
    let argv: Vec<ErlNifTerm> = args.iter().map(Term::as_c_arg).collect();
    unsafe {
        let flags = c_int::try_from(ERL_NIF_DIRTY_JOB_CPU_BOUND).unwrap_or(1);
        let scheduled = enif_schedule_nif(
            env.as_c_arg(),
            name.as_ptr(),
            flags,
            fun,
            argc,
            argv.as_ptr(),
        );
        Term::new(env, scheduled)
    }
}

// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Info {
    pub pid: i32,
    pub parent_pid: i32,
    pub process_group_id: i32,
}

#[cfg(target_os = "macos")]
mod sys {
    use super::Info;
    use std::mem::{offset_of, size_of};

    const PROC_PIDTBSDINFO: i32 = 3;
    const MAXCOMLEN: usize = 16;

    #[repr(C)]
    #[derive(Default, Copy, Clone)]
    pub struct ProcBsdInfo {
        pub pbi_flags: u32,
        pub pbi_status: u32,
        pub pbi_xstatus: u32,
        pub pbi_pid: u32,
        pub pbi_ppid: u32,
        pub pbi_uid: u32,
        pub pbi_gid: u32,
        pub pbi_ruid: u32,
        pub pbi_rgid: u32,
        pub pbi_svuid: u32,
        pub pbi_svgid: u32,
        pub rfu_1: u32,
        pub pbi_comm: [u8; MAXCOMLEN],
        pub pbi_name: [u8; 2 * MAXCOMLEN],
        pub pbi_nfiles: u32,
        pub pbi_pgid: u32,
        pub pbi_pjobc: u32,
        pub e_tdev: u32,
        pub e_tpgid: u32,
        pub pbi_nice: i32,
        pub pbi_start_tvsec: u64,
        pub pbi_start_tvusec: u64,
    }

    unsafe extern "C" {
        fn proc_pidinfo(
            pid: i32,
            flavor: i32,
            arg: u64,
            buffer: *mut core::ffi::c_void,
            buffersize: i32,
        ) -> i32;
        pub fn proc_listallpids(buffer: *mut core::ffi::c_void, buffersize: i32) -> i32;
    }

    pub fn info_for_pid(pid: i32) -> Option<Info> {
        if pid <= 0 {
            return None;
        }
        let mut raw = ProcBsdInfo::default();
        let copied = unsafe {
            proc_pidinfo(
                pid,
                PROC_PIDTBSDINFO,
                0,
                (&raw mut raw) as *mut _ as *mut _,
                size_of::<ProcBsdInfo>() as i32,
            )
        };
        let min = (offset_of!(ProcBsdInfo, pbi_pgid) + size_of::<u32>()) as i32;
        if copied < min {
            return None;
        }
        Some(Info {
            pid: raw.pbi_pid as i32,
            parent_pid: raw.pbi_ppid as i32,
            process_group_id: raw.pbi_pgid as i32,
        })
    }
}

#[cfg(target_os = "macos")]
pub fn info_for_pid(pid: i32) -> Option<Info> {
    sys::info_for_pid(pid)
}

#[cfg(not(target_os = "macos"))]
pub fn info_for_pid(_pid: i32) -> Option<Info> {
    None
}

pub fn is_same_launch_tree(candidate_pid: i32, target_pid: i32, target_info: Option<Info>) -> bool {
    if candidate_pid <= 0 || target_pid <= 0 {
        return false;
    }
    if candidate_pid == target_pid {
        return true;
    }

    let mut current = match info_for_pid(candidate_pid) {
        Some(i) => i,
        None => return false,
    };
    if shares_process_group(current, target_pid, target_info) {
        return true;
    }

    let mut depth = 0usize;
    while depth < 64 {
        let parent = current.parent_pid;
        if parent == target_pid {
            return true;
        }
        if parent <= 1 || parent == current.pid {
            return false;
        }
        current = match info_for_pid(parent) {
            Some(i) => i,
            None => return false,
        };
        depth += 1;
    }
    false
}

fn shares_process_group(candidate: Info, target_pid: i32, target_info: Option<Info>) -> bool {
    let cg = candidate.process_group_id;
    if cg <= 0 {
        return false;
    }
    if cg == target_pid {
        return true;
    }
    if let Some(t) = target_info {
        return t.process_group_id > 0 && cg == t.process_group_id;
    }
    false
}

#[cfg(target_os = "macos")]
fn append_pid(out: &mut Vec<i32>, max: usize, pid: i32) -> bool {
    if out.contains(&pid) {
        return true;
    }
    if out.len() >= max {
        return false;
    }
    out.push(pid);
    true
}

#[cfg(target_os = "macos")]
fn related_pids(
    target_pid: i32,
    target_info: Option<Info>,
    candidates: &[i32],
    max_count: usize,
) -> Vec<i32> {
    if target_pid <= 0 || max_count == 0 {
        return Vec::new();
    }
    let cap = candidates.len().max(1).min(max_count);
    let mut out: Vec<i32> = Vec::with_capacity(cap);
    let _ = append_pid(&mut out, cap, target_pid);
    for &pid in candidates {
        if out.len() >= cap {
            break;
        }
        if pid <= 0 || pid == target_pid {
            continue;
        }
        if !is_same_launch_tree(pid, target_pid, target_info) {
            continue;
        }
        let _ = append_pid(&mut out, cap, pid);
    }
    out
}

#[cfg(target_os = "macos")]
pub fn collect_related_pids(target_pid: i32, max_count: usize) -> Vec<i32> {
    if target_pid <= 0 {
        return Vec::new();
    }
    let reported = unsafe { sys::proc_listallpids(core::ptr::null_mut(), 0) };
    if reported <= 0 {
        return vec![target_pid];
    }
    let mut all_pids: Vec<i32> = vec![0; reported as usize];
    let copied = unsafe {
        sys::proc_listallpids(
            all_pids.as_mut_ptr() as *mut _,
            (all_pids.len() * core::mem::size_of::<i32>()) as i32,
        )
    };
    if copied <= 0 {
        return vec![target_pid];
    }
    let count = (copied as usize).min(all_pids.len());
    related_pids(
        target_pid,
        info_for_pid(target_pid),
        &all_pids[..count],
        max_count,
    )
}

#[cfg(not(target_os = "macos"))]
pub fn collect_related_pids(target_pid: i32, _max_count: usize) -> Vec<i32> {
    if target_pid <= 0 {
        return Vec::new();
    }
    vec![target_pid]
}

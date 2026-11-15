// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProcessEntry {
    pub pid: u32,
    pub parent: u32,
}

fn find_process(entries: &[ProcessEntry], pid: u32) -> Option<ProcessEntry> {
    entries.iter().copied().find(|entry| entry.pid == pid)
}

pub fn pid_is_our_descendant(entries: &[ProcessEntry], target: u32, self_pid: u32) -> bool {
    if target == 0 {
        return false;
    }
    if target == self_pid {
        return true;
    }
    let mut current = target;
    for _ in 0..64 {
        let Some(entry) = find_process(entries, current) else {
            return false;
        };
        if entry.parent == 0 || entry.parent == current {
            return false;
        }
        if entry.parent == self_pid {
            return true;
        }
        current = entry.parent;
    }
    false
}

pub fn pid_is_our_ancestor(entries: &[ProcessEntry], target: u32, self_pid: u32) -> bool {
    if target == 0 {
        return false;
    }
    let mut current = self_pid;
    for _ in 0..64 {
        if current == target {
            return true;
        }
        let Some(entry) = find_process(entries, current) else {
            return false;
        };
        if entry.parent == 0 || entry.parent == current {
            return false;
        }
        current = entry.parent;
    }
    false
}

pub fn pid_overlaps_our_process_tree(entries: &[ProcessEntry], target: u32, self_pid: u32) -> bool {
    pid_is_our_descendant(entries, target, self_pid)
        || pid_is_our_ancestor(entries, target, self_pid)
}

pub fn deduplicate_capture_roots(
    entries: &[ProcessEntry],
    capture_pids: &[u32],
    exclude_pids: &[u32],
) -> Vec<u32> {
    use std::collections::BTreeSet;

    let mut uncaptured: BTreeSet<u32> = capture_pids
        .iter()
        .copied()
        .filter(|pid| *pid != 0)
        .collect();
    let excluded: BTreeSet<u32> = exclude_pids
        .iter()
        .copied()
        .filter(|pid| *pid != 0)
        .collect();
    uncaptured.retain(|candidate| {
        !excluded
            .iter()
            .any(|excluded_pid| pid_is_our_descendant(entries, *excluded_pid, *candidate))
    });

    let mut explicit = BTreeSet::new();
    while !uncaptured.is_empty() {
        let before = uncaptured.len();
        for pid in uncaptured.clone() {
            let parent = find_process(entries, pid)
                .map(|entry| entry.parent)
                .unwrap_or(0);
            if !uncaptured.contains(&parent) {
                explicit.insert(pid);
            }
        }
        for pid in &explicit {
            uncaptured.remove(pid);
        }
        for pid in uncaptured.clone() {
            let parent = find_process(entries, pid)
                .map(|entry| entry.parent)
                .unwrap_or(0);
            if explicit.contains(&parent) {
                uncaptured.remove(&pid);
            }
        }
        if uncaptured.len() == before {
            explicit.extend(uncaptured.iter().copied());
            break;
        }
    }
    explicit.into_iter().collect()
}

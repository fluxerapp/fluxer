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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn self_pid_is_always_its_own_descendant() {
        assert!(pid_is_our_descendant(&[], 1234, 1234));
    }

    #[test]
    fn pid_of_zero_is_never_matched() {
        assert!(!pid_is_our_descendant(&[], 0, 0));
        assert!(!pid_is_our_descendant(&[], 0, 1234));
    }

    #[test]
    fn direct_child_of_self_pid_is_matched() {
        let entries = [ProcessEntry {
            pid: 4001,
            parent: 1234,
        }];
        assert!(pid_is_our_descendant(&entries, 4001, 1234));
    }

    #[test]
    fn transitive_descendant_via_two_hops_is_matched() {
        let entries = [
            ProcessEntry {
                pid: 4001,
                parent: 1234,
            },
            ProcessEntry {
                pid: 4099,
                parent: 4001,
            },
        ];
        assert!(pid_is_our_descendant(&entries, 4099, 1234));
    }

    #[test]
    fn ancestor_of_self_pid_is_matched() {
        let entries = [
            ProcessEntry { pid: 10, parent: 1 },
            ProcessEntry {
                pid: 1234,
                parent: 10,
            },
        ];
        assert!(pid_is_our_ancestor(&entries, 10, 1234));
        assert!(pid_overlaps_our_process_tree(&entries, 10, 1234));
    }

    #[test]
    fn deduplicate_capture_roots_keeps_parent_and_drops_child() {
        let entries = [
            ProcessEntry { pid: 10, parent: 1 },
            ProcessEntry {
                pid: 11,
                parent: 10,
            },
        ];
        assert_eq!(
            deduplicate_capture_roots(&entries, &[10, 11], &[]),
            vec![10]
        );
    }

    #[test]
    fn deduplicate_capture_roots_drops_parent_of_excluded_child() {
        let entries = [
            ProcessEntry { pid: 10, parent: 1 },
            ProcessEntry {
                pid: 11,
                parent: 10,
            },
        ];
        assert_eq!(
            deduplicate_capture_roots(&entries, &[10], &[11]),
            Vec::<u32>::new()
        );
    }

    #[test]
    fn deduplicate_capture_roots_drops_grandparent_of_excluded_grandchild() {
        let entries = [
            ProcessEntry { pid: 10, parent: 1 },
            ProcessEntry {
                pid: 11,
                parent: 10,
            },
            ProcessEntry {
                pid: 12,
                parent: 11,
            },
        ];
        assert_eq!(
            deduplicate_capture_roots(&entries, &[10], &[12]),
            Vec::<u32>::new()
        );
    }

    #[test]
    fn deduplicate_capture_roots_keeps_safe_sibling_after_excluding_parent_root() {
        let entries = [
            ProcessEntry { pid: 10, parent: 1 },
            ProcessEntry {
                pid: 11,
                parent: 10,
            },
            ProcessEntry {
                pid: 12,
                parent: 10,
            },
        ];
        assert_eq!(
            deduplicate_capture_roots(&entries, &[10, 12], &[11]),
            vec![12]
        );
    }

    #[test]
    fn deduplicate_capture_roots_drops_excluded_root_itself() {
        let entries = [ProcessEntry { pid: 10, parent: 1 }];
        assert_eq!(
            deduplicate_capture_roots(&entries, &[10], &[10]),
            Vec::<u32>::new()
        );
    }

    #[test]
    fn deduplicate_capture_roots_ignores_zero_pids() {
        let entries = [ProcessEntry { pid: 10, parent: 1 }];
        assert_eq!(
            deduplicate_capture_roots(&entries, &[0, 10], &[0]),
            vec![10]
        );
    }

    #[test]
    fn deduplicate_capture_roots_keeps_pid_when_parent_is_missing() {
        let entries = [ProcessEntry {
            pid: 10,
            parent: 9999,
        }];
        assert_eq!(deduplicate_capture_roots(&entries, &[10], &[]), vec![10]);
    }

    #[test]
    fn unrelated_pid_in_same_snapshot_is_not_matched() {
        let entries = [
            ProcessEntry {
                pid: 4001,
                parent: 1234,
            },
            ProcessEntry {
                pid: 8500,
                parent: 1,
            },
        ];
        assert!(!pid_is_our_descendant(&entries, 8500, 1234));
    }

    #[test]
    fn missing_parent_terminates_the_walk_safely() {
        let entries = [ProcessEntry {
            pid: 9000,
            parent: 9999,
        }];
        assert!(!pid_is_our_descendant(&entries, 9000, 1234));
    }

    #[test]
    fn ppid_equal_to_self_loop_is_rejected() {
        let entries = [ProcessEntry {
            pid: 5000,
            parent: 5000,
        }];
        assert!(!pid_is_our_descendant(&entries, 5000, 1234));
    }

    #[test]
    fn ppid_zero_terminates_the_walk() {
        let entries = [ProcessEntry { pid: 4, parent: 0 }];
        assert!(!pid_is_our_descendant(&entries, 4, 1234));
    }
}

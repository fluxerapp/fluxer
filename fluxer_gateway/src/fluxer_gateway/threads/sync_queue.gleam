// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/list

pub opaque type SyncQueue {
  SyncQueue(front: List(String), back: List(String))
}

pub fn new() -> SyncQueue {
  SyncQueue(front: [], back: [])
}

pub fn push(queue: SyncQueue, session_id: String) -> SyncQueue {
  SyncQueue(..queue, back: [session_id, ..queue.back])
}

pub fn pop(queue: SyncQueue) -> Result(#(String, SyncQueue), Nil) {
  case queue.front, queue.back {
    [first, ..rest], back -> Ok(#(first, SyncQueue(front: rest, back:)))
    [], [] -> Error(Nil)
    [], back -> pop(SyncQueue(front: list.reverse(back), back: []))
  }
}

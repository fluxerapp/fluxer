// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto
import bouncer_proto/codec_range
import bouncer_proto/key.{type BucketId}
import bouncer_proto/message.{type Beat, type Hello}
import bouncer_proto/worker.{
  type Entry, type Roster, type Status, type WorkerId, Draining, Entry, Gone,
  Joining, Ready,
}
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option.{Some}
import gleam/result

type Seen {
  Seen(entry: Entry, pinned: List(Int), roster_hash: Int, said_bye: Bool)
}

pub opaque type Directory {
  Directory(seen: Dict(Atom, Seen))
}

pub type DirectoryError {
  UnknownWorker(node: Atom)
  StaleIncarnation(worker: WorkerId)
  ProtocolMismatch(worker: WorkerId)
}

pub fn new() -> Directory {
  Directory(seen: dict.new())
}

pub fn hello(
  directory: Directory,
  hello: Hello,
  now_ms: Int,
) -> Result(#(Directory, Entry), DirectoryError) {
  let node = hello.worker.node
  let newer_known = case dict.get(directory.seen, node) {
    Ok(seen) -> seen.entry.worker.incarnation > hello.worker.incarnation
    Error(Nil) -> False
  }
  case
    newer_known,
    bouncer_proto.negotiate(bouncer_proto.own_range(), hello.protocol)
  {
    True, _ -> Error(StaleIncarnation(hello.worker))
    False, Error(Nil) -> Error(ProtocolMismatch(hello.worker))
    False, Ok(_) -> {
      let entry =
        Entry(
          worker: hello.worker,
          profile: hello.profile,
          status: Joining,
          last_beat_ms: now_ms,
        )
      let seen =
        Seen(
          entry: entry,
          pinned: hello.pinned,
          roster_hash: 0,
          said_bye: False,
        )
      Ok(#(Directory(seen: dict.insert(directory.seen, node, seen)), entry))
    }
  }
}

fn current(
  directory: Directory,
  worker: WorkerId,
) -> Result(Seen, DirectoryError) {
  case dict.get(directory.seen, worker.node) {
    Error(Nil) -> Error(UnknownWorker(worker.node))
    Ok(seen) if seen.entry.worker.incarnation == worker.incarnation -> Ok(seen)
    Ok(seen) if seen.entry.worker.incarnation > worker.incarnation ->
      Error(StaleIncarnation(worker))
    Ok(_) -> Error(UnknownWorker(worker.node))
  }
}

pub fn beat(
  directory: Directory,
  beat: Beat,
  now_ms: Int,
) -> Result(#(Directory, Entry), DirectoryError) {
  use seen <- result.try(current(directory, beat.worker))
  case seen.said_bye {
    True -> Error(StaleIncarnation(beat.worker))
    False -> {
      let status = case beat.draining {
        True -> Draining
        False -> Ready
      }
      let entry = Entry(..seen.entry, status: status, last_beat_ms: now_ms)
      let seen = Seen(..seen, entry: entry, roster_hash: beat.roster_hash)
      Ok(#(
        Directory(seen: dict.insert(directory.seen, beat.worker.node, seen)),
        entry,
      ))
    }
  }
}

pub fn bye(
  directory: Directory,
  worker: WorkerId,
) -> Result(Directory, DirectoryError) {
  use seen <- result.try(current(directory, worker))
  let seen =
    Seen(..seen, entry: Entry(..seen.entry, status: Gone), said_bye: True)
  Ok(Directory(seen: dict.insert(directory.seen, worker.node, seen)))
}

pub fn expire(
  directory: Directory,
  now_ms: Int,
  silence_ms: Int,
) -> #(Directory, List(WorkerId)) {
  dict.fold(directory.seen, #(directory, []), fn(acc, node, seen) {
    case
      seen.entry.status != Gone && now_ms - seen.entry.last_beat_ms > silence_ms
    {
      False -> acc
      True -> {
        let gone = Seen(..seen, entry: Entry(..seen.entry, status: Gone))
        #(Directory(seen: dict.insert({ acc.0 }.seen, node, gone)), [
          seen.entry.worker,
          ..acc.1
        ])
      }
    }
  })
}

pub fn prune(directory: Directory, now_ms: Int, keep_ms: Int) -> Directory {
  Directory(
    seen: dict.filter(directory.seen, fn(_, seen) {
      seen.entry.status != Gone || now_ms - seen.entry.last_beat_ms <= keep_ms
    }),
  )
}

pub fn lookup(
  directory: Directory,
  node: Atom,
) -> Result(Entry, DirectoryError) {
  case dict.get(directory.seen, node) {
    Ok(seen) -> Ok(seen.entry)
    Error(Nil) -> Error(UnknownWorker(node))
  }
}

pub fn find(
  directory: Directory,
  worker: WorkerId,
) -> Result(Entry, DirectoryError) {
  current(directory, worker) |> result.map(fn(seen) { seen.entry })
}

pub fn entries(directory: Directory) -> List(Entry) {
  directory.seen
  |> dict.values
  |> list.filter_map(fn(seen) {
    case seen.entry.status {
      Gone -> Error(Nil)
      _ -> Ok(seen.entry)
    }
  })
}

fn serving(status: Status) -> Bool {
  status == Ready || status == Draining
}

pub fn roster(directory: Directory) -> Roster {
  directory
  |> entries
  |> list.filter(fn(entry) { serving(entry.status) })
  |> list.map(fn(entry) { entry.worker })
  |> worker.roster
}

pub fn silent_ms(
  directory: Directory,
  worker: WorkerId,
  now_ms: Int,
) -> Result(Int, DirectoryError) {
  current(directory, worker)
  |> result.map(fn(seen) { now_ms - seen.entry.last_beat_ms })
}

pub fn reported_roster(
  directory: Directory,
  worker: WorkerId,
) -> Result(Int, DirectoryError) {
  current(directory, worker) |> result.map(fn(seen) { seen.roster_hash })
}

pub fn said_bye(directory: Directory, worker: WorkerId) -> Bool {
  case current(directory, worker) {
    Ok(seen) -> seen.said_bye
    Error(_) -> False
  }
}

pub fn superseded(directory: Directory, worker: WorkerId) -> Bool {
  case dict.get(directory.seen, worker.node) {
    Ok(seen) -> seen.entry.worker.incarnation > worker.incarnation
    Error(Nil) -> False
  }
}

fn handles(directory: Directory, entry: Entry, bucket: BucketId) -> Bool {
  let pinned = case dict.get(directory.seen, entry.worker.node) {
    Ok(seen) -> seen.pinned
    Error(Nil) -> []
  }
  let advertised =
    result.is_ok(codec_range.find(entry.profile.codecs, bucket.ns))
  case key.is_pinned(bucket) {
    True -> advertised && bucket.bucket < list.length(pinned)
    False -> advertised && pinned == []
  }
}

pub fn candidates(directory: Directory, bucket: BucketId) -> List(Entry) {
  directory
  |> entries
  |> list.filter(fn(entry) {
    entry.status == Ready && handles(directory, entry, bucket)
  })
}

pub fn namespaces(directory: Directory) -> List(#(Int, Int)) {
  directory.seen
  |> dict.values
  |> list.filter(fn(seen) { serving(seen.entry.status) })
  |> list.flat_map(fn(seen) {
    list.map(seen.entry.profile.codecs, fn(range) {
      case range.ns >= key.first_pinned {
        True -> #(range.ns, list.length(seen.pinned))
        False -> #(range.ns, key.bucket_count)
      }
    })
  })
  |> list.fold(dict.new(), fn(found, pair) {
    dict.upsert(found, pair.0, fn(known) {
      case known {
        Some(count) if count >= pair.1 -> count
        _ -> pair.1
      }
    })
  })
  |> dict.to_list
}

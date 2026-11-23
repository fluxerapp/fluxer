// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/int
import gleam/list

pub type CodecRange {
  CodecRange(ns: Int, oldest_read: Int, max_read: Int, max_write: Int)
}

pub fn unmovable(ns: Int) -> CodecRange {
  CodecRange(ns: ns, oldest_read: 0, max_read: 0, max_write: 0)
}

pub fn find(ranges: List(CodecRange), ns: Int) -> Result(CodecRange, Nil) {
  list.find(ranges, fn(range) { range.ns == ns })
}

pub fn write_version(
  source: CodecRange,
  target: CodecRange,
) -> Result(Int, Nil) {
  let version = int.min(source.max_write, target.max_read)
  case
    source.ns == target.ns
    && version >= 1
    && version >= source.max_write - 1
    && version >= target.oldest_read
  {
    True -> Ok(version)
    False -> Error(Nil)
  }
}

pub fn can_move(
  source: List(CodecRange),
  target: List(CodecRange),
  ns: Int,
) -> Result(Int, Nil) {
  case find(source, ns), find(target, ns) {
    Ok(from), Ok(to) -> write_version(from, to)
    _, _ -> Error(Nil)
  }
}

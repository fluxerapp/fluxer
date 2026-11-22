// SPDX-License-Identifier: AGPL-3.0-or-later

pub type ViewSet {
  ViewSet(epoch: Int, bits: BitArray)
}

pub fn contains(set: ViewSet, index: Int) -> Bool {
  bit_at(set.bits, index)
}

@internal
pub fn bit_at(bits: BitArray, index: Int) -> Bool {
  case index < 0 {
    True -> False
    False ->
      case bits {
        <<_:size(index), 1:1, _:bits>> -> True
        _ -> False
      }
  }
}

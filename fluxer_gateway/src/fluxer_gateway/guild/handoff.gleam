// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/router
import fluxer_gateway/config/durations
import fluxer_gateway/core/id
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/guild/handoff/freeze
import fluxer_gateway/guild/manager/registry
import gleam/erlang/process
import gleam/list
import gleam/result

const max_passes = 6

fn retry_delay_ms() -> Int {
  durations.guild().handoff_retry_delay_ms
}

fn move_spacing_ms() -> Int {
  durations.guild().handoff_move_spacing_ms
}

type Tally {
  Tally(done: Int, failed: Int)
}

pub fn export_for(nodes: List(Node)) -> Int {
  pass(nodes, 0, 0)
}

fn pass(nodes: List(Node), iteration: Int, moved: Int) -> Int {
  case iteration >= max_passes {
    True -> moved
    False -> {
      let candidates =
        registry.guild_ints()
        |> list.filter_map(fn(value) {
          use guild <- result.try(
            id.guild_id(value) |> result.replace_error(Nil),
          )
          case router.owner(router.GuildKey(guild), node.Guilds) {
            router.Remote(target) ->
              case list.contains(nodes, target) {
                True -> Ok(#(guild, target))
                False -> Error(Nil)
              }
            _ -> Error(Nil)
          }
        })
      let tally =
        list.fold(candidates, Tally(done: 0, failed: 0), fn(acc, pair) {
          let result = freeze.move(pair.0, pair.1)
          process.sleep(move_spacing_ms())
          case result {
            Ok(Nil) -> Tally(..acc, done: acc.done + 1)
            Error(freeze.NotRunning) -> acc
            Error(_) -> Tally(..acc, failed: acc.failed + 1)
          }
        })
      case tally.failed > 0 {
        True -> {
          process.sleep(retry_delay_ms())
          pass(nodes, iteration + 1, moved + tally.done)
        }
        False -> moved + tally.done
      }
    }
  }
}

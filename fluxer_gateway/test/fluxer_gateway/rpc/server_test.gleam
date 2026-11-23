// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/rpc/server

pub fn bad_json_is_internal_error_test() {
  assert server.execute("guild.get_counts", <<"{nope">>)
    == "{\"ok\":false,\"error\":\"internal_error\"}"
}

pub fn non_object_params_is_internal_error_test() {
  assert server.execute("guild.get_counts", <<"[1,2]">>)
    == "{\"ok\":false,\"error\":\"internal_error\"}"
}

pub fn unknown_method_test() {
  assert server.execute("nope.method", <<"{}">>)
    == "{\"ok\":false,\"error\":\"Unknown method: nope.method\"}"
}

pub fn handler_limit_test() {
  assert server.handler_limit_reached(512, 512)
  assert !server.handler_limit_reached(511, 512)
}

pub fn guild_route_reads_only_the_guild_id_test() {
  let assert Ok(guild) = id.guild_id(1_320_000_000_000_000_001)
  assert server.guild_route(<<
      "{\"event\":\"X\",\"data\":{\"guild_id\":\"5\"},\"guild_id\":\"1320000000000000001\"}",
    >>)
    == Ok(guild)
  assert server.guild_route(<<"{\"guild_id\":1320000000000000001}">>)
    == Ok(guild)
  assert server.guild_route(<<"{\"data\":{\"guild_id\":\"5\"}}">>) == Error(Nil)
  assert server.guild_route(<<"{\"guild_id\":null}">>) == Error(Nil)
  assert server.guild_route(<<"{nope">>) == Error(Nil)
}

pub fn owned_execution_answers_with_the_encoded_result_test() {
  assert server.execute_owned("guild.nope", <<"{}">>)
    == #(False, <<"Unknown method: guild.nope">>)
  assert server.execute_owned("guild.get_counts", <<"[1]">>)
    == #(False, <<"internal_error">>)
}

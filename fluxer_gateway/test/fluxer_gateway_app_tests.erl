%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_app_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

start_keeps_generational_gc_as_vm_default_test() ->
    ?assertEqual([10], vm_fullsweep_after_values()).

vm_fullsweep_after_values() ->
    {fluxer_gateway_app, Beam, _Path} = code:get_object_code(fluxer_gateway_app),
    {ok, {fluxer_gateway_app, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(Beam, [abstract_code]),
    collect_fullsweep_after(Forms, []).

collect_fullsweep_after([Form | Rest], Acc) ->
    collect_fullsweep_after(Rest, collect_fullsweep_after(Form, Acc));
collect_fullsweep_after(
    {call, _, {remote, _, {atom, _, erlang}, {atom, _, system_flag}}, [
        {atom, _, fullsweep_after}, {integer, _, Value}
    ]},
    Acc
) ->
    Acc ++ [Value];
collect_fullsweep_after(Form, Acc) when is_tuple(Form) ->
    collect_fullsweep_after(tuple_to_list(Form), Acc);
collect_fullsweep_after(_Form, Acc) ->
    Acc.

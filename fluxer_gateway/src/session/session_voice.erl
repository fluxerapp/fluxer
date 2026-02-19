%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(session_voice).

-export([
    init_voice_queue/0,
    process_voice_queue/1,
    handle_voice_state_update/2,
    handle_voice_disconnect/1
]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type user_id() :: session:user_id().

-type voice_state_reply() ::
    {reply, ok, session_state()}
    | {reply, {error, term(), term()}, session_state()}.

-spec init_voice_queue() -> #{voice_queue := queue:queue(), voice_queue_timer := undefined}.
init_voice_queue() ->
    #{voice_queue => queue:new(), voice_queue_timer => undefined}.

-spec process_voice_queue(session_state()) -> session_state().
process_voice_queue(State) ->
    VoiceQueue = maps:get(voice_queue, State, queue:new()),
    case queue:out(VoiceQueue) of
        {empty, _} ->
            State;
        {{value, Item}, NewQueue} ->
            process_voice_queue_item(Item, maps:put(voice_queue, NewQueue, State))
    end.

-spec process_voice_queue_item(map(), session_state()) -> session_state().
process_voice_queue_item(Item, State) ->
    case maps:get(type, Item, undefined) of
        voice_state_update ->
            Data = maps:get(data, Item),
            {reply, _, NewState} = handle_voice_state_update(Data, State),
            NewState;
        _ ->
            State
    end.

-spec handle_voice_state_update(map(), session_state()) -> voice_state_reply().
handle_voice_state_update(Data, State) ->
    GuildIdRaw = maps:get(<<"guild_id">>, Data, null),
    ChannelIdRaw = maps:get(<<"channel_id">>, Data, null),
    ConnectionId = maps:get(<<"connection_id">>, Data, null),
    SelfMute = maps:get(<<"self_mute">>, Data, false),
    SelfDeaf = maps:get(<<"self_deaf">>, Data, false),
    SelfVideo = maps:get(<<"self_video">>, Data, false),
    SelfStream = maps:get(<<"self_stream">>, Data, false),
    ViewerStreamKeys = maps:get(<<"viewer_stream_keys">>, Data, undefined),
    IsMobile = maps:get(<<"is_mobile">>, Data, false),
    Latitude = maps:get(<<"latitude">>, Data, null),
    Longitude = maps:get(<<"longitude">>, Data, null),
    SessionId = maps:get(id, State),
    UserId = maps:get(user_id, State),
    Guilds = maps:get(guilds, State),
    GuildIdResult = validation:validate_optional_snowflake(GuildIdRaw),
    ChannelIdResult = validation:validate_optional_snowflake(ChannelIdRaw),
    case {GuildIdResult, ChannelIdResult} of
        {{ok, GuildId}, {ok, ChannelId}} ->
            handle_validated_voice_state_update(
                GuildId,
                ChannelId,
                ConnectionId,
                SelfMute,
                SelfDeaf,
                SelfVideo,
                SelfStream,
                ViewerStreamKeys,
                IsMobile,
                Latitude,
                Longitude,
                SessionId,
                UserId,
                Guilds,
                State
            );
        {Error = {error, _, _}, _} ->
            {reply, Error, State};
        {_, Error = {error, _, _}} ->
            {reply, Error, State}
    end.

-spec handle_validated_voice_state_update(
    guild_id() | null,
    channel_id() | null,
    binary() | null,
    boolean(),
    boolean(),
    boolean(),
    boolean(),
    list() | undefined,
    boolean(),
    number() | null,
    number() | null,
    binary(),
    user_id(),
    map(),
    session_state()
) -> voice_state_reply().
handle_validated_voice_state_update(
    null,
    null,
    null,
    _SelfMute,
    _SelfDeaf,
    _SelfVideo,
    _SelfStream,
    _ViewerStreamKeys,
    _IsMobile,
    _Latitude,
    _Longitude,
    _SessionId,
    _UserId,
    _Guilds,
    State
) ->
    handle_voice_disconnect(State);
handle_validated_voice_state_update(
    null,
    null,
    ConnectionId,
    _SelfMute,
    _SelfDeaf,
    _SelfVideo,
    _SelfStream,
    _ViewerStreamKeys,
    _IsMobile,
    _Latitude,
    _Longitude,
    SessionId,
    UserId,
    _Guilds,
    State
) when is_binary(ConnectionId) ->
    Request = #{
        user_id => UserId,
        channel_id => null,
        session_id => SessionId,
        connection_id => ConnectionId,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_keys => [],
        is_mobile => false,
        latitude => null,
        longitude => null
    },
    StateWithSessionPid = maps:put(session_pid, self(), State),
    case dm_voice:voice_state_update(Request, StateWithSessionPid) of
        {reply, #{success := true}, NewState} ->
            CleanState = maps:remove(session_pid, NewState),
            {reply, ok, CleanState};
        {reply, {error, Category, ErrorAtom}, _StateWithPid} ->
            {reply, {error, Category, ErrorAtom}, State}
    end;
handle_validated_voice_state_update(
    null,
    ChannelId,
    ConnectionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKeys,
    IsMobile,
    Latitude,
    Longitude,
    SessionId,
    UserId,
    _Guilds,
    State
) when is_integer(ChannelId), (is_binary(ConnectionId) orelse ConnectionId =:= null) ->
    Request = #{
        user_id => UserId,
        channel_id => ChannelId,
        session_id => SessionId,
        connection_id => ConnectionId,
        self_mute => SelfMute,
        self_deaf => SelfDeaf,
        self_video => SelfVideo,
        self_stream => SelfStream,
        viewer_stream_keys => ViewerStreamKeys,
        is_mobile => IsMobile,
        latitude => Latitude,
        longitude => Longitude
    },
    StateWithSessionPid = maps:put(session_pid, self(), State),
    case dm_voice:voice_state_update(Request, StateWithSessionPid) of
        {reply, #{success := true, needs_token := true}, NewState} ->
            SessionPid = self(),
            spawn(fun() ->
                dm_voice:get_voice_token(
                    ChannelId, UserId, SessionId, SessionPid, Latitude, Longitude
                )
            end),
            CleanState = maps:remove(session_pid, NewState),
            {reply, ok, CleanState};
        {reply, #{success := true}, NewState} ->
            CleanState = maps:remove(session_pid, NewState),
            {reply, ok, CleanState};
        {reply, {error, Category, ErrorAtom}, _StateWithPid} ->
            {reply, {error, Category, ErrorAtom}, State}
    end;
handle_validated_voice_state_update(
    GuildId,
    ChannelId,
    ConnectionId,
    SelfMute,
    SelfDeaf,
    SelfVideo,
    SelfStream,
    ViewerStreamKeys,
    IsMobile,
    Latitude,
    Longitude,
    SessionId,
    UserId,
    Guilds,
    State
) when is_integer(GuildId) ->
    case maps:get(GuildId, Guilds, undefined) of
        undefined ->
            {reply, gateway_errors:error(voice_guild_not_found), State};
        {GuildPid, _Ref} when is_pid(GuildPid) ->
            Request = #{
                user_id => UserId,
                channel_id => ChannelId,
                session_id => SessionId,
                connection_id => ConnectionId,
                self_mute => SelfMute,
                self_deaf => SelfDeaf,
                self_video => SelfVideo,
                self_stream => SelfStream,
                viewer_stream_keys => ViewerStreamKeys,
                is_mobile => IsMobile,
                latitude => Latitude,
                longitude => Longitude
            },
            queue_guild_voice_state_update(
                GuildPid,
                GuildId,
                ChannelId,
                UserId,
                ConnectionId,
                SessionId,
                Request,
                Latitude,
                Longitude,
                State
            );
        _ ->
            {reply, gateway_errors:error(internal_error), State}
    end;
handle_validated_voice_state_update(
    _GuildId,
    _ChannelId,
    _ConnectionId,
    _SelfMute,
    _SelfDeaf,
    _SelfVideo,
    _SelfStream,
    _ViewerStreamKeys,
    _IsMobile,
    _Latitude,
    _Longitude,
    _SessionId,
    _UserId,
    _Guilds,
    State
) ->
    {reply, gateway_errors:error(validation_invalid_params), State}.

-spec queue_guild_voice_state_update(
    pid(),
    guild_id(),
    channel_id() | null,
    user_id(),
    binary() | null,
    binary(),
    map(),
    number() | null,
    number() | null,
    session_state()
) ->
    voice_state_reply().
queue_guild_voice_state_update(
    GuildPid,
    GuildId,
    ChannelId,
    UserId,
    ConnectionId,
    SessionId,
    Request,
    Latitude,
    Longitude,
    State
) ->
    SessionPid = self(),
    spawn(fun() ->
        handle_guild_voice_state_update(
            GuildPid,
            GuildId,
            ChannelId,
            UserId,
            ConnectionId,
            SessionId,
            Request,
            Latitude,
            Longitude,
            SessionPid
        )
    end),
    {reply, ok, State}.

-spec handle_guild_voice_state_update(
    pid(),
    guild_id(),
    channel_id() | null,
    user_id(),
    binary() | null,
    binary(),
    map(),
    number() | null,
    number() | null,
    pid()
) ->
    ok.
handle_guild_voice_state_update(
    GuildPid,
    GuildId,
    ChannelId,
    _UserId,
    _ConnectionId,
    _SessionId,
    Request,
    _Latitude,
    _Longitude,
    SessionPid
) ->
    case guild_client:voice_state_update(GuildPid, GuildId, Request, 12000) of
        {ok, Reply} when is_map(Reply) ->
            maybe_dispatch_voice_server_update_from_reply(Reply, GuildId, ChannelId, SessionPid),
            ok;
        {error, timeout} ->
            ok;
        {error, noproc} ->
            ok;
        {error, _Category, _ErrorAtom} ->
            ok
    end.

-spec maybe_dispatch_voice_server_update_from_reply(map(), guild_id(), channel_id() | null, pid()) ->
    ok.
maybe_dispatch_voice_server_update_from_reply(Reply, GuildId, ChannelId, SessionPid) ->
    case
        {
            maps:get(token, Reply, undefined),
            maps:get(endpoint, Reply, undefined),
            maps:get(connection_id, Reply, undefined),
            ChannelId
        }
    of
        {Token, Endpoint, ConnectionId, ChannelIdValue} when
            is_binary(Token),
            is_binary(Endpoint),
            is_binary(ConnectionId),
            is_integer(ChannelIdValue),
            is_pid(SessionPid)
        ->
            VoiceServerUpdate = #{
                <<"token">> => Token,
                <<"endpoint">> => Endpoint,
                <<"guild_id">> => integer_to_binary(GuildId),
                <<"channel_id">> => integer_to_binary(ChannelIdValue),
                <<"connection_id">> => ConnectionId
            },
            gen_server:cast(SessionPid, {dispatch, voice_server_update, VoiceServerUpdate}),
            ok;
        _ ->
            ok
    end.

-spec handle_voice_disconnect(session_state()) -> voice_state_reply().
handle_voice_disconnect(State) ->
    Guilds = maps:get(guilds, State),
    UserId = maps:get(user_id, State),
    SessionId = maps:get(id, State),
    ConnectionId = maps:get(connection_id, State, null),
    Request = #{
        user_id => UserId,
        channel_id => null,
        session_id => SessionId,
        connection_id => ConnectionId,
        self_mute => false,
        self_deaf => false,
        self_video => false,
        self_stream => false,
        viewer_stream_keys => []
    },
    dispatch_guild_voice_disconnects(Guilds, Request),
    {reply, #{success := true}, NewState} = dm_voice:disconnect_voice_user(UserId, State),
    {reply, ok, NewState}.

-spec dispatch_guild_voice_disconnects(map(), map()) -> ok.
dispatch_guild_voice_disconnects(Guilds, Request) ->
    lists:foreach(
        fun
            ({GuildId, {GuildPid, _Ref}}) when is_pid(GuildPid) ->
                spawn(fun() ->
                    _ = guild_client:voice_state_update(GuildPid, GuildId, Request, 10000),
                    ok
                end),
                ok;
            (_) ->
                ok
        end,
        maps:to_list(Guilds)
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_voice_queue_test() ->
    Result = init_voice_queue(),
    ?assert(maps:is_key(voice_queue, Result)),
    ?assert(maps:is_key(voice_queue_timer, Result)),
    ?assertEqual(undefined, maps:get(voice_queue_timer, Result)),
    ?assert(queue:is_empty(maps:get(voice_queue, Result))),
    ok.

process_voice_queue_empty_test() ->
    State = #{voice_queue => queue:new()},
    Result = process_voice_queue(State),
    ?assertEqual(State, Result),
    ok.

-endif.

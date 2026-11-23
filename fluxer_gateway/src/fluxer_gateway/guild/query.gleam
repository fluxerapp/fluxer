// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id.{type ChannelId, type UserId}
import fluxer_gateway/core/json_tree
import fluxer_gateway/core/member
import fluxer_gateway/core/permissions
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/data
import fluxer_gateway/guild/data/channels
import fluxer_gateway/guild/inbox.{
  type Query, type QueryReply, Forbidden, QueryFailed,
}
import fluxer_gateway/guild/members
import fluxer_gateway/guild/members/roles
import fluxer_gateway/guild/read_model.{type View}
import fluxer_gateway/perms
import fluxer_gateway/perms/query as perms_query
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

pub fn thread_bits(
  view: View,
  user: UserId,
  channel: ChannelId,
) -> Result(Int, inbox.QueryError) {
  case perms.permissions(view.reader, user, perms.ChannelScope(channel)) {
    Ok(granted) -> Ok(permissions.to_int(granted))
    Error(_) -> Error(inbox.QueryFailed("permissions_error"))
  }
}

pub fn answer(view: View, query: Query) -> QueryReply {
  let reply = reply(view, query)
  case read_model.is_live(view) {
    True -> reply
    False -> Error(QueryFailed("guild_not_found"))
  }
}

fn reply(view: View, query: Query) -> QueryReply {
  case query {
    inbox.GetGuildData(user) -> guild_data(view, user)
    inbox.GetAuthContext(user, target) -> auth_context(view, user, target)
    inbox.GetMember(user) ->
      Ok(case members.get(view.members, user) {
        Some(row) ->
          json.object([
            #("success", json.bool(True)),
            #("member_data", raw_json.to_json(member.wire(row))),
          ])
        None -> json.object([#("success", json.bool(False))])
      })
    inbox.HasMember(user) ->
      Ok(
        json.object([
          #("has_member", json.bool(members.has(view.members, user))),
        ]),
      )
    inbox.ListMembers(limit, offset) ->
      Ok(member_page(
        view,
        members.sorted_user_ints(view.members) |> list.drop(offset),
        limit,
      ))
    inbox.ListMembersCursor(limit, after) -> {
      let ids = members.sorted_user_ints(view.members)
      let ids = case after {
        Some(cursor) ->
          list.drop_while(ids, fn(user) { user <= id.user_id_to_int(cursor) })
        None -> ids
      }
      Ok(member_page(view, ids, limit))
    }
    inbox.MembersWithRole(role_id) ->
      Ok(
        json.object([
          #(
            "user_ids",
            json.array(members.with_role(view.members, role_id), fn(user) {
              id.json(user)
            }),
          ),
        ]),
      )
    inbox.CheckTargetMember(user, target) ->
      Ok(
        can_manage(roles.check_target_member(
          view.data,
          view.members,
          user,
          target,
        )),
      )
    inbox.CanManageRole(user, role_id) ->
      Ok(
        can_manage(roles.can_manage_role(view.data, view.members, user, role_id)),
      )
    inbox.CanManageRoles(user, role_id) ->
      case perms.permissions(view.reader, user, perms.GuildScope) {
        Ok(granted) ->
          Ok(
            can_manage(roles.can_manage_roles(
              view.data,
              view.members,
              granted,
              user,
              role_id,
            )),
          )
        Error(_) -> Error(QueryFailed("role_management_check_error"))
      }
    inbox.ViewableChannels(user) ->
      case members.has(view.members, user) {
        False ->
          Ok(json.object([#("channel_ids", json.preprocessed_array([]))]))
        True ->
          case
            perms.reply_json(view.reader, perms_query.ViewableChannels(user))
          {
            Ok(raw) -> Ok(raw_json.to_json(raw))
            Error(_) -> Error(QueryFailed("viewable_channels_error"))
          }
      }
    inbox.ChannelMentions(wanted) -> Ok(channel_mentions(view, wanted))
    inbox.FirstViewableTextChannel ->
      Ok(
        json.object([
          #("channel_id", case channels.first_everyone_viewable(view.data) {
            Some(found) -> id.channel_json(found)
            None -> json.null()
          }),
        ]),
      )
    inbox.CategoryChannelCount(category) ->
      Ok(
        json.object([
          #("count", json.int(channels.category_count(view.data, category))),
        ]),
      )
    inbox.ChannelCount ->
      Ok(json.object([#("count", json.int(list.length(view.data.channels)))]))
    inbox.MentionSources(_) ->
      Error(QueryFailed("resolve_mention_sources_page_error"))
    inbox.Counts ->
      Ok(
        json.object([
          #("member_count", json.int(core_guild.member_count(view.data.facts))),
          #("presence_count", json.int(view.public_online)),
        ]),
      )
    inbox.EvictVisitors(_) -> Error(QueryFailed("guild_visitors_error"))
  }
}

fn can_manage(allowed: Bool) -> Json {
  json.object([#("can_manage", json.bool(allowed))])
}

fn member_page(view: View, ids: List(Int), limit: Int) -> Json {
  let page =
    ids
    |> list.take(int.max(limit, 0))
    |> list.filter_map(fn(user) {
      use user_id <- result.try(id.user_id(user) |> result.replace_error(Nil))
      members.get(view.members, user_id)
      |> option.map(fn(row) { raw_json.to_json(member.wire(row)) })
      |> option.to_result(Nil)
    })
  json.object([
    #("members", json.preprocessed_array(page)),
    #("total", json.int(members.count(view.members))),
  ])
}

fn collection_fields(view: View) -> List(#(String, Json)) {
  [
    #("roles", data.roles_json(view.data)),
    #("member_count", json.int(core_guild.member_count(view.data.facts))),
    #("online_count", json.int(read_model.online_count(view))),
  ]
}

fn guild_data(view: View, user: Option(UserId)) -> QueryReply {
  use rows <- result.map(visible_channels(view, user))
  data.properties_json(view.data, [
    #("channels", data.channels_json(view.data, rows)),
    #("emojis", raw_json.to_json(view.data.emojis)),
    #("stickers", raw_json.to_json(view.data.stickers)),
    ..collection_fields(view)
  ])
}

fn visible_channels(
  view: View,
  user: Option(UserId),
) -> Result(List(channel.Channel), inbox.QueryError) {
  case user {
    None -> Ok(view.data.channels)
    Some(user_id) ->
      case members.has(view.members, user_id) {
        False -> Error(Forbidden)
        True ->
          case perms.viewable(view.reader, user_id) {
            Ok(ids) -> {
              let wanted = set.from_list(ids)
              Ok(
                list.filter(view.data.channels, fn(row) {
                  set.contains(wanted, channel.id(row))
                }),
              )
            }
            Error(_) -> Error(QueryFailed("guild_data_error"))
          }
      }
  }
}

fn auth_context(
  view: View,
  user: Option(UserId),
  target: Option(ChannelId),
) -> QueryReply {
  let allowed = case user {
    None -> True
    Some(user_id) -> members.has(view.members, user_id)
  }
  case allowed {
    False -> Error(Forbidden)
    True -> {
      let parent =
        target
        |> option.then(data.find_channel(view.data, _))
        |> option.map(fn(row) {
          raw_json.to_json(data.channel_wire(view.data, row))
        })
        |> option.unwrap(json.null())
      Ok(
        json.object([
          #("guild", data.properties_json(view.data, collection_fields(view))),
          #("parent_channel", parent),
        ]),
      )
    }
  }
}

fn channel_mentions(view: View, wanted: List(ChannelId)) -> Json {
  let found =
    channels.mentions(view.data, wanted, fn(row) {
      json_tree.from_raw(channel.raw(row))
      |> json_tree.field("name")
      |> result.try(json_tree.string)
      |> option.from_result
    })
  json.object([
    #(
      "channels",
      json.array(found, fn(mention) {
        json.object([
          #("id", id.channel_json(mention.id)),
          #("name", json.string(mention.name)),
          #("type", json.int(mention.kind)),
        ])
      }),
    ),
  ])
}

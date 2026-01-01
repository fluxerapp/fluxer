//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_admin/api/common.{
  type ApiError, Forbidden, NotFound, ServerError, Unauthorized,
}
import fluxer_admin/web
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/option

pub type OAuthClient {
  OAuthClient(
    client_id: String,
    name: String,
    description: String,
    icon_url: String,
    owner_user_id: String,
    team_id: String,
    type_: String,
    redirect_uris: List(String),
    scopes: List(String),
    grant_types: List(String),
    homepage_url: String,
    bot_user_id: String,
    bot_username: String,
    bot_discriminator: String,
    bot_is_public: Bool,
    created_at: String,
    updated_at: String,
  )
}

pub type OAuthTeam {
  OAuthTeam(
    team_id: String,
    name: String,
    owner_user_id: String,
    created_at: String,
  )
}

pub type OAuthTeamMember {
  OAuthTeamMember(
    team_id: String,
    user_id: String,
    role: String,
    added_at: String,
  )
}

pub type OAuthBotToken {
  OAuthBotToken(
    token: String,
    client_id: String,
    scopes: List(String),
    created_at: String,
    revoked: Bool,
  )
}

pub type NewOAuthClient {
  NewOAuthClient(
    name: String,
    description: String,
    icon_url: String,
    type_: String,
    redirect_uris: List(String),
    scopes: List(String),
    grant_types: List(String),
    homepage_url: String,
    team_id: String,
    bot_is_public: Bool,
  )
}

pub type UpdateOAuthClient {
  UpdateOAuthClient(
    name: String,
    description: String,
    icon_url: String,
    redirect_uris: List(String),
    scopes: List(String),
    grant_types: List(String),
    homepage_url: String,
  )
}

pub type NewTeam {
  NewTeam(name: String)
}

pub type NewTeamMember {
  NewTeamMember(user_id: String, role: String)
}

pub type ListOAuthClientsResponse {
  ListOAuthClientsResponse(clients: List(OAuthClient))
}

pub type ListOAuthTeamsResponse {
  ListOAuthTeamsResponse(teams: List(OAuthTeam))
}

pub type ListOAuthTeamMembersResponse {
  ListOAuthTeamMembersResponse(members: List(OAuthTeamMember))
}

pub type ListOAuthBotTokensResponse {
  ListOAuthBotTokensResponse(tokens: List(OAuthBotToken))
}

pub fn get_clients(
  ctx: web.Context,
  session: web.Session,
) -> Result(ListOAuthClientsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/applications"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let client_decoder = {
        use client_id <- decode.field("client_id", decode.string)
        use name <- decode.field("name", decode.string)
        use description <- decode.optional_field(
          "description",
          "",
          decode.string,
        )
        use icon_url <- decode.optional_field("icon_url", "", decode.string)
        use owner_user_id <- decode.optional_field(
          "owner_user_id",
          "",
          decode.string,
        )
        use team_id <- decode.optional_field("team_id", "", decode.string)
        use type_ <- decode.field("type", decode.string)
        use redirect_uris <- decode.field(
          "redirect_uris",
          decode.list(decode.string),
        )
        use scopes <- decode.field("scopes", decode.list(decode.string))
        use grant_types <- decode.optional_field(
          "grant_types",
          [],
          decode.list(decode.string),
        )
        use homepage_url <- decode.optional_field(
          "homepage_url",
          "",
          decode.string,
        )
        use bot_user_id <- decode.optional_field(
          "bot_user_id",
          "",
          decode.string,
        )
        use bot_username <- decode.optional_field(
          "bot_username",
          "",
          decode.string,
        )
        use bot_discriminator <- decode.optional_field(
          "bot_discriminator",
          "",
          decode.string,
        )
        use bot_is_public <- decode.optional_field(
          "bot_is_public",
          False,
          decode.bool,
        )
        use created_at <- decode.optional_field("created_at", "", decode.string)
        use updated_at <- decode.optional_field("updated_at", "", decode.string)
        decode.success(OAuthClient(
          client_id,
          name,
          description,
          icon_url,
          owner_user_id,
          team_id,
          type_,
          redirect_uris,
          scopes,
          grant_types,
          homepage_url,
          bot_user_id,
          bot_username,
          bot_discriminator,
          bot_is_public,
          created_at,
          updated_at,
        ))
      }

      let response_decoder = {
        use clients <- decode.field("clients", decode.list(client_decoder))
        decode.success(ListOAuthClientsResponse(clients))
      }

      case json.parse(resp.body, response_decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    _ -> Error(ServerError)
  }
}

pub fn create_client_simple(
  ctx: web.Context,
  session: web.Session,
  client: NewOAuthClient,
) -> Result(#(String, option.Option(String)), ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/applications"
  let body =
    json.object([
      #("name", json.string(client.name)),
      #("description", json.string(client.description)),
      #("icon_url", json.string(client.icon_url)),
      #("type", json.string(client.type_)),
      #("redirect_uris", json.array(client.redirect_uris, json.string)),
      #("scopes", json.array(client.scopes, json.string)),
      #("grant_types", json.array(client.grant_types, json.string)),
      #("homepage_url", json.string(client.homepage_url)),
      #("team_id", json.string(client.team_id)),
      #("bot_is_public", json.bool(client.bot_is_public)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("accept", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 201 -> {
      let resp_decoder = {
        use client_id <- decode.field("client_id", decode.string)
        use client_secret <- decode.optional_field(
          "client_secret",
          "",
          decode.string,
        )
        let maybe_secret = case client_secret {
          "" -> option.None
          _ -> option.Some(client_secret)
        }
        decode.success(#(client_id, maybe_secret))
      }

      case json.parse(resp.body, resp_decoder) {
        Ok(result) -> Ok(result)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 400 -> Error(Forbidden("Invalid client data"))
    _ -> Error(ServerError)
  }
}

pub fn get_client(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
) -> Result(OAuthClient, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/applications/" <> client_id

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let client_decoder = {
        use client_id <- decode.field("client_id", decode.string)
        use name <- decode.field("name", decode.string)
        use description <- decode.field("description", decode.string)
        use icon_url <- decode.field("icon_url", decode.string)
        use owner_user_id <- decode.field("owner_user_id", decode.string)
        use team_id <- decode.field("team_id", decode.string)
        use type_ <- decode.field("type", decode.string)
        use redirect_uris <- decode.field(
          "redirect_uris",
          decode.list(decode.string),
        )
        use scopes <- decode.field("scopes", decode.list(decode.string))
        use grant_types <- decode.field(
          "grant_types",
          decode.list(decode.string),
        )
        use homepage_url <- decode.field("homepage_url", decode.string)
        use bot_user_id <- decode.optional_field(
          "bot_user_id",
          "",
          decode.string,
        )
        use bot_username <- decode.optional_field(
          "bot_username",
          "",
          decode.string,
        )
        use bot_discriminator <- decode.optional_field(
          "bot_discriminator",
          "",
          decode.string,
        )
        use bot_is_public <- decode.optional_field(
          "bot_is_public",
          False,
          decode.bool,
        )
        use created_at <- decode.field("created_at", decode.string)
        use updated_at <- decode.field("updated_at", decode.string)
        decode.success(OAuthClient(
          client_id,
          name,
          description,
          icon_url,
          owner_user_id,
          team_id,
          type_,
          redirect_uris,
          scopes,
          grant_types,
          homepage_url,
          bot_user_id,
          bot_username,
          bot_discriminator,
          bot_is_public,
          created_at,
          updated_at,
        ))
      }

      case json.parse(resp.body, client_decoder) {
        Ok(client) -> Ok(client)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn update_client(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
  client: UpdateOAuthClient,
) -> Result(Nil, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/applications/" <> client_id
  let body =
    json.object([
      #("name", json.string(client.name)),
      #("description", json.string(client.description)),
      #("icon_url", json.string(client.icon_url)),
      #("redirect_uris", json.array(client.redirect_uris, json.string)),
      #("scopes", json.array(client.scopes, json.string)),
      #("grant_types", json.array(client.grant_types, json.string)),
      #("homepage_url", json.string(client.homepage_url)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Patch)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("accept", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(resp) if resp.status == 400 -> Error(Forbidden("Invalid client data"))
    _ -> Error(ServerError)
  }
}

pub fn delete_client(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
) -> Result(Nil, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/applications/" <> client_id

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Delete)
    |> request.set_header("authorization", "Bearer " <> session.access_token)

  case httpc.send(req) {
    Ok(resp) if resp.status == 204 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn rotate_client_secret(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
) -> Result(String, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/rotate-secret"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let secret_decoder = {
        use client_secret <- decode.field("client_secret", decode.string)
        decode.success(client_secret)
      }

      case json.parse(resp.body, secret_decoder) {
        Ok(secret) -> Ok(secret)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn get_bot_tokens(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
) -> Result(ListOAuthBotTokensResponse, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/bot-tokens"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let token_decoder = fn(client_id_value: String) {
        {
          use token <- decode.field("token", decode.string)
          use revoked <- decode.field("revoked", decode.bool)
          use created_at <- decode.field("created_at", decode.string)
          decode.success(OAuthBotToken(
            token,
            client_id_value,
            [],
            created_at,
            revoked,
          ))
        }
      }

      let response_decoder = fn(client_id_value: String) {
        {
          use tokens <- decode.field(
            "tokens",
            decode.list(token_decoder(client_id_value)),
          )
          decode.success(ListOAuthBotTokensResponse(tokens))
        }
      }

      case json.parse(resp.body, response_decoder(client_id)) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn create_bot_token(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
  scopes: List(String),
) -> Result(String, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/bot-tokens"
  let body =
    json.object([
      #("scopes", json.array(scopes, json.string)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("accept", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 || resp.status == 201 -> {
      let token_decoder = {
        use token <- decode.field("token", decode.string)
        decode.success(token)
      }

      case json.parse(resp.body, token_decoder) {
        Ok(token) -> Ok(token)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(resp) if resp.status == 400 -> Error(Forbidden("Invalid scopes"))
    _ -> Error(ServerError)
  }
}

pub fn revoke_bot_token(
  ctx: web.Context,
  session: web.Session,
  token: String,
) -> Result(Nil, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/bot-tokens/revoke"
  let body =
    json.object([
      #("token", json.string(token)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub type ApiKey {
  ApiKey(token_prefix: String, created_at: String)
}

pub type ListApiKeysResponse {
  ListApiKeysResponse(keys: List(ApiKey))
}

pub type CreatedApiKey {
  CreatedApiKey(token: String, token_prefix: String, created_at: String)
}

pub fn update_bot_privacy(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
  is_public: Bool,
) -> Result(Nil, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/bot-privacy"
  let body =
    json.object([#("is_public", json.bool(is_public))])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Patch)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn update_bot_username(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
  username: String,
) -> Result(#(String, String), ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/bot-username"
  let body =
    json.object([#("username", json.string(username))])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Patch)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let resp_decoder = {
        use username <- decode.field("bot_username", decode.string)
        use discriminator <- decode.field("bot_discriminator", decode.int)
        decode.success(#(username, int.to_string(discriminator)))
      }
      case json.parse(resp.body, resp_decoder) {
        Ok(result) -> Ok(result)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 429 -> Error(Forbidden("Rate limited"))
    _ -> Error(ServerError)
  }
}

pub fn list_api_keys(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
) -> Result(ListApiKeysResponse, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/api-keys"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let key_decoder = {
        use token_prefix <- decode.field("token_prefix", decode.string)
        use created_at <- decode.field("created_at", decode.string)
        decode.success(ApiKey(token_prefix, created_at))
      }

      let response_decoder = {
        use keys <- decode.field("keys", decode.list(key_decoder))
        decode.success(ListApiKeysResponse(keys))
      }

      case json.parse(resp.body, response_decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn create_api_key(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
) -> Result(CreatedApiKey, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/applications/" <> client_id <> "/api-keys"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 || resp.status == 201 -> {
      let resp_decoder = {
        use token <- decode.field("token", decode.string)
        use token_prefix <- decode.field("token_prefix", decode.string)
        use created_at <- decode.field("created_at", decode.string)
        decode.success(CreatedApiKey(token, token_prefix, created_at))
      }

      case json.parse(resp.body, resp_decoder) {
        Ok(result) -> Ok(result)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions or maximum keys reached"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(resp) if resp.status == 400 -> Error(Forbidden("Invalid request"))
    _ -> Error(ServerError)
  }
}

pub fn revoke_api_key(
  ctx: web.Context,
  session: web.Session,
  client_id: String,
  token_prefix: String,
) -> Result(Nil, ApiError) {
  let url =
    ctx.api_endpoint
    <> "/oauth2/applications/"
    <> client_id
    <> "/api-keys/"
    <> token_prefix

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Delete)
    |> request.set_header("authorization", "Bearer " <> session.access_token)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 || resp.status == 204 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn get_teams(
  ctx: web.Context,
  session: web.Session,
) -> Result(ListOAuthTeamsResponse, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/teams"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let team_decoder = {
        use team_id <- decode.field("team_id", decode.string)
        use name <- decode.field("name", decode.string)
        let owner_user_id = ""
        let created_at = ""
        decode.success(OAuthTeam(team_id, name, owner_user_id, created_at))
      }

      let response_decoder = {
        use teams <- decode.field("teams", decode.list(team_decoder))
        decode.success(ListOAuthTeamsResponse(teams))
      }

      case json.parse(resp.body, response_decoder) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    _ -> Error(ServerError)
  }
}

pub fn create_team(
  ctx: web.Context,
  session: web.Session,
  team: NewTeam,
) -> Result(String, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/teams"
  let body =
    json.object([
      #("name", json.string(team.name)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_header("accept", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 201 -> {
      let resp_decoder = {
        use team_id <- decode.field("team_id", decode.string)
        decode.success(team_id)
      }

      case json.parse(resp.body, resp_decoder) {
        Ok(team_id) -> Ok(team_id)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 400 -> Error(Forbidden("Invalid team data"))
    _ -> Error(ServerError)
  }
}

pub fn delete_team(
  ctx: web.Context,
  session: web.Session,
  team_id: String,
) -> Result(Nil, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/teams/" <> team_id

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Delete)
    |> request.set_header("authorization", "Bearer " <> session.access_token)

  case httpc.send(req) {
    Ok(resp) if resp.status == 204 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn get_team_members(
  ctx: web.Context,
  session: web.Session,
  team_id: String,
) -> Result(ListOAuthTeamMembersResponse, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/teams/" <> team_id <> "/members"

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Get)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("accept", "application/json")

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let member_decoder = fn(team_id_value: String) {
        {
          use user_id <- decode.field("user_id", decode.string)
          use role <- decode.field("role", decode.string)
          let added_at = ""
          decode.success(OAuthTeamMember(team_id_value, user_id, role, added_at))
        }
      }

      let response_decoder = fn(team_id_value: String) {
        {
          use members <- decode.field(
            "members",
            decode.list(member_decoder(team_id_value)),
          )
          decode.success(ListOAuthTeamMembersResponse(members))
        }
      }

      case json.parse(resp.body, response_decoder(team_id)) {
        Ok(response) -> Ok(response)
        Error(_) -> Error(ServerError)
      }
    }
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

pub fn add_team_member(
  ctx: web.Context,
  session: web.Session,
  team_id: String,
  member: NewTeamMember,
) -> Result(Nil, ApiError) {
  let url = ctx.api_endpoint <> "/oauth2/teams/" <> team_id <> "/members"
  let body =
    json.object([
      #("user_id", json.string(member.user_id)),
      #("role", json.string(member.role)),
    ])
    |> json.to_string

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("authorization", "Bearer " <> session.access_token)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(resp) if resp.status == 201 || resp.status == 204 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    Ok(resp) if resp.status == 400 -> Error(Forbidden("Invalid member data"))
    _ -> Error(ServerError)
  }
}

pub fn remove_team_member(
  ctx: web.Context,
  session: web.Session,
  team_id: String,
  user_id: String,
) -> Result(Nil, ApiError) {
  let url =
    ctx.api_endpoint <> "/oauth2/teams/" <> team_id <> "/members/" <> user_id

  let assert Ok(req) = request.to(url)
  let req =
    req
    |> request.set_method(http.Delete)
    |> request.set_header("authorization", "Bearer " <> session.access_token)

  case httpc.send(req) {
    Ok(resp) if resp.status == 204 -> Ok(Nil)
    Ok(resp) if resp.status == 401 -> Error(Unauthorized)
    Ok(resp) if resp.status == 403 ->
      Error(Forbidden("Insufficient permissions"))
    Ok(resp) if resp.status == 404 -> Error(NotFound)
    _ -> Error(ServerError)
  }
}

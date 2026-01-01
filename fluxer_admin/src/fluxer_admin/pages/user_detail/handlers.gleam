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

import fluxer_admin/api/messages
import fluxer_admin/api/users
import fluxer_admin/components/flash
import fluxer_admin/constants
import fluxer_admin/web.{type Context, type Session}
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import wisp.{type Request, type Response}

pub fn handle_update_flags(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let user_result = users.lookup_user(ctx, session, user_id)

  case user_result {
    Error(_) -> flash.redirect_with_error(ctx, redirect_url, "User not found")
    Ok(option.None) ->
      flash.redirect_with_error(ctx, redirect_url, "User not found")
    Ok(option.Some(current_user)) -> {
      let current_flags = case int.parse(current_user.flags) {
        Ok(flags) -> flags
        Error(_) -> 0
      }

      let submitted_flag_values =
        list.filter_map(form_data.values, fn(field) {
          case field.0 {
            "flags[]" -> int.parse(field.1)
            _ -> Error(Nil)
          }
        })

      let all_user_flags = constants.get_patchable_flags()

      let add_flags =
        list.filter(submitted_flag_values, fn(flag) {
          int.bitwise_and(current_flags, flag) == 0
        })
        |> list.map(int.to_string)

      let remove_flags =
        list.filter_map(all_user_flags, fn(flag_obj) {
          let has_flag = int.bitwise_and(current_flags, flag_obj.value) != 0
          let is_submitted =
            list.contains(submitted_flag_values, flag_obj.value)
          case has_flag && !is_submitted {
            True -> Ok(int.to_string(flag_obj.value))
            False -> Error(Nil)
          }
        })

      let result =
        users.update_user_flags(ctx, session, user_id, add_flags, remove_flags)

      case result {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            "User flags updated successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            "Failed to update user flags",
          )
      }
    }
  }
}

pub fn handle_update_suspicious_flags(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let flag_values =
    list.filter_map(form_data.values, fn(field) {
      case field.0 {
        "suspicious_flags[]" -> int.parse(field.1)
        _ -> Error(Nil)
      }
    })

  let total_flags =
    list.fold(flag_values, 0, fn(acc, flag) { int.bitwise_or(acc, flag) })

  let result =
    users.update_suspicious_activity_flags(ctx, session, user_id, total_flags)

  case result {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Suspicious activity flags updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update suspicious activity flags",
      )
  }
}

pub fn handle_update_acls(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let acls =
    list.filter_map(form_data.values, fn(field) {
      case field.0 {
        "acls[]" -> Ok(field.1)
        _ -> Error(Nil)
      }
    })

  let result = users.set_user_acls(ctx, session, user_id, acls)

  case result {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "ACLs updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to update ACLs")
  }
}

pub fn handle_disable_mfa(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.disable_mfa(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "MFA disabled successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to disable MFA")
  }
}

pub fn handle_verify_email(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.verify_email(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Email verified successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to verify email")
  }
}

pub fn handle_unlink_phone(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.unlink_phone(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Phone unlinked successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to unlink phone")
  }
}

pub fn handle_terminate_sessions(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.terminate_sessions(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Sessions terminated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to terminate sessions",
      )
  }
}

pub fn handle_clear_fields(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let fields =
    list.filter_map(form_data.values, fn(field) {
      case field.0 {
        "fields[]" -> Ok(field.1)
        _ -> Error(Nil)
      }
    })

  case list.is_empty(fields) {
    True ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "No fields selected to clear",
      )
    False -> {
      case users.clear_user_fields(ctx, session, user_id, fields) {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            "User fields cleared successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            "Failed to clear user fields",
          )
      }
    }
  }
}

pub fn handle_set_bot_status(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  let query = wisp.get_query(req)
  let status = list.key_find(query, "status") |> result.unwrap("false")
  let bot = case status {
    "true" -> True
    _ -> False
  }

  case users.set_bot_status(ctx, session, user_id, bot) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Bot status updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update bot status",
      )
  }
}

pub fn handle_set_system_status(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  let query = wisp.get_query(req)
  let status = list.key_find(query, "status") |> result.unwrap("false")
  let system = case status {
    "true" -> True
    _ -> False
  }

  case users.set_system_status(ctx, session, user_id, system) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "System status updated successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to update system status",
      )
  }
}

pub fn handle_change_username(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let username =
    list.key_find(form_data.values, "username") |> result.unwrap("")
  let discriminator =
    list.key_find(form_data.values, "discriminator")
    |> result.try(int.parse)
    |> option.from_result

  case username {
    "" ->
      flash.redirect_with_error(ctx, redirect_url, "Username cannot be empty")
    _ -> {
      case
        users.change_username(ctx, session, user_id, username, discriminator)
      {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            "Username changed successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            "Failed to change username",
          )
      }
    }
  }
}

pub fn handle_change_email(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let email = list.key_find(form_data.values, "email") |> result.unwrap("")

  case email {
    "" -> flash.redirect_with_error(ctx, redirect_url, "Email cannot be empty")
    _ -> {
      case users.change_email(ctx, session, user_id, email) {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            "Email changed successfully",
          )
        Error(_) ->
          flash.redirect_with_error(ctx, redirect_url, "Failed to change email")
      }
    }
  }
}

pub fn handle_temp_ban(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let duration =
    list.key_find(form_data.values, "duration")
    |> result.try(int.parse)
    |> result.unwrap(24)

  let reason =
    list.key_find(form_data.values, "reason")
    |> option.from_result

  let private_reason =
    list.key_find(form_data.values, "private_reason")
    |> option.from_result

  case
    users.temp_ban_user(ctx, session, user_id, duration, reason, private_reason)
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "User temporarily banned successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to temporarily ban user",
      )
  }
}

pub fn handle_unban(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.unban_user(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "User unbanned successfully",
      )
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Failed to unban user")
  }
}

pub fn handle_schedule_deletion(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let reason_code =
    list.key_find(form_data.values, "reason_code")
    |> result.try(int.parse)
    |> result.unwrap(0)

  let days =
    list.key_find(form_data.values, "days")
    |> result.try(int.parse)
    |> result.unwrap(30)

  let public_reason =
    list.key_find(form_data.values, "public_reason")
    |> option.from_result

  let private_reason =
    list.key_find(form_data.values, "private_reason")
    |> option.from_result

  case
    users.schedule_deletion(
      ctx,
      session,
      user_id,
      reason_code,
      public_reason,
      days,
      private_reason,
    )
  {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Account deletion scheduled successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to schedule account deletion",
      )
  }
}

pub fn handle_cancel_deletion(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.cancel_deletion(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Account deletion cancelled successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to cancel account deletion",
      )
  }
}

pub fn handle_cancel_bulk_message_deletion(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.cancel_bulk_message_deletion(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Bulk message deletion cancelled successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to cancel bulk message deletion",
      )
  }
}

pub fn handle_delete_all_messages(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let dry_run = case list.key_find(form_data.values, "dry_run") {
    Ok(value) ->
      case string.lowercase(value) {
        "false" -> False
        "0" -> False
        _ -> True
      }
    Error(_) -> True
  }

  case messages.delete_all_user_messages(ctx, session, user_id, dry_run) {
    Ok(response) -> {
      case dry_run {
        True -> {
          let location =
            redirect_url
            |> append_query_param("delete_all_messages_dry_run", "true")
            |> append_query_param(
              "delete_all_messages_channel_count",
              int.to_string(response.channel_count),
            )
            |> append_query_param(
              "delete_all_messages_message_count",
              int.to_string(response.message_count),
            )

          flash.redirect_with_success(
            ctx,
            location,
            "Dry run found "
              <> int.to_string(response.message_count)
              <> " messages across "
              <> int.to_string(response.channel_count)
              <> " channels. Confirm to delete them permanently.",
          )
        }
        False -> {
          let location = case response.job_id {
            option.Some(job_id) ->
              append_query_param(redirect_url, "message_shred_job_id", job_id)
            option.None -> redirect_url
          }

          let message = case response.job_id {
            option.Some(_) ->
              "Delete job queued. Monitor progress in the status panel."
            option.None -> "No messages found for deletion."
          }

          flash.redirect_with_success(ctx, location, message)
        }
      }
    }
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to delete all user messages",
      )
  }
}

pub fn handle_change_dob(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let date_of_birth = list.key_find(form_data.values, "date_of_birth")

  case date_of_birth {
    Ok(dob) -> {
      case users.change_dob(ctx, session, user_id, dob) {
        Ok(_) ->
          flash.redirect_with_success(
            ctx,
            redirect_url,
            "Date of birth changed successfully",
          )
        Error(_) ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            "Failed to change date of birth",
          )
      }
    }
    Error(_) ->
      flash.redirect_with_error(ctx, redirect_url, "Invalid date of birth")
  }
}

pub fn handle_send_password_reset(
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  case users.send_password_reset(ctx, session, user_id) {
    Ok(_) ->
      flash.redirect_with_success(
        ctx,
        redirect_url,
        "Password reset email sent successfully",
      )
    Error(_) ->
      flash.redirect_with_error(
        ctx,
        redirect_url,
        "Failed to send password reset email",
      )
  }
}

pub fn handle_message_shred(
  req: Request,
  ctx: Context,
  session: Session,
  user_id: String,
  redirect_url: String,
) -> Response {
  use form_data <- wisp.require_form(req)

  let csv_data =
    list.key_find(form_data.values, "csv_data")
    |> option.from_result
    |> option.unwrap("")

  let parsed_entries = parse_csv_entries(csv_data)

  case parsed_entries {
    Error(message) -> flash.redirect_with_error(ctx, redirect_url, message)
    Ok(entries) -> {
      case list.is_empty(entries) {
        True ->
          flash.redirect_with_error(
            ctx,
            redirect_url,
            "CSV did not contain any valid channel_id,message_id pairs.",
          )
        False -> {
          let entries_json =
            json.array(entries, fn(entry) {
              json.object([
                #("channel_id", json.string(entry.0)),
                #("message_id", json.string(entry.1)),
              ])
            })

          case
            messages.queue_message_shred(ctx, session, user_id, entries_json)
          {
            Ok(response) -> {
              let location =
                append_query_param(
                  redirect_url,
                  "message_shred_job_id",
                  response.job_id,
                )

              flash.redirect_with_success(
                ctx,
                location,
                "Message shred job queued",
              )
            }
            Error(_) ->
              flash.redirect_with_error(
                ctx,
                redirect_url,
                "Failed to queue message shred job",
              )
          }
        }
      }
    }
  }
}

fn parse_csv_entries(
  csv_data: String,
) -> Result(List(#(String, String)), String) {
  let normalized = string.trim(csv_data)
  let lines = string.split(normalized, "\n")

  list.fold(lines, Ok([]), fn(acc_result, line) {
    case acc_result {
      Error(message) -> Error(message)
      Ok(acc) -> {
        let trimmed = string.trim(line)

        case string.is_empty(trimmed) {
          True -> Ok(acc)
          False -> {
            let normalized_lower = trimmed |> string.lowercase

            case normalized_lower == "channel_id,message_id" {
              True -> Ok(acc)
              False -> {
                case string.split(trimmed, ",") {
                  [channel_raw, message_raw] -> {
                    let channel_trim = string.trim(channel_raw)
                    let message_trim = string.trim(message_raw)

                    case int.parse(channel_trim) {
                      Ok(channel_value) ->
                        case int.parse(message_trim) {
                          Ok(message_value) ->
                            Ok(
                              list.append(acc, [
                                #(
                                  int.to_string(channel_value),
                                  int.to_string(message_value),
                                ),
                              ]),
                            )
                          Error(_) ->
                            Error("Invalid message_id on row: " <> trimmed)
                        }
                      Error(_) ->
                        Error("Invalid channel_id on row: " <> trimmed)
                    }
                  }
                  _ ->
                    Error(
                      "Each row must contain channel_id and message_id separated by a comma",
                    )
                }
              }
            }
          }
        }
      }
    }
  })
}

fn append_query_param(url: String, key: String, value: String) -> String {
  let separator = case string.contains(url, "?") {
    True -> "&"
    False -> "?"
  }
  url <> separator <> key <> "=" <> value
}

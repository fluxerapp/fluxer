// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/call/manager
import fluxer_gateway/voice/call/server as call_server
import fluxer_gateway/voice/client_update
import fluxer_gateway/voice/dm/server as dm
import gleam/result

pub fn dm_voice_update(
  session: SessionRef,
  data: client_update.Update,
  calls calls: List(ChannelId),
) -> Nil {
  let _ = {
    use update <- result.try(dm_update(data, calls))
    use subject <- result.map(dm.ensure(session))
    dm.update(subject, update)
  }
  Nil
}

pub fn session_closed(session: SessionRef) -> Nil {
  case dm.lookup(session) {
    Ok(subject) -> dm.close(subject, session)
    Error(Nil) -> Nil
  }
}

pub fn session_handed_off(session: SessionRef) -> Nil {
  case dm.lookup(session) {
    Ok(subject) -> dm.detach(subject, session)
    Error(Nil) -> Nil
  }
}

pub fn session_rehydrated(
  session: SessionRef,
  channels: List(ChannelId),
) -> Nil {
  dm.adopt(session, channels)
}

pub fn voice_signal(ref: SessionRef, signal: client_update.Signal) -> Nil {
  case manager.lookup(signal.channel) {
    Ok(call) ->
      call_server.signal(
        call,
        session.session_id_to_string(ref.id),
        signal.to,
        signal.data,
      )
    Error(_) -> Nil
  }
}

fn dm_update(
  update: client_update.Update,
  calls: List(ChannelId),
) -> Result(dm.DmUpdate, Nil) {
  use channel <- result.try(update.channel)
  Ok(dm.DmUpdate(
    channel:,
    connection_id: update.connection_id,
    flags: update.flags,
    viewer_keys: update.viewer_keys,
    geo: update.geo,
    e2ee_capable: update.e2ee_capable,
    bot: update.bot,
    p2p: update.p2p,
    country_code: update.country_code,
    calls:,
  ))
}

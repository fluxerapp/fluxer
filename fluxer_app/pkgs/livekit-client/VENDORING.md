# Vendored: livekit-client

- **Upstream:** https://github.com/livekit/client-sdk-js
- **Version:** v2.17.2 (git tag `v2.17.2`)
- **License:** Apache-2.0
- **Date vendored:** 2026-05-25

## Fluxer modifications

Changes applied on top of the upstream v2.17.2 source. Previously maintained as
a pnpm patch at `patches/livekit-client@2.17.2.patch`; now maintained as regular
source edits in this package.

1. **AV1 E2EE support** (`src/e2ee/worker/av1Crypto.ts`, `FrameCryptor.ts`, `e2ee.worker.ts`)
   OBU-level AV1 encryption and decryption for end-to-end encrypted voice/video.

2. **UpdateTrackContext message** (`src/e2ee/types.ts`, worker dispatch)
   Replaced `updateCodec` with richer `updateTrackContext` carrying participant
   identity and track ID, preventing codec mismatch on track reuse.

3. **E2EEManager state tracking** (`src/e2ee/E2eeManager.ts`)
   Added `getE2EETransformState()` / `setE2EETransformState()` for proper
   transform lifecycle management.

4. **Screenshare scalability mode** (`src/room/participant/LocalParticipant.ts`)
   Preserve caller-supplied `scalabilityMode` for screen shares instead of forcing
   `L3T3_KEY`, so VP9/AV1 screen shares can use the browser's compatible default
   unless Fluxer explicitly requests an SVC layer layout.

5. **E2EE frame layout guards** (`src/e2ee/worker/FrameCryptor.ts`)
   Validate encrypted frame trailer, IV, tag, and clear-prefix bounds before
   constructing typed-array views, and drop malformed encrypted frames without
   tearing down the transform stream.

6. **Encrypted backup codec publishing** (`src/room/participant/LocalParticipant.ts`, `src/e2ee/E2eeManager.ts`)
   Allows backup codec tracks to be advertised and published while E2EE is
   enabled, and attaches sender transforms to backup codec senders using their
   cloned media track ID and codec.

7. **Publisher codec preferences** (`src/room/RTCEngine.ts`)
   Applies `RTCRtpTransceiver.setCodecPreferences()` to publisher transceivers
   so browser SDP follows the selected primary or backup codec, and prefers
   H.264 profiles that use Chromium's external/hardware encoder before the
   OpenH264 software profile.

8. **Media publishing defaults** (`src/room/defaults.ts`, `src/room/utils.ts`, `src/room/track/options.ts`)
   Falls back to H.264, then VP9, VP8, AV1, and HEVC/H.265 according to actual
   sender capabilities, pairs advanced codecs with H.264 backup simulcast, and
   uses maintain-resolution screen-share defaults with a 4K60-ready bitrate cap.
   The order puts AV1 and HEVC last because both are opt-in in Fluxer, so a
   fallback inside `publishTrack` must not land on a codec the user did not
   enable. Fluxer picks the codec itself before publishing, so this list only
   applies when the client overrides the request, such as the reconnect
   republish that runs outside Fluxer's own flows.

9. **High-fidelity Opus SDP munging** (`src/room/PCTransport.ts`)
   Forces Opus RED/FEC, stereo signaling, 10 ms packet time, no DTX, and a
   510 kbps maximum average bitrate in local offers and remote answers.

10. **Remote audio volume restore at exactly zero** (`src/room/track/RemoteAudioTrack.ts`)
    `attach()`, `connectWebAudio()` and `getVolume()` guarded the remembered
    `elementVolume` with a truthiness check, so a track deliberately held at `0`
    came back at full volume whenever it was re-attached or its Web Audio graph
    was rebuilt. All three guards now test `!== undefined`. Note that remote
    gains above `1.0` are only legal because `setVolume()` takes the Web Audio
    `gainNode` branch; the `el.volume` branch would throw `IndexSizeError`.
    `webAudioMix` must stay unconditional.

11. **Processor teardown before source stop** (`src/room/track/LocalTrack.ts`)
    `stop()` called `super.stop()` first, killing the source `MediaStreamTrack`
    and closing the readable feeding a track processor before `processor.destroy()`
    ran. A camera-effect worker therefore saw input EOF before its owner's stop
    command and reported an operational failure during an ordinary camera-off.
    The processor is now captured, detached, and its teardown initiated before
    `super.stop()`.

12. **Transactional source and processor swaps** (`src/room/track/LocalTrack.ts`,
    `LocalVideoTrack.ts`, `LocalAudioTrack.ts`)
    `setMediaStreamTrack()` applied the new source, restarted the processor and
    re-armed the sender with no unwind path, so a failure anywhere in the middle
    left a half-applied track: listeners moved, elements detached, sender pointing
    at a dead track. It now takes `SetMediaStreamTrackOptions`
    (`force`, `deferEndedListener`, `preservePreviousTrack`) and, on failure,
    restores the previous source, constraints, `enabled` state, listeners,
    processor and sender, throwing `TrackInvalidError` when the previous source is
    no longer `live` because an ended track cannot be restored. Both errors are
    surfaced together as an `AggregateError` when the unwind itself fails.
    `stageTrackReplacement()` / `commitStagedTrackReplacement()` expose a two-phase
    swap: the candidate becomes the active source with its `ended` listener
    deferred and the previous source preserved, and only the commit adopts the
    `ended` listener and clears the staged identity, so a caller can validate its
    publication before the swap is observable. `replaceTrack()` and `restart()`
    guard the `providedByUser` flip behind a `replacementCommitted` flag.
    `restart()` still detaches and stops the previous source before calling
    `getUserMedia()`, as upstream does, because Safari ends a freshly acquired
    track with a capture failure while the old track for the same device is
    live. `setSimulcastTrackSender()` routes an already-installed processor's
    `processedTrack` to a newly registered secondary sender so a backup codec
    never publishes raw frames while the primary is processed.
    Processor install and teardown in all three classes roll the processed/raw
    sender track back, including `LocalVideoTrack`'s secondary simulcast senders,
    and aggregate every cleanup failure instead of discarding it.

13. **Start bitrate for every video codec** (`src/room/PCTransport.ts`,
    `src/room/participant/LocalParticipant.ts`, `src/room/participant/publishUtils.ts`)
    `x-google-start-bitrate` was reachable only by AV1 and VP9, gated twice: the
    publish path registered a track bitrate only for SVC codecs, and the offer
    munging returned early for everything else. H264, H265 and VP8 therefore
    opened at the Chromium default and had to ramp, which showed up as a 3000 kbps
    screen share encoding at 346 kbps twenty seconds in. The bitrate is now
    registered for every video codec from the highest encoding
    (`maxEncodingBitrate()`, so a simulcast ladder contributes its top layer), and
    the offer munging applies the start bitrate whenever a max bitrate is known.
    The dependency descriptor extension stays SVC-only.
    `appendStartBitrateToFmtp()` holds the fmtp edit so it can be tested, and
    `setTrackCodecBitrate()` now replaces an entry for the same cid or transceiver
    instead of appending, since `trackBitrates` is never cleared.

## Updating from upstream

1. Check the upstream changelog for the target version.
2. `git diff v2.17.2..v<new> -- src/` to see what changed.
3. Apply relevant upstream changes to this package's `src/`.
4. Update the version field in `package.json` to match the new upstream version.
5. Update this file with the new version and date.

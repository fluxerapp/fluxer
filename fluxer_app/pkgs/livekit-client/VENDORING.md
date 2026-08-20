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
   Prefers AV1, then HEVC/H.265, H.264, VP9, and VP8 according to actual
   sender capabilities; pairs advanced codecs with H.264 backup simulcast; and
   uses maintain-resolution screen-share defaults with a 4K60-ready bitrate cap.

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

## Updating from upstream

1. Check the upstream changelog for the target version.
2. `git diff v2.17.2..v<new> -- src/` to see what changed.
3. Apply relevant upstream changes to this package's `src/`.
4. Update the version field in `package.json` to match the new upstream version.
5. Update this file with the new version and date.

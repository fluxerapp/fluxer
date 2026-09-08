# Vendored: livekit-client

- **Upstream:** https://github.com/livekit/client-sdk-js
- **Version:** v2.17.2 (git tag `v2.17.2`)
- **License:** Apache-2.0
- **Date vendored:** 2026-05-25

## Fluxer modifications

Changes applied on top of the upstream v2.17.2 source. These were previously a pnpm patch at `patches/livekit-client@2.17.2.patch` and are now plain source edits in this package.

1. **AV1 E2EE support** (`src/e2ee/worker/av1Crypto.ts`, `FrameCryptor.ts`, `e2ee.worker.ts`)

   OBU-level AV1 encryption and decryption for end-to-end encrypted voice and video.

2. **UpdateTrackContext message** (`src/e2ee/types.ts`, worker dispatch)

   `updateCodec` replaced with `updateTrackContext`, which carries participant identity and track id so a reused track cannot pick up the wrong codec.

3. **E2EEManager state tracking** (`src/e2ee/E2eeManager.ts`)

   `getE2EETransformState()` and `setE2EETransformState()` for transform lifecycle management.

4. **Screen share scalability mode** (`src/room/participant/LocalParticipant.ts`)

   A caller-supplied `scalabilityMode` is preserved for screen shares instead of being forced to `L3T3_KEY`, so VP9 and AV1 screen shares use the browser default unless Fluxer asks for a specific SVC layout.

5. **E2EE frame layout guards** (`src/e2ee/worker/FrameCryptor.ts`)

   Encrypted frame trailer, IV, tag and clear-prefix bounds are validated before any typed-array view is constructed, and malformed frames are dropped without tearing down the transform stream.

6. **Encrypted backup codec publishing** (`src/room/participant/LocalParticipant.ts`, `src/e2ee/E2eeManager.ts`)

   Backup codec tracks can be advertised and published while E2EE is on, with sender transforms attached using the cloned media track id and codec.

7. **Publisher codec preferences** (`src/room/RTCEngine.ts`)

   `setCodecPreferences()` is applied to publisher transceivers so the browser's SDP follows the selected primary or backup codec. H.264 profiles rank Baseline `42001f` first, then Constrained Baseline `42e01f`, then everything else. Main, High and Constrained High rank last on purpose.

   livekit-server registers H.264 High `640032` on the publisher peer connection but filters it off the subscriber peer connection, and its `CodecParametersFuzzySearch` falls back to a mime-only match. A High publication therefore reaches subscribers under their `42e01f` payload type and decodes to nothing on a Constrained-Baseline-only decoder such as Firefox's OpenH264 GMP. `42001f` is the one profile Chromium's accelerated encoder factory advertises that such a decoder can still handle, because Chromium's VAAPI encoder and OpenH264 both write a Constrained Baseline SPS for `H264PROFILE_BASELINE`.

   The trade is that livekit-server does not register `42001f` either, so Windows and macOS negotiate `42e01f`, which Chromium's accelerated encoder factory does not advertise there (`kPlatformH264CbpEncoding` is off by default on Windows, and `IsH264ConstrainedBaselineProfileAvailableForAcceleratedEncoder` returns false on Apple). Those publishers fall back to software H.264, which is what an unpatched browser does anyway. Linux, ChromeOS and Android keep hardware encoding.

8. **Media publishing defaults** (`src/room/defaults.ts`, `src/room/utils.ts`, `src/room/track/options.ts`)

   Codec fallback follows actual sender capabilities in the order H.264, VP9, VP8, AV1, HEVC. Advanced codecs are paired with an H.264 backup simulcast, and screen shares default to maintain-resolution with a 4K60-ready bitrate cap. AV1 and HEVC come last because both are opt-in in Fluxer, so a fallback inside `publishTrack` must not land on a codec the user did not enable. Fluxer picks the codec itself before publishing, so this order only applies when the client overrides the request, such as the reconnect republish that runs outside Fluxer's own flows.

9. **High-fidelity Opus SDP munging** (`src/room/PCTransport.ts`)

   Local offers and remote answers are munged to force Opus RED and FEC, 10 ms packet time, no DTX, and a 510 kbps maximum average bitrate. Stereo signalling stays opt-in. `stereo=1` and `sprop-stereo=1` are added only for publications whose `TrackBitrateInfo.stereo` is set (studio mode above the stereo bitrate threshold, and screen-share audio) and for the subscriber mids the server advertised as stereo, so a mono microphone is not encoded and decoded as a two-channel stream.

10. **Remote audio volume restore at exactly zero** (`src/room/track/RemoteAudioTrack.ts`)

    `attach()`, `connectWebAudio()` and `getVolume()` guarded the remembered `elementVolume` with a truthiness check, so a track deliberately held at `0` came back at full volume whenever it was re-attached or its Web Audio graph was rebuilt. All three now test `!== undefined`. Remote gains above `1.0` are only legal because `setVolume()` takes the Web Audio `gainNode` branch, as the `el.volume` branch would throw `IndexSizeError`, so `webAudioMix` must stay unconditional.

11. **Processor teardown before source stop** (`src/room/track/LocalTrack.ts`)

    `stop()` called `super.stop()` first, killing the source `MediaStreamTrack` and closing the readable that feeds a track processor before `processor.destroy()` ran. A camera-effect worker therefore saw input EOF before its owner's stop command and reported an operational failure during an ordinary camera-off. The processor is now captured, detached and torn down before `super.stop()`.

12. **Transactional source and processor swaps** (`src/room/track/LocalTrack.ts`, `LocalVideoTrack.ts`, `LocalAudioTrack.ts`)

    `setMediaStreamTrack()` applied the new source, restarted the processor and re-armed the sender with no unwind path, so a failure part-way through left a half-applied track with listeners moved, elements detached and the sender pointing at a dead track. It now takes `SetMediaStreamTrackOptions` (`force`, `deferEndedListener`, `preservePreviousTrack`) and, on failure, restores the previous source, constraints, `enabled` state, listeners, processor and sender. It throws `TrackInvalidError` when the previous source is no longer `live`, because an ended track cannot be restored, and surfaces both failures as an `AggregateError` when the unwind itself fails.

    `stageTrackReplacement()` and `commitStagedTrackReplacement()` add a two-phase swap. The candidate becomes the active source with its `ended` listener deferred and the previous source preserved, and only the commit adopts the `ended` listener and clears the staged identity, so a caller can validate its publication before the swap is observable. `replaceTrack()` and `restart()` guard the `providedByUser` flip behind a `replacementCommitted` flag. `restart()` still detaches and stops the previous source before calling `getUserMedia()`, as upstream does, because Safari ends a freshly acquired track with a capture failure while the old track for the same device is still live. `setSimulcastTrackSender()` routes an installed processor's `processedTrack` to a newly registered secondary sender so a backup codec never publishes raw frames while the primary is processed. Processor install and teardown in all three classes roll the processed and raw sender track back, including `LocalVideoTrack`'s secondary simulcast senders, and aggregate every cleanup failure instead of discarding it.

13. **Start bitrate for every video codec** (`src/room/PCTransport.ts`, `src/room/participant/LocalParticipant.ts`, `src/room/participant/publishUtils.ts`)

    `x-google-start-bitrate` was reachable only by AV1 and VP9 because it was gated twice. The publish path registered a track bitrate only for SVC codecs, and the offer munging returned early for everything else. H.264, H.265 and VP8 therefore opened at the Chromium default and had to ramp, which showed up as a 3000 kbps screen share encoding at 346 kbps twenty seconds in. The bitrate is now registered for every video codec from the highest encoding (`maxEncodingBitrate()`, so a simulcast ladder contributes its top layer), and the offer munging applies it whenever a max bitrate is known. The dependency descriptor extension stays SVC-only. `appendStartBitrateToFmtp()` holds the fmtp edit so it can be tested, and `setTrackCodecBitrate()` replaces an entry for the same cid or transceiver instead of appending, since `trackBitrates` is never cleared.

## Updating from upstream

1. Check the upstream changelog for the target version.
2. `git diff v2.17.2..v<new> -- src/` to see what changed.
3. Apply relevant upstream changes to this package's `src/`.
4. Update the version field in `package.json` to match the new upstream version.
5. Update this file with the new version and date.

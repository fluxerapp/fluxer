# Vendored livekit-client

Vendored from https://github.com/livekit/client-sdk-js at tag `v2.22.3` (commit `53dd840`) under the Apache-2.0 licence. Re-vendored from `v2.17.2` on 2026-09-17.

## Source transforms

Every file went through the same mechanical steps, so a fresh upstream tree can be brought into this shape before merging.

1. Test files, `src/test` and snapshots are dropped. Files that are not reachable from `src/index.ts` or `src/e2ee/worker/e2ee.worker.ts` are dropped too.
2. Comments are stripped. `/// <reference>` lines, `@ts-expect-error` directives that are still needed and `biome-ignore` lines stay.
3. Relative imports get explicit `.ts` extensions, and directory imports point at `index.ts`.
4. Each file starts with the LiveKit SPDX header.
5. `biome check --write --unsafe` runs with the repo config. The unsafe fixes are reviewed by hand, because `useOptionalChain` turned `callback && callback(x)` into a call on `false` in `room/debounce.ts`, and `noUnusedPrivateClassMembers` deleted a field that was still written in `utils/dataPacketBuffer.ts`.
6. The tree is made to pass `tsgo --noEmit` under `fluxer_app/tsconfig.json`. That means `override` modifiers, explicit `return undefined`, typed `let` declarations, no async promise executors, literal enum members and no unused `@ts-expect-error` directives.
7. `@livekit/throws-transformer/throws` imports point at `src/utils/throws.ts`, which copies its type definitions, so the dev-only package is not a dependency.

## Fluxer modifications

1. **AV1 end-to-end encryption** (`src/e2ee/worker/av1Utils.ts`, `FrameCryptor.ts`, `src/room/track/utils.ts`)

   OBU-level AV1 encryption and decryption with an E2EE metadata OBU. `video/AV1X` maps to `av1`.

2. **Cryptor context for reused senders** (`src/e2ee/E2eeManager.ts`, `src/e2ee/worker/FrameCryptor.ts`)

   Upstream 2.22 tracks the id of a reused receiver in `E2EE_TRACK_ID` and sends `updateCodec` with `previousTrackId`. Fluxer applies the same to senders, passes the codec and track id from `LocalSenderCreated`, and resets codec detection whenever a cryptor moves to another track id or codec. This replaces the old `updateTrackContext` message and `get/setE2EETransformState`.

3. **E2EE frame guards** (`src/e2ee/worker/FrameCryptor.ts`, `naluUtils.ts`)

   Trailer, IV, tag and clear-prefix bounds are checked before any view is built, and malformed frames are dropped without ending the transform. NALU clear bytes are clamped to the frame length. H.265 slices keep 3 clear bytes where upstream keeps 2. The SIF trailer check is bounds safe.

4. **No plaintext before the encryption state is known** (`src/e2ee/worker/FrameCryptor.ts`)

   The encoder drops frames while the participant's encryption state is unknown. Upstream passes them through. Upstream 2.22 already drops them on decode.

5. **Screen share scalability mode** (`src/room/participant/LocalParticipant.ts`)

   A caller-supplied `scalabilityMode` is kept for screen shares. Upstream forces `L1T3` and `contentHint = 'motion'`. `L3T3_KEY` is only defaulted for cameras, and SVC layers are only sent to the server when an encoding has a scalability mode.

6. **Encrypted backup codec publishing** (`src/room/participant/LocalParticipant.ts`, `src/e2ee/E2eeManager.ts`)

   Backup codecs are advertised and published while E2EE is on. The simulcast sender emits `LocalSenderCreated` with its codec and track id so it gets its own transform. A failed backup codec publish stops the cloned track and is logged instead of thrown.

7. **Publisher codec preferences** (`src/room/RTCEngine.ts`)

   `setCodecPreferences()` is applied to publisher transceivers so the browser's SDP follows the selected primary or backup codec. H.264 profiles rank Baseline `42001f` first, then Constrained Baseline `42e01f`, then everything else. Main, High and Constrained High rank last on purpose.

   livekit-server registers H.264 High `640032` on the publisher peer connection but filters it off the subscriber peer connection, and its `CodecParametersFuzzySearch` falls back to a mime-only match. A High publication therefore reaches subscribers under their `42e01f` payload type and decodes to nothing on a Constrained-Baseline-only decoder such as Firefox's OpenH264 GMP. `42001f` is the one profile Chromium's accelerated encoder factory advertises that such a decoder can still handle, because Chromium's VAAPI encoder and OpenH264 both write a Constrained Baseline SPS for `H264PROFILE_BASELINE`.

   The trade is that livekit-server does not register `42001f` either, so Windows and macOS negotiate `42e01f`, which Chromium's accelerated encoder factory does not advertise there (`kPlatformH264CbpEncoding` is off by default on Windows, and `IsH264ConstrainedBaselineProfileAvailableForAcceleratedEncoder` returns false on Apple). Those publishers fall back to software H.264, which is what an unpatched browser does anyway. Linux, ChromeOS and Android keep hardware encoding.

8. **Media publishing defaults** (`src/room/defaults.ts`, `src/room/utils.ts`, `src/room/track/options.ts`)

   Codec fallback follows actual sender capabilities in the order H.264, VP9, VP8, AV1, HEVC through `supportsVideoCodec()` and `selectPreferredVideoCodec()`. HEVC is never reported on Firefox. Advanced codecs get an H.264 backup simulcast, dynacast is on, DTX is off, degradation preference is `maintain-resolution`, and screen shares default to the `original` preset at 20 Mbps and 60 fps. `musicHighQualityStereo` is 510 kbps. AV1 and HEVC come last because both are opt-in in Fluxer, so a fallback inside `publishTrack` must not land on a codec the user did not enable.

9. **High-fidelity Opus SDP munging** (`src/room/PCTransport.ts`, `src/room/participant/LocalParticipant.ts`)

   Local offers and remote answers force Opus RED and FEC, 10 ms packet time, no DTX and a 510 kbps maximum average bitrate. `stereo=1` and `sprop-stereo=1` are only added for publications whose `TrackBitrateInfo.stereo` is set and for the subscriber mids the server advertised as stereo. Audio bitrates are registered on every browser, not only Firefox. The initial offer sent with the join request is not munged, as upstream does not munge it either.

10. **Remote audio volume held at zero** (`src/room/track/RemoteAudioTrack.ts`)

    `attach()`, `connectWebAudio()` and `getVolume()` test `elementVolume !== undefined`, so a track held at `0` stays silent after re-attach. Remote gains above `1.0` rely on the Web Audio `gainNode` branch, so `webAudioMix` must stay on.

11. **Processor teardown before source stop** (`src/room/track/LocalTrack.ts`)

    `stop()` detaches and destroys the processor before `super.stop()` ends the source track.

12. **Transactional source and processor swaps** (`src/room/track/LocalTrack.ts`, `LocalVideoTrack.ts`, `LocalAudioTrack.ts`)

    `setMediaStreamTrack()` takes `SetMediaStreamTrackOptions` (`force`, `deferEndedListener`, `preservePreviousTrack`, `isUnmuting`) and restores the previous source, constraints, `enabled` state, listeners, processor and sender when a swap fails. `stageTrackReplacement()` and `commitStagedTrackReplacement()` give a two-phase swap. `runWithTrackChangeLock()` and `stopProcessorIfCurrent()` are public for the app. Secondary simulcast senders get the processed track, and processor install and teardown roll back every sender. Upstream's `onSenderTrackSwapped()` hook runs after a successful `replaceTrack`, `stageTrackReplacement`, `setProcessor`, `stopProcessor` and `stopProcessorIfCurrent`.

13. **Track bitrate entries are replaced** (`src/room/PCTransport.ts`)

    `setTrackCodecBitrate()` replaces the entry for the same cid or transceiver instead of appending, because `trackBitrates` is never cleared. Start bitrates for every video codec are upstream since 2.22 through `applyVideoStartBitrate()`.

14. **Subscriber video decoder exclusions** (`src/options.ts`, `src/room/RTCEngine.ts`, `src/room/PCTransportManager.ts`, `src/room/PCTransport.ts`)

    `RoomOptions.subscriberVideoCodecExclusions` removes codecs from receive transceivers with `setCodecPreferences()` before every offer and answer, including the initial offer sent with the join request. Firefox and Safari are skipped.

15. **ICE diagnostics** (`src/room/PCTransportManager.ts`)

    ICE candidate errors are logged once per transport, code, URL and address until the next ICE restart. The transport connect timeout names each transport's connection, ICE and signalling state.

16. **Screen share capture options** (`src/room/track/options.ts`, `src/room/track/utils.ts`)

    `ScreenShareCaptureOptions` has `windowAudio`, `monitorTypeSurfaces` and `restrictOwnAudio`. `restrictOwnAudio` and `suppressLocalAudioPlayback` are passed as audio constraints.

17. **Stats and monitors** (`src/room/stats.ts`, `src/room/track/Track.ts`, `LocalVideoTrack.ts`, `RemoteVideoTrack.ts`, `RemoteTrack.ts`, `LocalAudioTrack.ts`)

    Sender stats have `encoderImplementation` and `powerEfficientEncoder`, receiver stats have `powerEfficientDecoder`. `runMonitor()` skips a stats poll while the previous one is still running. Adaptive stream tracks that are not visible skip receiver stats. `setPublishingLayersForSender` writes `maxFramerate` where upstream writes a misspelt `maxFrameRate`, and `videoLayersFromEncodings` handles an SVC codec without a scalability mode.

18. **Smaller fixes** (`src/api/SignalClient.ts`, `src/api/utils.ts`, `src/room/Room.ts`, `src/room/timers.ts`, `src/room/utils.ts`, `src/e2ee/worker/e2ee.worker.ts`)

    The abort path passes its reason to `SignalClient.close()` as the reason instead of `updateState`. `handleOnClose` is public so `simulateScenario` needs no suppression. Queue bypass checks no longer log every request. Abort reasons that are numbers, booleans, bigints or symbols become strings. `Room` connect rejections are always `Error` instances. `CriticalTimers` binds the native timers when the module loads. `Future` rejects when its executor throws. The e2ee worker listens with `addEventListener`, and the outgoing data stream sink no longer logs aborts to `console`.

## Removed upstream code

`connectionHelper`, `createLocalScreenTracks`, `getStereoAudioStreamTrack`, `isLocalPub`, the frame metadata worker, `room/token-source/test-tokens.ts`, `utils/subscribeToEvents.ts`, and the `DataTrackPacket`, data track packet extension and `LocalTrackRecorder` exports. `createV0RtcUrl`, `truncateBytes`, `videoQualityForRid` and `STOP_REFETCH_DELAY_MS` are no longer exported.

## Updating from upstream

1. Read the upstream changelog for the target version.
2. Run the source transforms on the old tag and on the new tag. Keep the old result as the merge base and the new result as the other side.
3. Strip blank lines from the base, this tree and the new tree, run `git merge-file --diff3 --diff-algorithm=histogram` per file, and put blank lines back from the new tree. Blank-line drift between the trees otherwise turns into conflicts.
4. Resolve conflicts with the modification list above. Where upstream rewrote a function that carries a Fluxer change, start from the upstream function and reapply the change.
5. Diff the result against the transformed new tag after compiling both to JavaScript without types. Every hunk should belong to a modification listed here.
6. Run `tsgo --noEmit` under the app tsconfig, `biome check`, `knip` and `pnpm --filter livekit-client test`, then update the version in `package.json` and this file.

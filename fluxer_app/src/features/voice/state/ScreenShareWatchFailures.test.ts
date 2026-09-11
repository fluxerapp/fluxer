// SPDX-License-Identifier: AGPL-3.0-or-later

import {screenShareWatchAttemptKey} from '@app/features/voice/components/useScreenShareWatchFailure';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {ScreenShareWatchErrorCode, ScreenShareWatchFailures} from '@app/features/voice/state/ScreenShareWatchFailures';
import type {RemoteTrackPublication} from 'livekit-client';
import {beforeEach, describe, expect, it} from 'vitest';

function inboundStatsReport(): RTCStatsReport {
	return new Map<string, unknown>([
		[
			'IT01V',
			{
				type: 'inbound-rtp',
				id: 'IT01V',
				kind: 'video',
				codecId: 'C1',
				packetsReceived: 4210,
				bytesReceived: 3_500_000,
				framesReceived: 0,
				framesDecoded: 0,
				keyFramesDecoded: 0,
				framesDropped: 0,
				pliCount: 7,
				firCount: 0,
				nackCount: 2,
				freezeCount: 0,
				decoderImplementation: 'ExternalDecoder (D3D11VideoDecoder)',
				powerEfficientDecoder: true,
				frameWidth: 1920,
				frameHeight: 1080,
			},
		],
		[
			'C1',
			{
				type: 'codec',
				id: 'C1',
				mimeType: 'video/H264',
				sdpFmtpLine: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
			},
		],
	]) as unknown as RTCStatsReport;
}

function publicationWithStats(report: RTCStatsReport): RemoteTrackPublication {
	return {
		videoTrack: {
			getRTCStatsReport: () => Promise.resolve(report),
		},
	} as unknown as RemoteTrackPublication;
}

async function settleStatsSnapshot(): Promise<void> {
	await Promise.resolve();
	await Promise.resolve();
	await Promise.resolve();
}

function fireWatchDeadlines(streamKey: string): void {
	const snapshot = voiceMediaGraphStore.getGraphSnapshot();
	const dueAt = voiceMediaGraphStore.nowMs() + 60_000;
	for (const [key, deadline] of snapshot.deadlinesByKey) {
		if (deadline.streamKey !== streamKey) continue;
		voiceMediaGraphStore.transition({type: 'time.deadlineFired', key, at: dueAt});
	}
}

function historyFor(streamKey: string) {
	return ScreenShareWatchFailures.getFailureHistory().filter((record) => record.streamKey === streamKey);
}

function watchDeadlineFor(streamKey: string) {
	const snapshot = voiceMediaGraphStore.getGraphSnapshot();
	for (const deadline of snapshot.deadlinesByKey.values()) {
		if (deadline.kind === 'watchAttempt' && deadline.streamKey === streamKey) return deadline;
	}
	return null;
}

describe('ScreenShareWatchFailures failure history', () => {
	beforeEach(() => {
		voiceMediaGraphStore.reset();
	});

	it('keeps a reported failure after the viewer stops watching', () => {
		const streamKey = 'guild-a:channel-a:connection-stop';
		ScreenShareWatchFailures.markWatchStarted(streamKey);
		ScreenShareWatchFailures.reportFailure({
			streamKey,
			participantIdentity: 'user_1_connection-stop',
			trackSid: 'TR_stop',
			code: ScreenShareWatchErrorCode.RemoteTrackSubscriptionFailed,
			reason: 'remote-track-subscription-failed',
		});
		expect(ScreenShareWatchFailures.getFailure({streamKey})).not.toBeNull();

		ScreenShareWatchFailures.markWatchStopped(streamKey);

		expect(ScreenShareWatchFailures.getFailure({streamKey})).toBeNull();
		expect(historyFor(streamKey).map((record) => [record.code, record.trackSid, record.participantIdentity])).toEqual([
			[ScreenShareWatchErrorCode.RemoteTrackSubscriptionFailed, 'TR_stop', 'user_1_connection-stop'],
		]);
	});

	it('keeps a failure raised by a watch deadline after the viewer stops watching', () => {
		const streamKey = 'guild-a:channel-a:connection-deadline';
		const attemptKey = screenShareWatchAttemptKey({streamKey, watchGeneration: 1});
		ScreenShareWatchFailures.markWatchStarted(streamKey);
		ScreenShareWatchFailures.ensureAttempt({streamKey}, attemptKey);
		ScreenShareWatchFailures.setWatchTarget(streamKey, {videoRef: {current: null}});

		fireWatchDeadlines(streamKey);

		const graphFailure = ScreenShareWatchFailures.getFailure({streamKey});
		expect(graphFailure).not.toBeNull();

		ScreenShareWatchFailures.markWatchStopped(streamKey);

		expect(ScreenShareWatchFailures.getFailure({streamKey})).toBeNull();
		expect(historyFor(streamKey).map((record) => record.code)).toEqual([graphFailure!.code]);
	});

	it('records the inbound counters of the watched publication alongside the failure', async () => {
		const streamKey = 'guild-a:channel-a:connection-counters';
		const attemptKey = screenShareWatchAttemptKey({streamKey, watchGeneration: 1});
		ScreenShareWatchFailures.markWatchStarted(streamKey);
		ScreenShareWatchFailures.ensureAttempt({streamKey}, attemptKey);
		ScreenShareWatchFailures.setWatchTarget(streamKey, {
			videoRef: {current: null},
			publication: publicationWithStats(inboundStatsReport()),
		});

		ScreenShareWatchFailures.reportFailure({
			streamKey,
			trackSid: 'TR_counters',
			code: ScreenShareWatchErrorCode.FirstFrameTimeout,
			reason: 'first-frame-timeout',
		});
		await settleStatsSnapshot();

		expect(historyFor(streamKey)[0].inbound).toEqual({
			packetsReceived: 4210,
			bytesReceived: 3_500_000,
			framesReceived: 0,
			framesDecoded: 0,
			keyFramesDecoded: 0,
			framesDropped: 0,
			pliCount: 7,
			firCount: 0,
			nackCount: 2,
			freezeCount: 0,
			decoderImplementation: 'ExternalDecoder (D3D11VideoDecoder)',
			powerEfficientDecoder: true,
			mimeType: 'video/H264',
			sdpFmtpLine: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
			frameWidth: 1920,
			frameHeight: 1080,
		});
	});

	it('records the size and readiness of the attached video element', () => {
		const streamKey = 'guild-a:channel-a:connection-tile';
		const attemptKey = screenShareWatchAttemptKey({streamKey, watchGeneration: 1});
		const video = {readyState: 0, videoWidth: 0, videoHeight: 0, clientWidth: 960, clientHeight: 540};
		ScreenShareWatchFailures.markWatchStarted(streamKey);
		ScreenShareWatchFailures.ensureAttempt({streamKey}, attemptKey);
		ScreenShareWatchFailures.setWatchTarget(streamKey, {
			videoRef: {current: video as unknown as HTMLVideoElement},
		});

		ScreenShareWatchFailures.reportFailure({
			streamKey,
			code: ScreenShareWatchErrorCode.FirstFrameTimeout,
			reason: 'first-frame-timeout',
		});

		expect(historyFor(streamKey)[0].tile).toMatchObject({
			hasVideoElement: true,
			readyState: 0,
			videoWidth: 0,
			videoHeight: 0,
			clientWidth: 960,
			clientHeight: 540,
		});
	});

	it('leaves the running watch deadline alone when the watch target is registered', () => {
		const streamKey = 'guild-a:channel-a:connection-target';
		const attemptKey = screenShareWatchAttemptKey({streamKey, watchGeneration: 1});
		ScreenShareWatchFailures.markWatchStarted(streamKey);
		ScreenShareWatchFailures.ensureAttempt({streamKey}, attemptKey);
		const deadline = watchDeadlineFor(streamKey);
		expect(deadline).not.toBeNull();

		ScreenShareWatchFailures.setWatchTarget(streamKey, {videoRef: {current: null}});
		ScreenShareWatchFailures.setWatchTarget(streamKey, {
			videoRef: {current: null},
			publication: publicationWithStats(inboundStatsReport()),
		});

		expect(watchDeadlineFor(streamKey)).toBe(deadline);
	});

	it('keeps only the most recent failures once the ring is full', () => {
		const streamKey = 'guild-a:channel-a:connection-ring';
		ScreenShareWatchFailures.markWatchStarted(streamKey);
		for (let index = 0; index < 20; index += 1) {
			ScreenShareWatchFailures.reportFailure({
				streamKey,
				trackSid: `TR_ring_${index}`,
				code: ScreenShareWatchErrorCode.FirstFrameTimeout,
				reason: 'first-frame-timeout',
			});
		}

		const history = historyFor(streamKey);
		expect(history.length).toBe(16);
		expect([history[0].trackSid, history[history.length - 1].trackSid]).toEqual(['TR_ring_4', 'TR_ring_19']);
	});
});

// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useScreenShareWatchFailure} from '@app/features/voice/components/useScreenShareWatchFailure';
import {clearVideoFrameWatchersForTests} from '@app/features/voice/components/VideoElementFrameState';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {ScreenShareWatchFailures} from '@app/features/voice/state/ScreenShareWatchFailures';
import type {RemoteTrackPublication} from 'livekit-client';
import {act, useRef} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const STREAM_KEY = 'guild-a:channel-a:connection-watch';

function WatchProbe({publication}: {publication: RemoteTrackPublication | null}) {
	const videoRef = useRef<HTMLVideoElement | null>(null);
	useScreenShareWatchFailure({
		enabled: true,
		streamKey: STREAM_KEY,
		participantIdentity: 'user_1_connection-watch',
		trackSid: 'TR_watch',
		hasPublication: publication != null,
		isPublicationDesired: publication != null,
		hasSubscribedVideo: publication != null,
		publication,
		videoRef,
	});
	return null;
}

function watchDeadlineFor(streamKey: string) {
	const snapshot = voiceMediaGraphStore.getGraphSnapshot();
	for (const deadline of snapshot.deadlinesByKey.values()) {
		if (deadline.kind === 'watchAttempt' && deadline.streamKey === streamKey) return deadline;
	}
	return null;
}

describe('useScreenShareWatchFailure', () => {
	let root: Root | null = null;
	let container: HTMLDivElement | null = null;

	beforeEach(() => {
		voiceMediaGraphStore.reset();
		container = document.createElement('div');
		document.body.append(container);
		root = createRoot(container);
	});

	afterEach(() => {
		act(() => {
			root?.unmount();
		});
		root = null;
		container?.remove();
		container = null;
		clearVideoFrameWatchersForTests();
	});

	it('keeps the running watch deadline when the publication arrives after the watch started', () => {
		ScreenShareWatchFailures.markWatchStarted(STREAM_KEY);
		act(() => {
			root?.render(<WatchProbe publication={null} />);
		});
		const deadline = watchDeadlineFor(STREAM_KEY);
		expect(deadline).not.toBeNull();

		act(() => {
			root?.render(<WatchProbe publication={{trackSid: 'TR_watch'} as unknown as RemoteTrackPublication} />);
		});

		expect(watchDeadlineFor(STREAM_KEY)).toBe(deadline);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import {
	getRecentScreenShares,
	recordScreenShareEncoderVerification,
	recordScreenShareEndedModal,
	recordScreenShareRequestedCodec,
	recordScreenShareStartError,
	recordScreenShareStarted,
	recordScreenShareStopped,
	resetRecentScreenSharesForTests,
} from '@app/features/voice/utils/ScreenShareLifecycleLog';
import {beforeEach, describe, expect, it} from 'vitest';

describe('ScreenShareLifecycleLog', () => {
	beforeEach(() => {
		resetRecentScreenSharesForTests();
		ActiveScreenShareSource.clear();
	});

	it('keeps the first trigger that stopped a share instead of the later unpublish', () => {
		recordScreenShareStarted();
		recordScreenShareStopped('media-track-ended');
		recordScreenShareStopped('server-unpublish');

		const [entry] = getRecentScreenShares();
		expect(entry.stopTrigger).toBe('media-track-ended');
		expect(entry.stoppedAt).not.toBeNull();
	});

	it('tells a user stop apart from a share that stopped on its own', () => {
		recordScreenShareStarted();
		recordScreenShareStopped('user');
		recordScreenShareStarted();
		recordScreenShareStopped('gateway-echo');

		expect(getRecentScreenShares().map((entry) => entry.stopTrigger)).toEqual(['user', 'gateway-echo']);
	});

	it('records the codec verification and the modal a stopped share showed', () => {
		recordScreenShareStarted();
		recordScreenShareRequestedCodec('h264');
		recordScreenShareEncoderVerification('correct-negotiated', ['vp9']);
		recordScreenShareEndedModal('codec-policy-failed');
		recordScreenShareStopped('codec-republish-failed');

		const [entry] = getRecentScreenShares();
		expect(entry.requestedCodec).toBe('h264');
		expect(entry.negotiatedCodecs).toEqual(['vp9']);
		expect(entry.encoderVerification).toBe('correct-negotiated');
		expect(entry.modalShown).toBe('codec-policy-failed');
		expect(entry.stopTrigger).toBe('codec-republish-failed');
	});

	it('freezes the published source when the share stops', () => {
		ActiveScreenShareSource.setPublishedSource('app', 'window:7');
		recordScreenShareStarted();
		recordScreenShareStopped('user');
		ActiveScreenShareSource.clear();

		expect(getRecentScreenShares()[0].sourceKind).toBe('app');
	});

	it('reports the live published source while a share is still running', () => {
		recordScreenShareStarted();
		ActiveScreenShareSource.setPublishedSource('display', 'screen:1');

		expect(getRecentScreenShares()[0].sourceKind).toBe('display');
	});

	it('records a start failure without marking it as a stop', () => {
		recordScreenShareStarted();
		recordScreenShareStartError(new Error('portal refused'));

		const [entry] = getRecentScreenShares();
		expect(entry.startError).toBe('Error: portal refused');
		expect(entry.stopTrigger).toBeNull();
		expect(entry.stoppedAt).toBeNull();
	});

	it('leaves a closed share untouched when a later trigger arrives', () => {
		recordScreenShareStarted();
		recordScreenShareStartError(new Error('portal refused'));
		recordScreenShareStopped('user');

		expect(getRecentScreenShares()[0].stopTrigger).toBeNull();
	});

	it('keeps only the eight most recent shares', () => {
		for (let index = 0; index < 11; index += 1) {
			recordScreenShareStarted();
			recordScreenShareStopped('user');
		}

		expect(getRecentScreenShares()).toHaveLength(8);
	});
});

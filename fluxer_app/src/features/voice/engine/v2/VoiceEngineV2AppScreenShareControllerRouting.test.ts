// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type VoiceEngineV2AppScreenShareControllerGateway,
	VoiceEngineV2AppScreenShareControllerRouting,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareControllerRouting';
import type {VoiceEngineV2AppScreenShareExecutionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter';
import {
	getRecentScreenShares,
	recordScreenShareStarted,
	resetRecentScreenSharesForTests,
} from '@app/features/voice/utils/ScreenShareLifecycleLog';
import {beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/engine/voice_screen_share_manager/shared', () => ({
	logger: {debug: () => undefined, info: () => undefined, warn: () => undefined, error: () => undefined},
}));

vi.mock('@app/features/voice/state/LocalVoiceState', () => ({
	default: {getSelfStream: () => true},
}));

const setEnabled = vi.fn(async () => undefined);

function createAdapter(): VoiceEngineV2AppScreenShareExecutionAdapter {
	return {liveKitFlows: {setEnabled}} as unknown as VoiceEngineV2AppScreenShareExecutionAdapter;
}

function createGateway(
	overrides: Partial<VoiceEngineV2AppScreenShareControllerGateway> = {},
): VoiceEngineV2AppScreenShareControllerGateway {
	return {
		isScreenCommandRoutable: () => true,
		hasScreenPublication: () => true,
		hasScreenDesired: () => false,
		clearScreenDesired: () => undefined,
		executingScreenOperationId: () => null,
		isScreenOperationPending: () => true,
		publishScreen: async () => undefined,
		unpublishScreen: async () => undefined,
		...overrides,
	};
}

describe('routed screen share stop attribution', () => {
	beforeEach(() => {
		resetRecentScreenSharesForTests();
		setEnabled.mockClear();
	});

	it('keeps a stop the user asked for as a user stop when the controller carries it out', async () => {
		const routing = new VoiceEngineV2AppScreenShareControllerRouting(createAdapter());
		let executingOperationId: number | null = null;
		routing.setGateway(
			createGateway({
				executingScreenOperationId: () => executingOperationId,
				unpublishScreen: async (onPlanned) => {
					onPlanned([7]);
					executingOperationId = 7;
					await routing.unpublishViaLiveKitFlows(null);
				},
			}),
		);
		recordScreenShareStarted();

		await routing.setEnabled(null, false, {sendUpdate: true, playSound: true});

		expect(setEnabled).toHaveBeenCalledTimes(1);
		expect(getRecentScreenShares()[0].stopTrigger).toBe('user');
	});

	it('records an unpublish the app never asked for as a gateway echo', async () => {
		const routing = new VoiceEngineV2AppScreenShareControllerRouting(createAdapter());
		routing.setGateway(createGateway());
		recordScreenShareStarted();

		await routing.unpublishViaLiveKitFlows(null);

		expect(getRecentScreenShares()[0].stopTrigger).toBe('gateway-echo');
	});
});

// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const voiceBackgroundsAvailable = vi.fn<() => boolean>();

vi.mock('@app/features/voice/utils/VoiceBackgroundAvailability', () => ({
	areVoiceBackgroundsAvailable: () => voiceBackgroundsAvailable(),
}));

vi.mock('@app/features/app/utils/LimitResolverAdapter', () => ({
	LimitResolver: {resolve: () => 0},
}));

vi.mock('@app/features/app/utils/LimitUtils', () => ({
	isLimitToggleEnabled: () => false,
}));

const {default: VoiceSettings, NONE_BACKGROUND_ID} = await import('./VoiceSettings');

const SAVED_BACKGROUND = {id: 'saved-background', createdAt: 1, mediaKind: 'static' as const};

describe('VoiceSettings background persistence when voice backgrounds are unavailable', () => {
	beforeEach(() => {
		voiceBackgroundsAvailable.mockReturnValue(true);
		VoiceSettings.updateSettings({backgroundImages: [SAVED_BACKGROUND], backgroundImageId: SAVED_BACKGROUND.id});
		voiceBackgroundsAvailable.mockReturnValue(false);
	});

	it('keeps the uploaded background list across a no-op revalidation', () => {
		VoiceSettings.updateSettings({});
		expect(VoiceSettings.backgroundImages).toEqual([SAVED_BACKGROUND]);
	});

	it('keeps the stored selection so it comes back once backgrounds are available again', () => {
		VoiceSettings.updateSettings({});
		expect(VoiceSettings.backgroundImageId).toBe(SAVED_BACKGROUND.id);
		voiceBackgroundsAvailable.mockReturnValue(true);
		expect(VoiceSettings.getBackgroundImageId()).toBe(SAVED_BACKGROUND.id);
	});

	it('never hands out an active background while the feature is unavailable', () => {
		VoiceSettings.updateSettings({});
		expect(VoiceSettings.getBackgroundImageId()).toBe(NONE_BACKGROUND_ID);
		expect(VoiceSettings.getBackgroundImages()).toEqual([SAVED_BACKGROUND]);
	});

	it('still drops a selection that no longer matches a stored background', () => {
		VoiceSettings.updateSettings({backgroundImages: []});
		expect(VoiceSettings.backgroundImageId).toBe(NONE_BACKGROUND_ID);
	});
});

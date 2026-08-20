// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeAll, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/utils/VoiceBackgroundAvailability', () => ({
	areVoiceBackgroundsAvailable: () => true,
}));

vi.mock('@app/features/app/utils/LimitResolverAdapter', () => ({
	LimitResolver: {resolve: () => 0},
}));

vi.mock('@app/features/app/utils/LimitUtils', () => ({
	isLimitToggleEnabled: () => false,
}));

const STORAGE_KEY = 'VoiceSettings';

async function loadVoiceSettingsWith(stored: Record<string, unknown> | null) {
	vi.resetModules();
	const {default: AppStorage} = await import('@app/features/platform/state/PersistentStorage');
	AppStorage.removeItem(STORAGE_KEY);
	if (stored) {
		AppStorage.setItem(STORAGE_KEY, JSON.stringify({...stored, __mps__: {version: 1}}));
	}
	const {default: VoiceSettings} = await import('./VoiceSettings');
	const {awaitHydration} = await import('@app/features/platform/utils/MobXPersistence');
	await awaitHydration(STORAGE_KEY);
	const readStored = (): Record<string, unknown> => {
		const raw = AppStorage.getItem(STORAGE_KEY);
		return raw ? JSON.parse(raw) : {};
	};
	return {VoiceSettings, readStored};
}

describe('VoiceSettings output volume recalibration migration', () => {
	beforeAll(async () => {
		await import('./VoiceSettings');
	});

	it('resets a boosted output volume once and records that it ran', async () => {
		const {VoiceSettings, readStored} = await loadVoiceSettingsWith({outputVolume: 200});
		expect(VoiceSettings.outputVolume).toBe(100);
		expect(VoiceSettings.outputVolumeRecalibratedV1).toBe(true);
		expect(readStored().outputVolumeRecalibratedV1).toBe(true);
	});

	it('never touches an output volume the user set after the migration ran', async () => {
		const {VoiceSettings} = await loadVoiceSettingsWith({
			outputVolume: 200,
			outputVolumeRecalibratedV1: true,
		});
		expect(VoiceSettings.outputVolume).toBe(200);
		expect(VoiceSettings.outputVolumeRecalibratedV1).toBe(true);
	});

	it('leaves an output volume at or below unity alone', async () => {
		const {VoiceSettings, readStored} = await loadVoiceSettingsWith({outputVolume: 60});
		expect(VoiceSettings.outputVolume).toBe(60);
		expect(readStored().outputVolumeRecalibratedV1).toBe(true);
	});

	it('marks an empty stored record as recalibrated', async () => {
		const {VoiceSettings, readStored} = await loadVoiceSettingsWith({});
		expect(VoiceSettings.outputVolume).toBe(100);
		expect(VoiceSettings.outputVolumeRecalibratedV1).toBe(true);
		expect(readStored().outputVolumeRecalibratedV1).toBe(true);
	});

	it('marks a fresh install as recalibrated so a later launch cannot reset a volume set today', async () => {
		const {VoiceSettings} = await loadVoiceSettingsWith(null);
		expect(VoiceSettings.outputVolume).toBe(100);
		expect(VoiceSettings.outputVolumeRecalibratedV1).toBe(true);
	});
});

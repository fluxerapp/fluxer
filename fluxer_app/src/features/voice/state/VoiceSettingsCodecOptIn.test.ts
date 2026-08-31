// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {runInAction} from 'mobx';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/utils/VoiceBackgroundAvailability', () => ({
	areVoiceBackgroundsAvailable: () => true,
}));

vi.mock('@app/features/app/utils/LimitResolverAdapter', () => ({
	LimitResolver: {resolve: () => 0},
}));

vi.mock('@app/features/app/utils/LimitUtils', () => ({
	isLimitToggleEnabled: () => false,
}));

const AppStorage = (await import('@app/features/platform/state/PersistentStorage')).default;

AppStorage.setItem('VoiceSettings', JSON.stringify({preferredScreenShareCodec: 'av1'}));

const storageWrites: Array<string> = [];
AppStorage.subscribe(
	(event) => {
		if (event.newValue != null) storageWrites.push(event.newValue);
	},
	{key: 'VoiceSettings'},
);

async function loadVoiceSettings() {
	const [{Logger}, persistenceModule] = await Promise.all([
		import('@app/features/platform/utils/AppLogger'),
		import('@app/features/platform/utils/MobXPersistence'),
	]);
	const debug = vi.spyOn(Logger.prototype, 'debug').mockImplementation(() => undefined);
	try {
		const [{default: VoiceSettings}, {default: MediaPermission}] = await Promise.all([
			import('./VoiceSettings'),
			import('@app/features/permissions/system/state/MediaPermission'),
		]);
		await Promise.all([
			persistenceModule.awaitHydration('MacPermissions'),
			persistenceModule.awaitHydration('VoiceSettings'),
			vi.waitFor(() => expect(MediaPermission.isInitialized()).toBe(true)),
		]);
		expect(debug).toHaveBeenCalledTimes(3);
		expect(debug).toHaveBeenCalledWith('Store MacPermissions hydrated from AppStorage and is now persisting.');
		expect(debug).toHaveBeenCalledWith('Initial permission state', {
			microphone: 'granted',
			camera: 'granted',
			micDenied: false,
			cameraDenied: false,
		});
		expect(debug).toHaveBeenCalledWith('Store VoiceSettings hydrated from AppStorage and is now persisting.');
		return VoiceSettings;
	} finally {
		debug.mockRestore();
	}
}

const VoiceSettings = await loadVoiceSettings();

describe('AV1/HEVC screen-share opt-in', () => {
	it('rewrites a stored AV1 screen-share preference back to automatic on first launch', () => {
		const migrated = JSON.parse(storageWrites[0]);
		expect(migrated.preferredScreenShareCodec).toBe('auto');
		expect(migrated.screenShareAv1OptOutMigratedV1).toBe(true);
		expect(migrated.screenShareHevcOptOutMigratedV1).toBe(true);
		expect(VoiceSettings.screenShareAv1OptOutMigratedV1).toBe(true);
		expect(VoiceSettings.screenShareHevcOptOutMigratedV1).toBe(true);
		expect(VoiceSettings.screenShareAv1OptIn).toBe(false);
		expect(VoiceSettings.screenShareHevcOptIn).toBe(false);
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('auto');
	});

	it('hands out automatic while a hydrated AV1 or HEVC preference is not opted in', () => {
		VoiceSettings.updateSettings({screenShareAv1OptIn: false, screenShareHevcOptIn: false});
		runInAction(() => {
			VoiceSettings.preferredScreenShareCodec = 'av1';
		});
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('auto');
		runInAction(() => {
			VoiceSettings.preferredScreenShareCodec = 'h265';
		});
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('auto');
	});

	it('refuses to store an AV1 or HEVC preference while the opt-in is off', () => {
		VoiceSettings.updateSettings({
			screenShareAv1OptIn: false,
			screenShareHevcOptIn: false,
			preferredScreenShareCodec: 'auto',
		});
		VoiceSettings.updateSettings({preferredScreenShareCodec: 'av1'});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('auto');
		VoiceSettings.updateSettings({preferredScreenShareCodec: 'h265'});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('auto');
	});

	it('accepts an AV1 preference together with its opt-in in one patch', () => {
		VoiceSettings.updateSettings({screenShareAv1OptIn: false, preferredScreenShareCodec: 'auto'});
		VoiceSettings.updateSettings({screenShareAv1OptIn: true, preferredScreenShareCodec: 'av1'});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('av1');
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('av1');
	});

	it('accepts an HEVC preference together with its opt-in in one patch', () => {
		VoiceSettings.updateSettings({screenShareHevcOptIn: false, preferredScreenShareCodec: 'auto'});
		VoiceSettings.updateSettings({screenShareHevcOptIn: true, preferredScreenShareCodec: 'h265'});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('h265');
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('h265');
	});

	it('drops the AV1 preference again when the opt-in is turned back off', () => {
		VoiceSettings.updateSettings({screenShareAv1OptIn: true, preferredScreenShareCodec: 'av1'});
		VoiceSettings.updateSettings({screenShareAv1OptIn: false});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('auto');
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('auto');
	});

	it('drops the HEVC preference again when the opt-in is turned back off', () => {
		VoiceSettings.updateSettings({screenShareHevcOptIn: true, preferredScreenShareCodec: 'h265'});
		VoiceSettings.updateSettings({screenShareHevcOptIn: false});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('auto');
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('auto');
	});

	it('leaves an always-available codec preference alone', () => {
		VoiceSettings.updateSettings({preferredScreenShareCodec: 'vp9'});
		expect(VoiceSettings.preferredScreenShareCodec).toBe('vp9');
		expect(VoiceSettings.getPreferredScreenShareCodec()).toBe('vp9');
	});
});

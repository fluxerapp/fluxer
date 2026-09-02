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

AppStorage.setItem(
	'VoiceSettings',
	JSON.stringify({
		preferredScreenShareCodec: 'av1',
		screenShareContentHintPrefV2: 'auto',
		screenshareResolution: 'low_240p',
		__mps__: {version: 1},
	}),
);

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

describe('screen share content hint default', () => {
	it('moves a stored automatic content hint to text on first launch', () => {
		const migrated = JSON.parse(storageWrites[0]);
		expect(migrated.screenShareContentHintPrefV2).toBe('text');
		expect(migrated.screenShareContentHintDefaultMigratedV1).toBe(true);
		expect(VoiceSettings.getScreenShareContentHint()).toBe('text');
		expect(VoiceSettings.getScreenShareContentHintOverride()).toBe('text');
	});

	it('keeps an automatic content hint chosen after the migration', () => {
		VoiceSettings.updateSettings({screenShareContentHint: 'auto'});
		expect(VoiceSettings.getScreenShareContentHint()).toBe('auto');
		expect(VoiceSettings.getScreenShareContentHintOverride()).toBeUndefined();
	});
});

describe('stored video quality preferences', () => {
	it('retires a stored 240p screen share preference to 480p on first launch', () => {
		const migrated = JSON.parse(storageWrites[0]);
		expect(migrated.screenshareResolution).toBe('low_480p');
		expect(VoiceSettings.screenshareResolution).toBe('low_480p');
	});

	it('upgrades a retired 240p patch to 480p', () => {
		VoiceSettings.updateSettings({screenshareResolution: 'low_240p'});
		expect(VoiceSettings.screenshareResolution).toBe('low_480p');
	});

	it('keeps premium video quality choices stored while the entitlement is missing', () => {
		VoiceSettings.updateSettings({screenshareResolution: 'ultra', cameraResolution: 'high', videoFrameRate: 60});
		VoiceSettings.updateSettings({outputDeviceId: 'default'});
		expect(VoiceSettings.screenshareResolution).toBe('ultra');
		expect(VoiceSettings.cameraResolution).toBe('high');
		expect(VoiceSettings.videoFrameRate).toBe(60);
	});

	it('still hands out free-tier video quality while the entitlement is missing', () => {
		VoiceSettings.updateSettings({screenshareResolution: 'ultra', cameraResolution: 'high', videoFrameRate: 60});
		expect(VoiceSettings.getScreenshareResolution()).toBe('medium');
		expect(VoiceSettings.getCameraResolution()).toBe('medium');
		expect(VoiceSettings.getVideoFrameRate()).toBe(30);
	});

	it('keeps every stored background image while the entitlement is missing', () => {
		const images = [1, 2, 3, 4, 5].map((index) => ({id: `background-${index}`, createdAt: index}));
		runInAction(() => {
			VoiceSettings.backgroundImages = images;
		});
		VoiceSettings.updateSettings({outputDeviceId: 'default'});
		expect(VoiceSettings.backgroundImages).toHaveLength(5);
		VoiceSettings.updateSettings({backgroundImages: images.slice(0, 4)});
		expect(VoiceSettings.backgroundImages).toHaveLength(4);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	advanceDesktopClientIntro,
	readDesktopClientIntroConditions,
} from '@app/features/auth/flow/client_intro/ClientIntroPreferences';
import {
	createDesktopClientIntroSnapshot,
	type DesktopClientIntroSnapshot,
	selectDesktopClientIntroModel,
} from '@app/features/auth/flow/client_intro/DesktopClientIntroStateMachine';
import type {PermissionKind} from '@app/features/permissions/system/utils/NativePermissions';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const host = vi.hoisted(() => ({
	storage: new Map<string, string>(),
	desktop: true,
	platform: 'macos',
	granted: [] as Array<string>,
}));

vi.mock('@app/features/platform/state/PersistentStorage', () => ({
	default: {
		getItem: (key: string) => host.storage.get(key) ?? null,
		setItem: (key: string, value: string) => {
			host.storage.set(key, value);
		},
	},
}));
vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	isDesktop: () => host.desktop,
	getNativePlatformSync: () => host.platform,
	getElectronAPI: () => null,
	isNativeMacOS: () => host.platform === 'macos',
}));

function settled(): Array<PermissionKind> {
	return host.granted as Array<PermissionKind>;
}

function launch(): DesktopClientIntroSnapshot {
	return createDesktopClientIntroSnapshot(readDesktopClientIntroConditions(true), settled());
}

function screen(snapshot: DesktopClientIntroSnapshot): string {
	const model = selectDesktopClientIntroModel(snapshot);
	return model.permission ?? model.stage;
}

class Setup {
	snapshot = launch();
	readonly screens = [screen(this.snapshot)];

	private send(event: Parameters<typeof advanceDesktopClientIntro>[1]): this {
		this.snapshot = advanceDesktopClientIntro(this.snapshot, event);
		this.screens.push(screen(this.snapshot));
		return this;
	}

	throughPreferences(): this {
		this.send({type: 'intro.continueWelcome'});
		return this.send({type: 'intro.completePreferences', settledPermissions: settled()});
	}

	allow(): this {
		const current = selectDesktopClientIntroModel(this.snapshot).permission;
		if (current !== null) host.granted.push(current);
		return this.autoAdvance();
	}

	autoAdvance(): this {
		return this.send({type: 'intro.advancePermission', settledPermissions: settled()});
	}

	notNow(): this {
		return this.send({type: 'intro.skipPermission', settledPermissions: settled()});
	}

	progress(): string {
		const model = selectDesktopClientIntroModel(this.snapshot);
		return `${model.permissionIndex + 1}/${model.permissionCount}`;
	}
}

beforeEach(() => {
	host.storage.clear();
	host.desktop = true;
	host.platform = 'macos';
	host.granted = [];
});

describe('desktop client intro', () => {
	test('macOS desktop asks for one permission per screen, in order, then moves on to sign in', () => {
		const setup = new Setup().throughPreferences().allow().allow().allow().allow();
		expect(setup.screens).toEqual([
			'welcome',
			'preferences',
			'microphone',
			'camera',
			'screen',
			'input-monitoring',
			'complete',
		]);
		expect(screen(launch())).toBe('complete');
	});

	test.each(['windows', 'linux'])('%s desktop has no permission screens', (platform) => {
		host.platform = platform;
		expect(new Setup().throughPreferences().screens).toEqual(['welcome', 'preferences', 'complete']);
		expect(screen(launch())).toBe('complete');
	});

	test('the web app never enters the intro', () => {
		host.desktop = false;
		const snapshot = createDesktopClientIntroSnapshot(readDesktopClientIntroConditions(false), []);
		expect(selectDesktopClientIntroModel(snapshot)).toEqual({
			stage: 'inactive',
			showWelcome: false,
			showPreferences: false,
			permission: null,
			permissionIndex: 0,
			permissionCount: 0,
			isComplete: false,
		});
	});

	test('permissions that are already granted get no screen and are left out of the progress count', () => {
		host.granted = ['microphone', 'screen'];
		const setup = new Setup().throughPreferences();
		expect(setup.progress()).toBe('1/2');
		setup.notNow();
		expect(setup.progress()).toBe('2/2');
		expect(setup.notNow().screens).toEqual(['welcome', 'preferences', 'camera', 'input-monitoring', 'complete']);
	});

	test('a Mac with every permission granted never sees a permission screen', () => {
		host.granted = ['microphone', 'camera', 'screen', 'input-monitoring'];
		expect(new Setup().throughPreferences().screens).toEqual(['welcome', 'preferences', 'complete']);
		expect(screen(launch())).toBe('complete');
	});

	test('a screen only moves on by itself once its permission is granted', () => {
		const setup = new Setup().throughPreferences().autoAdvance();
		expect(screen(setup.snapshot)).toBe('microphone');
		expect(screen(setup.allow().snapshot)).toBe('camera');
	});

	test('a permission granted elsewhere in the meantime is skipped when moving on', () => {
		const setup = new Setup().throughPreferences();
		host.granted.push('camera', 'screen');
		expect(screen(setup.allow().snapshot)).toBe('input-monitoring');
	});

	test('Not now skips one permission and is remembered across a relaunch', () => {
		const setup = new Setup().throughPreferences().notNow();
		expect(screen(setup.snapshot)).toBe('camera');
		expect(screen(launch())).toBe('camera');
		setup.allow();
		expect(screen(launch())).toBe('screen');
	});

	test('quitting in the middle resumes at the first permission that is neither granted nor skipped', () => {
		new Setup().throughPreferences().allow().notNow();
		const resumed = new Setup();
		expect(resumed.screens).toEqual(['screen']);
		expect(resumed.progress()).toBe('1/2');
	});

	test('answering the last screen finishes setup for good, whichever answer it was', () => {
		new Setup().throughPreferences().notNow().notNow().notNow().notNow();
		expect(screen(launch())).toBe('complete');
	});

	test('a relaunch before the permission screens resumes at the step that was open', () => {
		const setup = new Setup();
		setup.snapshot = advanceDesktopClientIntro(setup.snapshot, {type: 'intro.continueWelcome'});
		expect(screen(launch())).toBe('preferences');
	});

	test('an install that finished the intro before the permission screens existed skips them', () => {
		host.storage.set('fluxer:auth:desktop-client-intro:completed', '1');
		expect(screen(launch())).toBe('complete');
	});

	test('the register page starts at preferences and still asks for permissions on macOS', () => {
		const conditions = {...readDesktopClientIntroConditions(true), welcomeSeen: true};
		let snapshot = createDesktopClientIntroSnapshot(conditions, settled());
		expect(screen(snapshot)).toBe('preferences');
		snapshot = advanceDesktopClientIntro(snapshot, {type: 'intro.completePreferences', settledPermissions: settled()});
		expect(screen(snapshot)).toBe('microphone');
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const session = loadTsModule('@electron/main/LinuxSession', {stubs: {'node:fs': {existsSync: () => false}}});
const sandbox = loadTsModule('@electron/main/LinuxSandbox', {stubs: {'node:fs': {existsSync: () => false}}});

describe('detectLinuxSessionType', () => {
	const exists = (paths) => (candidate) => paths.includes(candidate);

	test('trusts XDG_SESSION_TYPE when it names x11 or wayland', () => {
		assert.equal(
			session.detectLinuxSessionType({XDG_SESSION_TYPE: 'x11', WAYLAND_DISPLAY: 'wayland-0'}, exists([])),
			'x11',
		);
		assert.equal(session.detectLinuxSessionType({XDG_SESSION_TYPE: 'wayland'}, exists([])), 'wayland');
	});

	test('otherwise requires the Wayland socket to exist', () => {
		const env = {
			XDG_SESSION_TYPE: 'tty',
			WAYLAND_DISPLAY: 'wayland-0',
			XDG_RUNTIME_DIR: '/run/user/1000',
			DISPLAY: ':0',
		};
		assert.equal(session.detectLinuxSessionType(env, exists(['/run/user/1000/wayland-0'])), 'wayland');
		assert.equal(session.detectLinuxSessionType(env, exists([])), 'x11');
		assert.equal(session.detectLinuxSessionType({WAYLAND_DISPLAY: '/tmp/wl'}, exists(['/tmp/wl'])), 'wayland');
		assert.equal(session.detectLinuxSessionType({}, exists([])), 'unknown');
	});
});

describe('detectLinuxDesktop', () => {
	test('scans every XDG_CURRENT_DESKTOP token', () => {
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'KDE'}), 'kde');
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'GNOME'}), 'gnome');
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'ubuntu:GNOME'}), 'gnome');
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'zorin:GNOME'}), 'gnome');
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'Custom:KDE'}), 'kde');
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'Hyprland:GNOME'}), 'hyprland');
		assert.equal(
			session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'GNOME', HYPRLAND_INSTANCE_SIGNATURE: 'x'}),
			'hyprland',
		);
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'sway'}), 'other');
		assert.equal(session.detectLinuxDesktop({XDG_CURRENT_DESKTOP: 'Hyprland'}), 'hyprland');
		assert.equal(session.detectLinuxDesktop({HYPRLAND_INSTANCE_SIGNATURE: 'abc'}), 'hyprland');
		assert.equal(session.detectLinuxDesktop({}), 'other');
	});

	test('detects Plasma 5 sessions', () => {
		assert.equal(session.isKdePlasma5Session({KDE_SESSION_VERSION: '5'}), true);
		assert.equal(session.isKdePlasma5Session({KDE_SESSION_VERSION: '6'}), false);
	});
});

describe('parseFlatpakInfoAppId', () => {
	test('reads the application name', () => {
		assert.equal(
			sandbox.parseFlatpakInfoAppId('[Application]\nname=app.fluxer.FluxerCanary\nruntime=x\n[Instance]\nname=nope\n'),
			'app.fluxer.FluxerCanary',
		);
		assert.equal(sandbox.parseFlatpakInfoAppId('[Instance]\nname=nope\n'), null);
	});
});

describe('getLinuxDesktopId', () => {
	test('names the Flatpak app inside the sandbox and the host entry outside it', () => {
		const platform = Object.getOwnPropertyDescriptor(process, 'platform');
		const previousId = process.env.FLATPAK_ID;
		Object.defineProperty(process, 'platform', {...platform, value: 'linux'});
		try {
			process.env.FLATPAK_ID = 'app.fluxer.Fluxer';
			const flatpak = loadTsModule('@electron/main/LinuxSandbox', {stubs: {'node:fs': {existsSync: () => true}}});
			assert.equal(flatpak.getLinuxDesktopId(), 'app.fluxer.Fluxer');
			const host = loadTsModule('@electron/main/LinuxSandbox', {stubs: {'node:fs': {existsSync: () => false}}});
			assert.equal(host.getLinuxDesktopId(), 'app.fluxer.FluxerDesktop');
		} finally {
			Object.defineProperty(process, 'platform', platform);
			if (previousId === undefined) delete process.env.FLATPAK_ID;
			else process.env.FLATPAK_ID = previousId;
		}
	});
});

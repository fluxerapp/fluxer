// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	desktopExecRunsPath,
	isAppImageIntegratorEntry,
	isWindowDesktopEntry,
	readDesktopEntryValue,
	setDesktopEntryValue,
} from '@electron/main/DesktopEntryText';
import {describe, expect, it} from 'vitest';

const STABLE_CLASSES = ['app.fluxer.FluxerDesktop', 'fluxer'];

function entry(...lines: Array<string>): string {
	return ['[Desktop Entry]', 'Type=Application', 'Name=Fluxer', ...lines, ''].join('\n');
}

describe('readDesktopEntryValue', () => {
	it('reads keys written with spaces around the equals sign', () => {
		expect(readDesktopEntryValue(entry('StartupWMClass = fluxer'), 'StartupWMClass')).toBe('fluxer');
	});

	it('ignores localized keys and keys in other groups', () => {
		const contents = [
			'[Desktop Entry]',
			'Name[de]=Fluxer DE',
			'Name=Fluxer',
			'',
			'[Desktop Action new-dm]',
			'Exec=/opt/other %U',
		].join('\n');
		expect(readDesktopEntryValue(contents, 'Name')).toBe('Fluxer');
		expect(readDesktopEntryValue(contents, 'Exec')).toBeNull();
	});
});

describe('setDesktopEntryValue', () => {
	it('replaces a spaced key in place without adding a duplicate', () => {
		const updated = setDesktopEntryValue(
			entry('StartupWMClass = fluxer'),
			'StartupWMClass',
			'app.fluxer.FluxerDesktop',
		);
		expect(updated.match(/StartupWMClass/g)).toHaveLength(1);
		expect(readDesktopEntryValue(updated, 'StartupWMClass')).toBe('app.fluxer.FluxerDesktop');
	});

	it('drops repeated keys so the file stays valid', () => {
		const updated = setDesktopEntryValue(entry('StartupWMClass=a', 'StartupWMClass=b'), 'StartupWMClass', 'c');
		expect(updated.match(/StartupWMClass/g)).toHaveLength(1);
	});

	it('inserts a missing key at the end of the main group only', () => {
		const contents = ['[Desktop Entry]', 'Name=Fluxer', '', '[Desktop Action a]', 'Name=A', ''].join('\n');
		expect(setDesktopEntryValue(contents, 'StartupWMClass', 'x')).toBe(
			['[Desktop Entry]', 'Name=Fluxer', 'StartupWMClass=x', '', '[Desktop Action a]', 'Name=A', ''].join('\n'),
		);
	});
});

describe('desktopExecRunsPath', () => {
	it('matches the launch command as a whole program token', () => {
		expect(desktopExecRunsPath('fluxer %U', 'fluxer')).toBe(true);
		expect(desktopExecRunsPath('/usr/bin/fluxer %U', 'fluxer')).toBe(true);
		expect(desktopExecRunsPath('env FOO=1 /opt/Fluxer/fluxer %U', '/opt/Fluxer/fluxer')).toBe(true);
	});

	it('does not match other channels or the Flatpak export for the bare fluxer command', () => {
		expect(desktopExecRunsPath('/opt/fluxer-canary/fluxer-canary %U', 'fluxer')).toBe(false);
		expect(desktopExecRunsPath('fluxer-canary %U', 'fluxer')).toBe(false);
		expect(
			desktopExecRunsPath(
				'/usr/bin/flatpak run --branch=stable --arch=x86_64 --command=fluxer --file-forwarding app.fluxer.Fluxer @@u %U @@',
				'fluxer',
			),
		).toBe(false);
	});

	it('does not match a sibling tar.gz install whose path starts with ours', () => {
		expect(desktopExecRunsPath('/home/u/fluxer/fluxer-canary-x64/fluxer-canary %U', '/home/u/fluxer/fluxer')).toBe(
			false,
		);
		expect(desktopExecRunsPath('/home/u/fluxer/fluxer2 %U', '/home/u/fluxer/fluxer')).toBe(false);
	});

	it('reads paths quoted and escaped the way our own entry writes them', () => {
		expect(desktopExecRunsPath('"/home/u/My Apps/Flu\\\\$x.AppImage" %U', '/home/u/My Apps/Flu$x.AppImage')).toBe(true);
		expect(desktopExecRunsPath("'/home/u/My Apps/fluxer.appimage' %U", '/home/u/My Apps/fluxer.appimage')).toBe(true);
	});
});

describe('isWindowDesktopEntry', () => {
	const official = {wmClasses: STABLE_CLASSES, launchPath: '/opt/Fluxer/fluxer'};

	it('claims legacy entries that name the old window class or our binary', () => {
		expect(isWindowDesktopEntry(entry('StartupWMClass=Fluxer', 'Exec=/somewhere/else %U'), official)).toBe(true);
		expect(isWindowDesktopEntry(entry('Exec=/opt/Fluxer/fluxer %U'), official)).toBe(true);
	});

	it('skips hidden entries and other channels', () => {
		expect(isWindowDesktopEntry(entry('Hidden=true', 'Exec=/opt/Fluxer/fluxer %U'), official)).toBe(false);
		expect(
			isWindowDesktopEntry(
				entry('StartupWMClass=app.fluxer.FluxerDesktopCanary', 'Exec="/opt/fluxer-canary/fluxer-canary" %U'),
				{wmClasses: STABLE_CLASSES, launchPath: 'fluxer'},
			),
		).toBe(false);
	});

	it('never claims other Electron apps when running on a stock Electron binary', () => {
		const stock = {wmClasses: STABLE_CLASSES, launchPath: '/usr/lib/electron44/electron'};
		expect(isWindowDesktopEntry(entry('Exec=electron44 /usr/lib/other/app.asar %U'), stock)).toBe(false);
		expect(isWindowDesktopEntry(entry('Exec=/usr/lib/electron44/electron %U', 'StartupWMClass=electron'), stock)).toBe(
			false,
		);
	});
});

describe('isAppImageIntegratorEntry', () => {
	const appImage = '/home/u/AppImages/fluxer.appimage';

	it('accepts the entry Gear Lever writes for the AppImage', () => {
		const gearLever = entry(
			`Exec=${appImage} %U`,
			`TryExec=${appImage}`,
			'X-AppImage-Version=2026.1116.172250',
			'X-AppImage-Name=Fluxer',
		);
		expect(isAppImageIntegratorEntry(gearLever, appImage)).toBe(true);
	});

	it('accepts the entry AppImageLauncher writes for the AppImage', () => {
		const launcher = entry(`Exec="${appImage}" %U`, 'X-AppImage-Identifier=0d1e');
		expect(isAppImageIntegratorEntry(launcher, appImage)).toBe(true);
	});

	it('leaves hand-written entries alone', () => {
		expect(isAppImageIntegratorEntry(entry(`Exec=${appImage} %U`, 'StartupWMClass=fluxer'), appImage)).toBe(false);
	});

	it('leaves integrator entries for other AppImages alone', () => {
		const canary = entry('Exec=/home/u/AppImages/fluxer_canary.appimage %U', 'X-AppImage-Name=Fluxer Canary');
		expect(isAppImageIntegratorEntry(canary, appImage)).toBe(false);
		expect(isAppImageIntegratorEntry(entry('Exec=fluxer %U', 'X-AppImage-Name=Fluxer'), 'fluxer')).toBe(false);
	});
});

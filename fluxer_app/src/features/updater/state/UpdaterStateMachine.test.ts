// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UpdaterDownloadOption} from '@app/features/platform/types/Electron';
import {describe, expect, it} from 'vitest';
import {
	createUpdaterMachineSnapshot,
	getUpdaterDisplayVersion,
	getUpdaterMachineStateValue,
	getUpdaterUpdateType,
	hasManualNativeDownload,
	transitionUpdaterMachineSnapshot,
	type UpdaterMachineEvent,
	type UpdaterMachineSnapshot,
} from './UpdaterStateMachine';

const NOW = 1_700_000_000_000;

const APPIMAGE_OPTION: UpdaterDownloadOption = {
	format: 'appimage',
	label: 'AppImage',
	url: 'https://api.fluxer.app/dl/desktop/stable/linux/x64/latest/appimage',
	suggestedName: 'Fluxer-1.2.3-linux-x86_64.AppImage',
	sha256: 'abc123',
};

const DEB_OPTION: UpdaterDownloadOption = {
	format: 'deb',
	label: 'DEB package',
	url: 'https://api.fluxer.app/dl/desktop/stable/linux/x64/latest/deb',
	suggestedName: 'Fluxer-1.2.3-linux-amd64.deb',
	sha256: 'def456',
};

function transition(snapshot: UpdaterMachineSnapshot, event: UpdaterMachineEvent): UpdaterMachineSnapshot {
	return transitionUpdaterMachineSnapshot(snapshot, event);
}

describe('updaterStateMachine', () => {
	it('starts idle with no update surface', () => {
		const snapshot = createUpdaterMachineSnapshot();
		expect(getUpdaterMachineStateValue(snapshot)).toBe('idle');
		expect(getUpdaterUpdateType(snapshot)).toBe(null);
		expect(hasManualNativeDownload(snapshot)).toBe(false);
		expect(snapshot.context.updateInfo.native.available).toBe(false);
		expect(snapshot.context.updateInfo.web.available).toBe(false);
	});

	it('keeps a downloaded native update pending restart when checks re-emit available or not-available', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '2.0.0',
			downloadSize: 1000,
			downloadStarted: true,
			downloadUrl: null,
			downloadOptions: [],
		});
		snapshot = transition(snapshot, {type: 'native.downloaded', version: '2.0.0'});
		expect(snapshot.context.updateInfo.native.downloaded).toBe(true);

		snapshot = transition(snapshot, {type: 'native.notAvailable', now: NOW});
		expect(snapshot.context.updateInfo.native.downloaded).toBe(true);
		expect(snapshot.context.updateInfo.native.available).toBe(true);

		snapshot = transition(snapshot, {
			type: 'native.available',
			version: null,
			downloadSize: null,
			downloadStarted: true,
			downloadUrl: null,
			downloadOptions: [],
		});
		expect(snapshot.context.updateInfo.native.downloaded).toBe(true);
		expect(snapshot.context.updateInfo.native.downloading).toBe(false);
		expect(snapshot.context.updateInfo.native.version).toBe('2.0.0');
	});

	it('tracks check start and finish separately from available updates', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {type: 'check.started'});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('checking');
		expect(snapshot.context.checkInProgress).toBe(true);
		snapshot = transition(snapshot, {type: 'check.finished', now: NOW});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('idle');
		expect(snapshot.context.checkInProgress).toBe(false);
		expect(snapshot.context.lastCheckedAt).toBe(NOW);
	});

	it('surfaces Linux manual native updates with package choices and versions', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '1.2.3',
			downloadSize: null,
			downloadStarted: false,
			downloadUrl: APPIMAGE_OPTION.url,
			downloadOptions: [APPIMAGE_OPTION, DEB_OPTION],
		});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('available');
		expect(getUpdaterUpdateType(snapshot)).toBe('native');
		expect(getUpdaterDisplayVersion(snapshot)).toBe('1.2.3');
		expect(hasManualNativeDownload(snapshot)).toBe(true);
		expect(snapshot.context.nativeManualDownloadOptions).toEqual([APPIMAGE_OPTION, DEB_OPTION]);
		expect(snapshot.context.updateInfo.native).toMatchObject({
			available: true,
			downloaded: false,
			downloading: false,
			version: '1.2.3',
		});
	});

	it('keeps automatic desktop downloads separate from manual package downloads', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '2.0.0',
			downloadSize: 4096,
			downloadStarted: false,
			downloadUrl: null,
			downloadOptions: [],
		});
		expect(hasManualNativeDownload(snapshot)).toBe(false);
		snapshot = transition(snapshot, {type: 'native.download.started', progressSupported: true, total: 4096});
		expect(snapshot.context.updateInfo.native.downloading).toBe(true);
		expect(snapshot.context.downloadProgress).toEqual({
			percent: 0,
			transferred: 0,
			total: 4096,
			bytesPerSecond: 0,
		});
		snapshot = transition(snapshot, {
			type: 'native.progress',
			progress: {percent: 50, transferred: 2048, total: 4096, bytesPerSecond: 512},
		});
		expect(snapshot.context.downloadProgress?.percent).toBe(50);
	});

	it('marks downloaded desktop updates ready and clears manual download choices', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '1.2.3',
			downloadSize: null,
			downloadStarted: false,
			downloadUrl: APPIMAGE_OPTION.url,
			downloadOptions: [APPIMAGE_OPTION],
		});
		snapshot = transition(snapshot, {type: 'native.downloaded', version: '1.2.3'});
		expect(snapshot.context.updateInfo.native).toMatchObject({
			available: true,
			downloaded: true,
			downloading: false,
		});
		expect(hasManualNativeDownload(snapshot)).toBe(false);
		expect(snapshot.context.nativeManualDownloadOptions).toEqual([]);
	});

	it('prioritizes desktop updates when desktop and web updates are both available', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {type: 'web.checked', available: true, version: 'web-2026.05.31'});
		expect(getUpdaterUpdateType(snapshot)).toBe('web');
		expect(getUpdaterDisplayVersion(snapshot)).toBe('web-2026.05.31');
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '3.0.0',
			downloadSize: null,
			downloadStarted: false,
			downloadUrl: null,
			downloadOptions: [],
		});
		expect(getUpdaterUpdateType(snapshot)).toBe('both');
		expect(getUpdaterDisplayVersion(snapshot)).toBe('3.0.0');
	});

	it('hides Flatpak and other system-managed installs from the native update surface', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '1.2.3',
			downloadSize: null,
			downloadStarted: false,
			downloadUrl: APPIMAGE_OPTION.url,
			downloadOptions: [APPIMAGE_OPTION],
		});
		snapshot = transition(snapshot, {
			type: 'native.unsupported',
			reason: 'managed-package',
			downloadUrl: null,
			now: NOW,
		});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('idle');
		expect(getUpdaterUpdateType(snapshot)).toBe(null);
		expect(snapshot.context.updateInfo.native.available).toBe(false);
		expect(snapshot.context.nativeUnsupported).toEqual({reason: 'managed-package', downloadUrl: null});
	});

	it('keeps an already-downloaded desktop update visible when a later check finds nothing newer', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {type: 'native.downloaded', version: '4.0.0'});
		snapshot = transition(snapshot, {type: 'native.notAvailable', now: NOW});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('available');
		expect(snapshot.context.updateInfo.native.downloaded).toBe(true);
		expect(getUpdaterDisplayVersion(snapshot)).toBe('4.0.0');
	});

	it('does not discard existing update availability when a later check fails', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {type: 'web.checked', available: true, version: 'web-2026.05.31'});
		snapshot = transition(snapshot, {type: 'check.started'});
		snapshot = transition(snapshot, {type: 'check.failed', now: NOW});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('available');
		expect(getUpdaterUpdateType(snapshot)).toBe('web');
		expect(snapshot.context.checkInProgress).toBe(false);
	});

	it('records native check errors and clears the flag on the next check', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {type: 'check.started'});
		snapshot = transition(snapshot, {type: 'native.error'});
		expect(snapshot.context.nativeCheckFailed).toBe(true);
		expect(snapshot.context.checkInProgress).toBe(false);
		snapshot = transition(snapshot, {type: 'check.started'});
		expect(snapshot.context.nativeCheckFailed).toBe(false);
	});

	it('tracks manual package download submission without changing update availability', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'native.available',
			version: '1.2.3',
			downloadSize: null,
			downloadStarted: false,
			downloadUrl: APPIMAGE_OPTION.url,
			downloadOptions: [APPIMAGE_OPTION],
		});
		snapshot = transition(snapshot, {type: 'manualDownload.started'});
		expect(snapshot.context.manualNativeDownloadInFlight).toBe(true);
		expect(getUpdaterUpdateType(snapshot)).toBe('native');
		snapshot = transition(snapshot, {type: 'manualDownload.finished'});
		expect(snapshot.context.manualNativeDownloadInFlight).toBe(false);
		expect(getUpdaterUpdateType(snapshot)).toBe('native');
	});

	it('reset clears all update state and in-flight flags', () => {
		let snapshot = createUpdaterMachineSnapshot();
		snapshot = transition(snapshot, {type: 'web.checked', available: true, version: 'web-2026.05.31'});
		snapshot = transition(snapshot, {type: 'manualDownload.started'});
		snapshot = transition(snapshot, {type: 'reset'});
		expect(getUpdaterMachineStateValue(snapshot)).toBe('idle');
		expect(getUpdaterUpdateType(snapshot)).toBe(null);
		expect(snapshot.context.manualNativeDownloadInFlight).toBe(false);
		expect(snapshot.context.lastCheckedAt).toBe(null);
	});
});

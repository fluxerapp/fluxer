// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {UpdaterEvent} from '@app/types/electron.d';
import {afterEach, describe, expect, test, vi} from 'vitest';

const {pushUpdateReadyModal} = vi.hoisted(() => ({pushUpdateReadyModal: vi.fn()}));

vi.mock('@app/features/updater/commands/UpdaterModalCommands', () => ({
	pushDesktopUpdateDownloadFailedModal: vi.fn(),
	pushDesktopUpdateInstallFailedModal: vi.fn(),
	pushManualUpdateAvailableModal: vi.fn(),
	pushUnsupportedUpdateModal: vi.fn(),
	pushUpdateAvailableModal: vi.fn(),
	pushUpdateCheckFailedModal: vi.fn(),
	pushUpdateReadyModal,
	pushUpToDateModal: vi.fn(),
}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: {message: string}) => descriptor,
}));

vi.mock('@app/features/platform/utils/ClientInfo', () => ({
	getClientInfo: () =>
		Promise.resolve({desktopVersion: '1.0.0', desktopChannel: 'canary', desktopArch: 'x64', arch: 'x64'}),
}));

installVoiceMenuTestBootstrap();

let nativeEventListener: ((event: UpdaterEvent) => void) | null = null;
let onUpdaterCheck: (() => void) | null = null;
let loadedUpdater: {dispose: () => void} | null = null;

function installElectronApi(): void {
	nativeEventListener = null;
	onUpdaterCheck = null;
	(window as unknown as {electron: unknown}).electron = {
		platform: 'win32',
		buildChannel: 'canary',
		onUpdaterEvent: (listener: (event: UpdaterEvent) => void) => {
			nativeEventListener = listener;
			return () => {
				nativeEventListener = null;
			};
		},
		updaterCheck: () => {
			onUpdaterCheck?.();
			return Promise.resolve();
		},
		updaterDownload: () => Promise.resolve(),
		updaterInstall: () => Promise.resolve(),
	};
}

function emit(event: UpdaterEvent): void {
	if (!nativeEventListener) throw new Error('Updater never subscribed to native updater events');
	nativeEventListener(event);
}

async function loadUpdater() {
	vi.resetModules();
	installElectronApi();
	const {default: Updater} = await import('@app/features/app/state/Updater');
	loadedUpdater = Updater;
	await vi.waitFor(() => {
		expect(nativeEventListener).not.toBeNull();
		expect(Updater.lastCheckedAt).not.toBeNull();
	});
	pushUpdateReadyModal.mockClear();
	return Updater;
}

function emitUserDownloadCompletion(version: string): void {
	emit({type: 'available', context: 'user', version, downloadSize: 1000, downloadStarted: true});
	emit({type: 'downloaded', context: 'user', version});
}

afterEach(() => {
	loadedUpdater?.dispose();
	loadedUpdater = null;
});

describe('updater update-ready surface', () => {
	test('does not push a blocking modal when a user-initiated download finishes outside a check', async () => {
		const Updater = await loadUpdater();
		emitUserDownloadCompletion('2.0.0');
		expect(Updater.nativeUpdateReady).toBe(true);
		expect(pushUpdateReadyModal).not.toHaveBeenCalled();
	});

	test('announces the ready update through a dismissible nagbar instead', async () => {
		const Updater = await loadUpdater();
		expect(Updater.shouldShowUpdateReadyNagbar).toBe(false);
		emitUserDownloadCompletion('2.0.0');
		expect(Updater.shouldShowUpdateReadyNagbar).toBe(true);
		Updater.dismissUpdateReadyNagbar();
		expect(Updater.shouldShowUpdateReadyNagbar).toBe(false);
		emit({type: 'downloaded', context: 'background', version: '2.1.0'});
		expect(Updater.shouldShowUpdateReadyNagbar).toBe(true);
	});

	test('still answers a user-initiated check with the update ready modal', async () => {
		const Updater = await loadUpdater();
		emitUserDownloadCompletion('2.0.0');
		pushUpdateReadyModal.mockClear();
		onUpdaterCheck = () => emit({type: 'available', context: 'user', version: '2.0.0', downloadStarted: false});
		await Updater.checkForUpdates(true, true);
		expect(pushUpdateReadyModal).toHaveBeenCalledTimes(1);
	});
});

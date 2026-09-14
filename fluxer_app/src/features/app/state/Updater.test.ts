// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UpdaterDownloadOption} from '@app/features/platform/types/Electron';
import {installVoiceMenuTestBootstrap} from '@app/features/ui/action_menu/items/__fixtures__/VoiceMenuTestBootstrap';
import type {UpdaterEvent} from '@app/types/electron.d';
import {afterEach, describe, expect, test, vi} from 'vitest';

interface ManualUpdatePrompt {
	options: ReadonlyArray<UpdaterDownloadOption>;
	onDownload: (option: UpdaterDownloadOption) => void | Promise<void>;
}

const {
	pushManualUpdateAvailableModal,
	pushUnsupportedUpdateModal,
	pushUpdateCheckFailedModal,
	pushUpdateReadyModal,
	pushUpToDateModal,
} = vi.hoisted(() => ({
	pushManualUpdateAvailableModal: vi.fn<(prompt: ManualUpdatePrompt) => void>(),
	pushUnsupportedUpdateModal: vi.fn(),
	pushUpdateCheckFailedModal: vi.fn(),
	pushUpdateReadyModal: vi.fn(),
	pushUpToDateModal: vi.fn(),
}));

vi.mock('@app/features/updater/commands/UpdaterModalCommands', () => ({
	pushDesktopUpdateDownloadFailedModal: vi.fn(),
	pushDesktopUpdateInstallFailedModal: vi.fn(),
	pushManualUpdateAvailableModal,
	pushUnsupportedUpdateModal,
	pushUpdateAvailableModal: vi.fn(),
	pushUpdateCheckFailedModal,
	pushUpdateReadyModal,
	pushUpToDateModal,
}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: {message: string}) => descriptor,
}));

vi.mock('@app/features/platform/utils/ClientInfo', () => ({
	getClientInfo: () =>
		Promise.resolve({desktopVersion: '1.0.0', desktopChannel: 'canary', desktopArch: 'x64', arch: 'x64'}),
}));

installVoiceMenuTestBootstrap();

const REPORTED_VERSION = '2026.908.173325';
const NEWER_VERSION = '2026.909.202036';
const CANARY_X64 = 'https://api.canary.fluxer.app/dl/desktop/canary/linux/x64';
const REPORTED_DEB_DOWNLOAD = [
	`${CANARY_X64}/${REPORTED_VERSION}/deb`,
	`Fluxer-Canary-${REPORTED_VERSION}-linux-amd64.deb`,
];
const NEWER_DEB_DOWNLOAD = [`${CANARY_X64}/${NEWER_VERSION}/deb`, `Fluxer-Canary-${NEWER_VERSION}-linux-amd64.deb`];

let nativeEventListener: ((event: UpdaterEvent) => void) | null = null;
let onUpdaterCheck: ((context: string) => void | Promise<void>) | null = null;
let loadedUpdater: {dispose: () => void} | null = null;

const updaterCheck = vi.fn((context: string) => Promise.resolve(onUpdaterCheck?.(context)));
const downloadFile = vi.fn<(url: string, suggestedName: string) => Promise<{success: boolean}>>(() =>
	Promise.resolve({success: true}),
);
const openExternal = vi.fn<(url: string) => Promise<void>>(() => Promise.resolve());

function installElectronApi(platform: string): void {
	nativeEventListener = null;
	onUpdaterCheck = null;
	(window as unknown as {electron: unknown}).electron = {
		platform,
		buildChannel: 'canary',
		onUpdaterEvent: (listener: (event: UpdaterEvent) => void) => {
			nativeEventListener = listener;
			return () => {
				nativeEventListener = null;
			};
		},
		updaterCheck,
		updaterDownload: () => Promise.resolve(),
		updaterInstall: () => Promise.resolve(),
		downloadFile,
		openExternal,
	};
}

function emit(event: UpdaterEvent): void {
	if (!nativeEventListener) throw new Error('Updater never subscribed to native updater events');
	nativeEventListener(event);
}

async function loadUpdater(platform = 'win32') {
	vi.resetModules();
	installElectronApi(platform);
	const {default: Updater} = await import('@app/features/app/state/Updater');
	loadedUpdater = Updater;
	await vi.waitFor(() => {
		expect(nativeEventListener).not.toBeNull();
		expect(Updater.lastCheckedAt).not.toBeNull();
	});
	vi.clearAllMocks();
	return Updater;
}

function emitUserDownloadCompletion(version: string): void {
	emit({type: 'available', context: 'user', version, downloadSize: 1000, downloadStarted: true});
	emit({type: 'downloaded', context: 'user', version});
}

function emitLinuxManualUpdate(context: 'user' | 'background', version: string): void {
	emit({
		type: 'available',
		context,
		version,
		downloadStarted: false,
		downloadUrl: `${CANARY_X64}/latest/appimage`,
		downloadOptions: [
			{
				format: 'deb',
				label: 'DEB package',
				url: `${CANARY_X64}/latest/deb`,
				suggestedName: `Fluxer-Canary-${version}-linux-amd64.deb`,
				sha256: `deb-${version}`,
			},
		],
	});
}

async function openLinuxManualUpdatePrompt() {
	const Updater = await loadUpdater('linux');
	emitLinuxManualUpdate('background', REPORTED_VERSION);
	await Updater.applyUpdate();
	const prompt = pushManualUpdateAvailableModal.mock.lastCall?.[0];
	const debOption = prompt?.options.find((option) => option.format === 'deb');
	if (!prompt || !debOption) throw new Error('Manual update prompt never offered a deb package');
	return {Updater, download: () => Promise.resolve(prompt.onDownload(debOption))};
}

afterEach(() => {
	vi.useRealTimers();
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

describe('updater manual Linux package download', () => {
	test('refreshes the check as the user and saves a newer build under its own name', async () => {
		const {Updater, download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => {
			emit({type: 'checking', context: 'user'});
			emitLinuxManualUpdate('user', NEWER_VERSION);
		};
		await download();
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([NEWER_DEB_DOWNLOAD]);
		expect(pushManualUpdateAvailableModal).toHaveBeenCalledTimes(1);
		expect(Updater.displayVersion).toBe(NEWER_VERSION);
		expect(Updater.isChecking).toBe(false);
	});

	test('saves the build the prompt named when the refresh fails', async () => {
		const {download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => emit({type: 'error', context: 'user', message: 'fetch failed'});
		await download();
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD]);
		expect(pushUpdateCheckFailedModal).not.toHaveBeenCalled();
	});

	test('saves the build the prompt named when the refresh finds no update', async () => {
		const {download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => emit({type: 'not-available', context: 'user'});
		await download();
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD]);
		expect(pushUpToDateModal).not.toHaveBeenCalled();
	});

	test('saves the build the prompt named when the refresh reports an unsupported install', async () => {
		const {download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => emit({type: 'unsupported', context: 'user', reason: 'managed-package'});
		await download();
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD]);
		expect(pushUnsupportedUpdateModal).not.toHaveBeenCalled();
	});

	test('saves the build the prompt named when the refresh does not answer in time', async () => {
		const {Updater, download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => new Promise<void>(() => {});
		vi.useFakeTimers();
		const downloaded = download();
		await vi.advanceTimersByTimeAsync(5_000);
		await downloaded;
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD]);
		expect(Updater.isChecking).toBe(false);
	});

	test.each([
		['a newer build', () => emitLinuxManualUpdate('user', NEWER_VERSION)],
		['no update', () => emit({type: 'not-available', context: 'user'})],
		['a failure', () => emit({type: 'error', context: 'user', message: 'fetch failed'})],
	])('stays quiet when a timed out refresh answers with %s after the download', async (_answer, answerLate) => {
		const {download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => new Promise<void>(() => {});
		vi.useFakeTimers();
		const downloaded = download();
		await vi.advanceTimersByTimeAsync(5_000);
		await downloaded;
		answerLate();
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD]);
		expect(pushManualUpdateAvailableModal).toHaveBeenCalledTimes(1);
		expect(pushUpToDateModal).not.toHaveBeenCalled();
		expect(pushUpdateCheckFailedModal).not.toHaveBeenCalled();
	});

	test('saves the refreshed build when a late failure from an earlier refresh lands inside the next one', async () => {
		const {download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => new Promise<void>(() => {});
		vi.useFakeTimers();
		const firstDownload = download();
		await vi.advanceTimersByTimeAsync(5_000);
		await firstDownload;
		onUpdaterCheck = () => {
			emit({type: 'error', context: 'user', message: 'fetch failed'});
			emitLinuxManualUpdate('user', NEWER_VERSION);
		};
		await download();
		expect(updaterCheck.mock.calls).toEqual([['user'], ['user']]);
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD, NEWER_DEB_DOWNLOAD]);
		expect(pushManualUpdateAvailableModal).toHaveBeenCalledTimes(1);
		expect(pushUpdateCheckFailedModal).not.toHaveBeenCalled();
	});

	test('still shows the failure of a user check made while a package is being saved', async () => {
		const {Updater, download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => emitLinuxManualUpdate('user', REPORTED_VERSION);
		downloadFile.mockImplementationOnce(() => new Promise<{success: boolean}>(() => {}));
		void download();
		await vi.waitFor(() => expect(downloadFile).toHaveBeenCalledTimes(1));
		onUpdaterCheck = () => emit({type: 'error', context: 'user', message: 'fetch failed'});
		await Updater.checkForUpdates(true, true);
		expect(updaterCheck.mock.calls).toEqual([['user'], ['user']]);
		expect(pushUpdateCheckFailedModal).toHaveBeenCalledTimes(1);
	});

	test('does not refresh when a check is already running at the click', async () => {
		const {Updater, download} = await openLinuxManualUpdatePrompt();
		void Updater.checkForUpdates(true);
		await download();
		expect(updaterCheck).not.toHaveBeenCalled();
		expect(downloadFile.mock.calls).toEqual([REPORTED_DEB_DOWNLOAD]);
	});

	test('saves one package when the button is pressed twice during the refresh', async () => {
		const {download} = await openLinuxManualUpdatePrompt();
		let answerRefresh = () => {};
		onUpdaterCheck = () =>
			new Promise<void>((resolve) => {
				answerRefresh = () => {
					emitLinuxManualUpdate('user', NEWER_VERSION);
					resolve();
				};
			});
		const firstClick = download();
		const secondClick = download();
		answerRefresh();
		await Promise.all([firstClick, secondClick]);
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(downloadFile.mock.calls).toEqual([NEWER_DEB_DOWNLOAD]);
	});

	test('opens the pinned link and never the download page when saving fails', async () => {
		const {download} = await openLinuxManualUpdatePrompt();
		onUpdaterCheck = () => emitLinuxManualUpdate('user', REPORTED_VERSION);
		downloadFile.mockResolvedValueOnce({success: false});
		await download();
		expect(updaterCheck.mock.calls).toEqual([['user']]);
		expect(openExternal.mock.calls).toEqual([[`${CANARY_X64}/${REPORTED_VERSION}/deb`]]);
	});
});

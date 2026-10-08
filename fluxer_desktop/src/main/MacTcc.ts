// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {hasRequestedMacScreenRecording, markMacScreenRecordingRequested} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import {ipcMain, systemPreferences} from 'electron';

const logger = createChildLogger('MacTcc');
const requireModule = createRequire(import.meta.url);

type TccStatus = 'granted' | 'denied' | 'not-determined';
type TccSurface = 'screen-recording' | 'input-monitoring';

interface MacTccModule {
	screenRecordingStatus: () => TccStatus;
	requestScreenRecording: () => TccStatus;
	inputMonitoringStatus: () => TccStatus;
	requestInputMonitoring: () => TccStatus;
	probeInputMonitoringAccess: () => TccStatus;
	loadError: Error | null;
}

interface ScreenRecordingProbeModule {
	probeScreenRecordingAccess?: () => Promise<string>;
	loadError: Error | null;
}

interface MacTccIpcOptions {
	onStatus?: (surface: TccSurface, status: TccStatus) => void;
}

let cached: MacTccModule | null | undefined;
let cachedProbe: ScreenRecordingProbeModule | null | undefined;
let screenRecordingLiveGranted = false;
let screenRecordingProbeInFlight: Promise<void> | null = null;
let inputMonitoringRequested = false;

function loadAddon(): MacTccModule | null {
	if (cached !== undefined) return cached;
	if (process.platform !== 'darwin') {
		cached = null;
		return cached;
	}
	try {
		const mod = requireModule('@fluxer/mac-tcc') as MacTccModule;
		if (mod.loadError) {
			logger.info('@fluxer/mac-tcc reported load error', {error: mod.loadError});
			cached = null;
			return cached;
		}
		cached = mod;
		return cached;
	} catch (error) {
		logger.info('@fluxer/mac-tcc not available; TCC pre-flight disabled', {error});
		cached = null;
		return cached;
	}
}

function loadProbe(): ScreenRecordingProbeModule | null {
	if (cachedProbe !== undefined) return cachedProbe;
	try {
		const mod = requireModule('@fluxer/mac-screen-capture') as ScreenRecordingProbeModule;
		cachedProbe = mod.loadError || typeof mod.probeScreenRecordingAccess !== 'function' ? null : mod;
	} catch (error) {
		logger.info('@fluxer/mac-screen-capture not available, live screen recording probe disabled', {error});
		cachedProbe = null;
	}
	return cachedProbe;
}

function screenRecordingStatusWithoutAddon(): TccStatus {
	switch (systemPreferences.getMediaAccessStatus('screen')) {
		case 'granted':
			return 'granted';
		case 'denied':
		case 'restricted':
			return 'denied';
		default:
			return 'not-determined';
	}
}

function statusWithoutAddon(surface: TccSurface): TccStatus {
	if (process.platform !== 'darwin') return 'not-determined';
	switch (surface) {
		case 'screen-recording':
			return screenRecordingStatusWithoutAddon();
		case 'input-monitoring':
			return 'not-determined';
	}
}

function screenRecordingStatus(mod: MacTccModule): TccStatus {
	if (mod.screenRecordingStatus() === 'granted' || screenRecordingLiveGranted) return 'granted';
	return hasRequestedMacScreenRecording() ? 'denied' : 'not-determined';
}

function inputMonitoringStatus(mod: MacTccModule): TccStatus {
	if (mod.inputMonitoringStatus() === 'not-determined' && !inputMonitoringRequested) return 'not-determined';
	return mod.probeInputMonitoringAccess() === 'granted' ? 'granted' : 'denied';
}

function statusOf(surface: TccSurface): TccStatus {
	const mod = loadAddon();
	if (!mod) return statusWithoutAddon(surface);
	switch (surface) {
		case 'screen-recording':
			return screenRecordingStatus(mod);
		case 'input-monitoring':
			return inputMonitoringStatus(mod);
	}
}

async function probeScreenRecording(probe: ScreenRecordingProbeModule): Promise<void> {
	try {
		const result = await probe.probeScreenRecordingAccess?.();
		if (result === 'granted') {
			screenRecordingLiveGranted = true;
		} else if (result === 'denied') {
			screenRecordingLiveGranted = false;
		}
	} catch (error) {
		logger.warn('Live screen recording probe failed', {error});
	}
}

async function refreshScreenRecordingLiveGrant(): Promise<void> {
	const mod = loadAddon();
	if (!mod || mod.screenRecordingStatus() === 'granted' || !hasRequestedMacScreenRecording()) {
		screenRecordingLiveGranted = false;
		return;
	}
	const probe = loadProbe();
	if (!probe) return;
	screenRecordingProbeInFlight ??= probeScreenRecording(probe).finally(() => {
		screenRecordingProbeInFlight = null;
	});
	await screenRecordingProbeInFlight;
}

function requestOf(surface: TccSurface): TccStatus {
	const mod = loadAddon();
	if (!mod) return statusWithoutAddon(surface);
	switch (surface) {
		case 'screen-recording': {
			const requested = mod.requestScreenRecording();
			markMacScreenRecordingRequested();
			return requested === 'granted' ? 'granted' : screenRecordingStatus(mod);
		}
		case 'input-monitoring': {
			const requested = mod.requestInputMonitoring();
			inputMonitoringRequested = true;
			return requested === 'granted' ? 'granted' : inputMonitoringStatus(mod);
		}
	}
}

export function getTccStatus(surface: TccSurface): TccStatus {
	return statusOf(surface);
}

export async function refreshTccStatus(surface: TccSurface): Promise<TccStatus> {
	if (surface === 'screen-recording') await refreshScreenRecordingLiveGrant();
	return statusOf(surface);
}

let registered = false;

export function registerMacTccIpcHandlers(options: MacTccIpcOptions = {}): void {
	if (registered) return;
	registered = true;
	ipcMain.handle('mac-tcc:status', async (_event, surface: TccSurface): Promise<TccStatus> => {
		const status = await refreshTccStatus(surface);
		options.onStatus?.(surface, status);
		return status;
	});
	ipcMain.handle('mac-tcc:request', (_event, surface: TccSurface): TccStatus => {
		const status = requestOf(surface);
		options.onStatus?.(surface, status);
		return status;
	});
}

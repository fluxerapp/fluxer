// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import type {VirtmicAvailability, VirtmicNode, VirtmicUnavailableReason} from '@electron/common/Types';
import {isFluxerAudioNode} from '@electron/main/FluxerAudioIdentity';
import {getLinuxPortalsMode, getNativeAudioMode} from '@electron/main/LaunchOptions';
import {
	isDBusObjectPathSegment,
	isX11WindowToken,
	parseWindowSourceToken,
} from '@electron/main/LinuxAudioCaptureHelpers';
import {isWaylandSession, isX11Session} from '@electron/main/LinuxSession';
import {ipcMain} from 'electron';

const logger = createChildLogger('LinuxAudioCapture');
const requireModule = createRequire(import.meta.url);

interface AudioBridgeInstance {
	inventory: (fields?: ReadonlyArray<string> | null) => Array<VirtmicNode>;
	release: () => void;
	backend?: () => 'pipewire' | 'none';
}

interface AudioBridgeCtor {
	new (): AudioBridgeInstance;
}

interface AudioCaptureModule {
	AudioBridge: AudioBridgeCtor;
	pipeWireAvailable: () => boolean;
	audioBackend?: () => 'pipewire' | 'none';
}

interface LoadResult {
	mod?: AudioCaptureModule;
	availability: VirtmicAvailability;
}

let cachedLoad: LoadResult | undefined;
let instance: AudioBridgeInstance | undefined;

const LINUX_AUDIO_TARGET_INVENTORY_FIELDS = [
	'media.class',
	'media.name',
	'media.title',
	'node.name',
	'node.nick',
	'node.description',
	'node.virtual',
	'device.id',
	'application.name',
	'application.process.binary',
	'application.process.id',
	'pipewire.sec.pid',
	'client.id',
	'object.serial',
] as const;

function unavailable(reason: VirtmicUnavailableReason): LoadResult {
	return {availability: {available: false, reason}};
}

function loadAddon(): LoadResult {
	if (cachedLoad) return cachedLoad;
	if (process.platform !== 'linux') {
		cachedLoad = unavailable('not-linux');
		return cachedLoad;
	}
	if (getNativeAudioMode(process.argv) === 'off') {
		cachedLoad = unavailable('disabled-by-launch');
		return cachedLoad;
	}
	let mod: AudioCaptureModule | undefined;
	try {
		mod = requireModule('@fluxer/linux-audio-capture') as AudioCaptureModule;
	} catch (error) {
		const message = String(
			(
				error as {
					message?: string;
				}
			)?.message ??
				error ??
				'',
		).toLowerCase();
		if (message.includes('cannot find module') || message.includes('module_not_found')) {
			logger.info('linux-audio-capture addon not built; per-app audio capture disabled');
			cachedLoad = unavailable('addon-not-installed');
			return cachedLoad;
		}
		logger.warn('linux-audio-capture addon load failed; per-app audio capture disabled', error);
		cachedLoad = unavailable('load-failed');
		return cachedLoad;
	}
	if (!mod?.AudioBridge || typeof mod.pipeWireAvailable !== 'function') {
		logger.warn('linux-audio-capture addon loaded but is missing the expected exports');
		cachedLoad = unavailable('load-failed');
		return cachedLoad;
	}
	let pipewireReachable = false;
	try {
		pipewireReachable = mod.pipeWireAvailable();
	} catch (error) {
		logger.warn('linux-audio-capture daemon probe threw; per-app audio capture disabled', error);
		cachedLoad = unavailable('load-failed');
		return cachedLoad;
	}
	if (!pipewireReachable) {
		cachedLoad = unavailable('no-pipewire');
		return cachedLoad;
	}
	cachedLoad = {mod, availability: {available: true, backend: 'pipewire'}};
	return cachedLoad;
}

function getInstance(): AudioBridgeInstance | undefined {
	const {mod, availability} = loadAddon();
	if (!availability.available || !mod) return undefined;
	if (!instance) {
		try {
			instance = new mod.AudioBridge();
		} catch (error) {
			logger.warn('Failed to instantiate AudioBridge', error);
			return undefined;
		}
	}
	return instance;
}

function getVirtmicAvailability(): VirtmicAvailability {
	return loadAddon().availability;
}

function listVirtmicTargets(_options?: {granular?: boolean}): {
	ok: boolean;
	targets?: Array<VirtmicNode>;
	availability: VirtmicAvailability;
} {
	const availability = getVirtmicAvailability();
	if (!availability.available) return {ok: false, availability};
	const bay = getInstance();
	if (!bay) return {ok: false, availability: {available: false, reason: 'load-failed'}};
	const props = Array.from(LINUX_AUDIO_TARGET_INVENTORY_FIELDS);
	try {
		const raw = bay.inventory(props);
		const targets = raw.filter((node) => !isFluxerAudioNode(node));
		if (raw.length !== targets.length) {
		}
		return {ok: true, targets, availability};
	} catch (error) {
		logger.warn('AudioBridge.inventory() threw', error);
		return {ok: false, availability};
	}
}

async function resolveWindowPidViaX11(xid: string): Promise<number | null> {
	if (getLinuxPortalsMode(process.argv) === 'off') return null;
	const portals = requireModule('@fluxer/linux-portals') as {
		resolveX11WindowPid: ((token: string) => Promise<number | null>) | null;
	};
	if (!portals.resolveX11WindowPid) {
		logger.warn('@fluxer/linux-portals.resolveX11WindowPid not available; X11 pid lookup skipped', {xid});
		return null;
	}
	try {
		const pid = await portals.resolveX11WindowPid(xid);
		return typeof pid === 'number' && Number.isFinite(pid) && pid > 0 ? pid : null;
	} catch (error) {
		logger.debug('native X11 _NET_WM_PID lookup failed', {xid, error});
		return null;
	}
}

async function resolveWindowPidViaKWin(token: string): Promise<number | null> {
	if (getLinuxPortalsMode(process.argv) === 'off') return null;
	if (!process.env.KDE_FULL_SESSION && !process.env.XDG_CURRENT_DESKTOP?.toLowerCase().includes('kde')) {
		return null;
	}
	if (!isDBusObjectPathSegment(token)) return null;
	const portals = requireModule('@fluxer/linux-portals') as {
		resolveKwinWindowPid: ((token: string) => Promise<number | null>) | null;
	};
	if (!portals.resolveKwinWindowPid) {
		logger.warn('@fluxer/linux-portals.resolveKwinWindowPid not available; KWin pid lookup skipped', {token});
		return null;
	}
	try {
		return await portals.resolveKwinWindowPid(token);
	} catch (error) {
		logger.debug('KWin window pid lookup failed', {token, error});
		return null;
	}
}

async function resolveWindowPidViaGnomeShell(token: string): Promise<number | null> {
	if (getLinuxPortalsMode(process.argv) === 'off') return null;
	const desktop = (process.env.XDG_CURRENT_DESKTOP ?? '').toLowerCase();
	if (!desktop.includes('gnome')) return null;
	if (!isDBusObjectPathSegment(token)) return null;
	const portals = requireModule('@fluxer/linux-portals') as {
		resolveWindowPid: ((spec: {backend: 'gnome-shell-eval'; token: string}) => Promise<number | null>) | null;
	};
	if (!portals.resolveWindowPid) {
		logger.warn('@fluxer/linux-portals.resolveWindowPid not available; GNOME pid lookup skipped', {token});
		return null;
	}
	try {
		const pid = await portals.resolveWindowPid({backend: 'gnome-shell-eval', token});
		return typeof pid === 'number' && Number.isFinite(pid) && pid > 0 ? pid : null;
	} catch (error) {
		logger.debug('gnome-shell Eval window pid lookup failed', {token, error});
		return null;
	}
}

export async function resolveVirtmicWindowPid(sourceId: unknown): Promise<number | null> {
	if (process.platform !== 'linux') return null;
	const token = parseWindowSourceToken(sourceId);
	if (!token) return null;
	if (isX11Session() && isX11WindowToken(token)) {
		const pid = await resolveWindowPidViaX11(token);
		if (pid) return pid;
	}
	if (isWaylandSession()) {
		const kwinPid = await resolveWindowPidViaKWin(token);
		if (kwinPid) return kwinPid;
		const gnomePid = await resolveWindowPidViaGnomeShell(token);
		if (gnomePid) return gnomePid;
	}
	return null;
}

let handlersRegistered = false;

export function registerVirtmicHandlers(): void {
	if (handlersRegistered) return;
	handlersRegistered = true;
	ipcMain.handle('virtmic:get-availability', (): VirtmicAvailability => getVirtmicAvailability());
	ipcMain.handle(
		'virtmic:list',
		(
			_event,
			options?: {
				granular?: boolean;
			},
		) => listVirtmicTargets(options),
	);
	ipcMain.handle('virtmic:stop', (): void => {});
}

export function cleanupVirtmic(): void {
	if (!handlersRegistered) return;
	ipcMain.removeHandler('virtmic:get-availability');
	ipcMain.removeHandler('virtmic:list');
	ipcMain.removeHandler('virtmic:stop');
	handlersRegistered = false;
}

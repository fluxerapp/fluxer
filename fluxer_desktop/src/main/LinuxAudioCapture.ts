// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import type {
	VirtmicAvailability,
	VirtmicLinkOptions,
	VirtmicNode,
	VirtmicRoutingGraph,
	VirtmicRoutingGraphResult,
	VirtmicUnavailableReason,
} from '@electron/common/Types';
import {buildFluxerAudioExcludePatterns, isFluxerAudioNode} from '@electron/main/FluxerAudioIdentity';
import {getLinuxPortalsMode, getNativeAudioMode} from '@electron/main/LaunchOptions';
import {
	isDBusObjectPathSegment,
	isWaylandSessionEnv,
	isX11SessionEnv,
	isX11WindowToken,
	parseWindowSourceToken,
} from '@electron/main/LinuxAudioCaptureHelpers';
import {
	isValidVirtmicLinkOptions,
	isValidVirtmicNodeList,
	isValidVirtmicSystemLinkOptions,
} from '@electron/main/NativeAudioValidation';
import {app, ipcMain} from 'electron';

const logger = createChildLogger('LinuxAudioCapture');
const requireModule = createRequire(import.meta.url);

interface RoutingRule {
	include?: ReadonlyArray<VirtmicNode>;
	exclude?: ReadonlyArray<VirtmicNode>;
	ignore_devices?: boolean;
	only_speakers?: boolean;
	only_default_speakers?: boolean;
	workaround?: ReadonlyArray<VirtmicNode>;
}

interface AudioBridgeInstance {
	inventory: (fields?: ReadonlyArray<string> | null) => Array<VirtmicNode>;
	routingGraph?: () => VirtmicRoutingGraph;
	apply: (rule: RoutingRule) => boolean;
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
let linkActive = false;
let lastRule: RoutingRule | undefined;
let relinkTimers: Array<NodeJS.Timeout> = [];

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

function clearRelinkTimers(): void {
	for (const timer of relinkTimers) clearTimeout(timer);
	relinkTimers = [];
}

function scheduleAudioServiceRelink(): void {
	clearRelinkTimers();
	if (!lastRule) return;
	for (const delay of [250, 1500, 4000]) {
		relinkTimers.push(
			setTimeout(() => {
				if (!linkActive || !lastRule) return;
				try {
					const refreshedRule = refreshDynamicRoutingRule(lastRule);
					if (instance?.apply(refreshedRule)) lastRule = refreshedRule;
				} catch (error) {
					logger.debug('delayed audio-service re-link failed', error);
				}
			}, delay),
		);
	}
}

function mergeFreshExclusions(data: RoutingRule): Array<VirtmicNode> {
	const merged = new Map<string, VirtmicNode>();
	const append = (node: VirtmicNode): void => {
		const key = JSON.stringify(node);
		if (!merged.has(key)) merged.set(key, node);
	};
	for (const entry of data.exclude ?? []) append(entry);
	for (const pattern of buildFluxerAudioExcludePatterns()) append(pattern);
	return Array.from(merged.values());
}

function refreshDynamicRoutingRule(data: RoutingRule): RoutingRule {
	const refreshed: RoutingRule = {...data, exclude: mergeFreshExclusions(data)};
	const workaround = buildRecordStreamPinTarget();
	if (workaround) refreshed.workaround = workaround;
	return refreshed;
}

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

function getRendererAudioServicePid(): string | undefined {
	try {
		const metrics = app.getAppMetrics();
		const audioService = metrics.find(
			(proc) =>
				(
					proc as {
						serviceName?: string;
					}
				).serviceName === 'audio.mojom.AudioService' || proc.name === 'Audio Service',
		);
		return audioService?.pid?.toString();
	} catch {
		return undefined;
	}
}

function buildExclusions(
	options: VirtmicLinkOptions,
	extraExcludes: ReadonlyArray<VirtmicNode> = [],
): Array<VirtmicNode> {
	const ignoreInputMedia = options.ignoreInputMedia ?? true;
	const excludes: Array<VirtmicNode> = [];
	excludes.push(...buildFluxerAudioExcludePatterns());
	for (const entry of extraExcludes) excludes.push(entry);
	if (ignoreInputMedia) excludes.push({'media.class': 'Stream/Input/Audio'});
	if (options.ignoreVirtual) excludes.push({'node.virtual': 'true'});
	return excludes;
}

function buildRecordStreamPinTarget(): Array<VirtmicNode> | undefined {
	const audioPid = getRendererAudioServicePid();
	if (!audioPid) return undefined;
	return [{'application.process.id': audioPid, 'media.name': 'RecordStream'}];
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

function getVirtmicRoutingGraph(): VirtmicRoutingGraphResult {
	const availability = getVirtmicAvailability();
	if (!availability.available) return {ok: false, availability};
	const bay = getInstance();
	if (!bay || typeof bay.routingGraph !== 'function') {
		return {ok: false, availability: {available: false, reason: 'load-failed'}};
	}
	try {
		return {ok: true, graph: bay.routingGraph(), availability};
	} catch (error) {
		logger.warn('AudioBridge.routingGraph() threw', error);
		return {ok: false, availability};
	}
}

function startVirtmicInclude(include: unknown, options: unknown = {}): boolean {
	if (!isValidVirtmicNodeList(include) || !isValidVirtmicLinkOptions(options)) {
		return false;
	}
	const bay = getInstance();
	if (!bay) {
		return false;
	}
	const data: RoutingRule = {
		include,
		exclude: buildExclusions(options),
		ignore_devices: options.ignoreDevices ?? true,
	};
	const workaround = buildRecordStreamPinTarget();
	if (workaround) data.workaround = workaround;
	try {
		const ok = bay.apply(data);
		if (ok) {
			linkActive = true;
			lastRule = data;
			scheduleAudioServiceRelink();
		}
		return ok;
	} catch (error) {
		logger.warn('AudioBridge.apply(include) threw', error);
		return false;
	}
}

function startVirtmicSystem(exclude: unknown, options: unknown = {}): boolean {
	if (!isValidVirtmicNodeList(exclude) || !isValidVirtmicSystemLinkOptions(options)) {
		return false;
	}
	const bay = getInstance();
	if (!bay) {
		return false;
	}
	const data: RoutingRule = {
		include: options.onlySpeakers === false ? [{'media.class': 'Stream/Output/Audio'}] : [],
		exclude: buildExclusions(options, exclude),
		ignore_devices: options.ignoreDevices ?? true,
		only_speakers: options.onlySpeakers ?? true,
		only_default_speakers: options.onlyDefaultSpeakers ?? true,
	};
	const workaround = buildRecordStreamPinTarget();
	if (workaround) data.workaround = workaround;
	try {
		const ok = bay.apply(data);
		if (ok) {
			linkActive = true;
			lastRule = data;
			scheduleAudioServiceRelink();
		}
		return ok;
	} catch (error) {
		logger.warn('AudioBridge.apply(system) threw', error);
		return false;
	}
}

function isX11Session(): boolean {
	return isX11SessionEnv(process.env);
}

function isWaylandSession(): boolean {
	return isWaylandSessionEnv(process.env);
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

function stopVirtmic(): void {
	clearRelinkTimers();
	lastRule = undefined;
	if (!instance || !linkActive) return;
	try {
		instance.release();
	} catch (error) {
		logger.warn('AudioBridge.release() threw', error);
	} finally {
		linkActive = false;
	}
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
	ipcMain.handle('virtmic:get-routing-graph', (): VirtmicRoutingGraphResult => getVirtmicRoutingGraph());
	ipcMain.handle('virtmic:start-include', (_event, include: unknown, options?: unknown): boolean =>
		startVirtmicInclude(include, options ?? {}),
	);
	ipcMain.handle('virtmic:start-system', (_event, exclude: unknown, options?: unknown): boolean =>
		startVirtmicSystem(exclude, options ?? {}),
	);
	ipcMain.handle(
		'virtmic:resolve-window-pid',
		(_event, sourceId: unknown): Promise<number | null> => resolveVirtmicWindowPid(sourceId),
	);
	ipcMain.handle('virtmic:stop', (): void => stopVirtmic());
}

export function cleanupVirtmic(): void {
	stopVirtmic();
	if (!handlersRegistered) return;
	ipcMain.removeHandler('virtmic:get-availability');
	ipcMain.removeHandler('virtmic:list');
	ipcMain.removeHandler('virtmic:get-routing-graph');
	ipcMain.removeHandler('virtmic:start-include');
	ipcMain.removeHandler('virtmic:start-system');
	ipcMain.removeHandler('virtmic:resolve-window-pid');
	ipcMain.removeHandler('virtmic:stop');
	handlersRegistered = false;
}

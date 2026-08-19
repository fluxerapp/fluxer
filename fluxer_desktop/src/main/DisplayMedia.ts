// SPDX-License-Identifier: AGPL-3.0-or-later

import {BrowserWindow, desktopCapturer, ipcMain, screen} from 'electron';
import log from 'electron-log';
import {
	isListOnlyDesktopSourcesOption,
	isValidDesktopSourceId,
	isValidDisplayMediaRequestId,
	normalizeDesktopSourceTypes,
	shouldHonorSelectedAudio,
} from './DisplayMediaValidation';
import {startWindowsScreenCaptureGuardForSource} from './WindowsScreenCaptureGuard';

type DisplayMediaPortalSurfacePreference = 'window' | 'monitor';

interface PendingDisplayMediaRequest {
	callback: (streams: Electron.Streams | null) => void;
	senderId: number;
	audioRequested: boolean;
	cachedSources?: Array<Electron.DesktopCapturerSource>;
}

const pendingDisplayMediaRequests = new Map<string, PendingDisplayMediaRequest>();
const MAX_PENDING_DISPLAY_MEDIA_REQUESTS = 8;
const DESKTOP_SOURCE_CACHE_TTL_MS = 60000;
const DESKTOP_SOURCE_THUMBNAIL_SIZE = {width: 960, height: 540};
const DESKTOP_SOURCE_LIST_ONLY_THUMBNAIL_SIZE = {width: 0, height: 0};

let displayMediaRequestCounter = 0;
let latestDesktopSources: Array<Electron.DesktopCapturerSource> = [];
let latestDesktopSourcesTimestamp = 0;
let nextWaylandPortalSurfacePreference: DisplayMediaPortalSurfacePreference | null = null;
let displayMediaHandlersRegistered = false;

function isUsableDesktopSourceDataUrl(value?: string | null): value is string {
	if (!value) {
		return false;
	}
	const trimmedValue = value.trim();
	if (!trimmedValue.startsWith('data:image/')) {
		return false;
	}
	const base64MarkerIndex = trimmedValue.indexOf('base64,');
	return base64MarkerIndex >= 0 && trimmedValue.length > base64MarkerIndex + 'base64,'.length;
}

function nativeImageToDataUrl(image?: Electron.NativeImage | null): string | undefined {
	if (!image || image.isEmpty()) {
		return undefined;
	}
	const dataUrl = image.toDataURL();
	return isUsableDesktopSourceDataUrl(dataUrl) ? dataUrl : undefined;
}

function desktopSourceHasThumbnail(source: Electron.DesktopCapturerSource): boolean {
	return Boolean(source.thumbnail && !source.thumbnail.isEmpty());
}

function isScreenDesktopSource(source: Electron.DesktopCapturerSource): boolean {
	return source.id.startsWith('screen:');
}

function mergeMissingScreenThumbnails(
	sources: Array<Electron.DesktopCapturerSource>,
	screenSources: Array<Electron.DesktopCapturerSource>,
): Array<Electron.DesktopCapturerSource> {
	const screenSourcesById = new Map(screenSources.map((source) => [source.id, source]));
	const screenSourcesByDisplayId = new Map(
		screenSources.filter((source) => source.display_id).map((source) => [source.display_id, source]),
	);
	let changed = false;
	const mergedSources = sources.map((source) => {
		if (!isScreenDesktopSource(source) || desktopSourceHasThumbnail(source)) {
			return source;
		}
		const retrySource = screenSourcesById.get(source.id) ?? screenSourcesByDisplayId.get(source.display_id);
		if (!retrySource || !desktopSourceHasThumbnail(retrySource)) {
			return source;
		}
		changed = true;
		return retrySource;
	});
	return changed ? mergedSources : sources;
}

function resolveSelectedDesktopSource(
	sources: Array<Electron.DesktopCapturerSource>,
	requestedSourceId: string,
): Electron.DesktopCapturerSource | null {
	const exactMatch = sources.find((source) => source.id === requestedSourceId);
	if (exactMatch) {
		return exactMatch;
	}
	return null;
}

function collectOwnWindowMediaSourceIds(): Set<string> {
	const ids = new Set<string>();
	try {
		for (const browserWindow of BrowserWindow.getAllWindows()) {
			if (browserWindow.isDestroyed()) continue;
			try {
				const mediaSourceId = browserWindow.getMediaSourceId();
				if (mediaSourceId) ids.add(mediaSourceId);
			} catch (error) {
				log.debug('[DisplayMedia] getMediaSourceId failed for window', {error});
			}
		}
	} catch (error) {
		log.debug('[DisplayMedia] Failed to enumerate Fluxer windows for own-source detection', {error});
	}
	return ids;
}

function _isOwnWindowSourceId(sourceId: string): boolean {
	return collectOwnWindowMediaSourceIds().has(sourceId);
}

function isWaylandSession(): boolean {
	return (
		process.platform === 'linux' && (Boolean(process.env.WAYLAND_DISPLAY) || process.env.XDG_SESSION_TYPE === 'wayland')
	);
}

function consumeWaylandPortalSurfacePreference(): DisplayMediaPortalSurfacePreference | null {
	const preference = nextWaylandPortalSurfacePreference;
	nextWaylandPortalSurfacePreference = null;
	return preference;
}

const DISPLAY_MEDIA_PORTAL_EMPTY_CHANNEL = 'display-media-portal-empty';

type WaylandPortalUnavailableReason = 'empty' | 'error';

type WaylandPortalSourceProvider = (
	preference: DisplayMediaPortalSurfacePreference | null,
) => Promise<Array<Electron.DesktopCapturerSource>>;

type WaylandPortalNotifier = (requestId: string, reason: WaylandPortalUnavailableReason) => void;

interface WaylandPortalResolutionContext {
	requestId: string;
	preference: DisplayMediaPortalSurfacePreference | null;
	getSources: WaylandPortalSourceProvider;
	notifyUnavailable: WaylandPortalNotifier;
}

async function defaultWaylandPortalSourceProvider(
	preference: DisplayMediaPortalSurfacePreference | null,
): Promise<Array<Electron.DesktopCapturerSource>> {
	const types: Array<'screen' | 'window'> = preference === 'window' ? ['window'] : ['screen'];
	return desktopCapturer.getSources({types});
}

async function resolveWaylandPortalDisplayMedia(
	context: WaylandPortalResolutionContext,
): Promise<Electron.Streams | null> {
	const {requestId, preference, getSources, notifyUnavailable} = context;
	let sources: Array<Electron.DesktopCapturerSource>;
	try {
		sources = await getSources(preference);
	} catch (error) {
		log.error('[DisplayMedia] xdg-desktop-portal picker failed', {requestId, preference, error});
		notifyUnavailable(requestId, 'error');
		return null;
	}
	const video = sources[0] ?? null;
	if (!video) {
		log.warn('[DisplayMedia] xdg-desktop-portal returned no source', {requestId, preference});
		notifyUnavailable(requestId, 'empty');
		return null;
	}
	return {video};
}

function _isWaylandPortalShareActive(): boolean {
	return isWaylandSession();
}

export function drainPendingDisplayMediaRequests(reason: string): void {
	if (pendingDisplayMediaRequests.size === 0) return;
	log.warn('[DisplayMedia] Draining pending requests', {reason, count: pendingDisplayMediaRequests.size});
	for (const [, pending] of pendingDisplayMediaRequests) {
		try {
			pending.callback(null);
		} catch (error) {
			log.warn('[DisplayMedia] Drain callback threw', {reason, error});
		}
	}
	pendingDisplayMediaRequests.clear();
}

export function registerDisplayMediaRequestHandler(session: Electron.Session, webContents: Electron.WebContents): void {
	session.setDisplayMediaRequestHandler((request, callback) => {
		const requestId = `display-media-${++displayMediaRequestCounter}`;
		let callbackInvoked = false;
		const invokeCallback = (streams: Electron.Streams | null): void => {
			if (callbackInvoked) {
				log.warn('[DisplayMedia] Callback already invoked for request:', requestId);
				return;
			}
			callbackInvoked = true;
			try {
				if (streams === null) {
					callback({});
				} else {
					callback(streams);
				}
			} catch (error) {
				log.warn('[DisplayMedia] Callback threw:', {requestId, error});
			}
		};
		const audioRequested = Boolean(request.audioRequested);
		const videoRequested = Boolean(request.videoRequested);
		if (!videoRequested) {
			log.warn('[DisplayMedia] Rejecting request without video stream', {
				requestId,
				audioRequested,
				videoRequested,
			});
			invokeCallback(null);
			return;
		}
		if (isWaylandSession()) {
			const surfacePreference = consumeWaylandPortalSurfacePreference();
			log.info('[DisplayMedia] Wayland session detected; delegating picker to xdg-desktop-portal', {
				audioRequested,
				surfacePreference,
			});
			resolveWaylandPortalDisplayMedia({
				requestId,
				preference: surfacePreference,
				getSources: defaultWaylandPortalSourceProvider,
				notifyUnavailable: (id, _reason) => {
					if (!webContents.isDestroyed()) {
						webContents.send(DISPLAY_MEDIA_PORTAL_EMPTY_CHANNEL, id);
					}
				},
			})
				.then((streams) => {
					invokeCallback(streams);
				})
				.catch((error) => {
					log.error('[DisplayMedia] Wayland portal resolution threw unexpectedly', {requestId, error});
					invokeCallback(null);
				});
			return;
		}
		if (pendingDisplayMediaRequests.size >= MAX_PENDING_DISPLAY_MEDIA_REQUESTS) {
			const oldestKey = pendingDisplayMediaRequests.keys().next().value;
			if (oldestKey !== undefined) {
				const stale = pendingDisplayMediaRequests.get(oldestKey);
				pendingDisplayMediaRequests.delete(oldestKey);
				try {
					stale?.callback(null);
				} catch (error) {
					log.warn('[DisplayMedia] Stale callback threw during eviction', {oldestKey, error});
				}
			}
		}
		pendingDisplayMediaRequests.set(requestId, {callback: invokeCallback, senderId: webContents.id, audioRequested});
		webContents.send('display-media-requested', requestId, {
			audioRequested,
			videoRequested,
			supportsLoopbackAudio: false,
		});
		setTimeout(() => {
			if (pendingDisplayMediaRequests.has(requestId)) {
				log.warn('[DisplayMedia] Request timed out:', requestId);
				pendingDisplayMediaRequests.delete(requestId);
				invokeCallback(null);
			}
		}, 60000);
	});
}

export function registerDisplayMediaHandlers(): void {
	if (displayMediaHandlersRegistered) return;
	displayMediaHandlersRegistered = true;
	ipcMain.handle(
		'set-display-media-portal-preference',
		(_event, preference: DisplayMediaPortalSurfacePreference): void => {
			if (preference !== 'window' && preference !== 'monitor') {
				log.warn('[DisplayMedia] Ignoring invalid portal surface preference', {preference});
				return;
			}
			nextWaylandPortalSurfacePreference = preference;
		},
	);
	ipcMain.handle(
		'get-desktop-sources',
		async (
			event,
			types: unknown,
			requestId?: unknown,
			options?: unknown,
		): Promise<
			Array<{
				id: string;
				name: string;
				thumbnailDataUrl?: string;
				appIconDataUrl?: string;
				display_id?: string;
				nativeWidth?: number;
				nativeHeight?: number;
				isOwnWindow?: boolean;
			}>
		> => {
			const requestedTypes = normalizeDesktopSourceTypes(types);
			const validRequestId = isValidDisplayMediaRequestId(requestId) ? requestId : null;
			const listOnly = isListOnlyDesktopSourcesOption(options);
			try {
				let sources = await desktopCapturer.getSources({
					types: requestedTypes,
					thumbnailSize: listOnly ? DESKTOP_SOURCE_LIST_ONLY_THUMBNAIL_SIZE : DESKTOP_SOURCE_THUMBNAIL_SIZE,
					fetchWindowIcons: !listOnly,
				});
				const shouldRetryMissingScreenThumbnails =
					!listOnly &&
					requestedTypes.includes('screen') &&
					sources.some((source) => isScreenDesktopSource(source) && !desktopSourceHasThumbnail(source));
				if (shouldRetryMissingScreenThumbnails) {
					try {
						const screenSources = await desktopCapturer.getSources({
							types: ['screen'],
							thumbnailSize: DESKTOP_SOURCE_THUMBNAIL_SIZE,
						});
						const nextSources = mergeMissingScreenThumbnails(sources, screenSources);
						const recoveredCount = nextSources.filter(
							(source, index) =>
								isScreenDesktopSource(source) && source !== sources[index] && desktopSourceHasThumbnail(source),
						).length;
						if (recoveredCount > 0) {
							log.debug('[getDesktopSources] Recovered missing screen thumbnails:', {recoveredCount});
						} else {
							log.warn('[getDesktopSources] Screen thumbnail retry returned no usable thumbnails');
						}
						sources = nextSources;
					} catch (retryError) {
						log.warn('[getDesktopSources] Screen thumbnail retry failed:', retryError);
					}
				}
				if (!listOnly) {
					latestDesktopSources = sources;
					latestDesktopSourcesTimestamp = Date.now();
					if (validRequestId) {
						const pending = pendingDisplayMediaRequests.get(validRequestId);
						if (pending && pending.senderId === event.sender.id) {
							pending.cachedSources = sources;
							log.debug('[getDesktopSources] Cached sources for request:', validRequestId);
						}
					}
				}
				const displays = screen.getAllDisplays();
				const nativeDimensionsById = new Map<
					string,
					{
						width: number;
						height: number;
					}
				>();
				for (const display of displays) {
					const scale = display.scaleFactor || 1;
					nativeDimensionsById.set(String(display.id), {
						width: Math.round(display.size.width * scale),
						height: Math.round(display.size.height * scale),
					});
				}
				const ownWindowIds = collectOwnWindowMediaSourceIds();
				const mapped = sources.map((source) => {
					const native = source.display_id ? nativeDimensionsById.get(source.display_id) : undefined;
					return {
						id: source.id,
						name: source.name,
						thumbnailDataUrl: listOnly ? undefined : nativeImageToDataUrl(source.thumbnail),
						appIconDataUrl: listOnly ? undefined : nativeImageToDataUrl(source.appIcon),
						display_id: source.display_id,
						nativeWidth: native?.width,
						nativeHeight: native?.height,
						isOwnWindow: ownWindowIds.has(source.id),
					};
				});
				mapped.sort((a, b) => {
					if (a.isOwnWindow === b.isOwnWindow) return 0;
					return a.isOwnWindow ? 1 : -1;
				});
				return mapped;
			} catch (error) {
				log.error('[getDesktopSources] Failed:', error);
				return [];
			}
		},
	);
	ipcMain.on(
		'select-display-media-source',
		async (event, requestId: unknown, sourceId: unknown, withAudio: unknown) => {
			if (!isValidDisplayMediaRequestId(requestId)) {
				log.warn('[selectDisplayMediaSource] Invalid request id:', requestId);
				return;
			}
			const pending = pendingDisplayMediaRequests.get(requestId);
			if (!pending) {
				log.warn('[selectDisplayMediaSource] No pending request for:', requestId);
				return;
			}
			if (pending.senderId !== event.sender.id) {
				log.warn('[selectDisplayMediaSource] Ignoring selection from non-owner renderer:', {
					requestId,
					ownerSenderId: pending.senderId,
					senderId: event.sender.id,
				});
				return;
			}
			pendingDisplayMediaRequests.delete(requestId);
			if (!sourceId) {
				log.info('[selectDisplayMediaSource] User cancelled');
				pending.callback(null);
				return;
			}
			if (!isValidDesktopSourceId(sourceId)) {
				log.warn('[selectDisplayMediaSource] Invalid source id:', sourceId);
				pending.callback(null);
				return;
			}
			try {
				let sources = pending.cachedSources;
				if (!sources || sources.length === 0) {
					const sourceCacheAgeMs = Date.now() - latestDesktopSourcesTimestamp;
					const hasFreshGlobalCache =
						latestDesktopSources.length > 0 && sourceCacheAgeMs >= 0 && sourceCacheAgeMs <= DESKTOP_SOURCE_CACHE_TTL_MS;
					if (hasFreshGlobalCache && (process.platform !== 'linux' || !isWaylandSession())) {
						log.debug('[selectDisplayMediaSource] Request cache miss, using fresh global source cache:', requestId);
						sources = latestDesktopSources;
					} else if (process.platform === 'linux' && isWaylandSession()) {
						log.warn(
							'[selectDisplayMediaSource] Source cache miss on Wayland; cancelling to avoid duplicate portal picker:',
							requestId,
						);
						pending.callback(null);
						return;
					} else {
						log.debug(
							'[selectDisplayMediaSource] Cache miss, resolving sources from desktopCapturer on non-Linux:',
							requestId,
						);
						sources = await desktopCapturer.getSources({
							types: ['screen', 'window'],
						});
					}
				} else {
					log.debug('[selectDisplayMediaSource] Cache hit for request:', requestId);
				}
				const selectedSource = resolveSelectedDesktopSource(sources, sourceId);
				if (!selectedSource) {
					log.error('[selectDisplayMediaSource] Source not found:', sourceId);
					pending.callback(null);
					return;
				}
				log.info('[selectDisplayMediaSource] Selected source:', {
					id: selectedSource.id,
					name: selectedSource.name,
					withAudio: withAudio === true,
				});
				startWindowsScreenCaptureGuardForSource(selectedSource);
				const _attachAudio = shouldHonorSelectedAudio(pending.audioRequested, withAudio);
				const streams: Electron.Streams = {
					video: selectedSource,
				};
				pending.callback(streams);
			} catch (error) {
				log.error('[selectDisplayMediaSource] Failed:', error);
				pending.callback(null);
			}
		},
	);
}

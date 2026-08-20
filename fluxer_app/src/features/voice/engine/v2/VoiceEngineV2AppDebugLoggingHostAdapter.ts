// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {
	fetchStatus,
	setEnabled,
	uploadEvents,
	type VoiceDebugLoggingEvent,
} from '@app/features/voice/commands/VoiceDebugLoggingCommands';
import {
	appendBrowserVoiceDebugEventSinkEntries,
	openBrowserVoiceDebugEventSinkPopout,
} from '@app/features/voice/diagnostics/VoiceDebugBrowserEventSinkPopout';
import {Store} from '@app/features/voice/engine/Store';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {getNativeAudioCaptureDiagnosticState} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import type {DesktopVoiceDebugEventSinkEntry, ElectronAPI} from '@app/types/electron.d';
import type {VoiceDebugLoggingStatusResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	LocalTrackPublication,
	Participant,
	RemoteTrack,
	RemoteTrackPublication,
	Room,
	TrackPublication,
} from 'livekit-client';
import {RoomEvent} from 'livekit-client';
import {
	assertBoolean,
	assertFiniteNumber,
	assertNonNullObject,
	assertString,
} from './VoiceEngineV2AppAdapterAssertions';

const logger = new Logger('VoiceEngineV2AppDebugLoggingHostAdapter');

const DEFAULT_POLL_INTERVAL_MS = 10000;
const DEFAULT_UPLOAD_INTERVAL_MS = 2000;
const MAX_QUEUED_EVENTS = 1000;
const MAX_UPLOAD_BATCH_EVENTS = 200;
export const VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_ENTRIES = 1000;
export const VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_LINE_CHARS = 262_144;
const SNAPSHOT_INTERVAL_MS = 10000;
const MAX_DEVICE_SNAPSHOT_AUDIO_TARGETS = 200;
const MAX_DEVICE_SNAPSHOT_PIPEWIRE_GRAPH_RECORDS = 500;
const SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC = 'fluxer.rtc.codec-negotiation.v1';
const TEXT_DECODER = new TextDecoder();
class BoundedRing<T> {
	private readonly items: Array<T | null>;
	private readonly capacity: number;
	private readonly label: string;
	private head = 0;
	private tail = 0;
	private count = 0;

	constructor(capacity: number, label: string) {
		assert.ok(capacity > 0, `${label} capacity must be positive`);
		assertString(label, 'label');
		this.capacity = capacity;
		this.label = label;
		this.items = new Array(capacity).fill(null);
	}

	get length(): number {
		return this.count;
	}

	pushDropOldest(item: T): T | null {
		let dropped: T | null = null;
		if (this.count >= this.capacity) {
			dropped = this.popFront();
		}
		this.pushBack(item);
		assert.ok(this.count <= this.capacity, `${this.label} must stay bounded`);
		return dropped;
	}

	drainFront(limit: number): Array<T> {
		assert.ok(limit > 0, `${this.label} drain limit must be positive`);
		const drained: Array<T> = [];
		while (drained.length < limit) {
			const item = this.popFront();
			if (item === null) break;
			drained.push(item);
		}
		return drained;
	}

	prependBatchDropBack(items: ReadonlyArray<T>): void {
		assert.ok(items.length <= this.capacity, `${this.label} prepend batch must fit capacity`);
		const existing = this.toArray();
		this.clear();
		for (const item of items) {
			this.pushBack(item);
		}
		for (const item of existing) {
			if (this.count >= this.capacity) return;
			this.pushBack(item);
		}
	}

	toArray(): Array<T> {
		const out: Array<T> = [];
		for (let index = 0; index < this.count; index += 1) {
			const slot = (this.head + index) % this.capacity;
			const item = this.items[slot];
			if (item === null) {
				assert.fail(`${this.label} slot must exist`);
			}
			out.push(item);
		}
		return out;
	}

	clear(): void {
		for (let index = 0; index < this.count; index += 1) {
			this.items[(this.head + index) % this.capacity] = null;
		}
		this.head = 0;
		this.tail = 0;
		this.count = 0;
	}

	private pushBack(item: T): void {
		assert.ok(this.count < this.capacity, `${this.label} must have free capacity before push`);
		this.items[this.tail] = item;
		this.tail = (this.tail + 1) % this.capacity;
		this.count += 1;
	}

	private popFront(): T | null {
		if (this.count === 0) return null;
		const item = this.items[this.head];
		assert.notEqual(item, null, `${this.label} front slot must exist`);
		this.items[this.head] = null;
		this.head = (this.head + 1) % this.capacity;
		this.count -= 1;
		return item;
	}
}

type VoiceDebugRoomEventHandler = (...args: Array<never>) => void;
type VoiceDebugRoomEventBinding = [RoomEvent, VoiceDebugRoomEventHandler];

export type VoiceDebugLoggingSnapshotCollector = () =>
	| Record<string, unknown>
	| null
	| undefined
	| Promise<Record<string, unknown> | null | undefined>;

interface VoiceDebugLoggingStartOptions {
	guildId: string | null;
	channelId: string;
	connectionId: string | null;
	room: Room | null;
	collectSnapshot: VoiceDebugLoggingSnapshotCollector;
}

interface RoomParticipantSummary {
	identity: string;
	sid: string;
	isLocal: boolean;
	name?: string;
	metadata?: string;
	attributes?: Record<string, string>;
	connectionQuality?: string;
	isSpeaking?: boolean;
	permissions?: unknown;
	trackPublications: Array<TrackPublicationSummary>;
}

interface TrackPublicationSummary {
	trackSid: string;
	trackName?: string;
	source?: string;
	kind?: string;
	mimeType?: string;
	isMuted?: boolean;
	isSubscribed?: boolean;
	isEnabled?: boolean;
	dimensions?: {
		width: number;
		height: number;
	};
}

function millisecondsToNanosecondsString(milliseconds: number): string {
	if (!Number.isFinite(milliseconds) || milliseconds < 0) return '0';
	const wholeMs = Math.trunc(milliseconds);
	const fractionalNs = Math.round((milliseconds - wholeMs) * 1000000);
	return (BigInt(wholeMs) * 1000000n + BigInt(fractionalNs)).toString();
}

function createDiagnosticEvent(type: string, data?: Record<string, unknown>): VoiceDebugLoggingEvent {
	const monotonicNow = typeof performance !== 'undefined' ? performance.now() : Date.now();
	const timeOrigin =
		typeof performance !== 'undefined' && Number.isFinite(performance.timeOrigin)
			? performance.timeOrigin
			: Date.now() - monotonicNow;
	return {
		type,
		timestamp_ns: millisecondsToNanosecondsString(timeOrigin + monotonicNow),
		monotonic_ns: millisecondsToNanosecondsString(monotonicNow),
		...(data ? {data} : {}),
	};
}

function errorToData(error: unknown): Record<string, unknown> {
	if (error instanceof Error) {
		return {
			name: error.name,
			message: error.message,
			stack: error.stack,
		};
	}
	return {message: String(error)};
}

function truncateEventSinkLine(line: string): string {
	assertString(line, 'event sink line');
	if (line.length <= VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_LINE_CHARS) return line;
	const omittedChars = line.length - VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_LINE_CHARS;
	return `${line.slice(0, VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_LINE_CHARS)}... [truncated ${omittedChars} chars]`;
}

function stringifyEventSinkEntry(sequence: number, event: VoiceDebugLoggingEvent): string {
	assert.ok(Number.isSafeInteger(sequence), 'event sink sequence must be a safe integer');
	assert.ok(sequence >= 1, 'event sink sequence must be >= 1');
	try {
		return truncateEventSinkLine(JSON.stringify({sequence, ...event}));
	} catch (error) {
		return truncateEventSinkLine(
			JSON.stringify({
				sequence,
				type: event.type,
				timestamp_ns: event.timestamp_ns,
				monotonic_ns: event.monotonic_ns,
				stringifyError: errorToData(error),
			}),
		);
	}
}

function createEventSinkEntry(sequence: number, event: VoiceDebugLoggingEvent): DesktopVoiceDebugEventSinkEntry {
	assert.ok(event !== null && typeof event === 'object', 'event sink event must be an object');
	assertString(event.type, 'event sink event type');
	assertString(event.timestamp_ns, 'event sink event timestamp');
	return {
		sequence,
		line: stringifyEventSinkEntry(sequence, event),
	};
}

function getTrackDimensions(publication: TrackPublication): TrackPublicationSummary['dimensions'] | undefined {
	const track = publication.track;
	if (!track || !('dimensions' in track)) return undefined;
	const dimensions = (track as {dimensions?: {width?: number; height?: number}}).dimensions;
	if (typeof dimensions?.width !== 'number' || typeof dimensions.height !== 'number') return undefined;
	return {
		width: dimensions.width,
		height: dimensions.height,
	};
}

function summarizePublication(publication: TrackPublication): TrackPublicationSummary {
	return {
		trackSid: publication.trackSid,
		trackName: publication.trackName,
		source: publication.source,
		kind: publication.kind,
		mimeType: publication.mimeType,
		isMuted: publication.isMuted,
		isSubscribed: 'isSubscribed' in publication ? Boolean(publication.isSubscribed) : undefined,
		isEnabled: 'isEnabled' in publication ? Boolean(publication.isEnabled) : undefined,
		dimensions: getTrackDimensions(publication),
	};
}

function summarizeParticipant(participant: Participant | undefined): RoomParticipantSummary | null {
	if (!participant) return null;
	const trackPublications: Array<TrackPublicationSummary> = [];
	participant.trackPublications.forEach((publication) => {
		trackPublications.push(summarizePublication(publication));
	});
	return {
		identity: participant.identity,
		sid: participant.sid,
		isLocal: participant.isLocal,
		name: participant.name,
		metadata: participant.metadata,
		attributes: participant.attributes,
		connectionQuality: participant.connectionQuality,
		isSpeaking: participant.isSpeaking,
		permissions: participant.permissions,
		trackPublications,
	};
}

function summarizeRoom(room: Room | null): Record<string, unknown> | null {
	if (!room) return null;
	return {
		name: room.name,
		state: room.state,
		numParticipants: room.numParticipants,
		localParticipant: summarizeParticipant(room.localParticipant),
		remoteParticipants: Array.from(room.remoteParticipants.values()).map((participant) =>
			summarizeParticipant(participant),
		),
	};
}

function summarizeTrackEvent(
	publication: TrackPublication | RemoteTrackPublication | LocalTrackPublication,
	participant: Participant | undefined,
): Record<string, unknown> {
	return {
		publication: summarizePublication(publication as TrackPublication),
		participant: summarizeParticipant(participant),
		isScreenShare: asVoiceTrackSource(publication.source) === VoiceTrackSource.ScreenShare,
		isScreenShareAudio: asVoiceTrackSource(publication.source) === VoiceTrackSource.ScreenShareAudio,
	};
}

function summarizeRemoteTrack(track: RemoteTrack): Record<string, unknown> {
	return {
		sid: track.sid,
		kind: track.kind,
		source: track.source,
		mediaStreamTrackId: track.mediaStreamTrack?.id,
		readyState: track.mediaStreamTrack?.readyState,
		muted: track.mediaStreamTrack?.muted,
	};
}

function getRtpCapabilities(kind: 'audio' | 'video'): Record<string, unknown> {
	const sender =
		typeof RTCRtpSender !== 'undefined' && typeof RTCRtpSender.getCapabilities === 'function'
			? RTCRtpSender.getCapabilities(kind)
			: null;
	const receiver =
		typeof RTCRtpReceiver !== 'undefined' && typeof RTCRtpReceiver.getCapabilities === 'function'
			? RTCRtpReceiver.getCapabilities(kind)
			: null;
	return {
		sender,
		receiver,
	};
}

function limitAudioTargetSnapshot(value: unknown): unknown {
	if (!value || typeof value !== 'object' || !('targets' in value)) return value;
	const record = value as {targets?: unknown};
	if (!Array.isArray(record.targets)) return value;
	return {
		...value,
		targets: record.targets.slice(0, MAX_DEVICE_SNAPSHOT_AUDIO_TARGETS),
		targetCount: record.targets.length,
		truncated: record.targets.length > MAX_DEVICE_SNAPSHOT_AUDIO_TARGETS,
	};
}

function limitGraphArray(record: Record<string, unknown>, key: string): Record<string, unknown> {
	const value = record[key];
	if (!Array.isArray(value)) return record;
	return {
		...record,
		[key]: value.slice(0, MAX_DEVICE_SNAPSHOT_PIPEWIRE_GRAPH_RECORDS),
		[`${key}Count`]: value.length,
		[`${key}Truncated`]: value.length > MAX_DEVICE_SNAPSHOT_PIPEWIRE_GRAPH_RECORDS,
	};
}

function limitPipewireRoutingGraph(graph: unknown): unknown {
	if (!graph || typeof graph !== 'object') return graph;
	let limited = graph as Record<string, unknown>;
	limited = limitGraphArray(limited, 'nodes');
	limited = limitGraphArray(limited, 'ports');
	limited = limitGraphArray(limited, 'ownedLinks');
	return limited;
}

function limitPipewireRoutingGraphResult(result: unknown): unknown {
	if (!result || typeof result !== 'object') return result;
	const record = result as Record<string, unknown>;
	if ('graph' in record) {
		return {...record, graph: limitPipewireRoutingGraph(record.graph)};
	}
	if (Array.isArray(record.graphs)) {
		return {
			...record,
			graphs: record.graphs.map((entry) =>
				entry && typeof entry === 'object'
					? {...entry, graph: limitPipewireRoutingGraph((entry as Record<string, unknown>).graph)}
					: entry,
			),
		};
	}
	return result;
}

async function collectMediaDevicesSnapshot(): Promise<unknown> {
	if (typeof navigator === 'undefined') return null;
	if (!navigator.mediaDevices?.enumerateDevices) return null;
	return navigator.mediaDevices
		.enumerateDevices()
		.then((devices) =>
			devices.map((device) => ({
				deviceId: device.deviceId,
				groupId: device.groupId,
				kind: device.kind,
				label: device.label,
			})),
		)
		.catch((error) => ({error: errorToData(error)}));
}

async function collectNativeAudioSnapshot(electron: NonNullable<ElectronAPI>): Promise<unknown> {
	if (!electron.nativeAudio) return null;
	return Promise.all([
		electron.nativeAudio.getAvailability().catch((error) => ({error: errorToData(error)})),
		electron.nativeAudio.getRoutingGraph().catch((error) => ({error: errorToData(error)})),
	]).then(([availability, routingGraph]) => ({
		availability,
		pipewireRoutingGraph: limitPipewireRoutingGraphResult(routingGraph),
	}));
}

async function collectVirtmicSnapshot(electron: NonNullable<ElectronAPI>): Promise<unknown> {
	if (!electron.virtmic) return null;
	return Promise.all([
		electron.virtmic.getAvailability().catch((error) => ({error: errorToData(error)})),
		electron.virtmic.listTargets({granular: true}).catch((error) => ({error: errorToData(error)})),
		electron.virtmic.getRoutingGraph().catch((error) => ({error: errorToData(error)})),
	]).then(([availability, targets, routingGraph]) => ({
		availability,
		targets: limitAudioTargetSnapshot(targets),
		pipewireRoutingGraph: limitPipewireRoutingGraphResult(routingGraph),
	}));
}

function collectScreenSnapshot(): unknown {
	if (typeof screen === 'undefined') return null;
	return {
		width: screen.width,
		height: screen.height,
		availWidth: screen.availWidth,
		availHeight: screen.availHeight,
		colorDepth: screen.colorDepth,
		pixelDepth: screen.pixelDepth,
		orientation: screen.orientation ? {type: screen.orientation.type, angle: screen.orientation.angle} : null,
	};
}

function collectNavigatorSnapshot(): unknown {
	if (typeof navigator === 'undefined') return null;
	const nav = navigator;
	return {
		userAgent: nav.userAgent,
		platform: nav.platform,
		language: nav.language,
		languages: nav.languages,
		hardwareConcurrency: nav.hardwareConcurrency,
		deviceMemory: (nav as Navigator & {deviceMemory?: number}).deviceMemory,
		maxTouchPoints: nav.maxTouchPoints,
		cookieEnabled: nav.cookieEnabled,
		doNotTrack: nav.doNotTrack,
		onLine: nav.onLine,
	};
}

function collectDocumentWindowSnapshots(): {document: unknown; window: unknown} {
	return {
		document:
			typeof document !== 'undefined' ? {visibilityState: document.visibilityState, hidden: document.hidden} : null,
		window:
			typeof window !== 'undefined'
				? {
						devicePixelRatio: window.devicePixelRatio,
						innerWidth: window.innerWidth,
						innerHeight: window.innerHeight,
						outerWidth: window.outerWidth,
						outerHeight: window.outerHeight,
					}
				: null,
	};
}

async function collectDesktopSnapshot(electron: ElectronAPI | null): Promise<Record<string, unknown>> {
	const desktopInfo = electron?.getDesktopInfo
		? await electron.getDesktopInfo().catch((error) => ({error: errorToData(error)}))
		: null;
	const gpuInfo = electron?.getGpuInfo
		? await electron.getGpuInfo().catch((error) => ({error: errorToData(error)}))
		: null;
	const appMetrics = electron?.getAppMetrics
		? await electron.getAppMetrics().catch((error) => ({error: errorToData(error)}))
		: null;
	const nativeScreenCapture = electron?.nativeScreenCapture
		? await electron.nativeScreenCapture.getAvailability().catch((error) => ({error: errorToData(error)}))
		: null;
	const nativeAudio = electron ? await collectNativeAudioSnapshot(electron) : null;
	const virtmic = electron ? await collectVirtmicSnapshot(electron) : null;
	return {
		platform: electron?.platform ?? null,
		buildChannel: electron?.buildChannel ?? null,
		info: desktopInfo,
		gpuInfo,
		appMetrics,
		nativeScreenCapture,
		nativeAudio,
		nativeAudioCapture: getNativeAudioCaptureDiagnosticState(),
		virtmic,
	};
}

async function collectDeviceSnapshot(): Promise<Record<string, unknown>> {
	const electron = getElectronAPI();
	const mediaDevices = await collectMediaDevicesSnapshot();
	const screenSnapshot = collectScreenSnapshot();
	const documentAndWindow = collectDocumentWindowSnapshots();
	const desktop = await collectDesktopSnapshot(electron);
	return {
		navigator: collectNavigatorSnapshot(),
		document: documentAndWindow.document,
		window: documentAndWindow.window,
		screen: screenSnapshot,
		mediaDevices,
		rtpCapabilities: {audio: getRtpCapabilities('audio'), video: getRtpCapabilities('video')},
		desktop,
	};
}

function parseAllowedDataMessage(payload: Uint8Array, topic: string | undefined): Record<string, unknown> {
	if (topic !== SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC) {
		return {
			topic: topic ?? null,
			payloadBytes: payload.byteLength,
			decoded: null,
		};
	}
	try {
		return {
			topic,
			payloadBytes: payload.byteLength,
			decoded: JSON.parse(TEXT_DECODER.decode(payload)) as unknown,
		};
	} catch (error) {
		return {
			topic,
			payloadBytes: payload.byteLength,
			decodeError: errorToData(error),
		};
	}
}

export interface VoiceEngineV2AppDebugLoggingHostAdapterScheduler {
	setInterval(handler: () => void, intervalMs: number): unknown;
	clearInterval(handle: unknown): void;
}

export interface VoiceEngineV2AppDebugLoggingHostAdapterOptions {
	now?: () => number;
	scheduler?: VoiceEngineV2AppDebugLoggingHostAdapterScheduler;
}

const DEFAULT_DEBUG_LOG_SCHEDULER: VoiceEngineV2AppDebugLoggingHostAdapterScheduler = {
	setInterval(handler, intervalMs) {
		return setInterval(handler, intervalMs);
	},
	clearInterval(handle) {
		clearInterval(handle as NodeJS.Timeout);
	},
};

export class VoiceEngineV2AppDebugLoggingHostAdapter extends Store {
	active = false;
	sessionId: string | null = null;
	activatedByUserId: string | null = null;
	startedAtMs: number | null = null;
	expiresAtMs: number | null = null;
	pollIntervalMs = DEFAULT_POLL_INTERVAL_MS;
	uploadIntervalMs = DEFAULT_UPLOAD_INTERVAL_MS;
	toggleInFlight = false;
	lastError: string | null = null;
	private readonly now: () => number;
	private readonly scheduler: VoiceEngineV2AppDebugLoggingHostAdapterScheduler;
	private channelId: string | null = null;
	private connectionId: string | null = null;
	private participantIdentity: string | null = null;
	private room: Room | null = null;
	private collectSnapshot: VoiceDebugLoggingSnapshotCollector | null = null;
	private pollTimer: unknown = null;
	private uploadTimer: unknown = null;
	private roomDisposer: (() => void) | null = null;
	private readonly queue = new BoundedRing<VoiceDebugLoggingEvent>(MAX_QUEUED_EVENTS, 'voice debug upload queue');
	private readonly eventSinkEntries = new BoundedRing<DesktopVoiceDebugEventSinkEntry>(
		VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_ENTRIES,
		'voice debug event sink history',
	);
	private uploadInFlight = false;
	private generation = 0;
	private eventSinkSequence = 0;
	private eventSinkForwardFailureCount = 0;
	private lastSnapshotAtMs = 0;

	constructor(options: VoiceEngineV2AppDebugLoggingHostAdapterOptions = {}) {
		super();
		this.now = options.now ?? (() => Date.now());
		this.scheduler = options.scheduler ?? DEFAULT_DEBUG_LOG_SCHEDULER;
		assert.equal(typeof this.now, 'function', 'now must be function');
		assertNonNullObject(this.scheduler, 'scheduler');
	}

	get connectedChannelId(): string | null {
		return this.channelId;
	}

	getEventSinkEntries(): Array<DesktopVoiceDebugEventSinkEntry> {
		assert.ok(
			this.eventSinkEntries.length <= VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_ENTRIES,
			'event sink history must stay bounded',
		);
		return this.eventSinkEntries.toArray();
	}

	async openEventSinkPopout(): Promise<void> {
		const electron = getElectronAPI();
		if (this.eventSinkEntries.length === 0) {
			this.appendEventSinkEntry(
				createDiagnosticEvent('voice.debug_logging.event_sink_opened_without_started_adapter', {
					active: this.active,
					channelId: this.channelId,
					connectionId: this.connectionId,
					roomAttached: this.room !== null,
				}),
			);
		}
		const entries = this.getEventSinkEntries();
		if (electron?.openVoiceDebugEventSinkPopout) {
			try {
				await electron.openVoiceDebugEventSinkPopout(entries);
				return;
			} catch (error) {
				logger.warn('Failed to open voice debug event sink desktop popout', {error});
			}
		}
		try {
			const opened = await openBrowserVoiceDebugEventSinkPopout(entries);
			if (!opened) {
				logger.warn('Failed to open voice debug event sink browser popout');
			}
		} catch (error) {
			logger.warn('Failed to open voice debug event sink browser popout', {error});
		}
	}

	private isStartIdempotent(options: VoiceDebugLoggingStartOptions): boolean {
		if (this.channelId !== options.channelId) return false;
		if (this.connectionId !== options.connectionId) return false;
		return this.room === options.room;
	}

	async start(options: VoiceDebugLoggingStartOptions): Promise<void> {
		assertNonNullObject(options, 'options');
		assertString(options.channelId, 'options.channelId');
		if (options.room !== null) {
			assertNonNullObject(options.room, 'options.room');
		}
		if (this.isStartIdempotent(options)) return;
		await this.stop('replaced');
		this.update(() => {
			this.generation++;
			this.channelId = options.channelId;
			this.connectionId = options.connectionId;
			this.room = options.room;
			this.participantIdentity = options.room?.localParticipant?.identity ?? null;
			this.collectSnapshot = options.collectSnapshot;
		});
		if (options.room) {
			this.bindRoom(options.room);
		}
		this.record('voice.debug_logging.client_started', {
			guildId: options.guildId,
			channelId: options.channelId,
			connectionId: options.connectionId,
			room: summarizeRoom(options.room),
		});
		await this.refreshStatus('joined');
		this.restartPollTimer(this.pollIntervalMs);
	}

	async stop(reason = 'stopped'): Promise<void> {
		assertString(reason, 'reason');
		const wasStarted = this.channelId !== null;
		const wasActive = this.active;
		if (wasStarted) {
			this.record('voice.debug_logging.client_stopped', {
				reason,
				active: wasActive,
				channelId: this.channelId,
				connectionId: this.connectionId,
			});
			if (wasActive) {
				await this.flush('stop');
			}
		}
		this.generation++;
		this.clearTimers();
		this.roomDisposer?.();
		this.roomDisposer = null;
		this.queue.clear();
		this.update(() => {
			this.active = false;
			this.sessionId = null;
			this.activatedByUserId = null;
			this.startedAtMs = null;
			this.expiresAtMs = null;
			this.pollIntervalMs = DEFAULT_POLL_INTERVAL_MS;
			this.uploadIntervalMs = DEFAULT_UPLOAD_INTERVAL_MS;
			this.lastError = null;
			this.channelId = null;
			this.connectionId = null;
			this.participantIdentity = null;
			this.room = null;
			this.collectSnapshot = null;
			this.lastSnapshotAtMs = 0;
		});
	}

	async setEnabled(enabled: boolean): Promise<void> {
		assertBoolean(enabled, 'enabled');
		const channelId = this.channelId;
		if (!channelId) return;
		if (this.toggleInFlight) return;
		this.update(() => {
			this.toggleInFlight = true;
			this.lastError = null;
		});
		try {
			const status = await setEnabled(channelId, enabled);
			await this.applyStatus(status, enabled ? 'staff-enabled' : 'staff-disabled');
		} catch (error) {
			logger.warn('Failed to toggle voice debug logging session', {error, enabled, channelId});
			this.update(() => {
				this.lastError = error instanceof Error ? error.message : String(error);
			});
		} finally {
			this.update(() => {
				this.toggleInFlight = false;
			});
		}
	}

	private buildRoomEventBindings(room: Room): Array<VoiceDebugRoomEventBinding> {
		return [
			[RoomEvent.Connected, () => this.record('livekit.room.connected', {room: summarizeRoom(room)})],
			[
				RoomEvent.Disconnected,
				(reason?: unknown) => {
					this.record('livekit.room.disconnected', {reason: String(reason ?? 'unknown'), room: summarizeRoom(room)});
					void this.flush('room-disconnected');
				},
			],
			[RoomEvent.Reconnecting, () => this.record('livekit.room.reconnecting', {room: summarizeRoom(room)})],
			[
				RoomEvent.Reconnected,
				() => {
					this.record('livekit.room.reconnected', {room: summarizeRoom(room)});
					void this.enqueueRuntimeSnapshot('livekit.room.reconnected');
				},
			],
			[
				RoomEvent.ParticipantConnected,
				(participant: Participant) =>
					this.record('livekit.participant.connected', {participant: summarizeParticipant(participant)}),
			],
			[
				RoomEvent.ParticipantDisconnected,
				(participant: Participant) =>
					this.record('livekit.participant.disconnected', {participant: summarizeParticipant(participant)}),
			],
			[
				RoomEvent.TrackPublished,
				(publication: RemoteTrackPublication, participant: Participant) =>
					this.record('livekit.track.published', summarizeTrackEvent(publication, participant)),
			],
			[
				RoomEvent.TrackUnpublished,
				(publication: RemoteTrackPublication, participant: Participant) =>
					this.record('livekit.track.unpublished', summarizeTrackEvent(publication, participant)),
			],
			[
				RoomEvent.TrackSubscribed,
				(track: RemoteTrack, publication: RemoteTrackPublication, participant: Participant) =>
					this.record('livekit.track.subscribed', {
						...summarizeTrackEvent(publication, participant),
						track: summarizeRemoteTrack(track),
					}),
			],
			[
				RoomEvent.TrackUnsubscribed,
				(track: RemoteTrack, publication: RemoteTrackPublication, participant: Participant) =>
					this.record('livekit.track.unsubscribed', {
						...summarizeTrackEvent(publication, participant),
						track: summarizeRemoteTrack(track),
					}),
			],
			[
				RoomEvent.TrackMuted,
				(publication: TrackPublication, participant: Participant) =>
					this.record('livekit.track.muted', summarizeTrackEvent(publication, participant)),
			],
			[
				RoomEvent.TrackUnmuted,
				(publication: TrackPublication, participant: Participant) =>
					this.record('livekit.track.unmuted', summarizeTrackEvent(publication, participant)),
			],
			[
				RoomEvent.LocalTrackPublished,
				(publication: LocalTrackPublication, participant: Participant) =>
					this.record('livekit.local_track.published', summarizeTrackEvent(publication, participant)),
			],
			[
				RoomEvent.LocalTrackUnpublished,
				(publication: LocalTrackPublication, participant: Participant) =>
					this.record('livekit.local_track.unpublished', summarizeTrackEvent(publication, participant)),
			],
			[
				RoomEvent.ActiveSpeakersChanged,
				(speakers: Array<Participant>) =>
					this.record('livekit.active_speakers.changed', {
						speakers: speakers.map((participant) => summarizeParticipant(participant)),
					}),
			],
			[
				RoomEvent.DataReceived,
				(payload: Uint8Array, participant: Participant | undefined, kind: unknown, topic?: string) =>
					this.record('livekit.data.received', {
						participant: summarizeParticipant(participant),
						kind: String(kind),
						...parseAllowedDataMessage(payload, topic),
					}),
			],
		] as Array<VoiceDebugRoomEventBinding>;
	}

	private bindRoom(room: Room): void {
		assertNonNullObject(room, 'room');
		this.roomDisposer?.();
		const bindings = this.buildRoomEventBindings(room);
		assert.ok(bindings.length > 0, 'expected at least one room event binding');
		for (const [event, handler] of bindings) {
			room.on(event, handler);
		}
		this.roomDisposer = () => {
			for (const [event, handler] of bindings) {
				room.off(event, handler);
			}
		};
	}

	private clearTimers(): void {
		if (this.pollTimer) {
			this.scheduler.clearInterval(this.pollTimer);
			this.pollTimer = null;
		}
		if (this.uploadTimer) {
			this.scheduler.clearInterval(this.uploadTimer);
			this.uploadTimer = null;
		}
	}

	private restartPollTimer(intervalMs: number): void {
		assertFiniteNumber(intervalMs, 'intervalMs');
		assert.ok(intervalMs > 0, 'intervalMs must be > 0');
		if (!this.channelId) return;
		if (this.pollTimer) {
			this.scheduler.clearInterval(this.pollTimer);
		}
		const generation = this.generation;
		this.pollTimer = this.scheduler.setInterval(() => {
			if (generation !== this.generation) return;
			void this.refreshStatus('poll');
		}, intervalMs);
	}

	private restartUploadTimer(intervalMs: number): void {
		assertFiniteNumber(intervalMs, 'intervalMs');
		assert.ok(intervalMs > 0, 'intervalMs must be > 0');
		if (this.uploadTimer) {
			this.scheduler.clearInterval(this.uploadTimer);
			this.uploadTimer = null;
		}
		if (!this.active) return;
		const generation = this.generation;
		this.uploadTimer = this.scheduler.setInterval(() => {
			if (generation !== this.generation) return;
			void this.flush('interval');
		}, intervalMs);
	}

	private async refreshStatus(reason: string): Promise<void> {
		const channelId = this.channelId;
		if (!channelId) return;
		try {
			const status = await fetchStatus(channelId);
			await this.applyStatus(status, reason);
		} catch (error) {
			logger.debug('Failed to refresh voice debug logging status', {error, channelId, reason});
			this.update(() => {
				this.lastError = error instanceof Error ? error.message : String(error);
			});
		}
	}

	private async applyStatus(status: VoiceDebugLoggingStatusResponse, reason: string): Promise<void> {
		const previousActive = this.active;
		const previousSessionId = this.sessionId;
		const becameActive =
			status.active && status.session_id !== null && (!previousActive || previousSessionId !== status.session_id);
		const becameInactive = previousActive && (!status.active || status.session_id === null);
		this.update(() => {
			this.active = status.active && status.session_id !== null;
			this.sessionId = status.session_id;
			this.activatedByUserId = status.activated_by_user_id;
			this.startedAtMs = status.started_at_ms;
			this.expiresAtMs = status.expires_at_ms;
			this.pollIntervalMs = status.poll_interval_ms || DEFAULT_POLL_INTERVAL_MS;
			this.uploadIntervalMs = status.upload_interval_ms || DEFAULT_UPLOAD_INTERVAL_MS;
			this.lastError = null;
		});
		this.restartPollTimer(this.pollIntervalMs);
		this.restartUploadTimer(this.uploadIntervalMs);
		if (becameActive) {
			this.record('voice.debug_logging.session_active', {
				reason,
				sessionId: status.session_id,
				activatedByUserId: status.activated_by_user_id,
				startedAtMs: status.started_at_ms,
				expiresAtMs: status.expires_at_ms,
			});
			await this.enqueueDeviceSnapshot();
			await this.enqueueRuntimeSnapshot('session_active');
			void this.flush('session-active');
		} else if (becameInactive) {
			this.record('voice.debug_logging.session_inactive', {reason, previousSessionId});
			this.queue.clear();
		}
	}

	private record(type: string, data?: Record<string, unknown>): void {
		const event = createDiagnosticEvent(type, data);
		this.queue.pushDropOldest(event);
		this.appendEventSinkEntry(event);
		if (this.active && this.queue.length >= MAX_UPLOAD_BATCH_EVENTS) {
			void this.flush('queue-full');
		}
	}

	private appendEventSinkEntry(event: VoiceDebugLoggingEvent): void {
		this.eventSinkSequence += 1;
		assert.ok(Number.isSafeInteger(this.eventSinkSequence), 'event sink sequence must stay safe');
		const entry = createEventSinkEntry(this.eventSinkSequence, event);
		this.eventSinkEntries.pushDropOldest(entry);
		this.forwardEventSinkEntries([entry]);
	}

	private forwardEventSinkEntries(entries: Array<DesktopVoiceDebugEventSinkEntry>): void {
		assert.ok(entries.length >= 1, 'event sink forward requires at least one entry');
		const electron = getElectronAPI();
		if (electron?.appendVoiceDebugEventSinkEntries) {
			try {
				electron.appendVoiceDebugEventSinkEntries(entries);
				this.eventSinkForwardFailureCount = 0;
			} catch (error) {
				this.eventSinkForwardFailureCount += 1;
				if (this.eventSinkForwardFailureCount === 1) {
					logger.warn('Failed to forward voice debug event sink entries to desktop popout', {error});
				}
			}
		}
		appendBrowserVoiceDebugEventSinkEntries(entries);
	}

	private async enqueueDeviceSnapshot(): Promise<void> {
		try {
			this.record('voice.debug_logging.device_snapshot', await collectDeviceSnapshot());
		} catch (error) {
			this.record('voice.debug_logging.device_snapshot_failed', errorToData(error));
		}
	}

	private async enqueueRuntimeSnapshot(reason: string): Promise<void> {
		const collector = this.collectSnapshot;
		if (!collector) return;
		try {
			this.record('voice.debug_logging.runtime_snapshot', {
				reason,
				room: summarizeRoom(this.room),
				snapshot: (await collector()) ?? null,
			});
			this.lastSnapshotAtMs = this.now();
		} catch (error) {
			this.record('voice.debug_logging.runtime_snapshot_failed', {reason, ...errorToData(error)});
		}
	}

	private async enqueuePeriodicSnapshotIfNeeded(reason: string): Promise<void> {
		assertString(reason, 'reason');
		if (!this.active) return;
		if (this.now() - this.lastSnapshotAtMs < SNAPSHOT_INTERVAL_MS) return;
		await this.enqueueRuntimeSnapshot(reason);
	}

	private async flush(reason: string): Promise<void> {
		if (this.uploadInFlight) return;
		const channelId = this.channelId;
		const sessionId = this.sessionId;
		if (!this.active || !channelId || !sessionId) return;
		await this.enqueuePeriodicSnapshotIfNeeded(reason);
		const events = this.queue.drainFront(MAX_UPLOAD_BATCH_EVENTS);
		if (events.length === 0) return;
		this.uploadInFlight = true;
		try {
			const response = await uploadEvents({
				channelId,
				sessionId,
				connectionId: this.connectionId,
				participantIdentity: this.participantIdentity,
				events,
			});
			if (!response.accepted || !response.active) {
				await this.refreshStatus('upload-rejected');
			}
		} catch (error) {
			logger.debug('Failed to upload voice debug logging events', {error, channelId, reason, count: events.length});
			this.queue.prependBatchDropBack(events);
			this.update(() => {
				this.lastError = error instanceof Error ? error.message : String(error);
			});
		} finally {
			this.uploadInFlight = false;
		}
	}
}

export default new VoiceEngineV2AppDebugLoggingHostAdapter();

export function createVoiceEngineV2AppDebugLoggingHostAdapterForTest(
	options: VoiceEngineV2AppDebugLoggingHostAdapterOptions = {},
): VoiceEngineV2AppDebugLoggingHostAdapter {
	return new VoiceEngineV2AppDebugLoggingHostAdapter(options);
}

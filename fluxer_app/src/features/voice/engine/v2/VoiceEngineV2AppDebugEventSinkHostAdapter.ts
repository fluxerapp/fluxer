// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {
	appendBrowserVoiceDebugEventSinkEntries,
	openBrowserVoiceDebugEventSinkPopout,
} from '@app/features/voice/diagnostics/VoiceDebugBrowserEventSinkPopout';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {DesktopVoiceDebugEventSinkEntry} from '@app/types/electron.d';
import type {
	LocalTrackPublication,
	Participant,
	RemoteTrack,
	RemoteTrackPublication,
	Room,
	TrackPublication,
} from 'livekit-client';
import {RoomEvent} from 'livekit-client';
import {assertNonNullObject, assertString} from './VoiceEngineV2AppAdapterAssertions';

const logger = new Logger('VoiceEngineV2AppDebugEventSinkHostAdapter');

export const VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_ENTRIES = 1000;
export const VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_LINE_CHARS = 262_144;
const SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC = 'fluxer.rtc.codec-negotiation.v1';
const TEXT_DECODER = new TextDecoder();

interface VoiceDebugEventSinkEvent {
	type: string;
	timestamp_ns: string;
	monotonic_ns?: string;
	data?: Record<string, unknown>;
}

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

type VoiceDebugEventSinkRoomEventHandler = (...args: Array<never>) => void;
type VoiceDebugEventSinkRoomEventBinding = [RoomEvent, VoiceDebugEventSinkRoomEventHandler];

interface VoiceDebugEventSinkStartOptions {
	guildId: string | null;
	channelId: string;
	connectionId: string | null;
	room: Room;
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

function createDiagnosticEvent(type: string, data?: Record<string, unknown>): VoiceDebugEventSinkEvent {
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

function stringifyEventSinkEntry(sequence: number, event: VoiceDebugEventSinkEvent): string {
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

function createEventSinkEntry(sequence: number, event: VoiceDebugEventSinkEvent): DesktopVoiceDebugEventSinkEntry {
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

export class VoiceEngineV2AppDebugEventSinkHostAdapter {
	private channelId: string | null = null;
	private connectionId: string | null = null;
	private room: Room | null = null;
	private roomDisposer: (() => void) | null = null;
	private readonly eventSinkEntries = new BoundedRing<DesktopVoiceDebugEventSinkEntry>(
		VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_ENTRIES,
		'voice debug event sink history',
	);
	private eventSinkSequence = 0;
	private eventSinkForwardFailureCount = 0;

	getEventSinkEntries(): Array<DesktopVoiceDebugEventSinkEntry> {
		assert.ok(
			this.eventSinkEntries.length <= VOICE_ENGINE_V2_APP_DEBUG_EVENT_SINK_MAX_ENTRIES,
			'event sink history must stay bounded',
		);
		return this.eventSinkEntries.toArray();
	}

	async openEventSinkPopout(): Promise<void> {
		const electron = getElectronAPI();
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

	private isStartIdempotent(options: VoiceDebugEventSinkStartOptions): boolean {
		if (this.channelId !== options.channelId) return false;
		if (this.connectionId !== options.connectionId) return false;
		return this.room === options.room;
	}

	start(options: VoiceDebugEventSinkStartOptions): void {
		assertNonNullObject(options, 'options');
		assertString(options.channelId, 'options.channelId');
		assert.ok(options.channelId.length > 0, 'options.channelId must not be empty');
		assertNonNullObject(options.room, 'options.room');
		if (this.isStartIdempotent(options)) return;
		this.stop('replaced');
		this.channelId = options.channelId;
		this.connectionId = options.connectionId;
		this.room = options.room;
		this.bindRoom(options.room);
		this.record('voice.debug_event_sink.tracking_started', {
			guildId: options.guildId,
			channelId: options.channelId,
			connectionId: options.connectionId,
			room: summarizeRoom(options.room),
		});
	}

	stop(reason = 'stopped'): void {
		assertString(reason, 'reason');
		if (this.channelId !== null) {
			this.record('voice.debug_event_sink.tracking_stopped', {
				reason,
				channelId: this.channelId,
				connectionId: this.connectionId,
				room: summarizeRoom(this.room),
			});
		}
		this.roomDisposer?.();
		this.roomDisposer = null;
		this.channelId = null;
		this.connectionId = null;
		this.room = null;
	}

	private buildRoomLifecycleEventBindings(room: Room): Array<VoiceDebugEventSinkRoomEventBinding> {
		return [
			[RoomEvent.Connected, () => this.record('livekit.room.connected', {room: summarizeRoom(room)})],
			[
				RoomEvent.Disconnected,
				(reason?: unknown) =>
					this.record('livekit.room.disconnected', {reason: String(reason ?? 'unknown'), room: summarizeRoom(room)}),
			],
			[RoomEvent.Reconnecting, () => this.record('livekit.room.reconnecting', {room: summarizeRoom(room)})],
			[RoomEvent.Reconnected, () => this.record('livekit.room.reconnected', {room: summarizeRoom(room)})],
		];
	}

	private buildParticipantEventBindings(): Array<VoiceDebugEventSinkRoomEventBinding> {
		return [
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
				RoomEvent.ActiveSpeakersChanged,
				(speakers: Array<Participant>) =>
					this.record('livekit.active_speakers.changed', {
						speakers: speakers.map((participant) => summarizeParticipant(participant)),
					}),
			],
		];
	}

	private buildTrackEventBindings(): Array<VoiceDebugEventSinkRoomEventBinding> {
		return [
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
		];
	}

	private buildDataEventBindings(): Array<VoiceDebugEventSinkRoomEventBinding> {
		return [
			[
				RoomEvent.DataReceived,
				(payload: Uint8Array, participant: Participant | undefined, kind: unknown, topic?: string) =>
					this.record('livekit.data.received', {
						participant: summarizeParticipant(participant),
						kind: String(kind),
						...parseAllowedDataMessage(payload, topic),
					}),
			],
		];
	}

	private buildRoomEventBindings(room: Room): Array<VoiceDebugEventSinkRoomEventBinding> {
		return [
			...this.buildRoomLifecycleEventBindings(room),
			...this.buildParticipantEventBindings(),
			...this.buildTrackEventBindings(),
			...this.buildDataEventBindings(),
		];
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

	private record(type: string, data?: Record<string, unknown>): void {
		const event = createDiagnosticEvent(type, data);
		this.appendEventSinkEntry(event);
	}

	private appendEventSinkEntry(event: VoiceDebugEventSinkEvent): void {
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
}

export default new VoiceEngineV2AppDebugEventSinkHostAdapter();

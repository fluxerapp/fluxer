// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {
	type DataPacket_Kind,
	type Encryption_Type,
	type ParticipantInfo,
	ParticipantInfo_State,
	ParticipantInfo_Kind as ParticipantKind,
	type ParticipantPermission,
	ConnectionQuality as ProtoQuality,
	type SipDTMF,
	type SubscriptionError,
} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type TypedEmitter from 'typed-emitter';
import log, {getLogger, LoggerNames, type StructuredLogger} from '../../logger.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {ParticipantEvent, TrackEvent} from '../events.ts';
import type LocalTrackPublication from '../track/LocalTrackPublication.ts';
import type LocalVideoTrack from '../track/LocalVideoTrack.ts';
import type {VideoCodec} from '../track/options.ts';
import type RemoteTrack from '../track/RemoteTrack.ts';
import type RemoteTrackPublication from '../track/RemoteTrackPublication.ts';
import {Track} from '../track/Track.ts';
import type {TrackPublication} from '../track/TrackPublication.ts';
import {diffAttributes} from '../track/utils.ts';
import type {ChatMessage, LoggerOptions, TranscriptionSegment} from '../types.ts';
import {Future, isAudioTrack} from '../utils.ts';

export enum ConnectionQuality {
	Excellent = 'excellent',
	Good = 'good',
	Poor = 'poor',
	Lost = 'lost',
	Unknown = 'unknown',
}

function qualityFromProto(q: ProtoQuality): ConnectionQuality {
	switch (q) {
		case ProtoQuality.EXCELLENT:
			return ConnectionQuality.Excellent;
		case ProtoQuality.GOOD:
			return ConnectionQuality.Good;
		case ProtoQuality.POOR:
			return ConnectionQuality.Poor;
		case ProtoQuality.LOST:
			return ConnectionQuality.Lost;
		default:
			return ConnectionQuality.Unknown;
	}
}

export {ParticipantKind};

export default class Participant extends (EventEmitter as new () => TypedEmitter<ParticipantEventCallbacks>) {
	protected participantInfo?: ParticipantInfo;

	audioTrackPublications: Map<string, TrackPublication>;

	videoTrackPublications: Map<string, TrackPublication>;

	trackPublications: Map<string, TrackPublication>;

	audioLevel: number = 0;

	isSpeaking: boolean = false;

	sid: string;

	identity: string;

	name?: string;

	metadata?: string;

	private _attributes: Record<string, string>;

	lastSpokeAt?: Date | undefined;

	permissions?: ParticipantPermission;

	protected _kind: ParticipantKind;

	private _connectionQuality: ConnectionQuality = ConnectionQuality.Unknown;

	protected audioContext?: AudioContext;

	protected log: StructuredLogger = log;

	protected loggerOptions?: LoggerOptions;

	protected activeFuture?: Future<void, Error>;

	protected get logContext() {
		return {
			...this.loggerOptions?.loggerContextCb?.(),
		};
	}

	get isEncrypted() {
		return this.trackPublications.size > 0 && Array.from(this.trackPublications.values()).every((tr) => tr.isEncrypted);
	}

	get isAgent() {
		return this.permissions?.agent || this.kind === ParticipantKind.AGENT;
	}

	get isActive() {
		return this.participantInfo?.state === ParticipantInfo_State.ACTIVE;
	}

	get kind() {
		return this._kind;
	}

	get attributes(): Readonly<Record<string, string>> {
		return Object.freeze({...this._attributes});
	}

	constructor(
		sid: string,
		identity: string,
		name?: string,
		metadata?: string,
		attributes?: Record<string, string>,
		loggerOptions?: LoggerOptions,
		kind: ParticipantKind = ParticipantKind.STANDARD,
	) {
		super();

		this.loggerOptions = loggerOptions;
		this.log = getLogger(loggerOptions?.loggerName ?? LoggerNames.Participant, () => this.logContext);

		this.setMaxListeners(100);
		this.sid = sid;
		this.identity = identity;
		this.name = name;
		this.metadata = metadata;
		this.audioTrackPublications = new Map();
		this.videoTrackPublications = new Map();
		this.trackPublications = new Map();
		this._kind = kind;
		this._attributes = attributes ?? {};
	}

	getTrackPublications(): Array<TrackPublication> {
		return Array.from(this.trackPublications.values());
	}

	getTrackPublication(source: Track.Source): TrackPublication | undefined {
		for (const [, pub] of this.trackPublications) {
			if (pub.source === source) {
				return pub;
			}
		}
		return undefined;
	}

	getTrackPublicationByName(name: string): TrackPublication | undefined {
		for (const [, pub] of this.trackPublications) {
			if (pub.trackName === name) {
				return pub;
			}
		}
		return undefined;
	}

	waitUntilActive(): Promise<void> {
		if (this.isActive) {
			return Promise.resolve();
		}

		if (this.activeFuture) {
			return this.activeFuture.promise;
		}

		this.activeFuture = new Future<void, Error>();

		this.once(ParticipantEvent.Active, () => {
			this.activeFuture?.resolve?.();
			this.activeFuture = undefined;
		});
		return this.activeFuture.promise;
	}

	get connectionQuality(): ConnectionQuality {
		return this._connectionQuality;
	}

	get isCameraEnabled(): boolean {
		const track = this.getTrackPublication(Track.Source.Camera);
		return !(track?.isMuted ?? true);
	}

	get isMicrophoneEnabled(): boolean {
		const track = this.getTrackPublication(Track.Source.Microphone);
		return !(track?.isMuted ?? true);
	}

	get isScreenShareEnabled(): boolean {
		const track = this.getTrackPublication(Track.Source.ScreenShare);
		return !!track;
	}

	get isLocal(): boolean {
		return false;
	}

	get joinedAt(): Date | undefined {
		if (this.participantInfo) {
			return new Date(Number.parseInt(this.participantInfo.joinedAt.toString(), 10) * 1000);
		}
		return new Date();
	}

	updateInfo(info: ParticipantInfo): boolean {
		if (this.participantInfo && this.participantInfo.sid === info.sid && this.participantInfo.version > info.version) {
			return false;
		}
		this.identity = info.identity;
		this.sid = info.sid;
		this._setName(info.name);
		this._setMetadata(info.metadata);
		this._setAttributes(info.attributes);
		if (info.state === ParticipantInfo_State.ACTIVE && this.participantInfo?.state !== ParticipantInfo_State.ACTIVE) {
			this.emit(ParticipantEvent.Active);
		}
		if (info.permission) {
			this.setPermissions(info.permission);
		}
		this.participantInfo = info;
		return true;
	}

	private _setMetadata(md: string) {
		const changed = this.metadata !== md;
		const prevMetadata = this.metadata;
		this.metadata = md;

		if (changed) {
			this.emit(ParticipantEvent.ParticipantMetadataChanged, prevMetadata);
		}
	}

	private _setName(name: string) {
		const changed = this.name !== name;
		this.name = name;

		if (changed) {
			this.emit(ParticipantEvent.ParticipantNameChanged, name);
		}
	}

	private _setAttributes(attributes: Record<string, string>) {
		const diff = diffAttributes(this.attributes, attributes);
		this._attributes = attributes;

		if (Object.keys(diff).length > 0) {
			this.emit(ParticipantEvent.AttributesChanged, diff);
		}
	}

	setPermissions(permissions: ParticipantPermission): boolean {
		const prevPermissions = this.permissions;
		const changed =
			permissions.canPublish !== this.permissions?.canPublish ||
			permissions.canSubscribe !== this.permissions?.canSubscribe ||
			permissions.canPublishData !== this.permissions?.canPublishData ||
			permissions.hidden !== this.permissions?.hidden ||
			permissions.recorder !== this.permissions?.recorder ||
			permissions.canPublishSources.length !== this.permissions.canPublishSources.length ||
			permissions.canPublishSources.some((value, index) => value !== this.permissions?.canPublishSources[index]) ||
			permissions.canSubscribeMetrics !== this.permissions?.canSubscribeMetrics;
		this.permissions = permissions;

		if (changed) {
			this.emit(ParticipantEvent.ParticipantPermissionsChanged, prevPermissions);
		}
		return changed;
	}

	setIsSpeaking(speaking: boolean) {
		if (speaking === this.isSpeaking) {
			return;
		}
		this.isSpeaking = speaking;
		if (speaking) {
			this.lastSpokeAt = new Date();
		}
		this.emit(ParticipantEvent.IsSpeakingChanged, speaking);
	}

	setConnectionQuality(q: ProtoQuality) {
		const prevQuality = this._connectionQuality;
		this._connectionQuality = qualityFromProto(q);
		if (prevQuality !== this._connectionQuality) {
			this.emit(ParticipantEvent.ConnectionQualityChanged, this._connectionQuality);
		}
	}

	setDisconnected() {
		if (this.activeFuture) {
			this.activeFuture.reject?.(new Error('Participant disconnected'));
			this.activeFuture = undefined;
		}
	}

	setAudioContext(ctx: AudioContext | undefined) {
		this.audioContext = ctx;
		this.audioTrackPublications.forEach((track) => isAudioTrack(track.track) && track.track.setAudioContext(ctx));
	}
	addTrackPublication(publication: TrackPublication) {
		this.log.debug(`adding track publication`, {
			trackSid: publication.trackSid,
			source: publication.source,
			kind: publication.kind,
		});
		publication.on(TrackEvent.Muted, () => {
			this.emit(ParticipantEvent.TrackMuted, publication);
		});

		publication.on(TrackEvent.Unmuted, () => {
			this.emit(ParticipantEvent.TrackUnmuted, publication);
		});

		const pub = publication;
		if (pub.track) {
			pub.track.sid = publication.trackSid;
		}

		this.trackPublications.set(publication.trackSid, publication);
		switch (publication.kind) {
			case Track.Kind.Audio:
				this.audioTrackPublications.set(publication.trackSid, publication);
				break;
			case Track.Kind.Video:
				this.videoTrackPublications.set(publication.trackSid, publication);
				break;
			default:
				break;
		}
	}
}
export type ParticipantEventArgumentMap = {
	trackPublished: [publication: RemoteTrackPublication];
	trackSubscribed: [track: RemoteTrack, publication: RemoteTrackPublication];
	trackSubscriptionFailed: [trackSid: string, reason?: SubscriptionError];
	trackUnpublished: [publication: RemoteTrackPublication];
	trackUnsubscribed: [track: RemoteTrack, publication: RemoteTrackPublication];
	trackMuted: [publication: TrackPublication];
	trackUnmuted: [publication: TrackPublication];
	localTrackPublished: [publication: LocalTrackPublication];
	localTrackUnpublished: [publication: LocalTrackPublication];
	localTrackCpuConstrained: [track: LocalVideoTrack, publication: LocalTrackPublication];
	localSenderCreated: [sender: RTCRtpSender, track: Track, codec?: VideoCodec, trackId?: string];
	participantMetadataChanged: [prevMetadata: string | undefined, participant?: unknown];
	participantNameChanged: [name: string];
	dataReceived: [payload: NonSharedUint8Array, kind: DataPacket_Kind, encryptionType?: Encryption_Type];
	sipDTMFReceived: [dtmf: SipDTMF];
	transcriptionReceived: [transcription: Array<TranscriptionSegment>, publication?: TrackPublication];
	isSpeakingChanged: [speaking: boolean];
	connectionQualityChanged: [connectionQuality: ConnectionQuality];
	trackStreamStateChanged: [publication: RemoteTrackPublication, streamState: Track.StreamState];
	trackSubscriptionPermissionChanged: [publication: RemoteTrackPublication, status: TrackPublication.PermissionStatus];
	mediaDevicesError: [error: Error, kind?: MediaDeviceKind];
	audioStreamAcquired: [];
	participantPermissionsChanged: [prevPermissions?: ParticipantPermission];
	trackSubscriptionStatusChanged: [publication: RemoteTrackPublication, status: TrackPublication.SubscriptionStatus];
	attributesChanged: [changedAttributes: Record<string, string>];
	localTrackSubscribed: [trackPublication: LocalTrackPublication];
	chatMessage: [msg: ChatMessage];
	active: [];
};

export type ParticipantEventCallbacks = {
	[E in keyof ParticipantEventArgumentMap]: (...args: ParticipantEventArgumentMap[E]) => void;
};

export type ParticipantEventArguments<E extends keyof ParticipantEventCallbacks> = ParticipantEventArgumentMap[E];

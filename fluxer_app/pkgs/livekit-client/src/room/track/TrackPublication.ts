// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import type {SubscriptionError, TrackInfo, UpdateSubscription, UpdateTrackSettings} from '@livekit/protocol';
import {Encryption_Type} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type TypedEventEmitter from 'typed-emitter';
import log, {getLogger, LoggerNames} from '../../logger.ts';
import {TrackEvent} from '../events.ts';
import type {LoggerOptions, TranscriptionSegment} from '../types.ts';
import {isAudioTrack, isVideoTrack} from '../utils.ts';
import type LocalAudioTrack from './LocalAudioTrack.ts';
import type LocalVideoTrack from './LocalVideoTrack.ts';
import type RemoteAudioTrack from './RemoteAudioTrack.ts';
import type RemoteTrack from './RemoteTrack.ts';
import type RemoteVideoTrack from './RemoteVideoTrack.ts';
import {Track} from './Track.ts';
import {getLogContextFromTrack} from './utils.ts';

export abstract class TrackPublication extends (EventEmitter as new () => TypedEventEmitter<PublicationEventCallbacks>) {
	kind: Track.Kind;

	trackName: string;

	trackSid: Track.SID;

	track?: Track;

	source: Track.Source;

	mimeType?: string;

	dimensions?: Track.Dimensions;

	simulcasted?: boolean;

	trackInfo?: TrackInfo;

	protected metadataMuted: boolean = false;

	protected encryption: Encryption_Type = Encryption_Type.NONE;

	protected log = log;

	private loggerContextCb?: LoggerOptions['loggerContextCb'];

	constructor(kind: Track.Kind, id: string, name: string, loggerOptions?: LoggerOptions) {
		super();
		this.log = getLogger(loggerOptions?.loggerName ?? LoggerNames.Publication);
		this.loggerContextCb = this.loggerContextCb;
		this.setMaxListeners(100);
		this.kind = kind;
		this.trackSid = id;
		this.trackName = name;
		this.source = Track.Source.Unknown;
	}

	setTrack(track?: Track) {
		if (this.track) {
			this.track.off(TrackEvent.Muted, this.handleMuted);
			this.track.off(TrackEvent.Unmuted, this.handleUnmuted);
		}

		this.track = track;

		if (track) {
			track.on(TrackEvent.Muted, this.handleMuted);
			track.on(TrackEvent.Unmuted, this.handleUnmuted);
		}
	}

	protected get logContext() {
		return {
			...this.loggerContextCb?.(),
			...getLogContextFromTrack(this),
		};
	}

	get isMuted(): boolean {
		return this.metadataMuted;
	}

	get isEnabled(): boolean {
		return true;
	}

	get isSubscribed(): boolean {
		return this.track !== undefined;
	}

	get isEncrypted(): boolean {
		return this.encryption !== Encryption_Type.NONE;
	}

	abstract get isLocal(): boolean;

	get audioTrack(): LocalAudioTrack | RemoteAudioTrack | undefined {
		if (isAudioTrack(this.track)) {
			return this.track;
		}
		return undefined;
	}

	get videoTrack(): LocalVideoTrack | RemoteVideoTrack | undefined {
		if (isVideoTrack(this.track)) {
			return this.track;
		}
		return undefined;
	}

	handleMuted = () => {
		this.emit(TrackEvent.Muted);
	};

	handleUnmuted = () => {
		this.emit(TrackEvent.Unmuted);
	};

	updateInfo(info: TrackInfo) {
		this.trackSid = info.sid;
		this.trackName = info.name;
		this.source = Track.sourceFromProto(info.source);
		this.mimeType = info.mimeType;
		if (this.kind === Track.Kind.Video && info.width > 0) {
			this.dimensions = {
				width: info.width,
				height: info.height,
			};
			this.simulcasted = info.simulcast;
		}
		this.encryption = info.encryption;
		this.trackInfo = info;
		this.log.debug('update publication info', {...this.logContext, info});
	}
}

export namespace TrackPublication {
	export enum SubscriptionStatus {
		Desired = 'desired',
		Subscribed = 'subscribed',
		Unsubscribed = 'unsubscribed',
	}

	export enum PermissionStatus {
		Allowed = 'allowed',
		NotAllowed = 'not_allowed',
	}
}

export type PublicationEventCallbacks = {
	muted: () => void;
	unmuted: () => void;
	ended: (track?: Track) => void;
	updateSettings: (settings: UpdateTrackSettings) => void;
	subscriptionPermissionChanged: (
		status: TrackPublication.PermissionStatus,
		prevStatus: TrackPublication.PermissionStatus,
	) => void;
	updateSubscription: (sub: UpdateSubscription) => void;
	subscribed: (track: RemoteTrack) => void;
	unsubscribed: (track: RemoteTrack) => void;
	subscriptionStatusChanged: (
		status: TrackPublication.SubscriptionStatus,
		prevStatus: TrackPublication.SubscriptionStatus,
	) => void;
	subscriptionFailed: (error: SubscriptionError) => void;
	transcriptionReceived: (transcription: Array<TranscriptionSegment>) => void;
	timeSyncUpdate: (timestamp: number) => void;
	cpuConstrained: (track: LocalVideoTrack) => void;
};

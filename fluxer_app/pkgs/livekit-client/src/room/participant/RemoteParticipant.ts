// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {
	ClientInfo_Capability,
	ParticipantInfo,
	SubscriptionError,
	UpdateSubscription,
	UpdateTrackSettings,
} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type {SignalClient} from '../../api/SignalClient.ts';
import {DeferrableMap} from '../../utils/deferrable-map.ts';
import {CLIENT_PROTOCOL_DEFAULT} from '../../version.ts';
import type IncomingDataTrackManager from '../data-track/incoming/IncomingDataTrackManager.ts';
import RemoteDataTrack from '../data-track/RemoteDataTrack.ts';
import {DataTrackInfo} from '../data-track/types.ts';
import {ParticipantEvent, TrackEvent} from '../events.ts';
import type {AudioOutputOptions} from '../track/options.ts';
import RemoteAudioTrack from '../track/RemoteAudioTrack.ts';
import type RemoteTrack from '../track/RemoteTrack.ts';
import RemoteTrackPublication from '../track/RemoteTrackPublication.ts';
import RemoteVideoTrack from '../track/RemoteVideoTrack.ts';
import {Track} from '../track/Track.ts';
import type {TrackPublication} from '../track/TrackPublication.ts';
import type {AdaptiveStreamSettings} from '../track/types.ts';
import {getLogContextFromTrack} from '../track/utils.ts';
import type {LoggerOptions} from '../types.ts';
import {isAudioTrack, isRemoteTrack} from '../utils.ts';
import type {ParticipantEventArguments, ParticipantEventCallbacks} from './Participant.ts';
import Participant, {ParticipantKind} from './Participant.ts';

export default class RemoteParticipant extends Participant {
	override audioTrackPublications: Map<string, RemoteTrackPublication>;
	override videoTrackPublications: Map<string, RemoteTrackPublication>;
	override trackPublications: Map<string, RemoteTrackPublication>;
	dataTracks: DeferrableMap<RemoteDataTrack['info']['name'], RemoteDataTrack>;

	signalClient: SignalClient;

	clientProtocol: number;

	capabilities: Array<ClientInfo_Capability>;

	private volumeMap: Map<Track.Source, number>;

	private audioOutput?: AudioOutputOptions;

	static fromParticipantInfo(
		signalClient: SignalClient,
		pi: ParticipantInfo,
		loggerOptions: LoggerOptions,
		manager: IncomingDataTrackManager,
	): RemoteParticipant {
		return new RemoteParticipant(
			signalClient,
			pi.sid,
			pi.identity,
			pi.name,
			pi.metadata,
			pi.attributes,
			loggerOptions,
			pi.kind,
			pi.dataTracks.map((dti) => {
				const info = DataTrackInfo.from(dti);
				return new RemoteDataTrack(info, manager, {publisherIdentity: pi.identity});
			}),
			pi.clientProtocol,
			pi.capabilities,
		);
	}

	protected override get logContext() {
		return {
			...super.logContext,
			remoteParticipantID: this.sid,
			remoteParticipant: this.identity,
		};
	}

	constructor(
		signalClient: SignalClient,
		sid: string,
		identity?: string,
		name?: string,
		metadata?: string,
		attributes?: Record<string, string>,
		loggerOptions?: LoggerOptions,
		kind: ParticipantKind = ParticipantKind.STANDARD,
		remoteDataTracks: Array<RemoteDataTrack> = [],
		clientProtocol: number = CLIENT_PROTOCOL_DEFAULT,
		capabilities: Array<ClientInfo_Capability> = [],
	) {
		super(sid, identity || '', name, metadata, attributes, loggerOptions, kind);
		this.signalClient = signalClient;
		this.trackPublications = new Map();
		this.audioTrackPublications = new Map();
		this.videoTrackPublications = new Map();
		this.dataTracks = new DeferrableMap(
			remoteDataTracks.map((remoteDataTrack) => {
				return [remoteDataTrack.info.name, remoteDataTrack];
			}),
		);
		this.volumeMap = new Map();
		this.clientProtocol = clientProtocol;
		this.capabilities = capabilities;
	}

	override addTrackPublication(publication: RemoteTrackPublication) {
		super.addTrackPublication(publication);

		publication.on(TrackEvent.UpdateSettings, (settings: UpdateTrackSettings) => {
			this.log.debug('send update settings', {
				...this.logContext,
				...getLogContextFromTrack(publication),
				settings,
			});
			this.signalClient.sendUpdateTrackSettings(settings);
		});
		publication.on(TrackEvent.UpdateSubscription, (sub: UpdateSubscription) => {
			sub.participantTracks.forEach((pt) => {
				pt.participantSid = this.sid;
			});
			this.signalClient.sendUpdateSubscription(sub);
		});
		publication.on(TrackEvent.SubscriptionPermissionChanged, (status: TrackPublication.PermissionStatus) => {
			this.emit(ParticipantEvent.TrackSubscriptionPermissionChanged, publication, status);
		});
		publication.on(TrackEvent.SubscriptionStatusChanged, (status: TrackPublication.SubscriptionStatus) => {
			this.emit(ParticipantEvent.TrackSubscriptionStatusChanged, publication, status);
		});
		publication.on(TrackEvent.Subscribed, (track: RemoteTrack) => {
			this.emit(ParticipantEvent.TrackSubscribed, track, publication);
		});
		publication.on(TrackEvent.Unsubscribed, (previousTrack: RemoteTrack) => {
			this.emit(ParticipantEvent.TrackUnsubscribed, previousTrack, publication);
		});
		publication.on(TrackEvent.SubscriptionFailed, (error: SubscriptionError) => {
			this.emit(ParticipantEvent.TrackSubscriptionFailed, publication.trackSid, error);
		});
	}

	override getTrackPublication(source: Track.Source): RemoteTrackPublication | undefined {
		const track = super.getTrackPublication(source);
		if (track) {
			return track as RemoteTrackPublication;
		}
		return undefined;
	}

	override getTrackPublicationByName(name: string): RemoteTrackPublication | undefined {
		const track = super.getTrackPublicationByName(name);
		if (track) {
			return track as RemoteTrackPublication;
		}
		return undefined;
	}

	setVolume(volume: number, source: Track.Source.Microphone | Track.Source.ScreenShareAudio = Track.Source.Microphone) {
		this.volumeMap.set(source, volume);
		const audioPublication = this.getTrackPublication(source);
		if (audioPublication?.track) {
			(audioPublication.track as RemoteAudioTrack).setVolume(volume);
		}
	}

	getVolume(source: Track.Source.Microphone | Track.Source.ScreenShareAudio = Track.Source.Microphone) {
		const audioPublication = this.getTrackPublication(source);
		if (audioPublication?.track) {
			return (audioPublication.track as RemoteAudioTrack).getVolume();
		}
		return this.volumeMap.get(source);
	}

	addSubscribedMediaTrack(
		mediaTrack: MediaStreamTrack,
		sid: Track.SID,
		mediaStream: MediaStream,
		receiver: RTCRtpReceiver,
		adaptiveStreamSettings?: AdaptiveStreamSettings,
		triesLeft?: number,
	) {
		let publication = this.getTrackPublicationBySid(sid);

		if (!publication) {
			if (!sid.startsWith('TR')) {
				this.trackPublications.forEach((p) => {
					if (!publication && mediaTrack.kind === p.kind.toString()) {
						publication = p;
					}
				});
			}
		}

		if (!publication) {
			if (triesLeft === 0) {
				this.log.error('could not find published track', {
					...this.logContext,
					trackSid: sid,
				});
				this.emit(ParticipantEvent.TrackSubscriptionFailed, sid);
				return;
			}

			if (triesLeft === undefined) triesLeft = 20;
			setTimeout(() => {
				this.addSubscribedMediaTrack(mediaTrack, sid, mediaStream, receiver, adaptiveStreamSettings, triesLeft! - 1);
			}, 150);
			return;
		}

		if (mediaTrack.readyState === 'ended') {
			this.log.error('unable to subscribe because MediaStreamTrack is ended. Do not call MediaStreamTrack.stop()', {
				...this.logContext,
				...getLogContextFromTrack(publication),
			});
			this.emit(ParticipantEvent.TrackSubscriptionFailed, sid);
			return;
		}

		const isVideo = mediaTrack.kind === 'video';
		let track: RemoteTrack;
		if (isVideo) {
			track = new RemoteVideoTrack(mediaTrack, sid, receiver, adaptiveStreamSettings);
		} else {
			track = new RemoteAudioTrack(mediaTrack, sid, receiver, this.audioContext, this.audioOutput);
		}

		track.source = publication.source;
		track.isMuted = publication.isMuted;
		track.setMediaStream(mediaStream);
		track.start();

		publication.setTrack(track);
		if (this.volumeMap.has(publication.source) && isRemoteTrack(track) && isAudioTrack(track)) {
			track.setVolume(this.volumeMap.get(publication.source)!);
		}

		return publication;
	}

	get hasMetadata(): boolean {
		return !!this.participantInfo;
	}

	getTrackPublicationBySid(sid: Track.SID): RemoteTrackPublication | undefined {
		return this.trackPublications.get(sid);
	}

	override updateInfo(info: ParticipantInfo): boolean {
		if (!super.updateInfo(info)) {
			return false;
		}

		const validTracks = new Map<string, RemoteTrackPublication>();
		const newTracks = new Map<string, RemoteTrackPublication>();

		info.tracks.forEach((ti) => {
			let publication = this.getTrackPublicationBySid(ti.sid);
			if (!publication) {
				const kind = Track.kindFromProto(ti.type);
				if (!kind) {
					return;
				}
				publication = new RemoteTrackPublication(kind, ti, this.signalClient.connectOptions?.autoSubscribe, {
					loggerContextCb: () => this.logContext,
					loggerName: this.loggerOptions?.loggerName,
				});
				publication.updateInfo(ti);
				newTracks.set(ti.sid, publication);
				const existingTrackOfSource = Array.from(this.trackPublications.values()).find(
					(publishedTrack) => publishedTrack.source === publication?.source,
				);
				if (existingTrackOfSource && publication.source !== Track.Source.Unknown) {
					this.log.debug(
						`received a second track publication for ${this.identity} with the same source: ${publication.source}`,
						{
							...this.logContext,
							oldTrack: getLogContextFromTrack(existingTrackOfSource),
							newTrack: getLogContextFromTrack(publication),
						},
					);
				}
				this.addTrackPublication(publication);
			} else {
				publication.updateInfo(ti);
			}
			validTracks.set(ti.sid, publication);
		});

		this.trackPublications.forEach((publication) => {
			if (!validTracks.has(publication.trackSid)) {
				this.log.trace('detected removed track on remote participant, unpublishing', {
					...this.logContext,
					...getLogContextFromTrack(publication),
				});
				this.unpublishTrack(publication.trackSid, true);
			}
		});

		newTracks.forEach((publication) => {
			this.emit(ParticipantEvent.TrackPublished, publication);
		});
		return true;
	}

	unpublishTrack(sid: Track.SID, sendUnpublish?: boolean) {
		const publication = <RemoteTrackPublication>this.trackPublications.get(sid);
		if (!publication) {
			return;
		}

		const {track} = publication;
		if (track) {
			track.stop();
			publication.setTrack(undefined);
		}

		this.trackPublications.delete(sid);

		switch (publication.kind) {
			case Track.Kind.Audio:
				this.audioTrackPublications.delete(sid);
				break;
			case Track.Kind.Video:
				this.videoTrackPublications.delete(sid);
				break;
			default:
				break;
		}

		if (sendUnpublish) {
			this.emit(ParticipantEvent.TrackUnpublished, publication);
		}
	}

	async setAudioOutput(output: AudioOutputOptions) {
		this.audioOutput = output;
		const promises: Array<Promise<void>> = [];
		this.audioTrackPublications.forEach((pub) => {
			if (isAudioTrack(pub.track) && isRemoteTrack(pub.track)) {
				promises.push(pub.track.setSinkId(output.deviceId ?? 'default'));
			}
		});
		await Promise.all(promises);
	}
	addRemoteDataTrack(remoteDataTrack: RemoteDataTrack) {
		this.dataTracks.set(remoteDataTrack.info.name, remoteDataTrack);
	}
	removeRemoteDataTrack(remoteDataTrackSid: RemoteDataTrack['info']['sid']) {
		for (const [name, dataTrack] of this.dataTracks.entries()) {
			if (remoteDataTrackSid === dataTrack.info.sid) {
				this.dataTracks.delete(name);
			}
		}
	}
	override emit<E extends keyof ParticipantEventCallbacks>(event: E, ...args: ParticipantEventArguments<E>): boolean {
		this.log.trace('participant event', {...this.logContext, event, args});
		return EventEmitter.prototype.emit.call(this, event, ...args);
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import ScreenShareCodecNegotiation, {
	type CodecNegotiationSelection,
} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {noteLocalVoiceActivity} from '@app/features/voice/engine/VoiceIdleActivityBridge';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {playSelfJoinChimeOnce, startVoiceJoinChimeSequence} from '@app/features/voice/engine/VoiceSelfJoinChime';
import {
	cancelDeferredStopWatchingStreamKey,
	deferStopWatchingStreamKey,
	getStreamKeyForParticipantIdentity,
} from '@app/features/voice/engine/VoiceStreamWatchState';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import {ScreenShareWatchErrorCode, ScreenShareWatchFailures} from '@app/features/voice/state/ScreenShareWatchFailures';
import {scheduleScreenShareDecoderVerification} from '@app/features/voice/utils/ScreenShareCodecDiagnostics';
import {markVideoDecoderRuntimeFailure} from '@app/features/voice/utils/VideoDecoderCapabilities';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import type {
	LocalParticipant,
	LocalTrackPublication,
	Participant,
	RemoteParticipant,
	RemoteTrack,
	RemoteTrackPublication,
	Room,
} from 'livekit-client';
import {ParticipantEvent, RoomEvent, Track} from 'livekit-client';

const logger = new Logger('VoiceRoomEventBinder');

export interface RoomEventCallbacks {
	onConnected: () => Promise<void>;
	onDisconnected: () => void;
	onReconnecting: () => void;
	onReconnected: () => void;
}

type GuardedRoomEventHandler = <T extends ReadonlyArray<unknown>>(
	attemptId: number,
	handler: (...args: T) => void | Promise<void>,
) => (...args: T) => void;

export interface RoomEventDependencies {
	connection: {
		createGuardedHandler: GuardedRoomEventHandler;
		isDisconnecting: () => boolean;
		isUserMovePending: () => boolean;
		markConnected: () => void;
		markDisconnected: (reason: 'error') => void;
		markReconnecting: () => void;
		markReconnected: () => void;
	};
	media: {
		applyAllLocalAudioPreferences: (room: Room | null) => void;
		ensureMicrophone: (room: Room, channelId: string) => Promise<void>;
		playEntranceSound: () => Promise<void>;
		resetStreamTracking: () => void;
	};
	mediaState: {
		handleLocalTrackStateChange: (source: unknown, isPublished: boolean) => boolean;
		resetLocalMediaState: (reason: 'room_disconnect') => void;
	};
	participants: {
		clear: () => void;
		hydrateFromRoom: (room: Room) => void;
		removeParticipant: (identity: string) => void;
		updateActiveSpeakers: (speakers: Array<Participant>) => void;
		upsertParticipant: (participant: Participant) => void;
	};
	permissions: {
		applyDeafen: (room: Room, deafened: boolean) => void;
		syncWithPermissionState: (guildId: string, channelId: string, room: Room) => void;
	};
	remoteSpeaking: {
		attachIfApplicable: (participant: Participant, publication: RemoteTrackPublication, track: RemoteTrack) => void;
		clear: () => void;
		detachByIdentity: (identity: string) => void;
		detachIfTrackMatches: (participant: Participant, publication: RemoteTrackPublication) => void;
		hydrateFromRoom: (room: Room) => void;
	};
	screenShare: {
		cleanupLingeringScreenShareTracks: (participant: LocalParticipant) => Promise<void>;
		handleLocalScreenShareTrackUnpublished: (
			room: Room,
			didChangeLocalState: boolean,
			publication?: LocalTrackPublication,
		) => void;
		isScreenShareCodecRepublishInFlight: () => boolean;
		renegotiateActiveScreenShareCodec: (room: Room, selection: CodecNegotiationSelection) => Promise<unknown>;
	};
	subscriptions: {
		isScreenShareSubscribed: (participantIdentity: string) => boolean;
		reattachScreenShareAfterPublish: (participantIdentity: string, publication?: RemoteTrackPublication) => void;
		reconcileSubscriptions: () => void;
	};
	remoteTrackLifecycle?: {
		bind: (track: RemoteTrack, options: {captureId: string; sourceId: string}) => () => void;
	};
}

interface ParticipantSpeakingDisposer {
	participant: Participant;
	dispose: () => void;
}

type LatencyTunedReceiver = RTCRtpReceiver & {
	jitterBufferTarget?: number;
	playoutDelayHint?: number;
};

function getRemoteTrackReceiver(track: RemoteTrack): LatencyTunedReceiver | undefined {
	return (track as RemoteTrack & {receiver?: LatencyTunedReceiver}).receiver;
}

function applyInteractiveReceiverBuffer(track: RemoteTrack, pub: RemoteTrackPublication): void {
	const receiver = getRemoteTrackReceiver(track);
	if (!receiver) return;
	try {
		if (pub.kind === Track.Kind.Video && pub.source === Track.Source.ScreenShare) {
			receiver.jitterBufferTarget = 80;
			receiver.playoutDelayHint = 0.04;
		} else if (pub.kind === Track.Kind.Audio) {
			receiver.jitterBufferTarget = 60;
		}
	} catch (error) {
		logger.debug('Failed to apply interactive receiver buffer target', {error, source: pub.source, kind: pub.kind});
	}
}

export function bindRoomEvents(
	room: Room,
	attemptId: number,
	guildId: string | null,
	channelId: string,
	callbacks: RoomEventCallbacks,
	dependencies: RoomEventDependencies,
): void {
	const guard = dependencies.connection.createGuardedHandler;
	const participantSpeakingDisposers = new Map<string, ParticipantSpeakingDisposer>();
	const screenShareDecoderVerificationTimers = new Map<string, NodeJS.Timeout>();
	const remoteTrackLifecycleDisposers = new Map<string, () => void>();
	let codecNegotiationDisposer: (() => void) | null = null;
	let screenShareMigrationDisposer: (() => void) | null = null;
	const remoteTrackLifecycleRoleFor = (pub: RemoteTrackPublication): 'remote-screen-share' | 'remote-camera' | null => {
		if (pub.kind !== Track.Kind.Video) return null;
		if (pub.source === Track.Source.ScreenShare) return 'remote-screen-share';
		if (pub.source === Track.Source.Camera) return 'remote-camera';
		return null;
	};
	const bindRemoteTrackLifecycleIfApplicable = (
		participant: Participant,
		pub: RemoteTrackPublication,
		track: RemoteTrack,
	): void => {
		const binder = dependencies.remoteTrackLifecycle;
		if (!binder) return;
		const role = remoteTrackLifecycleRoleFor(pub);
		if (!role) return;
		const trackSid = pub.trackSid;
		if (!trackSid) return;
		const captureId = `watch:${role}:${participant.identity}:${trackSid}`;
		const sourceId = `watch:${role}:${participant.identity}:${trackSid}`;
		remoteTrackLifecycleDisposers.get(trackSid)?.();
		const dispose = binder.bind(track, {captureId, sourceId});
		remoteTrackLifecycleDisposers.set(trackSid, dispose);
	};
	const unbindRemoteTrackLifecycleIfApplicable = (pub: RemoteTrackPublication): void => {
		const trackSid = pub.trackSid;
		if (!trackSid) return;
		const dispose = remoteTrackLifecycleDisposers.get(trackSid);
		if (!dispose) return;
		dispose();
		remoteTrackLifecycleDisposers.delete(trackSid);
	};
	const clearAllRemoteTrackLifecycleBindings = (): void => {
		for (const dispose of remoteTrackLifecycleDisposers.values()) {
			dispose();
		}
		remoteTrackLifecycleDisposers.clear();
	};
	const reportScreenSharePublicationObserved = (participantIdentity: string, trackSid: string | null): void => {
		voiceMediaGraphStore.transition({
			type: 'publication.observed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			trackSid,
			at: voiceMediaGraphStore.nowMs(),
		});
	};
	const reportScreenSharePublicationLost = (participantIdentity: string): void => {
		voiceMediaGraphStore.transition({
			type: 'publication.lost',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: voiceMediaGraphStore.nowMs(),
		});
	};
	const reportScreenShareSubscriptionActual = (
		participantIdentity: string,
		changes: {subscribed?: boolean | null; enabled?: boolean | null; trackSid?: string | null},
	): void => {
		voiceMediaGraphStore.transition({
			type: 'subscription.actualChanged',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: voiceMediaGraphStore.nowMs(),
			...changes,
		});
	};
	const reportScreenShareSubscriptionCommandFailed = (
		participantIdentity: string,
		code: number,
		reason: string,
	): void => {
		voiceMediaGraphStore.transition({
			type: 'subscription.commandFailed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: voiceMediaGraphStore.nowMs(),
			code,
			reason,
		});
	};
	const clearScreenShareDecoderVerification = (trackSid: string | undefined): void => {
		if (!trackSid) return;
		const timer = screenShareDecoderVerificationTimers.get(trackSid);
		if (!timer) return;
		clearTimeout(timer);
		screenShareDecoderVerificationTimers.delete(trackSid);
	};
	const clearAllScreenShareDecoderVerifications = (): void => {
		for (const timer of screenShareDecoderVerificationTimers.values()) {
			clearTimeout(timer);
		}
		screenShareDecoderVerificationTimers.clear();
	};
	const bindCodecNegotiation = (): void => {
		codecNegotiationDisposer?.();
		codecNegotiationDisposer = ScreenShareCodecNegotiation.bind(room, {
			onSelectedCodecChanged: (selection) => {
				void dependencies.screenShare.renegotiateActiveScreenShareCodec(room, selection).catch((error) => {
					logger.warn('Failed to apply negotiated screen share codec', {
						error,
						codec: selection.codec,
						reason: selection.reason,
					});
				});
			},
		});
	};
	const unbindCodecNegotiation = (): void => {
		ScreenShareCodecNegotiation.dispose();
		codecNegotiationDisposer = null;
	};
	const bindScreenShareMigration = (): void => {
		screenShareMigrationDisposer?.();
		screenShareMigrationDisposer = ScreenSharePublicationMigration.bind(room, {guildId, channelId});
	};
	const unbindScreenShareMigration = (): void => {
		ScreenSharePublicationMigration.dispose();
		screenShareMigrationDisposer = null;
	};
	const scheduleDecoderVerification = (track: RemoteTrack, pub: RemoteTrackPublication): void => {
		if (pub.source !== Track.Source.ScreenShare || pub.kind !== Track.Kind.Video) return;
		clearScreenShareDecoderVerification(pub.trackSid);
		const trackSid = pub.trackSid;
		screenShareDecoderVerificationTimers.set(
			trackSid,
			scheduleScreenShareDecoderVerification(
				() => track.getRTCStatsReport(),
				() => {
					screenShareDecoderVerificationTimers.delete(trackSid);
				},
				(failure) => {
					if (!markVideoDecoderRuntimeFailure(failure.codec, 'screen-share-decode-stalled')) return;
					void ScreenShareCodecNegotiation.publishLocalCapabilities(room, 'manual').catch((error) => {
						logger.warn('Failed to publish updated codec capabilities after decode stall', {
							error,
							codec: failure.codec,
						});
					});
				},
			),
		);
	};
	const bindParticipantSpeakingEvents = (participant: Participant): void => {
		const existing = participantSpeakingDisposers.get(participant.identity);
		if (existing?.participant === participant) return;
		existing?.dispose();
		const handleSpeakingChanged = guard(attemptId, (speaking?: boolean) => {
			noteLocalVoiceActivity(participant, {force: speaking === true, speaking});
			dependencies.participants.upsertParticipant(participant);
		});
		participant.on(ParticipantEvent.IsSpeakingChanged, handleSpeakingChanged);
		participantSpeakingDisposers.set(participant.identity, {
			participant,
			dispose: () => {
				participant.off(ParticipantEvent.IsSpeakingChanged, handleSpeakingChanged);
			},
		});
	};
	const unbindParticipantSpeakingEvents = (identity: string): void => {
		participantSpeakingDisposers.get(identity)?.dispose();
		participantSpeakingDisposers.delete(identity);
	};
	bindParticipantSpeakingEvents(room.localParticipant);
	room.remoteParticipants.forEach((participant) => bindParticipantSpeakingEvents(participant));
	room.on(
		RoomEvent.Connected,
		guard(attemptId, async () => {
			const suppressSelfJoinSound = dependencies.connection.isUserMovePending();
			dependencies.participants.hydrateFromRoom(room);
			bindParticipantSpeakingEvents(room.localParticipant);
			room.remoteParticipants.forEach((participant) => bindParticipantSpeakingEvents(participant));
			dependencies.remoteSpeaking.hydrateFromRoom(room);
			bindCodecNegotiation();
			bindScreenShareMigration();
			dependencies.permissions.applyDeafen(room, getEffectiveAudioState().effectiveDeaf);
			dependencies.connection.markConnected();
			await callbacks.onConnected();
			const {userId, connectionId} = parseVoiceParticipantIdentity(room.localParticipant.identity);
			if (userId) {
				void startVoiceJoinChimeSequence({userId, channelId}, connectionId || null, (signal) =>
					suppressSelfJoinSound
						? Promise.resolve(false)
						: playSelfJoinChimeOnce(connectionId || null, 'livekit-room', signal),
				);
			} else if (!suppressSelfJoinSound) {
				void playSelfJoinChimeOnce(connectionId || null, 'livekit-room');
			}
			await dependencies.media.playEntranceSound();
			if (guildId && channelId) {
				dependencies.permissions.syncWithPermissionState(guildId, channelId, room);
			}
			await dependencies.media.ensureMicrophone(room, channelId);
			dependencies.media.applyAllLocalAudioPreferences(room);
		}),
	);
	room.on(
		RoomEvent.Disconnected,
		guard(attemptId, () => {
			participantSpeakingDisposers.forEach(({dispose}) => dispose());
			participantSpeakingDisposers.clear();
			unbindCodecNegotiation();
			unbindScreenShareMigration();
			clearAllScreenShareDecoderVerifications();
			clearAllRemoteTrackLifecycleBindings();
			dependencies.remoteSpeaking.clear();
			dependencies.mediaState.resetLocalMediaState('room_disconnect');
			dependencies.media.resetStreamTracking();
			if ('localParticipant' in room && room.localParticipant) {
				void dependencies.screenShare.cleanupLingeringScreenShareTracks(room.localParticipant).catch((error) => {
					logger.warn('Failed to clean up screen-share audio on disconnect', {error});
				});
			}
			callbacks.onDisconnected();
			dependencies.participants.clear();
			dependencies.connection.markDisconnected('error');
		}),
	);
	room.on(
		RoomEvent.Reconnecting,
		guard(attemptId, () => {
			callbacks.onReconnecting();
			dependencies.connection.markReconnecting();
		}),
	);
	room.on(
		RoomEvent.Reconnected,
		guard(attemptId, () => {
			dependencies.participants.hydrateFromRoom(room);
			bindParticipantSpeakingEvents(room.localParticipant);
			room.remoteParticipants.forEach((participant) => bindParticipantSpeakingEvents(participant));
			dependencies.remoteSpeaking.hydrateFromRoom(room);
			dependencies.permissions.applyDeafen(room, getEffectiveAudioState().effectiveDeaf);
			dependencies.connection.markReconnected();
			callbacks.onReconnected();
			dependencies.media.applyAllLocalAudioPreferences(room);
			dependencies.subscriptions.reconcileSubscriptions();
		}),
	);
	room.on(
		RoomEvent.ParticipantConnected,
		guard(attemptId, (p: Participant) => {
			bindParticipantSpeakingEvents(p);
			dependencies.participants.upsertParticipant(p);
			if (!p.identity.startsWith('user_')) {
				SoundCommands.playSound(SoundType.ViewerJoin);
			}
		}),
	);
	room.on(
		RoomEvent.ParticipantDisconnected,
		guard(attemptId, (p: Participant) => {
			unbindParticipantSpeakingEvents(p.identity);
			dependencies.remoteSpeaking.detachByIdentity(p.identity);
			dependencies.participants.removeParticipant(p.identity);
			if (dependencies.connection.isDisconnecting()) return;
			if (p.identity === room.localParticipant?.identity) return;
			if (!p.identity.startsWith('user_')) {
				SoundCommands.playSound(SoundType.ViewerLeave);
			}
		}),
	);
	room.on(
		RoomEvent.TrackSubscribed,
		guard(attemptId, (track: RemoteTrack, pub: RemoteTrackPublication, participant: Participant) => {
			try {
				logger.debug('Track subscribed', {
					participantIdentity: participant.identity,
					source: pub.source,
					trackSid: pub.trackSid,
					isSubscribed: pub.isSubscribed,
					isScreenShareAudio: pub.source === Track.Source.ScreenShareAudio,
				});
				if (pub.kind === Track.Kind.Audio) {
					ParticipantVolume.applySettingsToParticipant(participant as RemoteParticipant);
				}
			} catch (error) {
				logger.warn('Failed to apply participant volume on track subscribe', {
					error,
					participantIdentity: participant.identity,
					trackSid: pub.trackSid,
				});
			}
			applyInteractiveReceiverBuffer(track, pub);
			scheduleDecoderVerification(track, pub);
			dependencies.remoteSpeaking.attachIfApplicable(participant, pub, track);
			bindRemoteTrackLifecycleIfApplicable(participant, pub, track);
			if (!participant.isLocal && pub.source === Track.Source.ScreenShare) {
				reportScreenShareSubscriptionActual(participant.identity, {
					subscribed: true,
					trackSid: pub.trackSid ?? null,
				});
			}
			dependencies.participants.upsertParticipant(participant);
		}),
	);
	room.on(
		RoomEvent.TrackUnsubscribed,
		guard(attemptId, (_t: RemoteTrack, pub: RemoteTrackPublication, p: Participant) => {
			clearScreenShareDecoderVerification(pub.trackSid);
			unbindRemoteTrackLifecycleIfApplicable(pub);
			dependencies.remoteSpeaking.detachIfTrackMatches(p, pub);
			if (!p.isLocal && pub.source === Track.Source.ScreenShare) {
				reportScreenShareSubscriptionActual(p.identity, {subscribed: false});
			}
			dependencies.participants.upsertParticipant(p);
		}),
	);
	room.on(
		RoomEvent.TrackSubscriptionFailed,
		guard(attemptId, (trackSid: string, participant: RemoteParticipant, reason?: unknown) => {
			const publication = participant.trackPublications.get(trackSid) as RemoteTrackPublication | undefined;
			if (publication?.source !== Track.Source.ScreenShare) return;
			const streamKey = getStreamKeyForParticipantIdentity(guildId, channelId, participant.identity);
			reportScreenShareSubscriptionActual(participant.identity, {subscribed: false});
			reportScreenShareSubscriptionCommandFailed(
				participant.identity,
				ScreenShareWatchErrorCode.RemoteTrackSubscriptionFailed,
				'remote-track-subscription-failed',
			);
			ScreenShareWatchFailures.reportFailure({
				streamKey: streamKey ?? undefined,
				participantIdentity: participant.identity,
				participantSid: participant.sid,
				trackSid,
				source: Track.Source.ScreenShare,
				code: ScreenShareWatchErrorCode.RemoteTrackSubscriptionFailed,
				reason: 'remote-track-subscription-failed',
				error: reason,
			});
		}),
	);
	room.on(
		RoomEvent.TrackUnpublished,
		guard(attemptId, (pub: RemoteTrackPublication, p: Participant) => {
			clearScreenShareDecoderVerification(pub.trackSid);
			if (!p.isLocal && pub.source === Track.Source.ScreenShare) {
				const replacementPublication = ScreenSharePublicationMigration.selectScreenSharePublication(p);
				if (replacementPublication) {
					reportScreenSharePublicationObserved(p.identity, replacementPublication.trackSid ?? null);
				} else {
					reportScreenSharePublicationLost(p.identity);
					const streamKey = getStreamKeyForParticipantIdentity(guildId, channelId, p.identity);
					if (streamKey) {
						deferStopWatchingStreamKey(streamKey, {guildId, channelId});
					}
				}
				dependencies.subscriptions.reconcileSubscriptions();
			}
			dependencies.participants.upsertParticipant(p);
		}),
	);
	room.on(
		RoomEvent.TrackMuted,
		guard(attemptId, (pub, p: Participant) => {
			if (!p.isLocal && pub.source === Track.Source.Microphone) {
				dependencies.remoteSpeaking.detachIfTrackMatches(p, pub as RemoteTrackPublication);
			}
			dependencies.participants.upsertParticipant(p);
		}),
	);
	room.on(
		RoomEvent.TrackUnmuted,
		guard(attemptId, (pub, p: Participant) => {
			if (!p.isLocal && pub.source === Track.Source.Microphone) {
				const remotePub = pub as RemoteTrackPublication;
				const track = remotePub.track;
				if (track && remotePub.isSubscribed) {
					dependencies.remoteSpeaking.attachIfApplicable(p, remotePub, track);
				}
			}
			dependencies.participants.upsertParticipant(p);
		}),
	);
	room.on(
		RoomEvent.ParticipantMetadataChanged,
		guard(attemptId, (_m, p: Participant) => dependencies.participants.upsertParticipant(p)),
	);
	room.on(
		RoomEvent.ParticipantAttributesChanged,
		guard(attemptId, (_a, p: Participant) => dependencies.participants.upsertParticipant(p)),
	);
	room.on(
		RoomEvent.ParticipantNameChanged,
		guard(attemptId, (_n, p: Participant) => dependencies.participants.upsertParticipant(p)),
	);
	room.on(
		RoomEvent.ConnectionQualityChanged,
		guard(attemptId, (_q, p: Participant) => dependencies.participants.upsertParticipant(p)),
	);
	room.on(
		RoomEvent.LocalTrackPublished,
		guard(attemptId, (pub, p: Participant) => {
			if (p.isLocal) {
				dependencies.mediaState.handleLocalTrackStateChange(pub.source, true);
			}
			dependencies.participants.upsertParticipant(p);
		}),
	);
	room.on(
		RoomEvent.LocalTrackUnpublished,
		guard(attemptId, (pub, p: Participant) => {
			if (p.isLocal) {
				if (pub.source === Track.Source.ScreenShare && dependencies.screenShare.isScreenShareCodecRepublishInFlight()) {
					dependencies.participants.upsertParticipant(p);
					return;
				}
				const didChangeLocalState = dependencies.mediaState.handleLocalTrackStateChange(pub.source, false);
				if (pub.source === Track.Source.ScreenShare && 'localParticipant' in room && room.localParticipant) {
					dependencies.screenShare.handleLocalScreenShareTrackUnpublished(
						room,
						didChangeLocalState,
						pub as LocalTrackPublication,
					);
				}
			}
			dependencies.participants.upsertParticipant(p);
		}),
	);
	room.on(
		RoomEvent.ActiveSpeakersChanged,
		guard(attemptId, (speakers: Array<Participant>) => {
			const localParticipant = room.localParticipant;
			noteLocalVoiceActivity(localParticipant, {
				speaking: speakers.some((speaker) => speaker.identity === localParticipant.identity),
			});
			dependencies.participants.updateActiveSpeakers(speakers);
		}),
	);
	room.on(
		RoomEvent.TrackPublished,
		guard(attemptId, (pub: RemoteTrackPublication, participant: Participant) => {
			try {
				logger.debug('Track published', {
					source: pub.source,
					trackSid: pub.trackSid,
				});
				if (pub.source === Track.Source.Microphone) {
					pub.setSubscribed(!getEffectiveAudioState().effectiveDeaf);
					return;
				}
				const isWatchedScreenShare =
					pub.source === Track.Source.ScreenShare &&
					!participant.isLocal &&
					dependencies.subscriptions.isScreenShareSubscribed(participant.identity);
				if (
					pub.source === Track.Source.Camera ||
					(pub.source === Track.Source.ScreenShare && !isWatchedScreenShare) ||
					pub.source === Track.Source.ScreenShareAudio
				) {
					pub.setSubscribed(false);
				} else if (isWatchedScreenShare) {
					pub.setSubscribed(true);
				}
				if (pub.source === Track.Source.ScreenShare && !participant.isLocal) {
					reportScreenSharePublicationObserved(participant.identity, pub.trackSid ?? null);
					const streamKey = getStreamKeyForParticipantIdentity(guildId, channelId, participant.identity);
					if (streamKey) {
						cancelDeferredStopWatchingStreamKey(streamKey);
					}
					dependencies.subscriptions.reattachScreenShareAfterPublish(participant.identity, pub);
					dependencies.subscriptions.reconcileSubscriptions();
				}
			} catch (error) {
				logger.warn('Failed to handle published voice track', {
					error,
					participantIdentity: participant.identity,
					source: pub.source,
					trackSid: pub.trackSid,
				});
			}
		}),
	);
}

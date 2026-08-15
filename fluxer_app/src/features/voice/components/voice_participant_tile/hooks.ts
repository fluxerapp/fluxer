// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import ContextMenu from '@app/features/ui/state/ContextMenu';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import {parseStreamKey} from '@app/features/voice/components/StreamKeys';
import {
	buildPreviewBlobFromDataUrl,
	buildPreviewBlobFromVideo,
} from '@app/features/voice/components/voice_participant_tile/previewEncoding';
import {
	getScreenShareVideoSubscriptionRecoveryKey,
	screenShareVideoSubscriptionRecoveryCoordinator,
} from '@app/features/voice/components/voice_participant_tile/ScreenShareVideoSubscriptionRecovery';
import {
	getUploadUrlExpiresAtMs,
	isUploadUrlFresh,
	StreamPreviewUploadScheduler,
	type StreamPreviewUploadUrlCacheEntryLike,
} from '@app/features/voice/components/voice_participant_tile/StreamPreviewUploadPolicy';
import {logger} from '@app/features/voice/components/voice_participant_tile/shared';
import {resolveDevicePixelRatio} from '@app/features/voice/DevicePixelRatio';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {isVoiceEngineV2NativeProjectionActiveFromMediaEngine} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	buildVoiceMediaGraphNativeCameraSubscriptionCommand,
	selectVoiceMediaGraphHasFailureForStreamKey,
	selectVoiceMediaGraphViewerStreamKeys,
	type VoiceMediaGraphSnapshot,
} from '@app/features/voice/engine/VoiceMediaGraph';
import type {VoiceMediaGraphVideoQuality} from '@app/features/voice/engine/VoiceMediaGraphSubscriptionTypes';
import {
	asVoiceTrackSource,
	isScreenShareAudioPublicationLike,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import {pickCameraSubscriptionQuality} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraResolutionPresets';
import VoiceEngineV2AppSubscriptionAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppSubscriptionAdapter';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {
	type ScreenSharePublicationOperation,
	syncScreenSharePublication,
	syncWatchedScreenSharePublications,
} from '@app/features/voice/utils/ScreenShareSubscriptionPolicy';
import {
	STREAM_PREVIEW_CONTENT_TYPE_JPEG,
	STREAM_PREVIEW_INITIAL_UPLOAD_INTERVAL_MS,
	STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS,
} from '@fluxer/constants/src/StreamConstants';
import type {StreamPreviewUploadUrlResponseSchema} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {isTrackReference, TrackRefContext, type TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {type Participant, ParticipantEvent, type RemoteTrackPublication, type Track} from 'livekit-client';
import {autorun} from 'mobx';
import type React from 'react';
import {useContext, useEffect, useMemo, useRef, useState} from 'react';

const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;
const SCREEN_SHARE_AUDIO_SOURCE = VoiceTrackSource.ScreenShareAudio as Track.Source;
const VOICE_ENGINE_V2_SCREEN_SOURCE = 'screen';
export const VIDEO_UNSUBSCRIBE_GRACE_MS = 2000;

export class UnsubscribeGraceGate {
	private timeoutId: ReturnType<typeof setTimeout> | null = null;

	constructor(private readonly graceMs: number) {}

	get isPending(): boolean {
		return this.timeoutId !== null;
	}

	scheduleDisable(onDisable: () => void): void {
		if (this.timeoutId !== null) return;
		this.timeoutId = setTimeout(() => {
			this.timeoutId = null;
			onDisable();
		}, this.graceMs);
	}

	cancel(): void {
		if (this.timeoutId === null) return;
		clearTimeout(this.timeoutId);
		this.timeoutId = null;
	}
}

interface AutoVideoSubscriptionRequest {
	participantIdentity: string | null;
	trackSid: string | null;
	desired: boolean | null;
	quality: VoiceMediaGraphVideoQuality | null;
}

function createEmptyAutoVideoSubscriptionRequest(): AutoVideoSubscriptionRequest {
	return {participantIdentity: null, trackSid: null, desired: null, quality: null};
}

function subscribeNativeCamera(participantIdentity: string, quality: VoiceMediaGraphVideoQuality): void {
	VoiceEngineV2AppSubscriptionAdapter.setRemoteTrackSubscription(
		buildVoiceMediaGraphNativeCameraSubscriptionCommand({participantIdentity, subscribed: true, quality}),
	).catch((error) => {
		logger.error('Native camera subscription update failed', error);
	});
}

function unsubscribeNativeCamera(participantIdentity: string): void {
	VoiceEngineV2AppSubscriptionAdapter.setRemoteTrackSubscription(
		buildVoiceMediaGraphNativeCameraSubscriptionCommand({participantIdentity, subscribed: false}),
	).catch((error) => {
		logger.error('Native camera subscription update failed', error);
	});
}

export function useNativeCameraSubscriptionQuality<T extends HTMLElement>(
	ref: React.RefObject<T | null>,
	enabled: boolean,
): VoiceMediaGraphVideoQuality {
	const [quality, setQuality] = useState<VoiceMediaGraphVideoQuality>('low');
	useEffect(() => {
		if (!enabled) {
			setQuality('low');
			return;
		}
		const element = ref.current;
		if (!element) return;
		const ownerWindow = element.ownerDocument.defaultView;
		const measure = (): void => {
			const ratio = resolveDevicePixelRatio(ownerWindow);
			setQuality(pickCameraSubscriptionQuality(element.clientWidth * ratio, element.clientHeight * ratio));
		};
		measure();
		if (ownerWindow == null || typeof ownerWindow.ResizeObserver === 'undefined') return;
		const observer = new ownerWindow.ResizeObserver(measure);
		observer.observe(element);
		return () => observer.disconnect();
	}, [enabled, ref]);
	return quality;
}

function isNativeCameraPlaceholderSubscriptionTarget(trackRef: TrackReferenceOrPlaceholder): boolean {
	return asVoiceTrackSource(trackRef.source) === VoiceTrackSource.Camera && Boolean(trackRef.participant?.identity);
}

interface StreamPreviewUploadUrlCacheEntry extends StreamPreviewUploadUrlCacheEntryLike {
	streamKey: string;
	response: StreamPreviewUploadUrlResponseSchema;
	expiresAtMs: number;
}

function shouldRefreshPreviewUploadUrlStatus(status: number): boolean {
	return status === 401 || status === 403;
}

function shouldRefreshPreviewUploadUrl(error: unknown): boolean {
	return error instanceof HttpError && shouldRefreshPreviewUploadUrlStatus(error.status);
}

function isVideoReadyForPreviewUpload(videoEl: HTMLVideoElement | null): videoEl is HTMLVideoElement {
	return videoEl != null && videoEl.readyState >= 2 && videoEl.videoWidth > 0 && videoEl.videoHeight > 0;
}

function isScreensharePreviewUploadSessionActive(streamKey: string, channelId: string | undefined): boolean {
	if (!channelId) return false;
	const parsedStreamKey = parseStreamKey(streamKey);
	if (!parsedStreamKey) return false;
	if (parsedStreamKey.channelId !== channelId) return false;
	if (parsedStreamKey.connectionId !== MediaEngine.connectionId) return false;
	if (MediaEngine.channelId !== channelId) return false;
	if (!MediaEngine.connected) return false;
	return LocalVoiceState.getSelfStream();
}

export function useEffectiveTrackRef(explicit?: TrackReferenceOrPlaceholder) {
	const ctx = useContext(TrackRefContext as React.Context<TrackReferenceOrPlaceholder | undefined>);
	return (explicit ?? ctx) as TrackReferenceOrPlaceholder | undefined;
}

export function useIntersection<T extends Element>(enabled: boolean) {
	const ref = useRef<T | null>(null);
	const [isIntersecting, setIsIntersecting] = useState(false);
	useEffect(() => {
		if (!enabled) {
			setIsIntersecting(false);
			return;
		}
		const el = ref.current;
		if (!el) return;
		const ownerWindow = el.ownerDocument.defaultView ?? window;
		if (typeof ownerWindow.IntersectionObserver === 'undefined') {
			setIsIntersecting(true);
			return;
		}
		const observer = new ownerWindow.IntersectionObserver(
			(observerEntries) => setIsIntersecting(observerEntries.some((entry) => entry.isIntersecting)),
			{rootMargin: '120px', threshold: [0, 0.1]},
		);
		observer.observe(el);
		return () => observer.disconnect();
	}, [enabled]);
	return {ref, isIntersecting};
}

export function useTileContextMenuActive(tileElRef: React.RefObject<HTMLElement | null>) {
	const [open, setOpen] = useState(false);
	useEffect(() => {
		const disposer = autorun(() => {
			const cm = ContextMenu.contextMenu;
			const target = cm?.target?.target;
			const el = tileElRef.current;
			setOpen(Boolean(cm && target instanceof Node && el && el.contains(target)));
		});
		return () => disposer();
	}, [tileElRef]);
	return open;
}

function unsubscribeManagedVideoPublication(
	managedPublicationRef: React.MutableRefObject<RemoteTrackPublication | null>,
	publication: RemoteTrackPublication | null,
): void {
	if (!publication) return;
	try {
		if (publication.isSubscribed) {
			publication.setSubscribed(false);
		}
	} catch (err) {
		logger.error('setSubscribed(false) failed for managed video publication', err);
	}
	if (managedPublicationRef.current === publication) {
		managedPublicationRef.current = null;
	}
}

export function useAutoVideoSubscription(opts: {
	enabled: boolean;
	trackRef: TrackReferenceOrPlaceholder;
	isIntersecting: boolean;
	videoLocallyDisabled: boolean;
	isLocalParticipant: boolean;
	isScreenShare: boolean;
	nativeCameraQuality: VoiceMediaGraphVideoQuality;
}) {
	const {
		enabled,
		trackRef,
		isIntersecting,
		videoLocallyDisabled,
		isLocalParticipant,
		isScreenShare,
		nativeCameraQuality,
	} = opts;
	const lastRequestedRef = useRef<AutoVideoSubscriptionRequest>(createEmptyAutoVideoSubscriptionRequest());
	const managedPublicationRef = useRef<RemoteTrackPublication | null>(null);
	const graceGateRef = useRef<UnsubscribeGraceGate | null>(null);
	if (graceGateRef.current === null) {
		graceGateRef.current = new UnsubscribeGraceGate(VIDEO_UNSUBSCRIBE_GRACE_MS);
	}
	useEffect(() => {
		const graceGate = graceGateRef.current;
		if (!graceGate) return;
		const isNativeEngine = isVoiceEngineV2NativeProjectionActiveFromMediaEngine();
		const unsubscribeNativeRequest = (): void => {
			const previous = lastRequestedRef.current;
			if (!isNativeEngine || !previous.participantIdentity) return;
			if (previous.desired !== true && managedPublicationRef.current === null) return;
			unsubscribeNativeCamera(previous.participantIdentity);
		};
		const unsubscribeManagedPublication = (publication: RemoteTrackPublication | null): void => {
			unsubscribeManagedVideoPublication(managedPublicationRef, publication);
		};
		const reset = (): void => {
			graceGate.cancel();
			unsubscribeNativeRequest();
			unsubscribeManagedPublication(managedPublicationRef.current);
			lastRequestedRef.current = createEmptyAutoVideoSubscriptionRequest();
		};
		if (!enabled || isLocalParticipant || isScreenShare) {
			reset();
			return;
		}
		if (isNativeEngine && !isTrackReference(trackRef) && isNativeCameraPlaceholderSubscriptionTarget(trackRef)) {
			const participantIdentity = trackRef.participant.identity;
			const shouldSubscribe = isIntersecting && !videoLocallyDisabled;
			const desiredQuality = shouldSubscribe ? nativeCameraQuality : null;
			const previousRequest = lastRequestedRef.current;
			const requestChanged =
				previousRequest.trackSid !== null || previousRequest.participantIdentity !== participantIdentity;
			if (requestChanged && previousRequest.participantIdentity) {
				unsubscribeNativeCamera(previousRequest.participantIdentity);
			}
			if (requestChanged) {
				lastRequestedRef.current = {participantIdentity, trackSid: null, desired: null, quality: null};
			}
			if (lastRequestedRef.current.desired === shouldSubscribe && lastRequestedRef.current.quality === desiredQuality) {
				return;
			}
			lastRequestedRef.current.desired = shouldSubscribe;
			lastRequestedRef.current.quality = desiredQuality;
			if (shouldSubscribe) {
				graceGate.cancel();
				subscribeNativeCamera(participantIdentity, nativeCameraQuality);
				managedPublicationRef.current = null;
				return;
			}
			if (videoLocallyDisabled) {
				graceGate.cancel();
				unsubscribeNativeCamera(participantIdentity);
				managedPublicationRef.current = null;
				return;
			}
			graceGate.scheduleDisable(() => {
				unsubscribeNativeCamera(participantIdentity);
			});
			return;
		}
		if (!isTrackReference(trackRef)) {
			reset();
			return;
		}
		const pub = trackRef.publication as RemoteTrackPublication | undefined;
		if (!pub || typeof pub.setSubscribed !== 'function') {
			reset();
			return;
		}
		if (managedPublicationRef.current && managedPublicationRef.current !== pub) {
			graceGate.cancel();
			unsubscribeManagedPublication(managedPublicationRef.current);
		}
		const shouldSubscribe = isIntersecting && !videoLocallyDisabled;
		const trackSid = pub.trackSid ?? null;
		const participantIdentity = trackRef.participant.identity;
		const previousRequest = lastRequestedRef.current;
		const requestChanged =
			previousRequest.trackSid !== trackSid || previousRequest.participantIdentity !== participantIdentity;
		if (isNativeEngine && requestChanged && previousRequest.participantIdentity && managedPublicationRef.current) {
			unsubscribeNativeCamera(previousRequest.participantIdentity);
		}
		if (requestChanged) {
			lastRequestedRef.current = {participantIdentity, trackSid, desired: null, quality: null};
		}
		if (isNativeEngine) {
			const desiredQuality = shouldSubscribe ? nativeCameraQuality : null;
			if (lastRequestedRef.current.desired === shouldSubscribe && lastRequestedRef.current.quality === desiredQuality) {
				return;
			}
			lastRequestedRef.current.desired = shouldSubscribe;
			lastRequestedRef.current.quality = desiredQuality;
			if (shouldSubscribe) {
				graceGate.cancel();
				subscribeNativeCamera(participantIdentity, nativeCameraQuality);
				managedPublicationRef.current = pub;
				return;
			}
			if (videoLocallyDisabled) {
				graceGate.cancel();
				unsubscribeNativeCamera(participantIdentity);
				managedPublicationRef.current = null;
				return;
			}
			graceGate.scheduleDisable(() => {
				unsubscribeNativeCamera(participantIdentity);
				if (managedPublicationRef.current === pub) {
					managedPublicationRef.current = null;
				}
			});
			return;
		}
		if (pub.isSubscribed === shouldSubscribe) {
			lastRequestedRef.current.desired = shouldSubscribe;
			if (shouldSubscribe) {
				graceGate.cancel();
				managedPublicationRef.current = pub;
			} else if (managedPublicationRef.current === pub) {
				managedPublicationRef.current = null;
			}
			return;
		}
		if (lastRequestedRef.current.desired === shouldSubscribe) return;
		lastRequestedRef.current.desired = shouldSubscribe;
		if (shouldSubscribe || videoLocallyDisabled) {
			graceGate.cancel();
			try {
				pub.setSubscribed(shouldSubscribe);
				managedPublicationRef.current = shouldSubscribe ? pub : null;
			} catch (err) {
				logger.error('setSubscribed failed', err);
			}
			return;
		}
		graceGate.scheduleDisable(() => {
			try {
				if (pub.isSubscribed) {
					pub.setSubscribed(false);
				}
				if (managedPublicationRef.current === pub) {
					managedPublicationRef.current = null;
				}
			} catch (err) {
				logger.error('setSubscribed(false) failed after unsubscribe grace period', err);
			}
		});
	}, [enabled, trackRef, isIntersecting, videoLocallyDisabled, isLocalParticipant, isScreenShare, nativeCameraQuality]);
	useEffect(() => {
		return () => {
			graceGateRef.current?.cancel();
			const previous = lastRequestedRef.current;
			if (
				isVoiceEngineV2NativeProjectionActiveFromMediaEngine() &&
				previous.participantIdentity &&
				(previous.desired === true || managedPublicationRef.current !== null)
			) {
				unsubscribeNativeCamera(previous.participantIdentity);
			}
			const publication = managedPublicationRef.current;
			managedPublicationRef.current = null;
			lastRequestedRef.current = createEmptyAutoVideoSubscriptionRequest();
			if (!publication) return;
			try {
				if (publication.isSubscribed) {
					publication.setSubscribed(false);
				}
			} catch (err) {
				logger.error('setSubscribed(false) failed during video publication cleanup', err);
			}
		};
	}, []);
}

interface ScreenShareAudioPublicationState {
	publication: RemoteTrackPublication | null;
	hasTrack: boolean;
}

function unsubscribeRemotePublication(publication: RemoteTrackPublication | null, label: string): void {
	syncScreenSharePublication({
		publication,
		label,
		shouldSubscribe: false,
		shouldEnable: false,
		onError: (operation, operationLabel, err) => logger.error(`${operation} failed for ${operationLabel}`, err),
	});
}

function getScreenShareAudioPublication(participant: Participant): RemoteTrackPublication | null {
	const direct = participant.getTrackPublication(SCREEN_SHARE_AUDIO_SOURCE) as RemoteTrackPublication | undefined;
	if (direct) return direct;
	for (const publication of participant.audioTrackPublications.values()) {
		if (isScreenShareAudioPublicationLike(publication)) {
			return publication as RemoteTrackPublication;
		}
	}
	return null;
}

export function useScreenShareAudioPublication(
	participant: Participant,
	enabled: boolean,
): ScreenShareAudioPublicationState {
	const [publication, setPublication] = useState<RemoteTrackPublication | null>(null);
	const [hasTrack, setHasTrack] = useState(false);
	useEffect(() => {
		if (!enabled) {
			setPublication(null);
			setHasTrack(false);
			return;
		}
		const update = () => {
			const next = getScreenShareAudioPublication(participant);
			logger.debug('Resolved screen share audio publication', {
				participantIdentity: participant.identity,
				trackSid: next?.trackSid ?? null,
				hasTrack: Boolean(next?.track),
				isSubscribed: next?.isSubscribed ?? null,
				source: next?.source ?? null,
				trackName: next?.trackName ?? null,
			});
			setPublication((previousPublication) => {
				const nextPublication = next ?? null;
				return previousPublication === nextPublication ? previousPublication : nextPublication;
			});
			setHasTrack(Boolean(next?.track));
		};
		update();
		participant.on(ParticipantEvent.TrackPublished, update);
		participant.on(ParticipantEvent.TrackUnpublished, update);
		participant.on(ParticipantEvent.TrackSubscribed, update);
		participant.on(ParticipantEvent.TrackUnsubscribed, update);
		participant.on(ParticipantEvent.TrackMuted, update);
		participant.on(ParticipantEvent.TrackUnmuted, update);
		return () => {
			participant.off(ParticipantEvent.TrackPublished, update);
			participant.off(ParticipantEvent.TrackUnpublished, update);
			participant.off(ParticipantEvent.TrackSubscribed, update);
			participant.off(ParticipantEvent.TrackUnsubscribed, update);
			participant.off(ParticipantEvent.TrackMuted, update);
			participant.off(ParticipantEvent.TrackUnmuted, update);
		};
	}, [participant, enabled]);
	return {publication, hasTrack};
}

export function useScreenshareWatchSubscription(opts: {
	isScreenShare: boolean;
	trackRef: TrackReferenceOrPlaceholder;
	userWantsToWatch: boolean;
	videoLocallyDisabled: boolean;
	isOwnScreenShare: boolean;
	audioEnabled: boolean;
	audioPublication?: RemoteTrackPublication | null;
	streamKey?: string;
	onVideoSubscriptionError?: (operation: ScreenSharePublicationOperation, label: string, error: unknown) => void;
	getGraphSnapshot: () => VoiceMediaGraphSnapshot;
}) {
	const {
		isScreenShare,
		trackRef,
		userWantsToWatch,
		videoLocallyDisabled,
		isOwnScreenShare,
		audioEnabled,
		audioPublication,
		streamKey,
		onVideoSubscriptionError,
		getGraphSnapshot,
	} = opts;
	const streamKeyRef = useRef(streamKey);
	streamKeyRef.current = streamKey;
	const getGraphSnapshotRef = useRef(getGraphSnapshot);
	getGraphSnapshotRef.current = getGraphSnapshot;
	useStoreVersion(ScreenSharePublicationMigration);
	const screenSharePublicationMigrationVersion = ScreenSharePublicationMigration.version;
	const publication = useMemo(() => {
		if (asVoiceTrackSource(trackRef.source) !== VoiceTrackSource.ScreenShare) return undefined;
		if (isTrackReference(trackRef)) return trackRef.publication as RemoteTrackPublication | undefined;
		return (
			ScreenSharePublicationMigration.selectScreenSharePublication(trackRef.participant) ??
			(trackRef.participant.getTrackPublication(SCREEN_SHARE_SOURCE) as RemoteTrackPublication | undefined)
		);
	}, [trackRef, screenSharePublicationMigrationVersion]);
	const publicationTrackSid = publication?.trackSid ?? null;
	const participantIdentity = trackRef.participant?.identity ?? null;
	const previousPublicationRef = useRef<RemoteTrackPublication | null>(null);
	const previousTrackSidRef = useRef<string | null>(null);
	const managedScreenShareParticipantIdentityRef = useRef<string | null>(null);
	const audioTrackSid = audioPublication?.trackSid ?? null;
	const previousAudioPublicationRef = useRef<RemoteTrackPublication | null>(null);
	const previousAudioTrackSidRef = useRef<string | null>(null);
	useEffect(() => {
		const previousParticipantIdentity = managedScreenShareParticipantIdentityRef.current;
		const shouldManageSubscription =
			isScreenShare && !isOwnScreenShare && userWantsToWatch && !videoLocallyDisabled && participantIdentity != null;
		if (!shouldManageSubscription) {
			if (previousParticipantIdentity) {
				MediaEngine.voiceEngineV2Controller.unwatchStream({
					participantIdentity: previousParticipantIdentity,
					source: VOICE_ENGINE_V2_SCREEN_SOURCE,
				});
				managedScreenShareParticipantIdentityRef.current = null;
			}
			return;
		}
		if (previousParticipantIdentity && previousParticipantIdentity !== participantIdentity) {
			MediaEngine.voiceEngineV2Controller.unwatchStream({
				participantIdentity: previousParticipantIdentity,
				source: VOICE_ENGINE_V2_SCREEN_SOURCE,
			});
		}
		MediaEngine.voiceEngineV2Controller.watchStream({
			participantIdentity,
			source: VOICE_ENGINE_V2_SCREEN_SOURCE,
			trackSid: publicationTrackSid,
			quality: 'high',
			enabled: true,
		});
		managedScreenShareParticipantIdentityRef.current = participantIdentity;
	}, [
		isScreenShare,
		isOwnScreenShare,
		userWantsToWatch,
		videoLocallyDisabled,
		participantIdentity,
		publicationTrackSid,
	]);
	useEffect(() => {
		const pub = publication;
		if (!isScreenShare || isOwnScreenShare || !pub) {
			unsubscribeRemotePublication(previousPublicationRef.current, 'previous screen share publication');
			previousPublicationRef.current = null;
			previousTrackSidRef.current = null;
			return;
		}
		if (previousTrackSidRef.current && previousTrackSidRef.current !== publicationTrackSid) {
			unsubscribeRemotePublication(previousPublicationRef.current, 'previous screen share publication');
		}
		previousPublicationRef.current = pub;
		previousTrackSidRef.current = publicationTrackSid;
	}, [isScreenShare, publication, publicationTrackSid, isOwnScreenShare]);
	useEffect(() => {
		const pub = publication;
		if (!isScreenShare || isOwnScreenShare || !userWantsToWatch || videoLocallyDisabled || !pub) return;
		const recoveryKey = getScreenShareVideoSubscriptionRecoveryKey({
			trackSid: publicationTrackSid,
			streamKey,
			participantIdentity,
		});
		if (!recoveryKey) return;
		return screenShareVideoSubscriptionRecoveryCoordinator.acquire({
			key: recoveryKey,
			publication: pub,
			streamKey,
			participantIdentity,
			isStillWanted: () => {
				const currentStreamKey = streamKeyRef.current;
				if (currentStreamKey == null) return false;
				const graph = getGraphSnapshotRef.current();
				if (!selectVoiceMediaGraphViewerStreamKeys(graph).includes(currentStreamKey)) return false;
				if (selectVoiceMediaGraphHasFailureForStreamKey(graph, currentStreamKey)) return false;
				return true;
			},
			onRetry: (retry) => {
				logger.warn('Retrying stalled screen share video subscription', retry);
			},
			recover: () => {
				if (participantIdentity == null) return;
				VoiceEngineV2AppSubscriptionAdapter.reattachScreenShareAfterPublish(participantIdentity);
			},
			onError: (operation, label, err) => {
				logger.error(`${operation} failed for ${label}`, err);
				onVideoSubscriptionError?.(operation, label, err);
			},
		});
	}, [
		isScreenShare,
		isOwnScreenShare,
		publication,
		publicationTrackSid,
		participantIdentity,
		streamKey,
		userWantsToWatch,
		videoLocallyDisabled,
		onVideoSubscriptionError,
	]);
	useEffect(() => {
		const pub = audioPublication;
		if (!isScreenShare || isOwnScreenShare || !pub) {
			unsubscribeRemotePublication(previousAudioPublicationRef.current, 'previous screen share audio publication');
			previousAudioPublicationRef.current = null;
			previousAudioTrackSidRef.current = null;
			return;
		}
		const shouldSubscribe = userWantsToWatch;
		logger.debug('Evaluating screen share audio subscription', {
			participantIdentity: trackRef.participant?.identity,
			trackSid: audioTrackSid,
			shouldSubscribe,
			audioEnabled,
			isSubscribed: pub.isSubscribed,
			hasTrack: Boolean(pub.track),
		});
		if (previousAudioTrackSidRef.current && previousAudioTrackSidRef.current !== audioTrackSid) {
			unsubscribeRemotePublication(previousAudioPublicationRef.current, 'previous screen share audio publication');
		}
		previousAudioPublicationRef.current = pub;
		previousAudioTrackSidRef.current = audioTrackSid;
		syncWatchedScreenSharePublications({
			isScreenShare,
			isOwnScreenShare,
			userWantsToWatch,
			videoLocallyDisabled,
			audioEnabled,
			videoPublication: null,
			audioPublication: pub,
			onError: (operation, label, err) => logger.error(`${operation} failed for ${label}`, err),
		});
	}, [
		isScreenShare,
		isOwnScreenShare,
		audioPublication,
		audioTrackSid,
		userWantsToWatch,
		audioEnabled,
		videoLocallyDisabled,
		trackRef.participant?.identity,
	]);
	useEffect(() => {
		return () => {
			const currentStreamKey = streamKeyRef.current;
			const stillWatching =
				currentStreamKey != null &&
				selectVoiceMediaGraphViewerStreamKeys(getGraphSnapshotRef.current()).includes(currentStreamKey);
			if (stillWatching) {
				logger.debug('Preserving screen share subscriptions for PiP transition', {streamKey: currentStreamKey});
				previousPublicationRef.current = null;
				previousTrackSidRef.current = null;
				managedScreenShareParticipantIdentityRef.current = null;
				previousAudioPublicationRef.current = null;
				previousAudioTrackSidRef.current = null;
				return;
			}
			const managedParticipantIdentity = managedScreenShareParticipantIdentityRef.current;
			if (managedParticipantIdentity) {
				MediaEngine.voiceEngineV2Controller.unwatchStream({
					participantIdentity: managedParticipantIdentity,
					source: VOICE_ENGINE_V2_SCREEN_SOURCE,
				});
			}
			unsubscribeRemotePublication(previousPublicationRef.current, 'screen share publication cleanup');
			unsubscribeRemotePublication(previousAudioPublicationRef.current, 'screen share audio publication cleanup');
			previousPublicationRef.current = null;
			previousTrackSidRef.current = null;
			managedScreenShareParticipantIdentityRef.current = null;
			previousAudioPublicationRef.current = null;
			previousAudioTrackSidRef.current = null;
		};
	}, []);
}

export function useScreensharePreviewUploader(
	enabled: boolean,
	streamKey: string,
	channelId: string | undefined,
	videoRef: React.RefObject<HTMLVideoElement | null>,
	fallbackDataUrl: string | null,
	hasSpectatorDemand: boolean,
) {
	const uploadInFlightRef = useRef(false);
	const initialUploadAttemptsRef = useRef(0);
	const hasUploadedPreviewRef = useRef(false);
	const lastNoPayloadLogAtRef = useRef(0);
	const uploadUrlRef = useRef<StreamPreviewUploadUrlCacheEntry | null>(null);
	const hasSpectatorDemandRef = useRef(hasSpectatorDemand);
	hasSpectatorDemandRef.current = hasSpectatorDemand;
	const wakeOnDemandRef = useRef<(() => void) | null>(null);
	useEffect(() => {
		logger.debug('useScreensharePreviewUploader effect', {enabled, streamKey, channelId});
		if (!enabled) return;
		if (!isScreensharePreviewUploadSessionActive(streamKey, channelId)) return;
		initialUploadAttemptsRef.current = 0;
		hasUploadedPreviewRef.current = false;
		uploadUrlRef.current = null;
		logger.debug('useScreensharePreviewUploader: starting upload schedule', {streamKey, channelId});
		let disposed = false;
		let pendingUploadAfterCurrent = false;
		let stopInitialFrameProbe = () => {};
		const scheduler = new StreamPreviewUploadScheduler();
		const isUploadActive = () => {
			if (disposed) return false;
			return isScreensharePreviewUploadSessionActive(streamKey, channelId);
		};
		const getPreviewUploadUrl = async (): Promise<StreamPreviewUploadUrlResponseSchema> => {
			const cachedUploadUrl = uploadUrlRef.current;
			if (isUploadUrlFresh(cachedUploadUrl, streamKey, Date.now())) {
				return cachedUploadUrl.response;
			}
			const response = await http.post<StreamPreviewUploadUrlResponseSchema>(
				Endpoints.STREAM_PREVIEW_UPLOAD_URL(streamKey),
				{
					body: {
						channel_id: channelId,
						content_type: STREAM_PREVIEW_CONTENT_TYPE_JPEG,
					},
				},
			);
			const uploadUrlResponse = response.body;
			uploadUrlRef.current = {
				streamKey,
				response: uploadUrlResponse,
				expiresAtMs: getUploadUrlExpiresAtMs(uploadUrlResponse, Date.now()),
			};
			return uploadUrlResponse;
		};
		const uploadPreviewWithPresignedUrl = async (body: Blob): Promise<boolean> => {
			for (let attempt = 0; attempt < 2; attempt += 1) {
				if (!isUploadActive()) return false;
				let uploadUrlResponse: StreamPreviewUploadUrlResponseSchema;
				try {
					uploadUrlResponse = await getPreviewUploadUrl();
				} catch (err) {
					if (shouldRefreshPreviewUploadUrl(err)) {
						uploadUrlRef.current = null;
					}
					throw err;
				}
				if (!isUploadActive()) return false;
				const contentType = uploadUrlResponse.content_type || STREAM_PREVIEW_CONTENT_TYPE_JPEG;
				if (body.size > uploadUrlResponse.max_bytes) {
					logger.warn('Skipping oversized screenshare preview upload', {
						streamKey,
						size: body.size,
						maxBytes: uploadUrlResponse.max_bytes,
					});
					return false;
				}
				try {
					const response = await http.put(uploadUrlResponse.upload_url, {
						body,
						headers: {'Content-Type': contentType},
					});
					if (!isUploadActive()) return false;
					if (response.ok) {
						return true;
					}
					if (shouldRefreshPreviewUploadUrlStatus(response.status)) {
						uploadUrlRef.current = null;
					}
					if (!isUploadActive()) return false;
					if (attempt === 1) {
						return false;
					}
				} catch (err) {
					if (shouldRefreshPreviewUploadUrl(err)) {
						uploadUrlRef.current = null;
					}
					if (!isUploadActive()) return false;
					if (attempt === 1) {
						throw err;
					}
				}
			}
			return false;
		};
		const uploadPreview = async (): Promise<boolean> => {
			if (!isUploadActive()) return false;
			if (uploadInFlightRef.current) {
				pendingUploadAfterCurrent = true;
				return false;
			}
			if (PrivacyPreferences.getDisableStreamPreviews()) {
				logger.debug('useScreensharePreviewUploader: stream previews disabled by user preference');
				return false;
			}
			uploadInFlightRef.current = true;
			try {
				if (!hasUploadedPreviewRef.current) {
					initialUploadAttemptsRef.current += 1;
				}
				const videoEl = videoRef.current;
				const previewBlob =
					(videoEl ? await buildPreviewBlobFromVideo(videoEl) : null) ||
					(fallbackDataUrl ? await buildPreviewBlobFromDataUrl(fallbackDataUrl) : null);
				if (!isUploadActive()) return false;
				if (!previewBlob) {
					const now = Date.now();
					if (now - lastNoPayloadLogAtRef.current > 10_000) {
						lastNoPayloadLogAtRef.current = now;
						logger.debug('useScreensharePreviewUploader: no preview payload', {streamKey});
					}
					return false;
				}
				logger.debug('useScreensharePreviewUploader: uploading with presigned URL', {
					streamKey,
					size: previewBlob.size,
				});
				const uploaded = await uploadPreviewWithPresignedUrl(previewBlob);
				logger.debug('useScreensharePreviewUploader: upload result', {ok: uploaded});
				if (uploaded && isUploadActive()) {
					hasUploadedPreviewRef.current = true;
					stopInitialFrameProbe();
				}
				return uploaded;
			} catch (err) {
				if (isUploadActive()) {
					logger.error('Failed to upload screenshare preview', err);
				}
				return false;
			} finally {
				uploadInFlightRef.current = false;
				if (pendingUploadAfterCurrent && isUploadActive()) {
					pendingUploadAfterCurrent = false;
					void uploadPreview();
				} else {
					pendingUploadAfterCurrent = false;
				}
			}
		};
		let initialFrameProbeTimeoutId: number | null = null;
		let observedInitialFrameVideoEl: HTMLVideoElement | null = null;
		let initialFrameCallbackHandle: number | null = null;
		let removeInitialFrameVideoListeners: (() => void) | null = null;
		const clearInitialFrameProbeTimer = () => {
			if (initialFrameProbeTimeoutId !== null) {
				window.clearTimeout(initialFrameProbeTimeoutId);
				initialFrameProbeTimeoutId = null;
			}
		};
		const clearInitialFrameVideoWatch = () => {
			removeInitialFrameVideoListeners?.();
			removeInitialFrameVideoListeners = null;
			const videoEl = observedInitialFrameVideoEl;
			if (videoEl && initialFrameCallbackHandle !== null) {
				videoEl.cancelVideoFrameCallback?.(initialFrameCallbackHandle);
			}
			initialFrameCallbackHandle = null;
			observedInitialFrameVideoEl = null;
		};
		stopInitialFrameProbe = () => {
			clearInitialFrameProbeTimer();
			clearInitialFrameVideoWatch();
		};
		const requestInitialFrameUpload = () => {
			if (!isUploadActive()) return;
			if (hasUploadedPreviewRef.current) return;
			if (initialUploadAttemptsRef.current >= STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS) {
				stopInitialFrameProbe();
				return;
			}
			if (PrivacyPreferences.getDisableStreamPreviews()) return;
			if (!fallbackDataUrl && !isVideoReadyForPreviewUpload(videoRef.current)) return;
			void uploadPreview();
		};
		const observeInitialFrameVideo = (videoEl: HTMLVideoElement | null) => {
			if (videoEl === observedInitialFrameVideoEl) return;
			clearInitialFrameVideoWatch();
			observedInitialFrameVideoEl = videoEl;
			if (!videoEl) return;
			const onVideoReady = () => requestInitialFrameUpload();
			const videoReadyEvents = ['loadeddata', 'canplay', 'playing', 'resize'] as const;
			for (const eventName of videoReadyEvents) {
				videoEl.addEventListener(eventName, onVideoReady);
			}
			removeInitialFrameVideoListeners = () => {
				for (const eventName of videoReadyEvents) {
					videoEl.removeEventListener(eventName, onVideoReady);
				}
			};
			const requestVideoFrameCallback = videoEl.requestVideoFrameCallback?.bind(videoEl);
			if (requestVideoFrameCallback) {
				initialFrameCallbackHandle = requestVideoFrameCallback(() => {
					initialFrameCallbackHandle = null;
					requestInitialFrameUpload();
				});
			}
		};
		const armInitialFrameProbe = () => {
			if (!isUploadActive()) return;
			if (hasUploadedPreviewRef.current) return;
			if (initialUploadAttemptsRef.current >= STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS) {
				stopInitialFrameProbe();
				return;
			}
			if (PrivacyPreferences.getDisableStreamPreviews()) return;
			clearInitialFrameProbeTimer();
			observeInitialFrameVideo(videoRef.current);
			requestInitialFrameUpload();
			if (hasUploadedPreviewRef.current) return;
			initialFrameProbeTimeoutId = window.setTimeout(
				armInitialFrameProbe,
				Math.min(250, STREAM_PREVIEW_INITIAL_UPLOAD_INTERVAL_MS),
			);
		};
		let timeoutId: number | null = null;
		const nextDriverDecision = () =>
			scheduler.decide({
				now: Date.now(),
				hasUploadedOnce: hasUploadedPreviewRef.current,
				initialAttempts: initialUploadAttemptsRef.current,
				hasSpectatorDemand: hasSpectatorDemandRef.current,
				previewsDisabled: PrivacyPreferences.getDisableStreamPreviews(),
			});
		const scheduleDriver = (delayMs: number) => {
			if (!isUploadActive()) return;
			if (timeoutId !== null) {
				window.clearTimeout(timeoutId);
			}
			timeoutId = window.setTimeout(() => {
				timeoutId = null;
				if (!isUploadActive()) return;
				const decision = nextDriverDecision();
				if (decision.action === 'wait') {
					scheduleDriver(decision.nextDelayMs);
					return;
				}
				void uploadPreview().finally(() => {
					if (isUploadActive()) scheduleDriver(decision.nextDelayMs);
				});
			}, delayMs);
		};
		wakeOnDemandRef.current = () => {
			scheduleDriver(0);
		};
		scheduleDriver(nextDriverDecision().nextDelayMs);
		armInitialFrameProbe();
		return () => {
			disposed = true;
			pendingUploadAfterCurrent = false;
			uploadUrlRef.current = null;
			wakeOnDemandRef.current = null;
			stopInitialFrameProbe();
			if (timeoutId !== null) {
				window.clearTimeout(timeoutId);
			}
		};
	}, [enabled, streamKey, channelId, videoRef, fallbackDataUrl]);
	useEffect(() => {
		if (!hasSpectatorDemand) return;
		wakeOnDemandRef.current?.();
	}, [hasSpectatorDemand]);
}

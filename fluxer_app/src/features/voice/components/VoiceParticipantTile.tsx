// SPDX-License-Identifier: AGPL-3.0-or-later

import {LongPressable} from '@app/features/app/components/LongPressable';
import {WATCH_STREAM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import Permission from '@app/features/permissions/state/Permission';
import {dimColor} from '@app/features/theme/utils/ColorUtils';
import {VoiceParticipantContextMenu} from '@app/features/ui/action_menu/VoiceParticipantContextMenu';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {LiveBadge} from '@app/features/ui/components/LiveBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {VoiceParticipantBottomSheet} from '@app/features/voice/components/bottomsheets/VoiceParticipantBottomSheet';
import {FeedHiddenOverlay} from '@app/features/voice/components/FeedHiddenOverlay';
import {getPlaceholderAvatarColor} from '@app/features/voice/components/GetPlaceholderAvatarColor';
import {MediaVerticalVolumeControl} from '@app/features/voice/components/media_player/components/MediaVerticalVolumeControl';
import {
	getOwnStreamHiddenState,
	useOwnScreenSharePreviewState,
	useWindowFocus,
} from '@app/features/voice/components/OwnStreamPreviewState';
import {PoppedOutOverlay} from '@app/features/voice/components/popout/PoppedOutOverlay';
import {
	selectPoppedOutOverlayTransition,
	shouldRenderPoppedOutOverlay,
} from '@app/features/voice/components/popout/PoppedOutSurfaceStateMachine';
import {usePoppedOutTransition} from '@app/features/voice/components/popout/usePoppedOutTransition';
import {VoicePopoutScopeContext} from '@app/features/voice/components/popout/VoicePopoutScopeContext';
import {ScreenShareBufferingFrame} from '@app/features/voice/components/ScreenShareBufferingFrame';
import {registerStreamAudioPrefsTouch} from '@app/features/voice/components/StreamAudioPrefsTouchScheduler';
import {StreamInfoPill} from '@app/features/voice/components/StreamInfoPill';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {StreamSpectatorsPopout} from '@app/features/voice/components/StreamSpectatorsPopout';
import {StreamWatchHoverCard} from '@app/features/voice/components/StreamWatchHoverCard';
import {useScreenShareWatchFailure} from '@app/features/voice/components/useScreenShareWatchFailure';
import {useStreamPreview} from '@app/features/voice/components/useStreamPreview';
import {useStreamSpectators} from '@app/features/voice/components/useStreamSpectators';
import {useStreamTrackInfo} from '@app/features/voice/components/useStreamTrackInfo';
import {useStreamWatchState} from '@app/features/voice/components/useStreamWatchState';
import {useVideoRenderedFrame} from '@app/features/voice/components/VideoElementFrameState';
import voiceCallStyles from '@app/features/voice/components/VoiceCallView.module.css';
import {resolveVoiceParticipantDisplayState} from '@app/features/voice/components/VoiceParticipantDisplayState';
import styles from '@app/features/voice/components/VoiceParticipantTile.module.css';
import {
	selectScreenShareBufferingPresentation,
	selectVoiceParticipantTileCameraActive,
	selectVoiceParticipantTileScreenShareState,
	shouldShowCameraBuffering,
	type VoiceParticipantTileScreenShareSignals,
} from '@app/features/voice/components/VoiceParticipantTileStateMachine';
import {useVoiceTileGroup} from '@app/features/voice/components/VoiceTileGroupContext';
import {FocusedCameraPlaceholder} from '@app/features/voice/components/voice_participant_tile/FocusedCameraPlaceholder';
import {
	useAutoVideoSubscription,
	useEffectiveTrackRef,
	useIntersection,
	useScreenShareAudioPublication,
	useScreensharePreviewUploader,
	useScreenshareWatchSubscription,
	useTileContextMenuActive,
} from '@app/features/voice/components/voice_participant_tile/hooks';
import LastFrameSnapshotCache from '@app/features/voice/components/voice_participant_tile/LastFrameSnapshotCache';
import {ScreenSharePlaceholder} from '@app/features/voice/components/voice_participant_tile/ScreenSharePlaceholder';
import {
	CAMERA_BUFFERING_DESCRIPTOR,
	CAMERA_HIDDEN_DESCRIPTOR,
	CONNECTION_DESCRIPTOR,
	DESKTOP_DEVICE_DESCRIPTOR,
	getSourceDataAttr,
	isCameraSource,
	logger,
	MOBILE_DEVICE_DESCRIPTOR,
	MUTED_DESCRIPTOR,
	PARTICIPANT_OPTIONS_FOR_DESCRIPTOR,
	PREVIEW_PAUSED_TO_SAVE_RESOURCES_DESCRIPTOR,
	SHOW_CAMERA_DESCRIPTOR,
	STREAM_BUFFERING_DESCRIPTOR,
	STREAM_ENDED_DESCRIPTOR,
	STREAM_HIDDEN_DESCRIPTOR,
	TILE_AVATAR_BASE,
	TILE_AVATAR_STYLE,
	type VoiceParticipantTileInnerProps,
	type VoiceParticipantTileProps,
	WATCH_DESCRIPTOR,
	WATCHING_DESCRIPTOR,
	WATCHING_FAILED_DESCRIPTOR,
	WATCHING_FAILED_ERROR_CODE_DESCRIPTOR,
	YOUR_STREAM_IS_STILL_LIVE_DESCRIPTOR,
} from '@app/features/voice/components/voice_participant_tile/shared';
import {WatchStreamOverlay} from '@app/features/voice/components/voice_participant_tile/WatchStreamOverlay';
import MediaEngine, {useMediaEngineVersion, useVoiceEngineV2Model} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {
	selectVoiceMediaGraphDeferredStopKeys,
	selectVoiceMediaGraphFailure,
	selectVoiceMediaGraphViewerStreamKeys,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {selectVoiceMediaGraphStreamTileState} from '@app/features/voice/engine/VoiceMediaGraphTileState';
import {
	asPinnableVoiceTrackSource,
	asVoiceTrackSource,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import {selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import CallMediaPrefs from '@app/features/voice/state/CallMediaPrefs';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import PopoutWindowManager, {getVoiceTilePopoutKey} from '@app/features/voice/state/PopoutWindowManager';
import {
	getScreenShareWatchFailureForPublicationOperation,
	ScreenShareWatchFailures,
} from '@app/features/voice/state/ScreenShareWatchFailures';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	type ScreenSharePublicationOperation,
	syncScreenSharePublication,
} from '@app/features/voice/utils/ScreenShareSubscriptionPolicy';
import {canViewStreamPreview} from '@app/features/voice/utils/StreamPreviewPermissionUtils';
import {
	getVoiceDeafenedByModeratorsStatusLabel,
	getVoiceDeafenedStatusLabel,
	getVoiceNoSpeakPermissionLabel,
	VOICE_MUTED_BY_MODERATORS_DESCRIPTOR,
	VOICE_STOP_WATCHING_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {isParticipantVoicePermissionMuted} from '@app/features/voice/utils/VoicePermissionUtils';
import {VOICE_VOLUME_MAX_SLIDER_VOLUME} from '@app/features/voice/utils/VoiceVolumeUtils';
import {DEFAULT_ACCENT_COLOR} from '@fluxer/constants/src/AppConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {isTrackReference, useParticipantTile, VideoTrack} from '@livekit/components-react';
import {
	DesktopIcon,
	DeviceMobileIcon,
	DotsThreeIcon,
	EyeIcon,
	MicrophoneSlashIcon,
	PauseIcon,
	SpeakerSlashIcon,
	VideoCameraSlashIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type {Participant, RemoteTrackPublication, Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useMemo, useRef, useState} from 'react';

const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback label for a voice participant tile when no user or participant name is available.',
});
const COLLAPSE_DEVICES_DESCRIPTOR = msg({
	message: 'Collapse devices',
	comment: 'Tooltip / aria label on a voice participant tile button that collapses expanded device tiles for one user.',
});
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;

function getVoiceMediaGraphSnapshotForTile() {
	return voiceMediaGraphStore.getGraphSnapshot();
}

export const VoiceParticipantTile = observer((props: VoiceParticipantTileProps) => {
	const {
		trackRef,
		guildId,
		channelId,
		onClick,
		isPinned,
		showFocusIndicator,
		allowAutoSubscribe = true,
		renderFocusedPlaceholder = false,
		presentation = 'grid',
		showParticipantMetadata = true,
	} = props;
	const effectiveTrackRef = useEffectiveTrackRef(trackRef);
	const {elementProps} = useParticipantTile({
		trackRef: effectiveTrackRef ?? undefined,
		htmlProps: {},
	});
	if (!effectiveTrackRef) return null;
	return (
		<VoiceParticipantTileInner
			trackRef={effectiveTrackRef}
			elementProps={elementProps}
			guildId={guildId}
			channelId={channelId}
			onClick={onClick}
			isPinned={isPinned}
			showFocusIndicator={showFocusIndicator}
			allowAutoSubscribe={allowAutoSubscribe}
			renderFocusedPlaceholder={renderFocusedPlaceholder}
			presentation={presentation}
			showParticipantMetadata={showParticipantMetadata}
			data-flx="voice.voice-participant-tile.voice-participant-tile-inner.click"
		/>
	);
});

function hasMultipleConnectionsForCurrentUser(
	guildId: string | null | undefined,
	participantUserId: string | undefined,
): boolean {
	if (!guildId || !participantUserId) return false;
	const guildVoiceStates = MediaEngine.getAllVoiceStates()[guildId];
	if (!guildVoiceStates) return false;
	let connectionCount = 0;
	for (const channelId in guildVoiceStates) {
		const channelVoiceStates = guildVoiceStates[channelId];
		if (!channelVoiceStates) continue;
		for (const connectionId in channelVoiceStates) {
			const voiceState = channelVoiceStates[connectionId];
			if (!voiceState) continue;
			if (voiceState.user_id !== participantUserId) continue;
			connectionCount++;
			if (connectionCount > 1) return true;
		}
	}
	return false;
}

const VoiceParticipantTileInner = observer(function VoiceParticipantTileInner({
	trackRef,
	elementProps,
	guildId,
	channelId,
	onClick,
	isPinned,
	showFocusIndicator,
	allowAutoSubscribe,
	renderFocusedPlaceholder,
	presentation,
	showParticipantMetadata,
}: VoiceParticipantTileInnerProps) {
	const {i18n} = useLingui();
	useMediaEngineVersion();
	const voiceEngineV2Model = useVoiceEngineV2Model();
	const participant = trackRef.participant;
	const identity = participant.identity;
	const {userId, connectionId} = useMemo(() => parseVoiceParticipantIdentity(identity), [identity]);
	const participantUser = Users.getUser(userId);
	const currentUser = Users.getCurrentUser();
	const isCurrentUser = currentUser?.id === participantUser?.id;
	const voiceState = MediaEngine.getVoiceStateByConnectionId(connectionId);
	const connectionParticipant = MediaEngine.getParticipantByUserIdAndConnectionId(userId, connectionId);
	const isLocalParticipant = Boolean((participant as Participant)?.isLocal);
	const isPermissionMuted = isParticipantVoicePermissionMuted({
		voiceState,
		guildId,
		channelId,
		isCurrentUser,
	});
	const displayState = resolveVoiceParticipantDisplayState({
		participant: connectionParticipant,
		voiceState,
		isLocalConnection: isLocalParticipant,
		localSelfMute: selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(voiceEngineV2Model),
		localSelfDeaf: LocalVoiceState.getSelfDeaf(),
		permissionMuted: isPermissionMuted,
	});
	const isSelfMuted = displayState.selfMute;
	const isSelfDeafened = displayState.selfDeaf;
	const isModeratorMuted = displayState.guildMute;
	const isModeratorDeafened = displayState.guildDeaf;
	const isMuteStatusVisible = isModeratorMuted || isSelfMuted;
	const muteStatusLabel = isModeratorMuted
		? i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR)
		: isPermissionMuted
			? getVoiceNoSpeakPermissionLabel(i18n, isCurrentUser)
			: i18n._(MUTED_DESCRIPTOR);
	const muteStatusClassName =
		isModeratorMuted || isPermissionMuted ? styles.participantIconRed : styles.participantIconMuted;
	const isDeafenStatusVisible = isModeratorDeafened || isSelfDeafened;
	const deafenStatusLabel = isModeratorDeafened
		? getVoiceDeafenedByModeratorsStatusLabel(i18n, isCurrentUser)
		: getVoiceDeafenedStatusLabel(i18n, isCurrentUser);
	const deafenStatusClassName = isModeratorDeafened ? styles.participantIconRed : styles.participantIconMuted;
	const isActuallySpeaking = displayState.speaking;
	const isMobileExperience = isMobileExperienceEnabled();
	const [bottomSheetOpen, setBottomSheetOpen] = useState(false);
	const tileGroup = useVoiceTileGroup();
	const groupHiddenCount = tileGroup?.hiddenConnectionCount ?? 0;
	const groupDeviceConnectionCount = tileGroup?.deviceConnectionCount ?? 1;
	const handleExpandGroup = useCallback(
		(event: React.MouseEvent | React.KeyboardEvent) => {
			event.stopPropagation();
			tileGroup?.onExpand();
		},
		[tileGroup],
	);
	const groupExpandTooltip = plural(
		{count: groupHiddenCount},
		{
			one: 'Expand # other device',
			other: 'Expand # other devices',
		},
	);
	const groupCollapseTooltip = i18n._(COLLAPSE_DEVICES_DESCRIPTOR);
	const isWindowFocused = useWindowFocus();
	const pauseOwnScreenSharePreviewOnUnfocus = VoiceSettings.pauseOwnScreenSharePreviewOnUnfocus;
	const sourceAttr = getSourceDataAttr(trackRef.source);
	const isScreenShare = asVoiceTrackSource(trackRef.source) === VoiceTrackSource.ScreenShare;
	const popoutScope = useContext(VoicePopoutScopeContext);
	const isInsideTilePopout = popoutScope === 'tile';
	const tilePopoutKey = getVoiceTilePopoutKey(identity, isScreenShare ? 'screen_share' : 'camera');
	const isTilePoppedOut = popoutScope !== 'tile' && PopoutWindowManager.isOpen(tilePopoutKey);
	const tilePopoutTransition = usePoppedOutTransition(isTilePoppedOut);
	const isFocusedPlaceholderTile = renderFocusedPlaceholder;
	const isFocusPresentationTile = presentation !== 'grid';
	const isGridTile = presentation === 'grid';
	const showDeviceCollapseControl =
		isGridTile && Boolean(tileGroup?.isExpanded && tileGroup.isPrimary && groupDeviceConnectionCount > 1);
	const isInteractiveScreenShareTile = isScreenShare && !isFocusedPlaceholderTile;
	const isOwnScreenShare = isScreenShare && isLocalParticipant;
	const isOwnContent = isLocalParticipant && isCurrentUser;
	const {isOwnScreenShareHidden, isOwnCameraHidden} = getOwnStreamHiddenState({
		isOwnContent,
		isScreenShare,
		showMyOwnCamera: VoiceSettings.showMyOwnCamera,
		showMyOwnScreenShare: VoiceSettings.showMyOwnScreenShare,
	});
	const callId = MediaEngine.connectionId ?? '';
	const streamKey = useMemo(() => getStreamKey(guildId, channelId, connectionId), [guildId, channelId, connectionId]);
	const {viewerIds, viewerUsers, spectatorEntries} = useStreamSpectators(isScreenShare ? streamKey : '', userId);
	const hasSpectatorDemand = viewerIds.length > 0;
	const isCameraTile = isCameraSource(trackRef.source);
	const cameraLocallyDisabled = callId !== '' && isCameraTile && CallMediaPrefs.isVideoDisabled(callId, identity);
	const screenSharePublicationMigrationVersion = ScreenSharePublicationMigration.version;
	const publication = useMemo(() => {
		if (isTrackReference(trackRef)) return trackRef.publication as RemoteTrackPublication | undefined;
		if (asVoiceTrackSource(trackRef.source) !== VoiceTrackSource.ScreenShare) return undefined;
		return (
			ScreenSharePublicationMigration.selectScreenSharePublication(participant) ??
			(participant.getTrackPublication(SCREEN_SHARE_SOURCE) as RemoteTrackPublication | undefined)
		);
	}, [participant, trackRef, screenSharePublicationMigrationVersion]);
	const {publication: screenShareAudioPublication, hasTrack: hasScreenShareAudioTrack} = useScreenShareAudioPublication(
		participant,
		isScreenShare,
	);
	const hasScreenShareAudio = Boolean(screenShareAudioPublication);
	const hasVideo = useMemo(() => {
		if (!isTrackReference(trackRef)) return false;
		const pub = trackRef.publication;
		return Boolean(pub?.track) && !pub?.isMuted && !cameraLocallyDisabled;
	}, [cameraLocallyDisabled, trackRef]);
	const videoRef = useRef<HTMLVideoElement | null>(null);
	const liveKitCameraTrackSid = isTrackReference(trackRef) ? trackRef.publication.trackSid : '';
	const cameraVideoFrameTrackKey = publication?.trackSid ?? liveKitCameraTrackSid ?? '';
	const cameraVideoFrameResetKey = isCameraTile
		? `livekit:${identity}:${cameraVideoFrameTrackKey}:${hasVideo ? 'video' : 'waiting'}`
		: '';
	const hasRenderedCameraVideoFrame = useVideoRenderedFrame({
		enabled: isCameraTile && hasVideo,
		resetKey: cameraVideoFrameResetKey,
		videoRef,
	});
	const streamVolume = StreamAudioPrefs.getVolume(streamKey);
	const isStreamMuted = StreamAudioPrefs.isMuted(streamKey);
	const hasStreamAudioPrefsEntry = StreamAudioPrefs.hasEntry(streamKey);
	const isSubscribed = Boolean(publication?.isSubscribed);
	const hasSubscribedScreenShareVideo = isSubscribed && hasVideo;
	const isScreenShareRepublishBuffering =
		isScreenShare && !isOwnScreenShare && ScreenSharePublicationMigration.isScreenShareBuffering(participant);
	const shouldAutoSubscribe = allowAutoSubscribe && !isFocusedPlaceholderTile;
	const {ref: tileRef, isIntersecting} = useIntersection<HTMLDivElement>(shouldAutoSubscribe);
	useAutoVideoSubscription({
		enabled: shouldAutoSubscribe,
		trackRef,
		isIntersecting,
		videoLocallyDisabled: cameraLocallyDisabled,
		isLocalParticipant,
		isScreenShare,
	});
	useStoreVersion(voiceMediaGraphStore);
	const graphSnapshot = voiceMediaGraphStore.getGraphSnapshot();
	const graphViewerStreamKeys = selectVoiceMediaGraphViewerStreamKeys(graphSnapshot);
	const isConnectedToTileChannel =
		Boolean(channelId) && MediaEngine.channelId === channelId && MediaEngine.guildId === (guildId ?? null);
	const isWatching = isConnectedToTileChannel && graphViewerStreamKeys.includes(streamKey);
	const graphTileState = selectVoiceMediaGraphStreamTileState(graphSnapshot, {
		streamKey: streamKey || null,
		participantIdentity: identity || null,
		source: VoiceTrackSource.ScreenShare,
	});
	const graphWatchFailure = isScreenShare
		? selectVoiceMediaGraphFailure(graphSnapshot, {
				streamKey,
				participantIdentity: identity,
				source: VoiceTrackSource.ScreenShare,
			})
		: null;
	const {
		startWatching,
		addStream,
		stopWatching: stopWatchingStream,
	} = useStreamWatchState({
		streamKey,
		guildId,
		channelId,
	});
	const stopWatching = useCallback(() => {
		syncScreenSharePublication({
			publication,
			label: 'screen share publication',
			shouldSubscribe: false,
			shouldEnable: false,
			onError: (operation, label, err) => logger.error(`${operation} failed for ${label}`, err),
		});
		stopWatchingStream();
	}, [publication, stopWatchingStream]);
	const reportVideoSubscriptionError = useCallback(
		(operation: ScreenSharePublicationOperation, _label: string, error: unknown) => {
			if (!isWatching) return;
			const failure = getScreenShareWatchFailureForPublicationOperation(operation);
			ScreenShareWatchFailures.reportFailure({
				streamKey,
				participantIdentity: identity,
				trackSid: publication?.trackSid,
				source: VoiceTrackSource.ScreenShare,
				code: failure.code,
				reason: failure.reason,
				error,
			});
		},
		[identity, isWatching, publication?.trackSid, streamKey],
	);
	useScreenshareWatchSubscription({
		isScreenShare: isInteractiveScreenShareTile,
		trackRef,
		userWantsToWatch: isWatching,
		videoLocallyDisabled: false,
		isOwnScreenShare,
		audioEnabled: !isStreamMuted,
		audioPublication: screenShareAudioPublication,
		streamKey,
		onVideoSubscriptionError: reportVideoSubscriptionError,
		getGraphSnapshot: getVoiceMediaGraphSnapshotForTile,
	});
	useEffect(() => {
		if (!isScreenShare || isOwnScreenShare || isFocusedPlaceholderTile) return;
		if (!isWatching) return;
		const pub = screenShareAudioPublication;
		if (!pub) return;
		const shouldEnable = !isStreamMuted;
		logger.debug('Applying runtime screen share audio enabled state', {
			trackSid: pub.trackSid,
			isWatching,
			isStreamMuted,
			shouldEnable,
		});
		syncScreenSharePublication({
			publication: pub,
			label: 'screen share audio publication',
			shouldSubscribe: true,
			shouldEnable,
			onError: (operation, label, err) => logger.error(`${operation} failed for ${label}`, err),
		});
		MediaEngine.applyLocalAudioPreferencesForUser(userId);
	}, [
		hasScreenShareAudioTrack,
		isScreenShare,
		isOwnScreenShare,
		isFocusedPlaceholderTile,
		isWatching,
		screenShareAudioPublication,
		streamVolume,
		isStreamMuted,
		userId,
	]);
	useEffect(() => {
		if (!isScreenShare || isOwnScreenShare || isFocusedPlaceholderTile) return;
		if (!hasScreenShareAudio || !hasStreamAudioPrefsEntry) return;
		return registerStreamAudioPrefsTouch(streamKey);
	}, [
		isScreenShare,
		isOwnScreenShare,
		isFocusedPlaceholderTile,
		hasScreenShareAudio,
		hasStreamAudioPrefsEntry,
		streamKey,
	]);
	const [previewPopoverOpen, setPreviewPopoverOpen] = useState(false);
	const canFetchStreamPreview = canViewStreamPreview({
		guildId,
		channelId,
		hasConnectPermission: () =>
			Permission.can(Permissions.CONNECT, {guildId: guildId ?? undefined, channelId: channelId ?? undefined}),
	});
	const previewEnabled =
		isTrackReference(trackRef) &&
		isScreenShare &&
		!isOwnScreenShare &&
		!isWatching &&
		!isSubscribed &&
		!isFocusedPlaceholderTile &&
		canFetchStreamPreview;
	const {previewUrl, isPreviewLoading} = useStreamPreview(previewEnabled, streamKey);
	const isStreamPlaceholder = isScreenShare && !isTrackReference(trackRef);
	const screenShareTrackSid = publication?.trackSid ?? null;
	const trackInfo = useStreamTrackInfo(isScreenShare && !isFocusPresentationTile ? trackRef : null, {
		nativeSource: isScreenShare ? VoiceTrackSource.ScreenShare : null,
		nativeTrackSid: screenShareTrackSid,
		participantIdentity: identity,
	});
	const isPublicationDesired = publication?.isDesired ?? publication?.isSubscribed ?? false;
	useScreenShareWatchFailure({
		enabled:
			isScreenShare &&
			!isOwnScreenShare &&
			!isFocusedPlaceholderTile &&
			isWatching &&
			!cameraLocallyDisabled &&
			!isStreamPlaceholder,
		streamKey,
		participantIdentity: identity,
		trackSid: screenShareTrackSid,
		hasPublication: publication != null,
		isPublicationDesired,
		hasSubscribedVideo: hasSubscribedScreenShareVideo,
		operationKey: isScreenShareRepublishBuffering ? `republish:${screenSharePublicationMigrationVersion}` : null,
		videoRef,
	});
	useStoreVersion(LastFrameSnapshotCache);
	const lastFrameSnapshotKey = isScreenShare && !isOwnScreenShare && !isFocusedPlaceholderTile ? streamKey : '';
	const retainedLastFrameUrl = lastFrameSnapshotKey
		? LastFrameSnapshotCache.getSnapshotUrl(lastFrameSnapshotKey)
		: null;
	useEffect(() => {
		if (!lastFrameSnapshotKey) return;
		if (!isScreenShareRepublishBuffering) return;
		LastFrameSnapshotCache.captureFromVideoElement(lastFrameSnapshotKey, videoRef.current);
	}, [isScreenShareRepublishBuffering, lastFrameSnapshotKey, screenShareTrackSid]);
	useEffect(() => {
		if (!lastFrameSnapshotKey) return;
		return () => {
			LastFrameSnapshotCache.captureFromVideoElement(lastFrameSnapshotKey, videoRef.current);
		};
	}, [lastFrameSnapshotKey, screenShareTrackSid]);
	const screenShareSignals: VoiceParticipantTileScreenShareSignals = {
		graphTileState,
		isScreenShare,
		isOwnScreenShare,
		isFocusedPlaceholderTile,
		isFocusPresentationTile,
		isTrackReference: isTrackReference(trackRef),
		cameraLocallyDisabled,
		isRepublishGracePending: selectVoiceMediaGraphDeferredStopKeys(graphSnapshot).has(streamKey),
	};
	const screenShareSurfaceState = selectVoiceParticipantTileScreenShareState(screenShareSignals);
	const screenShareBufferingPresentation = selectScreenShareBufferingPresentation({
		...screenShareSignals,
		hasRetainedLastFrame: retainedLastFrameUrl !== null,
	});
	const isScreenShareBuffering = screenShareSurfaceState === 'buffering';
	const showWatchFailed = screenShareSurfaceState === 'watchFailed';
	const showStreamEnded = screenShareSurfaceState === 'streamEnded';
	const showWatchStreamOverlay = screenShareSurfaceState === 'watchPrompt';
	const isCameraPublicationActive = isTrackReference(trackRef) && isCameraTile && !trackRef.publication.isMuted;
	const isParticipantCameraActive =
		isCameraTile &&
		Boolean(connectionParticipant?.isCameraEnabled || voiceState?.self_video || participant.isCameraEnabled);
	const isLocalCameraRequested = isCameraTile && isOwnContent && LocalVoiceState.getSelfVideo();
	const isCameraActive = selectVoiceParticipantTileCameraActive({
		isCameraTile,
		isOwnContent,
		isCameraPublicationActive,
		isParticipantCameraActive,
		isLocalCameraRequested,
	});
	const isCameraBuffering = shouldShowCameraBuffering({
		isScreenShare,
		isFocusedPlaceholderTile,
		cameraLocallyDisabled,
		isOwnCameraHidden,
		isCameraActive,
		hasVideo,
		hasRenderedVideoFrame: hasRenderedCameraVideoFrame,
	});
	const cameraBufferingLabel = i18n._(CAMERA_BUFFERING_DESCRIPTOR);
	const screenShareBufferingLabel = i18n._(STREAM_BUFFERING_DESCRIPTOR);
	const watchFailedTitle = i18n._(WATCHING_FAILED_DESCRIPTOR);
	const watchFailedSubtext =
		graphWatchFailure == null ? '' : i18n._(WATCHING_FAILED_ERROR_CODE_DESCRIPTOR, {code: graphWatchFailure.code});
	const placeholderColor = useMemo(
		() => getPlaceholderAvatarColor(participantUser, DEFAULT_ACCENT_COLOR),
		[participantUser],
	);
	const placeholderBackgroundColor = useMemo(() => dimColor(placeholderColor), [placeholderColor]);
	const placeholderStyle = useMemo<React.CSSProperties>(
		() => ({backgroundColor: placeholderBackgroundColor}),
		[placeholderBackgroundColor],
	);
	const focusedCameraPlaceholderStyle = useMemo<React.CSSProperties>(
		() => ({...placeholderStyle, opacity: 1}),
		[placeholderStyle],
	);
	const screenSharePlaceholderStyle = useMemo<React.CSSProperties>(
		() => ({backgroundColor: placeholderBackgroundColor, opacity: 1}),
		[placeholderBackgroundColor],
	);
	const tileContextMenuOpen = useTileContextMenuActive(tileRef);
	const participantDisplayName =
		(participantUser ? NicknameUtils.getNickname(participantUser, guildId, channelId) : participant.name) ||
		i18n._(UNKNOWN_USER_DESCRIPTOR);
	const showStreamAudioControls = isScreenShare && !isOwnScreenShare && isWatching;
	const viewerStreamCount = graphViewerStreamKeys.length;
	const addStreamTooltipText = plural(
		{count: viewerStreamCount},
		{
			one: 'Keep watching # stream and add this one',
			other: 'Keep watching # streams and add this one',
		},
	);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent | MouseEvent) => {
			if (!participantUser) return;
			const isGroupedItem = isCurrentUser && hasMultipleConnectionsForCurrentUser(guildId, participantUser.id);
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<VoiceParticipantContextMenu
					user={participantUser}
					participantName={participantDisplayName}
					onClose={onClose}
					guildId={guildId}
					connectionId={connectionId}
					isGroupedItem={isGroupedItem}
					streamKey={streamKey}
					isScreenShare={isScreenShare}
					isWatching={isWatching}
					hasScreenShareAudio={hasScreenShareAudio}
					isOwnScreenShare={isOwnScreenShare}
					onStopWatching={stopWatching}
					hiddenConnectionCount={groupHiddenCount}
					deviceConnectionCount={groupDeviceConnectionCount}
					isDeviceGroupExpanded={tileGroup?.isExpanded ?? false}
					onToggleDeviceGroup={tileGroup?.onExpand}
					data-flx="voice.voice-participant-tile.handle-context-menu.voice-participant-context-menu"
				/>
			));
		},
		[
			participantUser,
			participantDisplayName,
			guildId,
			connectionId,
			isCurrentUser,
			streamKey,
			isScreenShare,
			isWatching,
			hasScreenShareAudio,
			isOwnScreenShare,
			stopWatching,
			groupHiddenCount,
			groupDeviceConnectionCount,
			tileGroup,
		],
	);
	const pinnedParticipantSource = VoiceCallLayout.pinnedParticipantSource;
	const isFocusedOnThisTile =
		VoiceCallLayout.pinnedParticipantIdentity === identity &&
		(pinnedParticipantSource == null || pinnedParticipantSource === trackRef.source);
	const handleTileClick = useCallback(() => {
		if (isInsideTilePopout) return;
		const wasFocused =
			VoiceCallLayout.pinnedParticipantIdentity === identity &&
			(pinnedParticipantSource == null || pinnedParticipantSource === trackRef.source);
		if (wasFocused) {
			VoiceCallLayoutCommands.setPinnedParticipant(null);
		} else {
			VoiceCallLayoutCommands.setPinnedParticipant(identity, asPinnableVoiceTrackSource(trackRef.source));
			onClick?.(identity);
		}
		VoiceCallLayoutCommands.markUserOverride();
	}, [identity, isInsideTilePopout, onClick, pinnedParticipantSource, trackRef.source]);
	const handleTileKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			event.stopPropagation();
			handleTileClick();
		},
		[handleTileClick],
	);
	const handleMenuButtonKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLDivElement>) => {
			if (!isKeyboardActivationKey(event.key)) return;
			event.preventDefault();
			event.stopPropagation();
			const node = event.currentTarget as HTMLElement | null;
			if (!node) return;
			const rect = node.getBoundingClientRect();
			const x = rect.left + rect.width / 2 + (window.scrollX || window.pageXOffset);
			const y = rect.top + rect.height / 2 + (window.scrollY || window.pageYOffset);
			const syntheticEvent = new MouseEvent('contextmenu', {
				clientX: rect.left + rect.width / 2,
				clientY: rect.top + rect.height / 2,
				screenX: x,
				screenY: y,
				bubbles: true,
				cancelable: true,
			});
			handleContextMenu(syntheticEvent);
		},
		[handleContextMenu],
	);
	const handleStreamAudioToggle = useCallback(() => {
		StreamAudioPrefs.setMuted(streamKey, !isStreamMuted);
		MediaEngine.applyLocalAudioPreferencesForUser(userId);
	}, [streamKey, isStreamMuted, userId]);
	const handleStreamVolumeChange = useCallback(
		(newVolume: number) => {
			StreamAudioPrefs.setVolume(streamKey, Math.round(newVolume * 100));
			MediaEngine.applyLocalAudioPreferencesForUser(userId);
		},
		[streamKey, userId],
	);
	const handleWatch = useCallback(
		(e?: React.SyntheticEvent) => {
			e?.stopPropagation();
			startWatching();
			VoiceCallLayoutCommands.setPinnedParticipant(identity, VoiceTrackSource.ScreenShare);
		},
		[identity, startWatching],
	);
	const handleAddStream = useCallback(
		(event: React.SyntheticEvent) => {
			event.stopPropagation();
			addStream();
		},
		[addStream],
	);
	const handleRevealHiddenFeed = useCallback(
		(e: React.SyntheticEvent) => {
			e.stopPropagation();
			if (isOwnScreenShareHidden) {
				VoiceSettingsCommands.update({showMyOwnScreenShare: true});
			} else if (isOwnCameraHidden) {
				VoiceSettingsCommands.update({showMyOwnCamera: true});
			}
		},
		[isOwnScreenShareHidden, isOwnCameraHidden],
	);
	const handleMouseEnter = useCallback(() => {
		if (isFocusPresentationTile || !previewEnabled) return;
		setPreviewPopoverOpen(true);
	}, [isFocusPresentationTile, previewEnabled]);
	const handleMouseLeave = useCallback(() => setPreviewPopoverOpen(false), []);
	const {frozenFrameUrl, isOwnStreamPreviewPaused, shouldHideOwnScreenShareVideo} = useOwnScreenSharePreviewState({
		isOwnScreenShare,
		pausePreviewOnUnfocus: pauseOwnScreenSharePreviewOnUnfocus,
		isWindowFocused,
		videoRef,
	});
	const hasVisibleMediaTile =
		!isFocusedPlaceholderTile && isTrackReference(trackRef) && hasVideo && !shouldHideOwnScreenShareVideo;
	const isAvatarOnlyTile = !hasVisibleMediaTile && !isScreenShare;
	const shouldShowTileSpeakingIndicator =
		!isFocusedPlaceholderTile && isActuallySpeaking && !isScreenShare && !isAvatarOnlyTile;
	const isActiveLocalScreenShareConnection =
		MediaEngine.connected && MediaEngine.connectionId === connectionId && MediaEngine.channelId === channelId;
	const shouldUploadOwnScreenSharePreview =
		isOwnScreenShare &&
		!isFocusedPlaceholderTile &&
		isActiveLocalScreenShareConnection &&
		LocalVoiceState.getSelfStream();
	useScreensharePreviewUploader(
		shouldUploadOwnScreenSharePreview,
		streamKey,
		channelId,
		videoRef,
		frozenFrameUrl,
		hasSpectatorDemand,
	);
	const mediaNode = useMemo(() => {
		if (isFocusedPlaceholderTile) {
			if (isScreenShare) {
				return (
					<ScreenSharePlaceholder
						guildId={guildId}
						participantUser={participantUser}
						showLiveBadge
						style={screenSharePlaceholderStyle}
						data-flx="voice.voice-participant-tile.media-node.screen-share-placeholder"
					/>
				);
			}
			return (
				<FocusedCameraPlaceholder
					guildId={guildId}
					participantUser={participantUser}
					style={focusedCameraPlaceholderStyle}
					data-flx="voice.voice-participant-tile.media-node.focused-camera-placeholder"
				/>
			);
		}
		if (isTrackReference(trackRef) && hasVideo && !shouldHideOwnScreenShareVideo) {
			return (
				<VideoTrack
					ref={videoRef}
					trackRef={trackRef}
					manageSubscription={false}
					data-flx="voice.voice-participant-tile.media-node.video-track"
				/>
			);
		}
		if (shouldHideOwnScreenShareVideo && frozenFrameUrl) {
			return (
				<img
					src={frozenFrameUrl}
					alt=""
					className={styles.frozenFrame}
					data-flx="voice.voice-participant-tile.media-node.frozen-frame"
				/>
			);
		}
		if (shouldHideOwnScreenShareVideo) {
			return (
				<div className={styles.frozenFrame} data-flx="voice.voice-participant-tile.media-node.paused-preview-frame" />
			);
		}
		if (isScreenShare && !isOwnScreenShare) {
			if (previewUrl) {
				return (
					<img
						src={previewUrl}
						alt=""
						className={styles.screensharePreviewBackground}
						data-flx="voice.voice-participant-tile.media-node.screenshare-preview-background"
					/>
				);
			}
			return (
				<div
					className={styles.streamPreviewUnavailableSurface}
					data-flx="voice.voice-participant-tile.media-node.stream-preview-unavailable-surface"
				/>
			);
		}
		if (isScreenShare) {
			return (
				<ScreenSharePlaceholder
					guildId={guildId}
					participantUser={participantUser}
					showLiveBadge={false}
					style={screenSharePlaceholderStyle}
					data-flx="voice.voice-participant-tile.media-node.screen-share-placeholder--3"
				/>
			);
		}
		return (
			<div
				style={placeholderStyle}
				className={voiceCallStyles.lkParticipantPlaceholder}
				data-flx="voice.voice-participant-tile.media-node.div"
			>
				{participantUser && (
					<div
						className={clsx(styles.tileAvatarRing, isActuallySpeaking ? styles.avatarRingSpeaking : styles.avatarRing)}
						data-flx="voice.voice-participant-tile.media-node.avatar-ring"
					>
						<Avatar
							user={participantUser}
							size={TILE_AVATAR_BASE}
							className={styles.avatarFlexShrink}
							style={TILE_AVATAR_STYLE}
							guildId={guildId}
							data-flx="voice.voice-participant-tile.media-node.avatar-flex-shrink"
						/>
					</div>
				)}
			</div>
		);
	}, [
		focusedCameraPlaceholderStyle,
		frozenFrameUrl,
		guildId,
		hasVideo,
		isActuallySpeaking,
		isFocusedPlaceholderTile,
		isOwnScreenShare,
		isScreenShare,
		participantUser,
		placeholderStyle,
		previewUrl,
		screenSharePlaceholderStyle,
		trackRef,
		shouldHideOwnScreenShareVideo,
	]);
	return (
		<>
			<FocusRing offset={-2} data-flx="voice.voice-participant-tile.voice-participant-tile-inner.focus-ring">
				<LongPressable
					ref={tileRef as React.Ref<HTMLDivElement>}
					data-flx="voice.voice-participant-tile.voice-participant-tile-inner.cursor-pointer.tile-click"
					{...elementProps}
					className={clsx(
						voiceCallStyles.lkParticipantTile,
						elementProps.className,
						isPinned && voiceCallStyles.pinnedParticipant,
						!isInsideTilePopout && styles.cursorPointer,
						tileContextMenuOpen && voiceCallStyles.tileContextMenuActive,
					)}
					data-speaking={shouldShowTileSpeakingIndicator}
					data-video-muted={isFocusedPlaceholderTile || !hasVideo || (shouldHideOwnScreenShareVideo && !frozenFrameUrl)}
					data-source={sourceAttr}
					data-tile-presentation={presentation}
					onContextMenu={handleContextMenu}
					onClick={isInsideTilePopout ? undefined : handleTileClick}
					onKeyDown={isInsideTilePopout ? undefined : handleTileKeyDown}
					onLongPress={() => {
						if (isMobileExperience && participantUser) setBottomSheetOpen(true);
					}}
					onMouseEnter={handleMouseEnter}
					onMouseLeave={handleMouseLeave}
					role={isInsideTilePopout ? undefined : 'button'}
					tabIndex={isInsideTilePopout ? undefined : 0}
				>
					{mediaNode}
					{isScreenShareBuffering &&
						!isFocusedPlaceholderTile &&
						screenShareBufferingPresentation === 'last-frame' &&
						retainedLastFrameUrl != null && (
							<>
								<img
									src={retainedLastFrameUrl}
									alt=""
									className={clsx(styles.frozenFrame, styles.dimmedLastFrame)}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.dimmed-last-frame"
								/>
								<ScreenShareBufferingFrame
									variant="corner"
									label={screenShareBufferingLabel}
									className={styles.cornerBufferingIndicator}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.screen-share-buffering-corner"
								/>
							</>
						)}
					{isScreenShareBuffering && !isFocusedPlaceholderTile && screenShareBufferingPresentation === 'spinner' && (
						<ScreenShareBufferingFrame
							label={screenShareBufferingLabel}
							className={styles.screenShareBufferingFrame}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.screen-share-buffering-frame"
						/>
					)}
					{isCameraBuffering && (
						<ScreenShareBufferingFrame
							label={cameraBufferingLabel}
							className={styles.screenShareBufferingFrame}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.camera-buffering-frame"
						/>
					)}
					{showWatchFailed && !isFocusedPlaceholderTile && (
						<ScreenShareBufferingFrame
							status="failed"
							label={`${watchFailedTitle} ${watchFailedSubtext}`.trim()}
							title={watchFailedTitle}
							subtext={watchFailedSubtext}
							className={styles.screenShareBufferingFrame}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.screen-share-watch-failed-frame"
						/>
					)}
					{showWatchStreamOverlay && (
						<WatchStreamOverlay
							addStreamTooltipText={addStreamTooltipText}
							canAddStream={viewerStreamCount > 0}
							onAddStream={handleAddStream}
							onWatch={handleWatch}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.watch-stream-overlay"
						/>
					)}
					{showStreamEnded && (
						<div
							className={styles.streamEndedOverlay}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-ended-overlay"
						>
							<div
								className={styles.streamEndedContent}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-ended-content"
							>
								<span
									className={styles.streamEndedTitle}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-ended-title"
								>
									{i18n._(STREAM_ENDED_DESCRIPTOR)}
								</span>
								<Button
									variant="secondary"
									compact
									fitContent
									onClick={(event: React.MouseEvent<HTMLButtonElement>) => {
										event.stopPropagation();
										stopWatching();
									}}
									className={styles.streamEndedButton}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-ended-button.stop-propagation"
								>
									{i18n._(VOICE_STOP_WATCHING_DESCRIPTOR)}
								</Button>
							</div>
						</div>
					)}
					{isOwnScreenShareHidden && !isFocusedPlaceholderTile && (
						<FeedHiddenOverlay
							message={i18n._(STREAM_HIDDEN_DESCRIPTOR)}
							buttonLabel={i18n._(WATCH_STREAM_DESCRIPTOR)}
							onReveal={handleRevealHiddenFeed}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.feed-hidden-overlay"
						/>
					)}
					{isOwnCameraHidden && !isFocusedPlaceholderTile && (
						<FeedHiddenOverlay
							message={i18n._(CAMERA_HIDDEN_DESCRIPTOR)}
							buttonLabel={i18n._(SHOW_CAMERA_DESCRIPTOR)}
							onReveal={handleRevealHiddenFeed}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.feed-hidden-overlay--2"
						/>
					)}
					{isOwnScreenShare && !isOwnScreenShareHidden && !isFocusedPlaceholderTile && isOwnStreamPreviewPaused && (
						<div
							className={clsx(styles.selfStreamOverlay, styles.paused)}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.self-stream-overlay"
						>
							<div
								className={styles.selfStreamPreviewPaused}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.self-stream-preview-paused"
							>
								<PauseIcon
									weight="fill"
									className={styles.pausedIcon}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.paused-icon"
								/>
								<span
									className={styles.pausedText}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.paused-text"
								>
									{i18n._(PREVIEW_PAUSED_TO_SAVE_RESOURCES_DESCRIPTOR)}
								</span>
								<span
									className={styles.pausedSubtext}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.paused-subtext"
								>
									{i18n._(YOUR_STREAM_IS_STILL_LIVE_DESCRIPTOR)}
								</span>
							</div>
						</div>
					)}
					{isScreenShare &&
						!isFocusPresentationTile &&
						!isFocusedPlaceholderTile &&
						!isOwnScreenShareHidden &&
						!showStreamEnded &&
						!showWatchFailed &&
						!isOwnStreamPreviewPaused && (
							<div
								className={styles.streamInfoHud}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-info-hud"
							>
								{trackInfo ? (
									<StreamInfoPill
										info={trackInfo}
										tone="voice_tile"
										showLiveBadge
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-info-pill"
									/>
								) : (
									<LiveBadge
										showTooltip={false}
										tone="voice_tile"
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.live-badge"
									/>
								)}
							</div>
						)}
					{isScreenShare &&
						previewEnabled &&
						previewPopoverOpen &&
						!isOwnScreenShare &&
						!isFocusPresentationTile &&
						!isFocusedPlaceholderTile && (
							<div
								className={styles.previewPopover}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.preview-popover"
							>
								<StreamWatchHoverCard
									variant="compact"
									previewUrl={previewUrl}
									isPreviewLoading={isPreviewLoading}
									watchLabel={i18n._(WATCH_DESCRIPTOR)}
									watchDisabled={false}
									onWatch={handleWatch}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-watch-hover-card"
								/>
							</div>
						)}
					{cameraLocallyDisabled && !isOwnCameraHidden && !isFocusedPlaceholderTile && (
						<div
							className={styles.videoDisabledOverlay}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.video-disabled-overlay"
						>
							<VideoCameraSlashIcon
								weight="fill"
								className={styles.videoDisabledIcon}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.video-disabled-icon"
							/>
						</div>
					)}
					{showFocusIndicator && !isFocusPresentationTile && isFocusedOnThisTile && !isFocusedPlaceholderTile && (
						<div
							className={styles.focusOverlay}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.focus-overlay"
						>
							<EyeIcon
								weight="fill"
								className={styles.focusOverlayIcon}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.focus-overlay-icon"
							/>
						</div>
					)}
					{!isFocusPresentationTile &&
						!isFocusedPlaceholderTile &&
						(showStreamAudioControls ||
							(isScreenShare && viewerUsers.length > 0) ||
							(isGridTile && groupHiddenCount > 0) ||
							showDeviceCollapseControl) && (
							<div
								className={clsx(
									voiceCallStyles.tileControlPill,
									isScreenShare && viewerUsers.length > 0 && voiceCallStyles.tileControlPillPersistent,
								)}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.tile-control-pill"
							>
								{showStreamAudioControls && (
									<div
										className={clsx(voiceCallStyles.tileControlPillSlot, isStreamMuted && styles.streamAudioSlotMuted)}
										role="group"
										onClick={(e) => e.stopPropagation()}
										onKeyDown={(e) => e.stopPropagation()}
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-audio-volume"
									>
										<MediaVerticalVolumeControl
											volume={streamVolume / 100}
											isMuted={isStreamMuted}
											maxVolume={VOICE_VOLUME_MAX_SLIDER_VOLUME}
											onVolumeChange={handleStreamVolumeChange}
											onToggleMute={handleStreamAudioToggle}
											iconSize={14}
											position="below"
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-audio-volume-control"
										/>
									</div>
								)}
								{showDeviceCollapseControl && (
									<Tooltip
										text={groupCollapseTooltip}
										position="top"
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-collapse-tooltip"
									>
										<FocusRing
											offset={-2}
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-collapse-focus-ring"
										>
											<div
												role="button"
												tabIndex={0}
												className={clsx(voiceCallStyles.tileControlPillSlot, styles.groupExpandPillSlot)}
												onClick={handleExpandGroup}
												onKeyDown={(event) => {
													if (!isKeyboardActivationKey(event.key)) return;
													event.preventDefault();
													handleExpandGroup(event);
												}}
												aria-label={groupCollapseTooltip}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-collapse-pill"
											>
												<span
													className={styles.groupExpandPillSign}
													data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-expand-pill-sign"
												>
													-
												</span>
												{groupDeviceConnectionCount}
											</div>
										</FocusRing>
									</Tooltip>
								)}
								{isGridTile && groupHiddenCount > 0 && (
									<Tooltip
										text={groupExpandTooltip}
										position="top"
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-expand-tooltip"
									>
										<FocusRing
											offset={-2}
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-expand-focus-ring"
										>
											<div
												role="button"
												tabIndex={0}
												className={clsx(voiceCallStyles.tileControlPillSlot, styles.groupExpandPillSlot)}
												onClick={handleExpandGroup}
												onKeyDown={(event) => {
													if (!isKeyboardActivationKey(event.key)) return;
													event.preventDefault();
													handleExpandGroup(event);
												}}
												aria-label={groupExpandTooltip}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-expand-pill"
											>
												<span
													className={styles.groupExpandPillSign}
													data-flx="voice.voice-participant-tile.voice-participant-tile-inner.group-expand-pill-sign--2"
												>
													+
												</span>
												{groupHiddenCount}
											</div>
										</FocusRing>
									</Tooltip>
								)}
								{isScreenShare && viewerUsers.length > 0 && (
									<StreamSpectatorsPopout
										viewerUsers={viewerUsers}
										spectatorEntries={spectatorEntries}
										guildId={guildId}
										channelId={channelId}
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.stream-spectators-popout"
									>
										<div
											className={clsx(voiceCallStyles.tileControlPillSlot, voiceCallStyles.tileControlPillViewerSlot)}
											role="img"
											aria-label={i18n._(WATCHING_DESCRIPTOR, {length: viewerUsers.length})}
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.viewer-count"
										>
											<EyeIcon
												weight="fill"
												className={styles.tilePillIcon}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.viewer-icon"
											/>
											<span data-flx="voice.voice-participant-tile.voice-participant-tile-inner.viewer-count-text">
												{viewerUsers.length}
											</span>
										</div>
									</StreamSpectatorsPopout>
								)}
							</div>
						)}
					{showParticipantMetadata && (
						<div
							className={voiceCallStyles.lkParticipantMetadata}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.div"
						>
							<div
								className={voiceCallStyles.lkParticipantMetadataItem}
								data-flx="voice.voice-participant-tile.voice-participant-tile-inner.div--2"
							>
								{isGridTile && !isLocalParticipant && participantUser && (
									<FocusRing
										offset={-2}
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.focus-ring.menu"
									>
										<div
											role="button"
											tabIndex={0}
											className={voiceCallStyles.participantMetadataMenuButton}
											onClick={(e) => {
												e.stopPropagation();
												handleContextMenu(e);
											}}
											onKeyDown={handleMenuButtonKeyDown}
											aria-label={i18n._(PARTICIPANT_OPTIONS_FOR_DESCRIPTOR, {participantDisplayName})}
											aria-haspopup="menu"
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.menu-button"
										>
											<DotsThreeIcon
												weight="bold"
												className={styles.tilePillIcon}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.menu-button-icon"
											/>
										</div>
									</FocusRing>
								)}
								<div
									className={voiceCallStyles.lkParticipantIcons}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.div--3"
								>
									{isMuteStatusVisible && (
										<Tooltip
											text={muteStatusLabel}
											position="top"
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.tooltip--2"
										>
											<MicrophoneSlashIcon
												weight="fill"
												className={muteStatusClassName}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.participant-icon"
											/>
										</Tooltip>
									)}
									{isDeafenStatusVisible && (
										<Tooltip
											text={deafenStatusLabel}
											position="top"
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.tooltip--3"
										>
											<SpeakerSlashIcon
												weight="fill"
												className={deafenStatusClassName}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.participant-icon--2"
											/>
										</Tooltip>
									)}
									<Tooltip
										text={voiceState?.is_mobile ? i18n._(MOBILE_DEVICE_DESCRIPTOR) : i18n._(DESKTOP_DEVICE_DESCRIPTOR)}
										position="top"
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.tooltip--4"
									>
										{voiceState?.is_mobile ? (
											<DeviceMobileIcon
												weight="regular"
												className={styles.participantIconWhite}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.participant-icon-white"
											/>
										) : (
											<DesktopIcon
												weight="regular"
												className={styles.participantIconWhite}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.participant-icon-white--2"
											/>
										)}
									</Tooltip>
								</div>
								<div
									className={voiceCallStyles.participantMetadataLabel}
									data-flx="voice.voice-participant-tile.voice-participant-tile-inner.div--4"
								>
									<Tooltip
										text={participantDisplayName as string}
										position="top"
										data-flx="voice.voice-participant-tile.voice-participant-tile-inner.tooltip--5"
									>
										<span
											className={clsx(styles.participantNameText, voiceCallStyles.participantName)}
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.participant-name-text"
										>
											{participantDisplayName}
										</span>
									</Tooltip>
									{connectionId && (
										<Tooltip
											text={i18n._(CONNECTION_DESCRIPTOR, {connectionId})}
											position="top"
											data-flx="voice.voice-participant-tile.voice-participant-tile-inner.tooltip--6"
										>
											<span
												className={clsx(styles.participantConnectionText, voiceCallStyles.participantConn)}
												data-flx="voice.voice-participant-tile.voice-participant-tile-inner.participant-connection-text"
											>
												({connectionId})
											</span>
										</Tooltip>
									)}
								</div>
							</div>
						</div>
					)}
					{shouldRenderPoppedOutOverlay(tilePopoutTransition.snapshot) && (
						<PoppedOutOverlay
							popoutKey={tilePopoutKey}
							variant="tile"
							transition={selectPoppedOutOverlayTransition(tilePopoutTransition.snapshot)}
							onTransitionEnd={tilePopoutTransition.handleTransitionEnd}
							data-flx="voice.voice-participant-tile.voice-participant-tile-inner.popped-out-overlay"
						/>
					)}
				</LongPressable>
			</FocusRing>
			{isMobileExperience && participantUser && (
				<VoiceParticipantBottomSheet
					isOpen={bottomSheetOpen}
					onClose={() => setBottomSheetOpen(false)}
					user={participantUser}
					participant={connectionParticipant}
					guildId={guildId}
					connectionId={connectionId}
					isConnectionItem
					streamKey={streamKey}
					isScreenShare={isScreenShare}
					isWatching={isWatching}
					hasScreenShareAudio={hasScreenShareAudio}
					isOwnScreenShare={isOwnScreenShare}
					onStopWatching={stopWatching}
					data-flx="voice.voice-participant-tile.voice-participant-tile-inner.voice-participant-bottom-sheet"
				/>
			)}
		</>
	);
});

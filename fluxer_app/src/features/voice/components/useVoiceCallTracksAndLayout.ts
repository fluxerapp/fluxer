// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Users from '@app/features/user/state/Users';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import {parseStreamKey} from '@app/features/voice/components/StreamKeys';
import {usePinnedTrackRef} from '@app/features/voice/components/usePinnedTrackRef';
import {
	dedupeScreenShareTracks,
	splitVoiceCallRenderableTracks,
} from '@app/features/voice/components/VoiceCallTrackPartition';
import {
	consolidateVoiceGridTracks,
	type VoiceGridEntry,
} from '@app/features/voice/components/VoiceParticipantConsolidation';
import {
	countKnownVoiceParticipants,
	isKnownVoiceParticipantIdentity,
	isKnownVoiceTrackRef,
} from '@app/features/voice/components/VoiceParticipantRenderUtils';
import {
	compareVoiceTrackReferencesWithSnapshot,
	createVoiceParticipantSortSnapshot,
	syncVoiceParticipantSortSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {selectVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {pruneInactiveWatchedStreamsForChannel} from '@app/features/voice/engine/VoiceStreamWatchState';
import {asVoiceTrackSource, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {isVoiceEngineV2AppParticipantSpeaking} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import CallMediaPrefs from '@app/features/voice/state/CallMediaPrefs';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	buildVoiceParticipantIdentity,
	parseVoiceParticipantIdentity,
} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {isTrackReference, type TrackReferenceOrPlaceholder, useMaybeRoomContext} from '@livekit/components-react';
import {type Participant, type Room, RoomEvent, type Track} from 'livekit-client';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface UseVoiceCallTracksAndLayoutArgs {
	channel: Channel;
	expandedUserIds: ReadonlySet<string>;
}

interface VoiceGridTrackActivity {
	priority: number;
	promotedOrder: number;
}

interface VoiceGridTrackActivityState {
	lastActivityAt: number;
	promotedOrder: number;
}

const VOICE_GRID_SPEAKER_PRIORITY_HOLD_MS = 4500;
const CAMERA_SOURCE = VoiceTrackSource.Camera as Track.Source;
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;

interface VoiceCallTrackSourceOption {
	source: Track.Source;
	withPlaceholder: boolean;
}

interface VoiceCallTrackRefsState {
	tracks: Array<TrackReferenceOrPlaceholder>;
	participants: Array<Participant>;
}

const VOICE_CALL_TRACK_SOURCES: ReadonlyArray<VoiceCallTrackSourceOption> = [
	{source: CAMERA_SOURCE, withPlaceholder: true},
	{source: SCREEN_SHARE_SOURCE, withPlaceholder: false},
];

const LIVEKIT_TRACK_UPDATE_EVENTS: ReadonlyArray<RoomEvent> = [
	RoomEvent.ParticipantConnected,
	RoomEvent.ParticipantDisconnected,
	RoomEvent.ConnectionStateChanged,
	RoomEvent.LocalTrackPublished,
	RoomEvent.LocalTrackUnpublished,
	RoomEvent.TrackPublished,
	RoomEvent.TrackUnpublished,
	RoomEvent.TrackSubscriptionStatusChanged,
	RoomEvent.TrackMuted,
	RoomEvent.TrackUnmuted,
	RoomEvent.TrackSubscribed,
	RoomEvent.TrackUnsubscribed,
	RoomEvent.ActiveSpeakersChanged,
];

function difference<T>(setA: Set<T>, setB: Set<T>): Set<T> {
	const result = new Set(setA);
	for (const value of setB) {
		result.delete(value);
	}
	return result;
}

function collectLiveKitTrackRefs(
	room: Room,
	sources: ReadonlyArray<Track.Source>,
	onlySubscribedTracks: boolean,
): VoiceCallTrackRefsState {
	const participants = [room.localParticipant, ...Array.from(room.remoteParticipants.values())];
	const tracks: Array<TrackReferenceOrPlaceholder> = [];
	for (const participant of participants) {
		for (const source of sources) {
			for (const publication of participant.trackPublications.values()) {
				if (publication.source !== source) continue;
				if (onlySubscribedTracks && !publication.track) continue;
				tracks.push({participant, publication, source: publication.source});
			}
		}
	}
	return {tracks, participants};
}

function requiredLiveKitPlaceholders(
	sources: ReadonlyArray<VoiceCallTrackSourceOption>,
	participants: ReadonlyArray<Participant>,
): Map<Participant['identity'], Array<Track.Source>> {
	const placeholderSources = sources.filter((source) => source.withPlaceholder).map((source) => source.source);
	const placeholderMap = new Map<Participant['identity'], Array<Track.Source>>();
	for (const participant of participants) {
		const publishedSources = participant
			.getTrackPublications()
			.map((publication) => publication.track?.source ?? publication.source)
			.filter((trackSource): trackSource is Track.Source => trackSource !== undefined);
		const neededSources = Array.from(difference(new Set(placeholderSources), new Set(publishedSources)));
		if (neededSources.length > 0) {
			placeholderMap.set(participant.identity, neededSources);
		}
	}
	return placeholderMap;
}

function addLiveKitPlaceholders(
	tracks: ReadonlyArray<TrackReferenceOrPlaceholder>,
	participants: ReadonlyArray<Participant>,
	sources: ReadonlyArray<VoiceCallTrackSourceOption>,
): Array<TrackReferenceOrPlaceholder> {
	const tracksWithPlaceholders = Array.from(tracks);
	const placeholders = requiredLiveKitPlaceholders(sources, participants);
	for (const participant of participants) {
		const sourcesToAdd = placeholders.get(participant.identity);
		if (!sourcesToAdd) continue;
		for (const source of sourcesToAdd) {
			if (
				tracks.some(
					(trackRef) =>
						trackRef.participant.identity === participant.identity &&
						isTrackReference(trackRef) &&
						trackRef.publication.source === source,
				)
			) {
				continue;
			}
			tracksWithPlaceholders.push({participant, source});
		}
	}
	return tracksWithPlaceholders;
}

function useOptionalLiveKitVoiceCallTrackRefs(
	sources: ReadonlyArray<VoiceCallTrackSourceOption>,
	options: {onlySubscribed: boolean},
): VoiceCallTrackRefsState {
	const room = useMaybeRoomContext();
	const [state, setState] = useState<VoiceCallTrackRefsState>({tracks: [], participants: []});
	const sourceValues = useMemo(() => sources.map((source) => source.source), [sources]);
	useEffect(() => {
		if (!room) {
			setState({tracks: [], participants: []});
			return;
		}
		const update = () => {
			setState(collectLiveKitTrackRefs(room, sourceValues, options.onlySubscribed));
		};
		update();
		for (const event of LIVEKIT_TRACK_UPDATE_EVENTS) {
			room.on(event, update);
		}
		return () => {
			for (const event of LIVEKIT_TRACK_UPDATE_EVENTS) {
				room.off(event, update);
			}
		};
	}, [room, options.onlySubscribed, sourceValues]);
	const tracks = useMemo(
		() => addLiveKitPlaceholders(state.tracks, state.participants, sources),
		[state.tracks, state.participants, sources],
	);
	return useMemo(() => ({tracks, participants: state.participants}), [tracks, state.participants]);
}

function getTrackActivityKey(trackRef: TrackReferenceOrPlaceholder): string {
	return `${trackRef.participant.identity}:${trackRef.source}`;
}

export function useVoiceCallTracksAndLayout({channel, expandedUserIds}: UseVoiceCallTracksAndLayoutArgs) {
	useMediaEngineVersion();
	const {layoutMode, pinnedParticipantIdentity, pinnedParticipantSource} = VoiceCallLayout;
	const trackSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const trackActivityStateRef = useRef(new Map<string, VoiceGridTrackActivityState>());
	const trackActivityNextOrderRef = useRef(0);
	const compareTracks = useCallback(
		(left: TrackReferenceOrPlaceholder, right: TrackReferenceOrPlaceholder) =>
			compareVoiceTrackReferencesWithSnapshot(
				left,
				right,
				trackSortSnapshotRef.current,
				channel.guildId ?? null,
				channel.id,
			),
		[channel.guildId, channel.id],
	);
	const liveKitTrackRefs = useOptionalLiveKitVoiceCallTrackRefs(VOICE_CALL_TRACK_SOURCES, {onlySubscribed: false});
	const connectionVoiceStates = MediaEngine.connectionVoiceStates;
	const participantSnapshots = MediaEngine.participants;
	const tracks = liveKitTrackRefs.tracks;
	const participants = liveKitTrackRefs.participants;
	const userCacheSize = Users.usersList.length;
	const participantCount = useMemo(() => countKnownVoiceParticipants(participants), [participants, userCacheSize]);
	const renderableTracks = useMemo(() => tracks.filter(isKnownVoiceTrackRef), [tracks, userCacheSize]);
	const viewerStreamKeys = selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.graph);
	const screenSharePublicationMigrationVersion = ScreenSharePublicationMigration.version;
	const localConnectionId = MediaEngine.connectionId;
	const localSelfStream = LocalVoiceState.getSelfStream();
	const {screenShareTracks, cameraTracksAll} = useMemo(
		() => splitVoiceCallRenderableTracks(renderableTracks),
		[renderableTracks, screenSharePublicationMigrationVersion],
	);
	const activeScreenShareState = useMemo(() => {
		const connectionIds = new Set<string>();
		const participantIdentities = new Set<string>();
		for (const tr of screenShareTracks) {
			participantIdentities.add(tr.participant.identity);
			const parsedIdentity = parseVoiceParticipantIdentity(tr.participant.identity);
			if (parsedIdentity.connectionId) {
				connectionIds.add(parsedIdentity.connectionId);
			}
		}
		return {connectionIds, participantIdentities};
	}, [screenShareTracks]);
	const activeScreenShareConnectionIds = activeScreenShareState.connectionIds;
	const activeScreenShareParticipantIdentities = activeScreenShareState.participantIdentities;
	const participantsByIdentity = useMemo(() => {
		const map = new Map<string, TrackReferenceOrPlaceholder['participant']>();
		for (const participant of participants) {
			map.set(participant.identity, participant);
		}
		return map;
	}, [participants]);
	const virtualScreenShareTracks = useMemo<Array<TrackReferenceOrPlaceholder>>(() => {
		if (viewerStreamKeys.length === 0) return [];
		const virtualTracks: Array<TrackReferenceOrPlaceholder> = [];
		for (const viewerStreamKey of viewerStreamKeys) {
			const parsed = parseStreamKey(viewerStreamKey);
			if (!parsed) continue;
			if (parsed.channelId !== channel.id) continue;
			const channelGuildId = channel.guildId ?? null;
			if (parsed.guildId !== channelGuildId) continue;
			const voiceState = connectionVoiceStates[parsed.connectionId];
			if (!voiceState?.user_id) continue;
			if (voiceState.channel_id !== channel.id) continue;
			const isLocalStream = parsed.connectionId === localConnectionId;
			const isStreamActive =
				activeScreenShareConnectionIds.has(parsed.connectionId) ||
				(isLocalStream ? localSelfStream : voiceState.self_stream);
			if (!isStreamActive) continue;
			const identity = buildVoiceParticipantIdentity(voiceState.user_id, parsed.connectionId);
			if (!isKnownVoiceParticipantIdentity(identity)) continue;
			if (activeScreenShareParticipantIdentities.has(identity)) continue;
			const participant = participantsByIdentity.get(identity);
			if (!participant) continue;
			virtualTracks.push({participant, source: SCREEN_SHARE_SOURCE});
		}
		return virtualTracks;
	}, [
		activeScreenShareConnectionIds,
		activeScreenShareParticipantIdentities,
		channel.id,
		channel.guildId,
		connectionVoiceStates,
		localConnectionId,
		localSelfStream,
		participantsByIdentity,
		userCacheSize,
		viewerStreamKeys,
	]);
	const screenShareTracksWithVirtual = useMemo(
		() =>
			dedupeScreenShareTracks(
				virtualScreenShareTracks.length > 0 ? [...screenShareTracks, ...virtualScreenShareTracks] : screenShareTracks,
			),
		[screenShareTracks, virtualScreenShareTracks, screenSharePublicationMigrationVersion],
	);
	const trackSnapshotMembers = useMemo(
		() =>
			[...cameraTracksAll, ...screenShareTracksWithVirtual].map((trackRef) => {
				const identity = parseVoiceParticipantIdentity(trackRef.participant.identity);
				return {
					participantKey: `${identity.userId}:${identity.connectionId}`,
					userId: identity.userId,
				};
			}),
		[cameraTracksAll, screenShareTracksWithVirtual],
	);
	syncVoiceParticipantSortSnapshot(
		trackSortSnapshotRef.current,
		trackSnapshotMembers,
		channel.guildId ?? null,
		channel.id,
	);
	const sortedCameraTracksAll = useMemo(
		() => [...cameraTracksAll].sort(compareTracks),
		[cameraTracksAll, compareTracks],
	);
	const filteredCameraTracks = useMemo(() => {
		if (VoiceSettings.showNonVideoParticipants) return cameraTracksAll;
		return cameraTracksAll.filter((tr) => {
			if (!isTrackReference(tr)) return false;
			if (!tr.publication) return false;
			return !tr.publication.isMuted;
		});
	}, [cameraTracksAll, VoiceSettings.showNonVideoParticipants]);
	const sortedFilteredCameraTracks = useMemo(
		() => [...filteredCameraTracks].sort(compareTracks),
		[filteredCameraTracks, compareTracks],
	);
	const sortedScreenShareTracksWithVirtual = useMemo(
		() => [...screenShareTracksWithVirtual].sort(compareTracks),
		[screenShareTracksWithVirtual, compareTracks],
	);
	const hasScreenShare = sortedScreenShareTracksWithVirtual.length > 0;
	const prioritizeSpeakingParticipants = VoiceSettings.prioritizeSpeakingParticipants;
	const gridTracks = useMemo(() => {
		const now = Date.now();
		const activityStates = trackActivityStateRef.current;
		const trackRefs = hasScreenShare
			? [...sortedScreenShareTracksWithVirtual, ...sortedFilteredCameraTracks]
			: sortedFilteredCameraTracks;
		if (!prioritizeSpeakingParticipants) {
			activityStates.clear();
			trackActivityNextOrderRef.current = 0;
			return trackRefs;
		}
		const activeKeys = new Set<string>();
		const activityByKey = new Map<string, VoiceGridTrackActivity>();
		for (const trackRef of trackRefs) {
			const key = getTrackActivityKey(trackRef);
			const parsedIdentity = parseVoiceParticipantIdentity(trackRef.participant.identity);
			const participantSnapshot = participantSnapshots[trackRef.participant.identity];
			const voiceState = parsedIdentity.connectionId ? connectionVoiceStates[parsedIdentity.connectionId] : undefined;
			const isStreaming =
				asVoiceTrackSource(trackRef.source) === VoiceTrackSource.ScreenShare ||
				participantSnapshot?.isScreenShareEnabled === true ||
				voiceState?.self_stream === true;
			const isSpeaking = isVoiceEngineV2AppParticipantSpeaking(participantSnapshot);
			const lastSpokeAt = participantSnapshot?.lastSpokeAt ?? 0;
			const liveActivityAt = isSpeaking ? now : 0;
			const observedActivityAt = Math.max(liveActivityAt, lastSpokeAt);
			let activityState = activityStates.get(key);
			if (observedActivityAt > 0) {
				if (!activityState) {
					activityState = {
						lastActivityAt: observedActivityAt,
						promotedOrder: trackActivityNextOrderRef.current,
					};
					trackActivityNextOrderRef.current += 1;
					activityStates.set(key, activityState);
				} else {
					activityState.lastActivityAt = Math.max(activityState.lastActivityAt, observedActivityAt);
				}
			}
			if (activityState && now - activityState.lastActivityAt > VOICE_GRID_SPEAKER_PRIORITY_HOLD_MS) {
				activityStates.delete(key);
				activityState = undefined;
			}
			const priority = isStreaming ? 3 : isSpeaking ? 2 : activityState ? 1 : 0;
			activeKeys.add(key);
			activityByKey.set(key, {priority, promotedOrder: activityState?.promotedOrder ?? Number.MAX_SAFE_INTEGER});
		}
		for (const key of activityStates.keys()) {
			if (!activeKeys.has(key)) {
				activityStates.delete(key);
			}
		}
		if (activityStates.size === 0) {
			trackActivityNextOrderRef.current = 0;
		}
		return [...trackRefs].sort((left, right) => {
			const leftActivity = activityByKey.get(getTrackActivityKey(left));
			const rightActivity = activityByKey.get(getTrackActivityKey(right));
			const byPriority = (rightActivity?.priority ?? 0) - (leftActivity?.priority ?? 0);
			if (byPriority !== 0) return byPriority;
			if ((leftActivity?.priority ?? 0) > 0 || (rightActivity?.priority ?? 0) > 0) {
				const byPromotedOrder =
					(leftActivity?.promotedOrder ?? Number.MAX_SAFE_INTEGER) -
					(rightActivity?.promotedOrder ?? Number.MAX_SAFE_INTEGER);
				if (byPromotedOrder !== 0) return byPromotedOrder;
			}
			return compareTracks(left, right);
		});
	}, [
		compareTracks,
		connectionVoiceStates,
		hasScreenShare,
		participantSnapshots,
		prioritizeSpeakingParticipants,
		sortedFilteredCameraTracks,
		sortedScreenShareTracksWithVirtual,
	]);
	useEffect(() => {
		pruneInactiveWatchedStreamsForChannel({
			guildId: channel.guildId,
			channelId: channel.id,
			isStreamActive: (connectionId) => {
				if (activeScreenShareConnectionIds.has(connectionId)) return true;
				if (connectionId === localConnectionId) return localSelfStream;
				const voiceState = connectionVoiceStates[connectionId];
				return Boolean(voiceState?.self_stream && voiceState.channel_id === channel.id);
			},
		});
	}, [
		activeScreenShareConnectionIds,
		channel.guildId,
		channel.id,
		connectionVoiceStates,
		localConnectionId,
		localSelfStream,
		viewerStreamKeys,
	]);
	useEffect(() => {
		const callId = MediaEngine.connectionId ?? '';
		const currentUserId = Users.currentUser?.id;
		if (!callId || !currentUserId) return;
		for (const tr of cameraTracksAll) {
			if (!isTrackReference(tr)) continue;
			if (asVoiceTrackSource(tr.source) !== VoiceTrackSource.Camera) continue;
			const identity = tr.participant.identity || '';
			if (!identity.startsWith(`user_${currentUserId}_`)) continue;
			const {connectionId} = parseVoiceParticipantIdentity(identity);
			if (connectionId !== localConnectionId) continue;
			const isPublishing = Boolean(tr.publication && !tr.publication.isMuted);
			const disabled = !VoiceSettings.showMyOwnCamera && isPublishing;
			CallMediaPrefs.setVideoDisabled(callId, identity, disabled);
		}
	}, [cameraTracksAll, VoiceSettings.showMyOwnCamera, localConnectionId]);
	useEffect(() => {
		if (!pinnedParticipantIdentity) return;
		if (cameraTracksAll.length === 0) return;
		const stillExists = cameraTracksAll.some((tr) => tr.participant.identity === pinnedParticipantIdentity);
		if (!stillExists) VoiceCallLayoutCommands.setPinnedParticipant(null);
	}, [cameraTracksAll, pinnedParticipantIdentity]);
	const {
		mainTrack: focusMainTrack,
		carouselTracks,
		pipTrack,
	} = usePinnedTrackRef({
		layoutMode,
		pinnedParticipantIdentity,
		pinnedParticipantSource,
		filteredCameraTracks: sortedFilteredCameraTracks,
		cameraTracksAll: sortedCameraTracksAll,
		screenShareTracks: sortedScreenShareTracksWithVirtual,
		compareTracks,
		participantSnapshots,
	});
	const gridEntries = useMemo<Array<VoiceGridEntry>>(
		() => consolidateVoiceGridTracks({tracks: gridTracks, expandedUserIds}),
		[gridTracks, expandedUserIds],
	);
	return {
		tracks: renderableTracks,
		screenShareTracks: sortedScreenShareTracksWithVirtual,
		cameraTracksAll: sortedCameraTracksAll,
		filteredCameraTracks: sortedFilteredCameraTracks,
		hasScreenShare,
		gridEntries,
		participantCount,
		layoutMode,
		pinnedParticipantIdentity,
		pinnedParticipantSource,
		focusMainTrack,
		carouselTracks,
		pipTrack,
		channel,
	};
}

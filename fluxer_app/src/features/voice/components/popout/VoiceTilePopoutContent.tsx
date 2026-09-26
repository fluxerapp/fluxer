// SPDX-License-Identifier: AGPL-3.0-or-later

import {Avatar} from '@app/features/ui/components/Avatar';
import Users from '@app/features/user/state/Users';
import styles from '@app/features/voice/components/popout/VoicePopoutHost.module.css';
import {VoiceParticipantTile} from '@app/features/voice/components/VoiceParticipantTile';
import {useMaybeVoiceRoom} from '@app/features/voice/components/VoiceRoomContext';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import PopoutWindowManager, {type VoiceTilePopoutDescriptor} from '@app/features/voice/state/PopoutWindowManager';
import {isTrackReference, type TrackReferenceOrPlaceholder, useTracks} from '@livekit/components-react';
import {type Room, RoomEvent, type Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const CAMERA_SOURCE = VoiceTrackSource.Camera as Track.Source;
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;

function unsupportedVoiceTilePopoutSource(source: never): never {
	throw new Error(`Unsupported voice tile popout source: ${String(source)}`);
}

function getDescriptorTrackSource(descriptor: VoiceTilePopoutDescriptor): Track.Source | null {
	switch (descriptor.source) {
		case 'user':
			return null;
		case 'camera':
			return CAMERA_SOURCE;
		case 'screen_share':
			return SCREEN_SHARE_SOURCE;
		default:
			return unsupportedVoiceTilePopoutSource(descriptor.source);
	}
}

function isVoiceTilePopoutTargetLive(
	descriptor: VoiceTilePopoutDescriptor,
	trackRef: TrackReferenceOrPlaceholder | null,
): boolean {
	if (descriptor.source === 'user') return true;
	const voiceState = MediaEngine.getVoiceStateByConnectionId(descriptor.connectionId);
	if (voiceState && voiceState.user_id !== descriptor.userId) return false;
	const connectionParticipant = MediaEngine.getParticipantByUserIdAndConnectionId(
		descriptor.userId,
		descriptor.connectionId,
	);
	const targetSource = getDescriptorTrackSource(descriptor);
	const hasMatchingLiveKitTrack = Boolean(
		trackRef &&
			isTrackReference(trackRef) &&
			trackRef.participant.identity === descriptor.participantIdentity &&
			trackRef.source === targetSource,
	);
	switch (descriptor.source) {
		case 'camera':
			return Boolean(connectionParticipant?.isCameraEnabled || hasMatchingLiveKitTrack || voiceState?.self_video);
		case 'screen_share':
			return Boolean(connectionParticipant?.isScreenShareEnabled || hasMatchingLiveKitTrack || voiceState?.self_stream);
		default:
			return unsupportedVoiceTilePopoutSource(descriptor.source);
	}
}

function useTilePopoutLiveKitTrackRef(
	descriptor: VoiceTilePopoutDescriptor,
	room: Room,
): TrackReferenceOrPlaceholder | null {
	useStoreVersion(ScreenSharePublicationMigration);
	const tracks = useTracks(
		[
			{source: CAMERA_SOURCE, withPlaceholder: true},
			{source: SCREEN_SHARE_SOURCE, withPlaceholder: true},
		],
		{
			updateOnlyOn: [
				RoomEvent.TrackPublished,
				RoomEvent.TrackUnpublished,
				RoomEvent.TrackSubscribed,
				RoomEvent.TrackUnsubscribed,
				RoomEvent.TrackMuted,
				RoomEvent.TrackUnmuted,
			],
			onlySubscribed: false,
			room,
		},
	);
	const screenSharePublicationMigrationVersion = ScreenSharePublicationMigration.version;
	return useMemo(() => {
		const targetSource = getDescriptorTrackSource(descriptor);
		if (targetSource === null) return null;
		const matchingTrackRefs = tracks.filter(
			(trackRef) =>
				trackRef.participant.identity === descriptor.participantIdentity && trackRef.source === targetSource,
		);
		if (targetSource !== SCREEN_SHARE_SOURCE) {
			return matchingTrackRefs[0] ?? null;
		}
		const migratedTrackRef = matchingTrackRefs.find((trackRef) => {
			if (!isTrackReference(trackRef)) return false;
			const selected = ScreenSharePublicationMigration.selectScreenSharePublication(trackRef.participant);
			return selected?.trackSid === trackRef.publication.trackSid;
		});
		return migratedTrackRef ?? matchingTrackRefs[0] ?? null;
	}, [tracks, descriptor, screenSharePublicationMigrationVersion]);
}

interface VoiceTilePopoutContentBaseProps {
	descriptor: VoiceTilePopoutDescriptor;
	trackRef: TrackReferenceOrPlaceholder | null;
}

const VoiceTilePopoutContentBase = observer(function VoiceTilePopoutContentBase({
	descriptor,
	trackRef,
}: VoiceTilePopoutContentBaseProps) {
	useMediaEngineVersion();
	const isTrackLive = isVoiceTilePopoutTargetLive(descriptor, trackRef);
	const standaloneUser = descriptor.source === 'user' && !trackRef ? Users.getUser(descriptor.userId) : undefined;
	useEffect(() => {
		if (isTrackLive) return;
		PopoutWindowManager.close(descriptor.key, descriptor.generation);
	}, [isTrackLive, descriptor.generation, descriptor.key]);
	return (
		<div className={styles.tileContent} data-flx="voice.voice-tile-popout-content.tile-content">
			{trackRef && (
				<VoiceParticipantTile
					trackRef={trackRef}
					guildId={descriptor.guildId ?? undefined}
					channelId={descriptor.channelId}
					showFocusIndicator={false}
					presentation="focus-main"
					data-flx="voice.voice-tile-popout-content.voice-participant-tile"
				/>
			)}
			{standaloneUser && (
				<div
					className={styles.standaloneUserContent}
					data-flx="voice.popout.voice-tile-popout-content.voice-tile-popout-content-base.standalone-user-content"
				>
					<Avatar
						user={standaloneUser}
						size={128}
						guildId={descriptor.guildId}
						data-flx="voice.popout.voice-tile-popout-content.voice-tile-popout-content-base.avatar"
					/>
					<div
						className={styles.standaloneUserName}
						data-flx="voice.popout.voice-tile-popout-content.voice-tile-popout-content-base.standalone-user-name"
					>
						{descriptor.title}
					</div>
				</div>
			)}
		</div>
	);
});

const VoiceTilePopoutContentWithLiveKit = observer(function VoiceTilePopoutContentWithLiveKit({
	descriptor,
	room,
}: {
	descriptor: VoiceTilePopoutDescriptor;
	room: Room;
}) {
	const trackRef = useTilePopoutLiveKitTrackRef(descriptor, room);
	return (
		<VoiceTilePopoutContentBase
			descriptor={descriptor}
			trackRef={trackRef}
			data-flx="voice.voice-tile-popout-content.base.livekit"
		/>
	);
});

export const VoiceTilePopoutContent: React.FC<{descriptor: VoiceTilePopoutDescriptor}> = observer(
	function VoiceTilePopoutContent({descriptor}) {
		const room = useMaybeVoiceRoom() ?? null;
		if (!room) {
			return (
				<VoiceTilePopoutContentBase
					descriptor={descriptor}
					trackRef={null}
					data-flx="voice.voice-tile-popout-content.base.native"
				/>
			);
		}
		return (
			<VoiceTilePopoutContentWithLiveKit
				descriptor={descriptor}
				room={room}
				data-flx="voice.voice-tile-popout-content.with-livekit"
			/>
		);
	},
);

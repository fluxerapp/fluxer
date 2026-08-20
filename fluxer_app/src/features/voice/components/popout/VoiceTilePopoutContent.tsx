// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/popout/VoicePopoutHost.module.css';
import {VoiceParticipantTile} from '@app/features/voice/components/VoiceParticipantTile';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import ScreenSharePublicationMigration from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import PopoutWindowManager, {type VoiceTilePopoutDescriptor} from '@app/features/voice/state/PopoutWindowManager';
import {
	isTrackReference,
	type TrackReferenceOrPlaceholder,
	useMaybeRoomContext,
	useTracks,
} from '@livekit/components-react';
import {type Room, RoomEvent, type Track} from 'livekit-client';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const CAMERA_SOURCE = VoiceTrackSource.Camera as Track.Source;
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;

function getDescriptorTrackSource(descriptor: VoiceTilePopoutDescriptor): Track.Source {
	return descriptor.source === 'screen_share' ? SCREEN_SHARE_SOURCE : CAMERA_SOURCE;
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
	const connectionParticipant = MediaEngine.getParticipantByUserIdAndConnectionId(
		descriptor.userId,
		descriptor.connectionId,
	);
	const isTrackLive =
		descriptor.source === 'camera'
			? Boolean(connectionParticipant?.isCameraEnabled)
			: Boolean(connectionParticipant?.isScreenShareEnabled);
	useEffect(() => {
		if (isTrackLive) return;
		PopoutWindowManager.close(descriptor.key);
	}, [isTrackLive, descriptor.key]);
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
		const room = useMaybeRoomContext() ?? null;
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

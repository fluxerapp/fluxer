// SPDX-License-Identifier: AGPL-3.0-or-later

import {STREAM_VOLUME_DESCRIPTOR} from '@app/features/ui/action_menu/items/voice_participant_menu_data/shared';
import {MediaVerticalVolumeControl} from '@app/features/voice/components/media_player/components/MediaVerticalVolumeControl';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {
	asVoiceTrackSource,
	isScreenShareAudioPublicationLike,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {VOICE_VOLUME_MAX_SLIDER_VOLUME} from '@app/features/voice/utils/VoiceVolumeUtils';
import {useLingui} from '@lingui/react/macro';
import type {TrackReferenceOrPlaceholder} from '@livekit/components-react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface FocusedStreamVolumeControlProps {
	track: TrackReferenceOrPlaceholder | null;
	guildId: string | null | undefined;
	channelId: string;
	className?: string;
}

export const FocusedStreamVolumeControl: React.FC<FocusedStreamVolumeControlProps> = observer(
	function FocusedStreamVolumeControl({track, guildId, channelId, className}) {
		const {i18n} = useLingui();
		useMediaEngineVersion();
		const isRemoteScreenShare =
			track != null && asVoiceTrackSource(track.source) === VoiceTrackSource.ScreenShare && !track.participant.isLocal;
		const {userId, connectionId} = isRemoteScreenShare
			? parseVoiceParticipantIdentity(track.participant.identity)
			: {userId: null, connectionId: null};
		const streamKey = connectionId ? getStreamKey(guildId, channelId, connectionId) : '';
		const hasStreamAudio =
			isRemoteScreenShare &&
			[...track.participant.audioTrackPublications.values()].some((publication) =>
				isScreenShareAudioPublicationLike(publication),
			);
		const isMuted = streamKey ? StreamAudioPrefs.isMuted(streamKey) : false;
		const handleToggleMute = useCallback(() => {
			if (!streamKey || !userId) return;
			StreamAudioPrefs.setMuted(streamKey, !StreamAudioPrefs.isMuted(streamKey));
			MediaEngine.applyLocalAudioPreferencesForUser(userId);
		}, [streamKey, userId]);
		const handleVolumeChange = useCallback(
			(volume: number) => {
				if (!streamKey || !userId) return;
				StreamAudioPrefs.setVolume(streamKey, Math.round(volume * 100));
				MediaEngine.applyLocalAudioPreferencesForUser(userId);
			},
			[streamKey, userId],
		);
		if (!streamKey || !userId || !hasStreamAudio) return null;
		return (
			<MediaVerticalVolumeControl
				volume={StreamAudioPrefs.getVolume(streamKey) / 100}
				isMuted={isMuted}
				maxVolume={VOICE_VOLUME_MAX_SLIDER_VOLUME}
				onVolumeChange={handleVolumeChange}
				onToggleMute={handleToggleMute}
				iconSize={18}
				className={className}
				position="above"
				ariaLabel={i18n._(STREAM_VOLUME_DESCRIPTOR)}
				data-flx="voice.focused-stream-volume-control.media-vertical-volume-control"
			/>
		);
	},
);

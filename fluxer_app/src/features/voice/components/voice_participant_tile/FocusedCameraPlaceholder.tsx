// SPDX-License-Identifier: AGPL-3.0-or-later

import {Avatar} from '@app/features/ui/components/Avatar';
import type {User} from '@app/features/user/models/User';
import voiceCallStyles from '@app/features/voice/components/VoiceCallView.module.css';
import styles from '@app/features/voice/components/VoiceParticipantTile.module.css';
import {
	TILE_AVATAR_BASE,
	TILE_AVATAR_MEDIA_SIZE,
	TILE_AVATAR_STYLE,
} from '@app/features/voice/components/voice_participant_tile/shared';
import {VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

interface FocusedCameraPlaceholderProps {
	guildId?: string;
	participantUser: User | undefined;
	style: React.CSSProperties;
}

export function FocusedCameraPlaceholder({guildId, participantUser, style}: FocusedCameraPlaceholderProps) {
	return (
		<>
			<div
				style={style}
				className={voiceCallStyles.lkParticipantPlaceholder}
				data-flx="voice.voice-participant-tile.focused-camera-placeholder.div"
			>
				{participantUser && (
					<div
						className={clsx(styles.tileAvatarRing, styles.avatarRing, styles.focusedPlaceholderAvatarDimmed)}
						data-flx="voice.voice-participant-tile.focused-camera-placeholder.avatar-ring"
					>
						<Avatar
							user={participantUser}
							size={TILE_AVATAR_BASE}
							mediaSize={TILE_AVATAR_MEDIA_SIZE}
							className={styles.avatarFlexShrink}
							style={TILE_AVATAR_STYLE}
							guildId={guildId}
							data-flx="voice.voice-participant-tile.focused-camera-placeholder.avatar-flex-shrink"
						/>
					</div>
				)}
			</div>
			<div
				className={styles.focusedPlaceholderCameraOverlay}
				data-flx="voice.voice-participant-tile.focused-camera-placeholder.focused-placeholder-camera-overlay"
			/>
			<div
				className={styles.focusedPlaceholderIconLayer}
				data-flx="voice.voice-participant-tile.focused-camera-placeholder.focused-placeholder-icon-layer"
			>
				<VideoCameraIcon
					weight="fill"
					className={styles.focusedPlaceholderIcon}
					data-flx="voice.voice-participant-tile.focused-camera-placeholder.focused-placeholder-icon"
				/>
			</div>
		</>
	);
}

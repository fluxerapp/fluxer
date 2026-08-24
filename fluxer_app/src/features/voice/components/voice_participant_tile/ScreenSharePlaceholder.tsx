// SPDX-License-Identifier: AGPL-3.0-or-later

import {Avatar} from '@app/features/ui/components/Avatar';
import {LiveBadge} from '@app/features/ui/components/LiveBadge';
import type {User} from '@app/features/user/models/User';
import styles from '@app/features/voice/components/VoiceParticipantTile.module.css';
import {
	TILE_AVATAR_BASE,
	TILE_AVATAR_MEDIA_SIZE,
	TILE_AVATAR_STYLE,
} from '@app/features/voice/components/voice_participant_tile/shared';
import {MonitorPlayIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

interface ScreenSharePlaceholderProps {
	guildId?: string;
	participantUser: User | undefined;
	showLiveBadge: boolean;
	style: React.CSSProperties;
}

export function ScreenSharePlaceholder({guildId, participantUser, showLiveBadge, style}: ScreenSharePlaceholderProps) {
	return (
		<>
			<div
				style={style}
				className={styles.focusedPlaceholderScreenSurface}
				data-flx="voice.voice-participant-tile.screen-share-placeholder.focused-placeholder-screen-surface"
			>
				{participantUser && (
					<div
						className={clsx(styles.tileAvatarRing, styles.avatarRing, styles.focusedPlaceholderAvatarDimmed)}
						data-flx="voice.voice-participant-tile.screen-share-placeholder.avatar-ring"
					>
						<Avatar
							user={participantUser}
							size={TILE_AVATAR_BASE}
							mediaSize={TILE_AVATAR_MEDIA_SIZE}
							className={styles.avatarFlexShrink}
							style={TILE_AVATAR_STYLE}
							guildId={guildId}
							data-flx="voice.voice-participant-tile.screen-share-placeholder.avatar-flex-shrink"
						/>
					</div>
				)}
			</div>
			<div
				className={styles.focusedPlaceholderCameraOverlay}
				data-flx="voice.voice-participant-tile.screen-share-placeholder.focused-placeholder-camera-overlay"
			/>
			<div
				className={styles.focusedPlaceholderIconLayer}
				data-flx="voice.voice-participant-tile.screen-share-placeholder.focused-placeholder-icon-layer"
			>
				<MonitorPlayIcon
					weight="fill"
					className={styles.focusedPlaceholderIcon}
					data-flx="voice.voice-participant-tile.screen-share-placeholder.focused-placeholder-icon"
				/>
			</div>
			{showLiveBadge && (
				<div
					className={styles.focusedPlaceholderLiveBadge}
					data-flx="voice.voice-participant-tile.screen-share-placeholder.focused-placeholder-live-badge"
				>
					<LiveBadge
						showTooltip={false}
						tone="voice_tile"
						data-flx="voice.voice-participant-tile.screen-share-placeholder.live-badge"
					/>
				</div>
			)}
		</>
	);
}

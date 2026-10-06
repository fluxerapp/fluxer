// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/VoiceStateIcons.module.css';
import {CAMERA_ON_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {LiveBadge} from '@app/features/ui/components/LiveBadge';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	getVoiceDeafenedByModeratorsStatusLabel,
	getVoiceDeafenedStatusLabel,
	getVoiceNoSpeakPermissionLabel,
	VOICE_MUTED_BY_MODERATORS_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MicrophoneSlashIcon, SpeakerSlashIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const MUTED_DESCRIPTOR = msg({
	message: 'Muted',
	context: 'microphone-state',
	comment: 'Short label in the app layout voice state icons.',
});

interface Props {
	isSelfMuted: boolean;
	isSelfDeafened: boolean;
	isGuildMuted: boolean;
	isGuildDeafened: boolean;
	isPermissionMuted?: boolean;
	isCurrentUser?: boolean;
	isCameraOn?: boolean;
	isScreenSharing?: boolean;
	className?: string;
}

export const VoiceStateIcons = observer(
	({
		isSelfMuted,
		isSelfDeafened,
		isGuildMuted,
		isGuildDeafened,
		isPermissionMuted = false,
		isCurrentUser = false,
		isCameraOn,
		isScreenSharing,
		className,
	}: Props) => {
		const {i18n} = useLingui();
		const muteLabel = isGuildMuted
			? i18n._(VOICE_MUTED_BY_MODERATORS_DESCRIPTOR)
			: isPermissionMuted
				? getVoiceNoSpeakPermissionLabel(i18n, isCurrentUser)
				: i18n._(MUTED_DESCRIPTOR);
		const muteClassName = isGuildMuted || isPermissionMuted ? styles.iconGuildAction : styles.iconMuted;
		const deafenLabel = isGuildDeafened
			? getVoiceDeafenedByModeratorsStatusLabel(i18n, isCurrentUser)
			: getVoiceDeafenedStatusLabel(i18n, isCurrentUser);
		return (
			<div className={clsx(styles.container, className)} data-flx="app.voice-state-icons.container">
				{isCameraOn && (
					<Tooltip text={i18n._(CAMERA_ON_DESCRIPTOR)} data-flx="app.voice-state-icons.tooltip">
						<VideoCameraIcon
							weight="fill"
							className={clsx(styles.icon, styles.iconMuted)}
							data-flx="app.voice-state-icons.icon"
						/>
					</Tooltip>
				)}
				{(isGuildMuted || isPermissionMuted || isSelfMuted) && (
					<Tooltip text={muteLabel} data-flx="app.voice-state-icons.tooltip--2">
						<MicrophoneSlashIcon
							weight="fill"
							className={clsx(styles.icon, muteClassName)}
							data-flx="app.voice-state-icons.icon--2"
						/>
					</Tooltip>
				)}
				{(isGuildDeafened || isSelfDeafened) && (
					<Tooltip text={deafenLabel} data-flx="app.voice-state-icons.tooltip--3">
						<SpeakerSlashIcon
							weight="fill"
							className={clsx(styles.icon, isGuildDeafened ? styles.iconGuildAction : styles.iconMuted)}
							data-flx="app.voice-state-icons.icon--3"
						/>
					</Tooltip>
				)}
				{isScreenSharing && <LiveBadge data-flx="app.voice-state-icons.live-badge" />}
			</div>
		);
	},
);

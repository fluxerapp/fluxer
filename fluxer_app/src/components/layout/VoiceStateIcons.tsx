/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {MicrophoneSlashIcon, SpeakerSlashIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './VoiceStateIcons.module.css';

interface Props {
	isSelfMuted: boolean;
	isSelfDeafened: boolean;
	isGuildMuted: boolean;
	isGuildDeafened: boolean;
	isCameraOn?: boolean;
	isScreenSharing?: boolean;
	className?: string;
}

export const VoiceStateIcons = observer(
	({isSelfMuted, isSelfDeafened, isGuildMuted, isGuildDeafened, isCameraOn, isScreenSharing, className}: Props) => {
		const {t} = useLingui();
		return (
			<div className={clsx(styles.container, className)}>
				{isScreenSharing && (
					<Tooltip text={t`Screen Sharing`}>
						<span className={styles.liveBadge}>{t`Live`}</span>
					</Tooltip>
				)}
				{isCameraOn && (
					<Tooltip text={t`Camera On`}>
						<VideoCameraIcon weight="fill" className={clsx(styles.icon, styles.iconMuted)} />
					</Tooltip>
				)}
				{(isGuildMuted || isSelfMuted) && (
					<Tooltip text={isGuildMuted ? t`Community Muted` : t`Muted`}>
						<MicrophoneSlashIcon
							weight="fill"
							className={clsx(styles.icon, isGuildMuted ? styles.iconGuildAction : styles.iconMuted)}
						/>
					</Tooltip>
				)}
				{(isGuildDeafened || isSelfDeafened) && (
					<Tooltip text={isGuildDeafened ? t`Community Deafened` : t`Deafened`}>
						<SpeakerSlashIcon
							weight="fill"
							className={clsx(styles.icon, isGuildDeafened ? styles.iconGuildAction : styles.iconMuted)}
						/>
					</Tooltip>
				)}
			</div>
		);
	},
);

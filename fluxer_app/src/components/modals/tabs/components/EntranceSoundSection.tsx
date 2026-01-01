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

import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon, SpeakerHighIcon, TrashIcon, UploadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import {Button} from '~/components/uikit/Button/Button';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useEntranceSound} from '../hooks/useEntranceSound';
import styles from './EntranceSoundSection.module.css';

interface EntranceSoundSectionProps {
	hasPremium: boolean;
}

export const EntranceSoundSection: React.FC<EntranceSoundSectionProps> = observer(({hasPremium}) => {
	const {t} = useLingui();
	const {entranceSound, isPreviewing, remove, preview, openUploadDialog} = useEntranceSound(hasPremium);

	return (
		<div>
			<div className={styles.label}>
				<Trans>Entrance Sound</Trans>
			</div>
			{!hasPremium ? (
				<div className={styles.premiumCard}>
					<div className={styles.premiumCardHeader}>
						<CrownIcon weight="fill" size={18} className={styles.premiumCardIcon} />
						<span className={styles.premiumCardTitle}>
							<Trans>Custom Entrance Sounds with Plutonium</Trans>
						</span>
					</div>
					<p className={styles.premiumCardDescription}>
						<Trans>
							Upload a custom sound that plays automatically when you join a voice channel. Maximum duration: 5.2
							seconds. Supported formats: MP3, WAV, OGG, M4A, AAC, FLAC, Opus, WebM (max 2MB).
						</Trans>
					</p>
					<Button variant="secondary" small={true} onClick={() => PremiumModalActionCreators.open()}>
						<Trans>Get Plutonium</Trans>
					</Button>
				</div>
			) : (
				<div className={styles.content}>
					<div className={styles.hint}>
						<Trans>
							Upload a custom sound that plays when you join a voice channel (max 5.2 seconds, 2MB). Supported formats:
							MP3, WAV, OGG, M4A, AAC, FLAC, Opus, WebM.
						</Trans>
					</div>

					{entranceSound ? (
						<div className={styles.soundCard}>
							<div className={styles.soundCardContent}>
								<div className={styles.soundCardMain}>
									<Tooltip text={t`Preview sound`}>
										<button
											type="button"
											onClick={preview}
											disabled={isPreviewing}
											className={styles.previewButton}
											aria-label={t`Preview sound`}
										>
											<SpeakerHighIcon size={20} weight="fill" className={styles.previewIcon} />
										</button>
									</Tooltip>
									<div className={styles.soundInfo}>
										<span className={styles.soundFileName}>{entranceSound.fileName}</span>
										<span className={styles.soundDuration}>{entranceSound.duration.toFixed(1)}s</span>
									</div>
								</div>
								<Tooltip text={t`Remove entrance sound`}>
									<button
										type="button"
										onClick={remove}
										className={styles.deleteButton}
										aria-label={t`Remove entrance sound`}
									>
										<TrashIcon size={16} className={styles.deleteIcon} />
									</button>
								</Tooltip>
							</div>
						</div>
					) : (
						<Button variant="primary" fitContainer={false} className={styles.actionButton} onClick={openUploadDialog}>
							<div className={styles.uploadButtonContent}>
								<UploadIcon size={16} />
								<Trans>Upload Entrance Sound</Trans>
							</div>
						</Button>
					)}

					{entranceSound && (
						<Button variant="primary" fitContainer={false} className={styles.actionButton} onClick={openUploadDialog}>
							<div className={styles.uploadButtonContent}>
								<UploadIcon size={16} />
								<Trans>Replace Entrance Sound</Trans>
							</div>
						</Button>
					)}
				</div>
			)}
		</div>
	);
});

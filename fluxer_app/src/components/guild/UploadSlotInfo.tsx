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

import {Trans} from '@lingui/react/macro';
import {UploadIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Button} from '~/components/uikit/Button/Button';
import styles from './UploadSlotInfo.module.css';

interface UploadSlotInfoProps {
	title: React.ReactNode;
	currentCount: number;
	maxCount: number;
	description: React.ReactNode;
	uploadButtonText: React.ReactNode;
	onUploadClick: () => void;
	additionalSlots?: React.ReactNode;
}

export const UploadSlotInfo: React.FC<UploadSlotInfoProps> = observer(
	({title, currentCount, maxCount, description, uploadButtonText, onUploadClick, additionalSlots}) => {
		return (
			<div className={styles.container}>
				<div className={styles.header}>
					<div>
						<h3 className={styles.title}>{title}</h3>
						<div className={styles.stats}>
							{additionalSlots || (
								<span>
									<Trans>
										{currentCount} / {maxCount === Number.POSITIVE_INFINITY ? '∞' : maxCount}
									</Trans>
								</span>
							)}
						</div>
					</div>
					<div className={styles.uploadButtonDesktop}>
						<Button onClick={onUploadClick} leftIcon={<UploadIcon className={styles.icon} />}>
							{uploadButtonText}
						</Button>
					</div>
				</div>
				<p className={styles.description}>{description}</p>
				<div className={styles.uploadButtonMobile}>
					<Button onClick={onUploadClick} leftIcon={<UploadIcon className={styles.icon} />}>
						{uploadButtonText}
					</Button>
				</div>
			</div>
		);
	},
);

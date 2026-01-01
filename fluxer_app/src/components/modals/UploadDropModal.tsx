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
import {ArrowFatUpIcon, FileIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import type {ChannelRecord} from '~/records/ChannelRecord';
import * as ChannelUtils from '~/utils/ChannelUtils';
import styles from './UploadDropModal.module.css';

interface UploadDropModalProps {
	channel: ChannelRecord;
	isShiftHeld: boolean;
	isSlowmodeActive: boolean;
}

export const UploadDropModal = ({channel, isShiftHeld, isSlowmodeActive}: UploadDropModalProps) => {
	const {t} = useLingui();
	const displayName = channel.isPrivate()
		? ChannelUtils.getDMDisplayName(channel)
		: channel.name
			? `#${channel.name}`
			: t`Unknown Channel`;

	return (
		<motion.div
			className={styles.overlay}
			initial={{opacity: 0}}
			animate={{opacity: 1}}
			exit={{opacity: 0}}
			transition={{type: 'spring', damping: 30, stiffness: 400, mass: 0.8}}
		>
			<motion.div
				className={styles.dialog}
				initial={{scale: 0.9, opacity: 0}}
				animate={{scale: 1, opacity: 1}}
				exit={{scale: 0.9, opacity: 0}}
				transition={{type: 'spring', damping: 25, stiffness: 300, mass: 0.8}}
			>
				<div className={styles.dialogIconCircle}>
					<FileIcon className={styles.dialogIcon} weight="fill" />
				</div>

				<div className={styles.dialogTextBlock}>
					<motion.h2
						className={styles.dialogTitle}
						key={isShiftHeld ? 'direct' : 'normal'}
						initial={{opacity: 0, y: -10}}
						animate={{opacity: 1, y: 0}}
						transition={{type: 'spring', damping: 20, stiffness: 300}}
					>
						{isShiftHeld ? t`Upload directly to ${displayName}` : t`Upload to ${displayName}`}
					</motion.h2>
					<motion.p
						className={styles.dialogDescription}
						key={isShiftHeld ? 'direct-desc' : 'normal-desc'}
						initial={{opacity: 0, y: 10}}
						animate={{opacity: 1, y: 0}}
						transition={{type: 'spring', damping: 20, stiffness: 300, delay: 0.05}}
					>
						{isShiftHeld
							? t`Files will be sent immediately without preview.`
							: isSlowmodeActive
								? t`You can add comments before uploading. Direct upload is disabled during slowmode.`
								: t`You can add comments before uploading. Hold shift to upload directly.`}
					</motion.p>
				</div>

				<div
					className={clsx(styles.statusBanner, isShiftHeld ? styles.statusBannerActive : styles.statusBannerDefault)}
				>
					<motion.div
						className={clsx(
							styles.statusIndicator,
							isShiftHeld ? styles.statusIndicatorActive : styles.statusIndicatorDefault,
						)}
						animate={{scale: isShiftHeld ? 1.1 : 1}}
						transition={{type: 'spring', damping: 20, stiffness: 400}}
					>
						<motion.div
							animate={{
								rotate: isShiftHeld ? [0, -5, 5, 0] : 0,
							}}
							transition={{
								duration: isShiftHeld ? 0.3 : 0.1,
								ease: 'easeInOut',
							}}
						>
							<ArrowFatUpIcon className={styles.statusIcon} weight="fill" />
						</motion.div>
					</motion.div>
					<motion.span
						key={isShiftHeld ? 'active' : 'hold'}
						initial={{opacity: 0, x: -10}}
						animate={{opacity: 1, x: 0}}
						transition={{type: 'spring', damping: 25, stiffness: 400}}
					>
						{isShiftHeld ? t`Direct upload active` : t`Hold for instant upload`}
					</motion.span>
				</div>
			</motion.div>
		</motion.div>
	);
};

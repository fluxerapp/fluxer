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

import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';
import type {ToastPropsExtended} from '~/components/uikit/Toast';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import styles from './Toast.module.css';

const MINIMUM_TIMEOUT = 1500;

export const Toast = observer(
	({id, type, children, timeout = MINIMUM_TIMEOUT, onClick, onTimeout, onClose, closeToast}: ToastPropsExtended) => {
		const isMobileExperience = isMobileExperienceEnabled();

		useEffect(() => {
			const finalTimeout = Math.max(timeout, MINIMUM_TIMEOUT);
			const timer = setTimeout(() => {
				if (onTimeout) onTimeout();
				else closeToast(id);
			}, finalTimeout);
			return () => clearTimeout(timer);
		}, [timeout, onTimeout, closeToast, id]);

		useEffect(() => {
			return () => {
				if (onClose) onClose();
			};
		}, [onClose]);

		const handleClick = useCallback(
			(event: React.MouseEvent) => {
				if (onClick) onClick(event);
				else closeToast(id);
			},
			[onClick, closeToast, id],
		);

		return (
			<motion.div
				onClick={handleClick}
				className={`${styles.toast} ${isMobileExperience ? styles.toastMobile : styles.toastDesktop}`}
				initial={{opacity: 0, y: -30}}
				animate={{opacity: 1, y: 0}}
				exit={{opacity: 0, y: -30}}
				transition={{
					duration: 0.2,
					ease: 'easeOut',
				}}
			>
				{type === 'success' ? (
					<CheckIcon
						weight="bold"
						className={`${styles.icon} ${styles.iconSuccess} ${isMobileExperience ? styles.iconMobile : styles.iconDesktop}`}
					/>
				) : type === 'error' ? (
					<XIcon
						weight="bold"
						className={`${styles.icon} ${styles.iconError} ${isMobileExperience ? styles.iconMobile : styles.iconDesktop}`}
					/>
				) : null}
				<span className={`${styles.text} ${isMobileExperience ? styles.textMobile : styles.textDesktop}`}>
					{children}
				</span>
			</motion.div>
		);
	},
);

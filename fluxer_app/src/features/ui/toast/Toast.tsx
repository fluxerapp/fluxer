// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {ToastPropsExtended} from '@app/features/ui/toast';
import styles from '@app/features/ui/toast/Toast.module.css';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {getReducedMotionProps} from '@app/features/ui/utils/ReducedMotionAnimation';
import {CheckIcon, XIcon} from '@phosphor-icons/react';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';

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
		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent) => {
				if (!isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				if (onClick) onClick(event);
				else closeToast(id);
			},
			[onClick, closeToast, id],
		);
		const toastMotion = getReducedMotionProps(
			{
				initial: {opacity: 0, y: -30},
				animate: {opacity: 1, y: 0},
				exit: {opacity: 0, y: -30},
				transition: {duration: 0.2, ease: 'easeOut'},
			},
			Accessibility.useReducedMotion,
		);
		return (
			<motion.div
				onClick={handleClick}
				onKeyDown={handleKeyDown}
				className={`${styles.toast} ${isMobileExperience ? styles.toastMobile : styles.toastDesktop}`}
				role={type === 'error' ? 'alert' : 'status'}
				aria-live={type === 'error' ? 'assertive' : 'polite'}
				aria-atomic="true"
				tabIndex={0}
				data-flx="ui.toast.toast.toast.click"
				{...toastMotion}
			>
				{type === 'success' ? (
					<CheckIcon
						weight="bold"
						className={`${styles.icon} ${styles.iconSuccess} ${isMobileExperience ? styles.iconMobile : styles.iconDesktop}`}
						data-flx="ui.toast.toast.icon"
					/>
				) : null}
				{type === 'error' ? (
					<XIcon
						weight="bold"
						className={`${styles.icon} ${styles.iconError} ${isMobileExperience ? styles.iconMobile : styles.iconDesktop}`}
						data-flx="ui.toast.toast.icon--2"
					/>
				) : null}
				<span
					className={`${styles.text} ${isMobileExperience ? styles.textMobile : styles.textDesktop}`}
					data-flx="ui.toast.toast.text"
				>
					{children}
				</span>
			</motion.div>
		);
	},
);

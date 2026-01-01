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
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {NativeDragRegion} from '~/components/layout/NativeDragRegion';
import {Button} from '~/components/uikit/Button/Button';
import AccessibilityStore from '~/stores/AccessibilityStore';
import {settingsModalStyles} from '../shared/SettingsModalLayout';
import styles from './SettingsModalHeader.module.css';

interface SettingsModalHeaderProps {
	title: string;
	showUnsavedBanner: boolean;
	flashBanner: boolean;
	tabData: {
		onReset?: () => void;
		onSave?: () => void;
		isSubmitting?: boolean;
	};
	onClose: () => void;
}

export const SettingsModalHeader: React.FC<SettingsModalHeaderProps> = observer(
	({title, showUnsavedBanner, flashBanner, tabData, onClose}) => {
		const {t} = useLingui();
		const prefersReducedMotion = AccessibilityStore.useReducedMotion;

		return (
			<NativeDragRegion
				className={`${settingsModalStyles.desktopHeader} ${styles.headerTransition}`}
				style={{
					transitionDuration: prefersReducedMotion ? '0ms' : '200ms',
					backgroundColor:
						showUnsavedBanner && flashBanner
							? 'var(--status-danger)'
							: showUnsavedBanner
								? 'var(--background-primary)'
								: undefined,
				}}
			>
				<AnimatePresence mode="wait">
					{showUnsavedBanner ? (
						<motion.div
							key="banner"
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : {duration: 0.25, ease: 'easeOut'}}
							className={styles.bannerContent}
						>
							<div className={styles.bannerTextContainer}>
								<div
									className={clsx(styles.bannerText, flashBanner ? styles.bannerTextFlash : styles.bannerTextNormal)}
								>
									<Trans>Careful! You have unsaved changes.</Trans>
								</div>
							</div>
							<div className={styles.bannerActions}>
								<Button variant="secondary" small={true} onClick={tabData.onReset}>
									<Trans>Reset</Trans>
								</Button>
								<Button small={true} onClick={tabData.onSave} submitting={tabData.isSubmitting}>
									<Trans>Save Changes</Trans>
								</Button>
							</div>
						</motion.div>
					) : (
						<motion.div
							key="title"
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : {duration: 0.25, ease: 'easeOut'}}
							className={styles.titleContent}
						>
							<h1 className={styles.title}>{title}</h1>
							<button type="button" aria-label={t`Close`} onClick={onClose} className={settingsModalStyles.closeButton}>
								<XIcon weight="regular" className={styles.icon} />
							</button>
						</motion.div>
					)}
				</AnimatePresence>
			</NativeDragRegion>
		);
	},
);

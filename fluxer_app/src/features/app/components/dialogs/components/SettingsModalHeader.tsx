// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/app/components/dialogs/components/SettingsModalHeader.module.css';
import {SettingsHeadingLinkButton} from '@app/features/app/components/dialogs/shared/SettingsHeadingLinkButton';
import {settingsModalStyles} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import {UnsavedChangesBannerContent} from '@app/features/app/components/dialogs/shared/UnsavedChangesBannerContent';
import {NativeDragRegion} from '@app/features/app/components/layout/NativeDragRegion';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {TabData} from '@app/features/ui/state/UnsavedChanges';
import {useToastAnchor} from '@app/features/ui/toast/ToastAnchor';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

interface SettingsModalHeaderProps {
	title: React.ReactNode;
	pageLinkHref?: string | null;
	showUnsavedBanner: boolean;
	flashBanner: boolean;
	tabData: TabData;
	onClose: () => void;
}

export const SettingsModalHeader: React.FC<SettingsModalHeaderProps> = observer(
	({title, pageLinkHref, showUnsavedBanner, flashBanner, tabData, onClose}) => {
		const {i18n} = useLingui();
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const headerRef = useRef<HTMLElement>(null);
		useToastAnchor(headerRef);
		let backgroundColor: string | undefined;
		if (showUnsavedBanner) {
			backgroundColor = flashBanner ? 'var(--status-danger)' : 'var(--background-primary)';
		}
		return (
			<NativeDragRegion
				ref={headerRef}
				className={`${settingsModalStyles.desktopHeader} ${styles.headerTransition}`}
				style={{
					transitionDuration: prefersReducedMotion ? '0ms' : '200ms',
					backgroundColor,
				}}
				data-flx="app.settings-modal-header.header-transition"
			>
				<AnimatePresence mode="wait" data-flx="app.settings-modal-header.animate-presence">
					{showUnsavedBanner ? (
						<motion.div
							key="banner"
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : {duration: 0.25, ease: 'easeOut'}}
							className={styles.bannerContent}
							data-flx="app.settings-modal-header.banner-content"
						>
							<UnsavedChangesBannerContent
								tabData={tabData}
								textContainerClassName={styles.bannerTextContainer}
								textClassName={clsx(styles.bannerText, flashBanner ? styles.bannerTextFlash : styles.bannerTextNormal)}
								actionsClassName={styles.bannerActions}
								dataFlx={{
									textContainer: 'app.settings-modal-header.banner-text-container',
									text: 'app.settings-modal-header.banner-text',
									actions: 'app.settings-modal-header.banner-actions',
									resetButton: 'app.settings-modal-header.button.reset',
									saveButton: 'app.settings-modal-header.button.save',
								}}
								data-flx="app.settings-modal-header.unsaved-changes-banner-content"
							/>
						</motion.div>
					) : (
						<motion.div
							key="title"
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : {duration: 0.25, ease: 'easeOut'}}
							className={styles.titleContent}
							data-flx="app.settings-modal-header.title-content"
						>
							<div className={styles.titleWrapper} data-flx="app.settings-modal-header.title-wrapper">
								<h1 className={styles.title} data-flx="app.settings-modal-header.title">
									{title}
								</h1>
								{pageLinkHref ? (
									<SettingsHeadingLinkButton
										href={pageLinkHref}
										target="page"
										data-flx="app.settings-modal-header.heading-link-button"
									/>
								) : null}
							</div>
							<button
								type="button"
								aria-label={i18n._(CLOSE_DESCRIPTOR)}
								onClick={onClose}
								className={settingsModalStyles.closeButton}
								data-flx="app.settings-modal-header.button.close"
							>
								<XIcon weight="bold" className={styles.icon} data-flx="app.settings-modal-header.icon" />
							</button>
						</motion.div>
					)}
				</AnimatePresence>
			</NativeDragRegion>
		);
	},
);

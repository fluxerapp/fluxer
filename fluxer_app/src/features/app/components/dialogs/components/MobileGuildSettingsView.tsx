// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/app/components/dialogs/components/MobileGuildSettingsView.module.css';
import {
	MobileHeader,
	MobileHeaderWithBanner,
	MobileSettingsDangerItem,
	MobileSettingsList,
} from '@app/features/app/components/dialogs/shared/MobileSettingsComponents';
import Authentication from '@app/features/auth/state/Authentication';
import {GuildDeleteModal} from '@app/features/guild/components/modals/GuildDeleteModal';
import type {Guild} from '@app/features/guild/models/Guild';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import userSettingsStyles from '@app/features/user/components/modals/UserSettingsModal.module.css';
import type {
	GuildSettingsTab,
	GuildSettingsTabType,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {getGuildSettingsCategoryLabel} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import type {MobileNavigationState} from '@app/features/user/hooks/useMobileNavigation';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type UIEvent, useCallback, useEffect, useRef} from 'react';

const DELETE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Delete community',
	comment: 'Short label in the settings dialog mobile guild settings view. Keep the tone plain and specific.',
});

interface MobileGuildSettingsViewProps {
	guild: Guild;
	groupedSettingsTabs: Record<string, Array<GuildSettingsTab>>;
	currentTab?: GuildSettingsTab;
	mobileNav: MobileNavigationState<GuildSettingsTabType>;
	onBack: () => void;
	onTabSelect: (tabType: string, title: string) => void;
}

const contentFadeVariants = {
	enter: {
		opacity: 0,
	},
	center: {
		opacity: 1,
	},
	exit: {
		opacity: 0,
	},
};
const headerFadeVariants = {
	enter: {
		opacity: 0,
	},
	center: {
		opacity: 1,
	},
	exit: {
		opacity: 0,
	},
};
export const MobileGuildSettingsView: React.FC<MobileGuildSettingsViewProps> = observer(
	({guild, groupedSettingsTabs, currentTab, mobileNav, onBack, onTabSelect}) => {
		const {i18n} = useLingui();
		const reducedMotion = Accessibility.useReducedMotion;
		const currentTabId = mobileNav.currentView?.tab;
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(currentTabId);
		const CATEGORY_LABELS = {
			expressions: getGuildSettingsCategoryLabel(i18n, 'expressions'),
			community: getGuildSettingsCategoryLabel(i18n, 'community'),
			integrations: getGuildSettingsCategoryLabel(i18n, 'integrations'),
			user_management: getGuildSettingsCategoryLabel(i18n, 'user_management'),
		};
		const handleDeleteGuild = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<GuildDeleteModal
						guildId={guild.id}
						data-flx="app.mobile-guild-settings-view.handle-delete-guild.guild-delete-modal"
					/>
				)),
			);
		}, [guild.id]);
		const handleBack = useCallback(() => {
			if (checkUnsavedChanges()) return;
			onBack();
		}, [checkUnsavedChanges, onBack]);
		const handleTabSelect = useCallback(
			(tabType: string, title: string) => {
				if (checkUnsavedChanges()) return;
				onTabSelect(tabType, title);
			},
			[checkUnsavedChanges, onTabSelect],
		);
		const showMobileList = mobileNav.isRootView;
		const showMobileContent = !mobileNav.isRootView;
		const listScrollPositionRef = useRef(0);
		const listScrollerRef = useRef<ScrollerHandle | null>(null);
		const handleListScroll = useCallback((event: UIEvent<HTMLDivElement>) => {
			listScrollPositionRef.current = event.currentTarget.scrollTop;
		}, []);
		useEffect(() => {
			if (!showMobileList) return;
			const scroller = listScrollerRef.current;
			if (!scroller) return;
			const target = listScrollPositionRef.current;
			if (target === 0) return;
			scroller.scrollTo({to: target, animate: false});
		}, [showMobileList]);
		const dangerAction =
			guild.isOwner(Authentication.currentUserId) && !isStockCommunityGuild(guild.id) ? (
				<MobileSettingsDangerItem
					icon={TrashIcon}
					label={i18n._(DELETE_COMMUNITY_DESCRIPTOR)}
					onClick={handleDeleteGuild}
					data-flx="app.mobile-guild-settings-view.mobile-settings-danger-item.delete-guild"
				/>
			) : null;
		return (
			<div className={userSettingsStyles.mobileWrapper} data-flx="app.mobile-guild-settings-view.div">
				<div className={userSettingsStyles.mobileHeaderContainer} data-flx="app.mobile-guild-settings-view.div--2">
					<AnimatePresence
						mode="wait"
						custom={mobileNav.direction}
						data-flx="app.mobile-guild-settings-view.animate-presence"
					>
						{showMobileList && (
							<motion.div
								key="mobile-list-header"
								variants={reducedMotion ? undefined : headerFadeVariants}
								initial="center"
								animate="center"
								exit={reducedMotion ? 'center' : 'exit'}
								transition={{duration: reducedMotion ? 0 : 0.08, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileHeaderContent}
								data-flx="app.mobile-guild-settings-view.div--3"
							>
								<MobileHeader
									title={guild.name}
									onBack={handleBack}
									data-flx="app.mobile-guild-settings-view.mobile-header"
								/>
							</motion.div>
						)}
						{showMobileContent && currentTab && (
							<motion.div
								key={`mobile-content-header-${mobileNav.currentView?.tab}`}
								variants={reducedMotion ? undefined : headerFadeVariants}
								initial={reducedMotion ? 'center' : 'enter'}
								animate="center"
								exit={reducedMotion ? 'center' : 'exit'}
								transition={{duration: reducedMotion ? 0 : 0.08, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileHeaderContent}
								data-flx="app.mobile-guild-settings-view.div--4"
							>
								<MobileHeaderWithBanner
									title={currentTab.label || mobileNav.currentView?.title}
									onBack={handleBack}
									showUnsavedBanner={showUnsavedBanner}
									flashBanner={flashBanner}
									tabData={tabData}
									data-flx="app.mobile-guild-settings-view.mobile-header-with-banner"
								/>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
				<div className={userSettingsStyles.mobileContentContainer} data-flx="app.mobile-guild-settings-view.div--5">
					<AnimatePresence
						mode="wait"
						custom={mobileNav.direction}
						data-flx="app.mobile-guild-settings-view.animate-presence--2"
					>
						{showMobileList && (
							<motion.div
								key="mobile-list-content"
								custom={mobileNav.direction}
								variants={reducedMotion ? undefined : contentFadeVariants}
								initial="center"
								animate="center"
								exit={reducedMotion ? 'center' : 'exit'}
								transition={{duration: reducedMotion ? 0 : 0.15, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileContentPane}
								data-flx="app.mobile-guild-settings-view.div--6"
							>
								<MobileSettingsList
									groupedTabs={groupedSettingsTabs}
									onTabSelect={handleTabSelect}
									categoryLabels={CATEGORY_LABELS}
									hiddenCategories={['guild_settings']}
									dangerContent={dangerAction}
									scrollRef={listScrollerRef}
									onScroll={handleListScroll}
									data-flx="app.mobile-guild-settings-view.mobile-settings-list"
								/>
							</motion.div>
						)}
						{showMobileContent && currentTab && (
							<motion.div
								key={`mobile-content-${mobileNav.currentView?.tab}`}
								custom={mobileNav.direction}
								variants={reducedMotion ? undefined : contentFadeVariants}
								initial={reducedMotion ? 'center' : 'enter'}
								animate="center"
								exit={reducedMotion ? 'center' : 'exit'}
								transition={{duration: reducedMotion ? 0 : 0.15, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileContentPane}
								data-flx="app.mobile-guild-settings-view.div--7"
							>
								<Scroller
									className={styles.scrollerFlex}
									key="mobile-guild-settings-content-scroller"
									data-flx="app.mobile-guild-settings-view.scroller-flex"
								>
									<div className={styles.contentContainer} data-flx="app.mobile-guild-settings-view.content-container">
										<currentTab.component
											guildId={guild.id}
											data-flx="app.mobile-guild-settings-view.current-tab-component"
										/>
									</div>
								</Scroller>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
			</div>
		);
	},
);

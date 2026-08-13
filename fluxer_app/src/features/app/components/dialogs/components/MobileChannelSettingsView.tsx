// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {
	MobileHeader,
	MobileHeaderWithBanner,
	MobileSettingsDangerItem,
	MobileSettingsList,
} from '@app/features/app/components/dialogs/shared/MobileSettingsComponents';
import {resolveSettingsTitle} from '@app/features/app/components/dialogs/shared/SettingsContentPresentation';
import {usePreservedScrollerPosition} from '@app/features/app/components/dialogs/shared/UsePreservedScrollerPosition';
import {ChannelDeleteModal} from '@app/features/channel/components/modals/ChannelDeleteModal';
import type {Channel} from '@app/features/channel/models/Channel';
import {DELETE_CATEGORY_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import channelStyles from '@app/features/guild/components/modals/GuildSettingsModal.module.css';
import Permission from '@app/features/permissions/state/Permission';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import styles from '@app/features/user/components/modals/UserSettingsModal.module.css';
import type {
	ChannelSettingsTab,
	ChannelSettingsTabType,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import type {MobileNavigationState} from '@app/features/user/hooks/useMobileNavigation';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const DELETE_CHANNEL_DESCRIPTOR = msg({
	message: 'Delete channel',
	comment: 'Short label in the settings dialog mobile channel settings view. Keep the tone plain and specific.',
});

interface MobileChannelSettingsViewProps {
	channel: Channel;
	groupedSettingsTabs: Record<string, Array<ChannelSettingsTab>>;
	currentTab?: ChannelSettingsTab;
	mobileNav: MobileNavigationState<ChannelSettingsTabType>;
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
export const MobileChannelSettingsView: React.FC<MobileChannelSettingsViewProps> = observer(
	({channel, groupedSettingsTabs, currentTab, mobileNav, onBack, onTabSelect}) => {
		const {i18n} = useLingui();
		const reducedMotion = Accessibility.useReducedMotion;
		const currentTabId = mobileNav.currentView?.tab;
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(currentTabId);
		const handleDeleteChannel = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<ChannelDeleteModal
						channelId={channel.id}
						data-flx="app.mobile-channel-settings-view.handle-delete-channel.channel-delete-modal"
					/>
				)),
			);
		}, [channel.id]);
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
		const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
		const canManageChannel = Permission.can(Permissions.MANAGE_CHANNELS, {
			channelId: channel.id,
			guildId: channel.guildId,
		});
		const showMobileList = mobileNav.isRootView;
		const showMobileContent = !mobileNav.isRootView;
		const {scrollerRef: listScrollerRef, handleScroll: handleListScroll} = usePreservedScrollerPosition(showMobileList);
		const currentTabLabel = currentTab ? currentTab.label : null;
		const currentViewTitle = mobileNav.currentView ? mobileNav.currentView.title : null;
		const mobileTitle = resolveSettingsTitle(currentTabLabel, currentViewTitle);
		const dangerAction = canManageChannel ? (
			<MobileSettingsDangerItem
				icon={TrashIcon}
				label={isCategory ? i18n._(DELETE_CATEGORY_DESCRIPTOR) : i18n._(DELETE_CHANNEL_DESCRIPTOR)}
				onClick={handleDeleteChannel}
				data-flx="app.mobile-channel-settings-view.mobile-settings-danger-item.delete-channel"
			/>
		) : null;
		return (
			<div className={styles.mobileWrapper} data-flx="app.mobile-channel-settings-view.mobile-wrapper">
				<div
					className={styles.mobileHeaderContainer}
					data-flx="app.mobile-channel-settings-view.mobile-header-container"
				>
					<AnimatePresence
						mode="wait"
						custom={mobileNav.direction}
						data-flx="app.mobile-channel-settings-view.animate-presence"
					>
						{showMobileList && (
							<motion.div
								key="mobile-list-header"
								variants={reducedMotion ? undefined : headerFadeVariants}
								initial="center"
								animate="center"
								exit={reducedMotion ? 'center' : 'exit'}
								transition={{duration: reducedMotion ? 0 : 0.08, ease: 'easeInOut'}}
								className={styles.mobileHeaderContent}
								data-flx="app.mobile-channel-settings-view.mobile-header-content"
							>
								<MobileHeader
									title={
										<span
											className={channelStyles.channelNameWithIcon}
											data-flx="app.mobile-channel-settings-view.channel-name-with-icon"
										>
											{ChannelUtils.getIcon(channel, {className: channelStyles.channelNameIcon, weight: 'bold'})}
											<span
												className={channelStyles.channelNameText}
												data-flx="app.mobile-channel-settings-view.channel-name"
											>
												{channel.name}
											</span>
										</span>
									}
									onBack={handleBack}
									data-flx="app.mobile-channel-settings-view.mobile-header"
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
								className={styles.mobileHeaderContent}
								data-flx="app.mobile-channel-settings-view.mobile-header-content--2"
							>
								<MobileHeaderWithBanner
									pageLinkHref={null}
									title={mobileTitle}
									onBack={handleBack}
									showUnsavedBanner={showUnsavedBanner}
									flashBanner={flashBanner}
									tabData={tabData}
									data-flx="app.mobile-channel-settings-view.mobile-header-with-banner"
								/>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
				<div
					className={styles.mobileContentContainer}
					data-flx="app.mobile-channel-settings-view.mobile-content-container"
				>
					<AnimatePresence
						mode="wait"
						custom={mobileNav.direction}
						data-flx="app.mobile-channel-settings-view.animate-presence--2"
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
								className={styles.mobileContentPane}
								data-flx="app.mobile-channel-settings-view.mobile-content-pane"
							>
								<MobileSettingsList
									groupedTabs={groupedSettingsTabs}
									onTabSelect={handleTabSelect}
									hiddenCategories={['channel_settings']}
									dangerContent={dangerAction}
									scrollRef={listScrollerRef}
									onScroll={handleListScroll}
									data-flx="app.mobile-channel-settings-view.mobile-settings-list"
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
								className={styles.mobileContentPane}
								data-flx="app.mobile-channel-settings-view.mobile-content-pane--2"
							>
								<Scroller
									className={styles.mobileContentScroller}
									key="mobile-channel-settings-content-scroller"
									data-flx="app.mobile-channel-settings-view.mobile-content-scroller"
								>
									<div
										className={styles.mobileContentInner}
										data-flx="app.mobile-channel-settings-view.mobile-content-inner"
									>
										<currentTab.component
											channelId={channel.id}
											data-flx="app.mobile-channel-settings-view.current-tab-component"
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

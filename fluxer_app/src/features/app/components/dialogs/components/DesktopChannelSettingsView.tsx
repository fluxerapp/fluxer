// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SettingsModalHeader} from '@app/features/app/components/dialogs/components/SettingsModalHeader';
import {resolveSettingsTitle} from '@app/features/app/components/dialogs/shared/SettingsContentPresentation';
import {
	SettingsModalDesktopContent,
	SettingsModalDesktopScroll,
	SettingsModalDesktopSidebar,
	SettingsModalSidebarCategory,
	SettingsModalSidebarCategoryTitle,
	SettingsModalSidebarFooter,
	SettingsModalSidebarItem,
	SettingsModalSidebarNav,
} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import {ChannelDeleteModal} from '@app/features/channel/components/modals/ChannelDeleteModal';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import styles from '@app/features/guild/components/modals/GuildSettingsModal.module.css';
import {openMessageHistoryThresholdSettings} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {BACK_TO_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import type {
	ChannelSettingsTab,
	ChannelSettingsTabType,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {
	CATEGORY_SETTINGS_LABEL_DESCRIPTOR,
	CHANNEL_SETTINGS_LABEL_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/ChannelSettingsConstants';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, TrashIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef} from 'react';

const BACK_TO_OVERRIDES_DESCRIPTOR = msg({
	message: 'Back to overrides',
	comment: 'Back-navigation label that returns from permission override editing to the overrides list.',
});
const DELETE_CATEGORY_DESCRIPTOR = msg({
	message: 'Delete category',
	comment: 'Destructive footer button in category settings. Opens the delete-category confirmation.',
});
const DELETE_CHANNEL_DESCRIPTOR = msg({
	message: 'Delete channel',
	comment: 'Destructive footer button in channel settings. Opens the delete-channel confirmation.',
});
const MESSAGE_HISTORY_THRESHOLD_DESCRIPTOR = msg({
	message: 'Message history threshold',
	comment: 'Channel settings tab label for configuring how much message history new members can see.',
});

interface DesktopChannelSettingsViewProps {
	channel: Channel;
	groupedSettingsTabs: Record<string, Array<ChannelSettingsTab>>;
	currentTab?: ChannelSettingsTab;
	selectedTab: ChannelSettingsTabType;
	onTabSelect: (tab: ChannelSettingsTabType) => void;
}

const CATEGORY_LABELS = {
	channel_settings: '',
};
export const DesktopChannelSettingsView: React.FC<DesktopChannelSettingsViewProps> = observer(
	({channel, groupedSettingsTabs, currentTab, selectedTab, onTabSelect}) => {
		const {i18n} = useLingui();
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(selectedTab);
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const contentRef = useRef<HTMLDivElement>(null);
		const focusContentPanel = useCallback(() => {
			contentRef.current?.focus();
		}, []);
		const channelPermissionsOverrideOwnerId = useMemo(() => `channel-permissions-${channel.id}`, [channel.id]);
		const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId: channel.guildId ?? ''});
		const canManageChannel = Permission.can(Permissions.MANAGE_CHANNELS, {
			channelId: channel.id,
			guildId: channel.guildId,
		});
		const handleTabSelect = useCallback(
			(tabType: ChannelSettingsTabType) => {
				if (checkUnsavedChanges()) return;
				if (
					tabType === 'permissions' &&
					SettingsSidebar.ownerId === channelPermissionsOverrideOwnerId &&
					SettingsSidebar.isDismissed(channelPermissionsOverrideOwnerId)
				) {
					SettingsSidebar.activateOverride(channelPermissionsOverrideOwnerId);
				}
				onTabSelect(tabType);
			},
			[checkUnsavedChanges, onTabSelect, channelPermissionsOverrideOwnerId],
		);
		const handleDeleteChannel = useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalCommands.push(
				modal(() => (
					<ChannelDeleteModal
						channelId={channel.id}
						data-flx="app.desktop-channel-settings-view.handle-delete-channel.channel-delete-modal"
					/>
				)),
			);
		}, [channel.id, checkUnsavedChanges]);
		const handleClose = useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalCommands.pop();
		}, [checkUnsavedChanges]);
		const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
		const useOverride = SettingsSidebar.useOverride;
		const activeTabPanelId = selectedTab ? `channel-settings-tabpanel-${selectedTab}` : undefined;
		const activeTabId = selectedTab ? `channel-settings-tab-${selectedTab}` : undefined;
		const fallbackSettingsTitle = i18n._(
			isCategory ? CATEGORY_SETTINGS_LABEL_DESCRIPTOR : CHANNEL_SETTINGS_LABEL_DESCRIPTOR,
		);
		const currentTabLabel = currentTab ? currentTab.label : null;
		const settingsTitle = resolveSettingsTitle(currentTabLabel, fallbackSettingsTitle);
		const scrollKey = useMemo(
			() => `channel-settings-${channel.id}-${selectedTab ?? 'none'}`,
			[channel.id, selectedTab],
		);
		return (
			<>
				<SettingsModalDesktopSidebar data-flx="app.desktop-channel-settings-view.settings-modal-desktop-sidebar">
					<div className={styles.sidebarHeader} data-flx="app.desktop-channel-settings-view.sidebar-header">
						<div className={styles.guildName} data-flx="app.desktop-channel-settings-view.guild-name">
							<span
								className={styles.channelNameWithIcon}
								data-flx="app.desktop-channel-settings-view.channel-name-with-icon"
							>
								{ChannelUtils.getIcon(channel, {className: styles.channelNameIcon, weight: 'bold'})}
								<span className={styles.channelNameText} data-flx="app.desktop-channel-settings-view.channel-name">
									{channel.name}
								</span>
							</span>
						</div>
					</div>
					<SettingsModalSidebarNav data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-nav">
						<AnimatePresence mode="wait" initial={false} data-flx="app.desktop-channel-settings-view.animate-presence">
							{SettingsSidebar.hasOverride && useOverride ? (
								<motion.div
									key="custom"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
									data-flx="app.desktop-channel-settings-view.div"
								>
									<div
										className={styles.sidebarButtonWrapper}
										data-flx="app.desktop-channel-settings-view.sidebar-button-wrapper"
									>
										<Button
											variant="secondary"
											leftIcon={
												<ArrowLeftIcon
													className={styles.sidebarButtonIcon}
													data-flx="app.desktop-channel-settings-view.sidebar-button-icon"
												/>
											}
											onClick={() => SettingsSidebar.dismissOverride()}
											data-flx="app.desktop-channel-settings-view.button.dismiss-override"
										>
											{i18n._(BACK_TO_SETTINGS_DESCRIPTOR)}
										</Button>
									</div>
									{SettingsSidebar.overrideContent}
								</motion.div>
							) : (
								<motion.div
									key="global"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
									data-flx="app.desktop-channel-settings-view.div--2"
								>
									{SettingsSidebar.hasOverride &&
										SettingsSidebar.ownerId === channelPermissionsOverrideOwnerId &&
										!SettingsSidebar.isDismissed(channelPermissionsOverrideOwnerId) && (
											<div
												className={styles.sidebarButtonWrapper}
												data-flx="app.desktop-channel-settings-view.sidebar-button-wrapper--2"
											>
												<Button
													variant="secondary"
													rightIcon={
														<ArrowRightIcon
															className={styles.sidebarButtonIcon}
															data-flx="app.desktop-channel-settings-view.sidebar-button-icon--2"
														/>
													}
													onClick={() => SettingsSidebar.activateOverride(channelPermissionsOverrideOwnerId)}
													data-flx="app.desktop-channel-settings-view.button.activate-override"
												>
													{i18n._(BACK_TO_OVERRIDES_DESCRIPTOR)}
												</Button>
											</div>
										)}
									{Object.entries(groupedSettingsTabs).map(([category, tabs]) => (
										<SettingsModalSidebarCategory
											key={category}
											data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-category"
										>
											{category !== 'channel_settings' && (
												<SettingsModalSidebarCategoryTitle data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-category-title">
													{CATEGORY_LABELS[category as keyof typeof CATEGORY_LABELS]}
												</SettingsModalSidebarCategoryTitle>
											)}
											{tabs.map((tab) => {
												const tabId = `channel-settings-tab-${tab.type}`;
												const panelId = `channel-settings-tabpanel-${tab.type}`;
												const requiresExplicitKeyboardActivation = tab.type === 'permissions';
												return (
													<SettingsModalSidebarItem
														key={tab.type}
														icon={tab.icon}
														label={tab.label}
														selected={tab.type === selectedTab}
														autoSelectOnKeyboardNavigation={!requiresExplicitKeyboardActivation}
														onClick={() => handleTabSelect(tab.type)}
														onRequestContentFocus={focusContentPanel}
														id={tabId}
														controlsId={panelId}
														data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-item.tab-select"
													/>
												);
											})}
										</SettingsModalSidebarCategory>
									))}
									{!useOverride && canManageChannel && (
										<SettingsModalSidebarItem
											icon={TrashIcon}
											label={isCategory ? i18n._(DELETE_CATEGORY_DESCRIPTOR) : i18n._(DELETE_CHANNEL_DESCRIPTOR)}
											danger={true}
											onClick={handleDeleteChannel}
											data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-item.delete-channel"
										/>
									)}
								</motion.div>
							)}
						</AnimatePresence>
					</SettingsModalSidebarNav>
					{selectedTab === 'permissions' && canManageGuild && channel.guildId && SettingsSidebar.useOverride && (
						<SettingsModalSidebarFooter data-flx="app.desktop-channel-settings-view.settings-modal-sidebar-footer">
							<Button
								variant="secondary"
								small={true}
								onClick={() => openMessageHistoryThresholdSettings(channel.guildId!)}
								data-flx="app.desktop-channel-settings-view.button.open-message-history-threshold-settings"
							>
								{i18n._(MESSAGE_HISTORY_THRESHOLD_DESCRIPTOR)}
							</Button>
						</SettingsModalSidebarFooter>
					)}
				</SettingsModalDesktopSidebar>
				<SettingsModalDesktopContent
					ref={contentRef}
					tabpanelId={activeTabPanelId}
					labelledBy={activeTabId}
					data-flx="app.desktop-channel-settings-view.settings-modal-desktop-content"
				>
					<SettingsModalHeader
						title={settingsTitle}
						pageLinkHref={null}
						showUnsavedBanner={showUnsavedBanner}
						flashBanner={flashBanner}
						tabData={tabData}
						onClose={handleClose}
						data-flx="app.desktop-channel-settings-view.settings-modal-header"
					/>
					<SettingsModalDesktopScroll
						scrollKey={scrollKey}
						data-flx="app.desktop-channel-settings-view.settings-modal-desktop-scroll"
					>
						{currentTab && (
							<currentTab.component
								channelId={channel.id}
								data-flx="app.desktop-channel-settings-view.current-tab-component"
							/>
						)}
					</SettingsModalDesktopScroll>
				</SettingsModalDesktopContent>
			</>
		);
	},
);

// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SettingsModalHeader} from '@app/features/app/components/dialogs/components/SettingsModalHeader';
import {
	SettingsModalDesktopContent,
	SettingsModalDesktopScroll,
	SettingsModalDesktopSidebar,
	SettingsModalSidebarCategory,
	SettingsModalSidebarCategoryTitle,
	SettingsModalSidebarFooter,
	SettingsModalSidebarItem,
	SettingsModalSidebarNav,
	settingsModalStyles,
} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import Authentication from '@app/features/auth/state/Authentication';
import EmojiStickerLayout from '@app/features/emoji/state/EmojiStickerLayout';
import {GuildDeleteModal} from '@app/features/guild/components/modals/GuildDeleteModal';
import styles from '@app/features/guild/components/modals/GuildSettingsModal.module.css';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildMemberLayout from '@app/features/guild/state/GuildMemberLayout';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import {openMessageHistoryThresholdSettings} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {BACK_TO_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import type {
	GuildSettingsTab,
	GuildSettingsTabCategories,
	GuildSettingsTabType,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {
	GUILD_SETTINGS_LABEL_DESCRIPTOR,
	getGuildSettingsCategoryLabel,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowRightIcon, ArrowSquareOutIcon, TrashIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef} from 'react';

const BACK_TO_ROLES_DESCRIPTOR = msg({
	message: 'Back to roles',
	comment: 'Back-navigation label that returns from an individual role editor to the roles list.',
});
const BACK_TO_OVERRIDES_DESCRIPTOR = msg({
	message: 'Back to overrides',
	comment: 'Back-navigation label that returns from permission override editing to the overrides list.',
});
const DELETE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Delete community',
	comment: 'Destructive footer button in community settings. Opens the delete-community confirmation.',
});
const MESSAGE_HISTORY_THRESHOLD_DESCRIPTOR = msg({
	message: 'Message history threshold',
	comment: 'Community settings tab label for configuring how much message history new members can see.',
});
const MEMBERS_PAGE_ACTION_DESCRIPTOR = msg({
	message: 'Opens the Members page. Press Space or Enter to open.',
	comment:
		'Screen-reader-only hint for the Members entry in community settings. The entry receives keyboard focus without automatically opening.',
});

interface DesktopGuildSettingsViewProps {
	guild: Guild;
	groupedSettingsTabs: Record<string, Array<GuildSettingsTab>>;
	currentTab?: GuildSettingsTab;
	selectedTab: GuildSettingsTabType;
	onTabSelect: (tab: GuildSettingsTabType) => void;
}

export const DesktopGuildSettingsView: React.FC<DesktopGuildSettingsViewProps> = observer(
	({guild, groupedSettingsTabs, currentTab, selectedTab, onTabSelect}) => {
		const {i18n} = useLingui();
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(selectedTab);
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const contentRef = useRef<HTMLDivElement>(null);
		const focusContentPanel = useCallback(() => {
			contentRef.current?.focus();
		}, []);
		const guildOverrideOwnerId = useMemo(() => `guild-roles-${guild.id}`, [guild.id]);
		const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId: guild.id});
		const handleTabSelect = useCallback(
			(tabType: GuildSettingsTabType) => {
				if (checkUnsavedChanges()) return;
				if (
					tabType === 'roles' &&
					SettingsSidebar.ownerId === guildOverrideOwnerId &&
					SettingsSidebar.isDismissed(guildOverrideOwnerId)
				) {
					SettingsSidebar.activateOverride(guildOverrideOwnerId);
				}
				onTabSelect(tabType);
			},
			[checkUnsavedChanges, onTabSelect, guildOverrideOwnerId],
		);
		const handleDeleteGuild = useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalCommands.push(
				modal(() => (
					<GuildDeleteModal
						guildId={guild.id}
						data-flx="app.desktop-guild-settings-view.handle-delete-guild.guild-delete-modal"
					/>
				)),
			);
		}, [guild.id, checkUnsavedChanges]);
		const handleClose = useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalCommands.pop();
		}, [checkUnsavedChanges]);
		const useOverride = SettingsSidebar.useOverride;
		const activeTabPanelId = selectedTab ? `guild-settings-tabpanel-${selectedTab}` : undefined;
		const activeTabId = selectedTab ? `guild-settings-tab-${selectedTab}` : undefined;
		const emojiLayout = EmojiStickerLayout.getEmojiLayout();
		const stickerViewMode = EmojiStickerLayout.getStickerViewMode();
		const memberViewMode = GuildMemberLayout.getViewMode();
		const scrollKey = useMemo(() => {
			const baseKey = `guild-settings-${guild.id}-${selectedTab ?? 'none'}`;
			switch (selectedTab) {
				case 'emoji':
					return `${baseKey}-emoji-${emojiLayout}`;
				case 'stickers':
					return `${baseKey}-stickers-${stickerViewMode}`;
				case 'members':
					return `${baseKey}-members-${memberViewMode}`;
				default:
					return baseKey;
			}
		}, [guild.id, selectedTab, emojiLayout, stickerViewMode, memberViewMode]);
		return (
			<>
				<SettingsModalDesktopSidebar data-flx="app.desktop-guild-settings-view.settings-modal-desktop-sidebar">
					<div className={styles.sidebarHeader} data-flx="app.desktop-guild-settings-view.sidebar-header">
						<div className={styles.guildName} data-flx="app.desktop-guild-settings-view.guild-name">
							{guild.name}
						</div>
					</div>
					<SettingsModalSidebarNav data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-nav">
						<AnimatePresence mode="wait" initial={false} data-flx="app.desktop-guild-settings-view.animate-presence">
							{SettingsSidebar.hasOverride && useOverride ? (
								<motion.div
									key="custom"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
									data-flx="app.desktop-guild-settings-view.div"
								>
									<div
										className={styles.sidebarButtonWrapper}
										data-flx="app.desktop-guild-settings-view.sidebar-button-wrapper"
									>
										<Button
											variant="secondary"
											leftIcon={
												<ArrowLeftIcon
													className={styles.sidebarButtonIcon}
													data-flx="app.desktop-guild-settings-view.sidebar-button-icon"
												/>
											}
											onClick={() => SettingsSidebar.dismissOverride()}
											data-flx="app.desktop-guild-settings-view.button.dismiss-override"
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
									data-flx="app.desktop-guild-settings-view.div--2"
								>
									{SettingsSidebar.hasOverride &&
										SettingsSidebar.ownerId === guildOverrideOwnerId &&
										!SettingsSidebar.isDismissed(guildOverrideOwnerId) && (
											<div
												className={styles.sidebarButtonWrapper}
												data-flx="app.desktop-guild-settings-view.sidebar-button-wrapper--2"
											>
												<Button
													variant="secondary"
													rightIcon={
														<ArrowRightIcon
															className={styles.sidebarButtonIcon}
															data-flx="app.desktop-guild-settings-view.sidebar-button-icon--2"
														/>
													}
													onClick={() => SettingsSidebar.activateOverride(guildOverrideOwnerId)}
													data-flx="app.desktop-guild-settings-view.button.activate-override"
												>
													{selectedTab === 'roles'
														? i18n._(BACK_TO_ROLES_DESCRIPTOR)
														: i18n._(BACK_TO_OVERRIDES_DESCRIPTOR)}
												</Button>
											</div>
										)}
									{Object.entries(groupedSettingsTabs).map(([category, tabs]) => (
										<SettingsModalSidebarCategory
											key={category}
											data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-category"
										>
											{category !== 'guild_settings' && (
												<SettingsModalSidebarCategoryTitle data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-category-title">
													{getGuildSettingsCategoryLabel(i18n, category as GuildSettingsTabCategories)}
												</SettingsModalSidebarCategoryTitle>
											)}
											{tabs.map((tab) => {
												const tabId = `guild-settings-tab-${tab.type}`;
												const panelId = `guild-settings-tabpanel-${tab.type}`;
												const isExternalTab = tab.type === 'members';
												const requiresExplicitKeyboardActivation = tab.type === 'roles' || isExternalTab;
												return (
													<SettingsModalSidebarItem
														key={tab.type}
														icon={tab.icon}
														iconWeight={tab.iconWeight}
														label={
															tab.type === 'members' ? (
																<span
																	className={styles.externalTabLabel}
																	data-flx="app.desktop-guild-settings-view.external-tab-label"
																>
																	<span
																		className={styles.externalTabLabelText}
																		data-flx="app.desktop-guild-settings-view.external-tab-label-text"
																	>
																		{tab.label}
																	</span>
																	<span
																		className={settingsModalStyles.srOnly}
																		data-flx="app.desktop-guild-settings-view.external-tab-screen-reader-hint"
																	>
																		{i18n._(MEMBERS_PAGE_ACTION_DESCRIPTOR)}
																	</span>
																	<ArrowSquareOutIcon
																		size={remFromPx(16)}
																		weight="bold"
																		className={styles.externalTabIcon}
																		aria-hidden="true"
																		data-flx="app.desktop-guild-settings-view.external-tab-icon"
																	/>
																</span>
															) : (
																tab.label
															)
														}
														selected={tab.type === selectedTab}
														autoSelectOnKeyboardNavigation={!requiresExplicitKeyboardActivation}
														onClick={() => handleTabSelect(tab.type)}
														onRequestContentFocus={isExternalTab ? undefined : focusContentPanel}
														id={tabId}
														controlsId={panelId}
														data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-item.tab-select"
													/>
												);
											})}
										</SettingsModalSidebarCategory>
									))}
									{guild.isOwner(Authentication.currentUserId) && !isStockCommunityGuild(guild.id) && (
										<SettingsModalSidebarItem
											icon={TrashIcon}
											label={i18n._(DELETE_COMMUNITY_DESCRIPTOR)}
											danger={true}
											onClick={handleDeleteGuild}
											data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-item.delete-guild"
										/>
									)}
								</motion.div>
							)}
						</AnimatePresence>
					</SettingsModalSidebarNav>
					{selectedTab === 'roles' && canManageGuild && SettingsSidebar.useOverride && (
						<SettingsModalSidebarFooter data-flx="app.desktop-guild-settings-view.settings-modal-sidebar-footer">
							<Button
								variant="secondary"
								small={true}
								onClick={() => openMessageHistoryThresholdSettings(guild.id)}
								data-flx="app.desktop-guild-settings-view.button.open-message-history-threshold-settings"
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
					data-flx="app.desktop-guild-settings-view.settings-modal-desktop-content"
				>
					<SettingsModalHeader
						title={currentTab?.label || i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR)}
						showUnsavedBanner={showUnsavedBanner}
						flashBanner={flashBanner}
						tabData={tabData}
						onClose={handleClose}
						data-flx="app.desktop-guild-settings-view.settings-modal-header"
					/>
					<SettingsModalDesktopScroll
						scrollKey={scrollKey}
						data-flx="app.desktop-guild-settings-view.settings-modal-desktop-scroll"
					>
						{currentTab && (
							<currentTab.component
								guildId={guild.id}
								data-flx="app.desktop-guild-settings-view.current-tab-component"
							/>
						)}
					</SettingsModalDesktopScroll>
				</SettingsModalDesktopContent>
			</>
		);
	},
);

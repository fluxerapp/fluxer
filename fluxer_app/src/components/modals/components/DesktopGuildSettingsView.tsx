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
import {ArrowLeftIcon, ArrowRightIcon, TrashIcon} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {GuildDeleteModal} from '~/components/modals/GuildDeleteModal';
import {Button} from '~/components/uikit/Button/Button';
import type {GuildRecord} from '~/records/GuildRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import EmojiStickerLayoutStore from '~/stores/EmojiStickerLayoutStore';
import GuildMemberLayoutStore from '~/stores/GuildMemberLayoutStore';
import SettingsSidebarStore from '~/stores/SettingsSidebarStore';
import {SettingsModalHeader} from '../components/SettingsModalHeader';
import styles from '../GuildSettingsModal.module.css';
import {useUnsavedChangesFlash} from '../hooks/useUnsavedChangesFlash';
import {
	SettingsModalDesktopContent,
	SettingsModalDesktopScroll,
	SettingsModalDesktopSidebar,
	SettingsModalSidebarCategory,
	SettingsModalSidebarCategoryTitle,
	SettingsModalSidebarItem,
	SettingsModalSidebarNav,
} from '../shared/SettingsModalLayout';
import type {GuildSettingsTab, GuildSettingsTabType} from '../utils/guildSettingsConstants';

interface DesktopGuildSettingsViewProps {
	guild: GuildRecord;
	groupedSettingsTabs: Record<string, Array<GuildSettingsTab>>;
	currentTab?: GuildSettingsTab;
	selectedTab: GuildSettingsTabType;
	onTabSelect: (tab: GuildSettingsTabType) => void;
}

export const DesktopGuildSettingsView: React.FC<DesktopGuildSettingsViewProps> = observer(
	({guild, groupedSettingsTabs, currentTab, selectedTab, onTabSelect}) => {
		const {t} = useLingui();
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(selectedTab);

		const CATEGORY_LABELS = {
			guild_settings: '',
			user_management: t`User Management`,
		};
		const prefersReducedMotion = AccessibilityStore.useReducedMotion;
		const contentRef = React.useRef<HTMLDivElement>(null);
		const focusContentPanel = React.useCallback(() => {
			contentRef.current?.focus();
		}, []);

		const handleTabSelect = React.useCallback(
			(tabType: GuildSettingsTabType) => {
				if (checkUnsavedChanges()) return;
				onTabSelect(tabType);
			},
			[checkUnsavedChanges, onTabSelect],
		);

		const handleDeleteGuild = React.useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalActionCreators.push(modal(() => <GuildDeleteModal guildId={guild.id} />));
		}, [guild.id, checkUnsavedChanges]);

		const handleClose = React.useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalActionCreators.pop();
		}, [checkUnsavedChanges]);

		const useOverride = SettingsSidebarStore.useOverride;
		const activeTabPanelId = selectedTab ? `guild-settings-tabpanel-${selectedTab}` : undefined;
		const activeTabId = selectedTab ? `guild-settings-tab-${selectedTab}` : undefined;
		const hasSelectedTabInSidebar = React.useMemo(() => {
			if (!selectedTab || useOverride) {
				return false;
			}
			return Object.values(groupedSettingsTabs).some((tabs) => tabs.some((tab) => tab.type === selectedTab));
		}, [groupedSettingsTabs, selectedTab, useOverride]);
		const emojiLayout = EmojiStickerLayoutStore.getEmojiLayout();
		const stickerViewMode = EmojiStickerLayoutStore.getStickerViewMode();
		const memberViewMode = GuildMemberLayoutStore.getViewMode();

		const scrollKey = React.useMemo(() => {
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
				<SettingsModalDesktopSidebar>
					<div className={styles.sidebarHeader}>
						<div className={styles.guildName}>{guild.name}</div>
					</div>
					<SettingsModalSidebarNav hasSelectedTabInView={hasSelectedTabInSidebar}>
						<AnimatePresence mode="wait" initial={false}>
							{SettingsSidebarStore.hasOverride && useOverride ? (
								<motion.div
									key="custom"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
								>
									<div className={styles.sidebarButtonWrapper}>
										<Button
											variant="secondary"
											leftIcon={<ArrowLeftIcon className={styles.sidebarButtonIcon} />}
											onClick={() => SettingsSidebarStore.setUseOverride(false)}
										>
											{t`Back to Settings`}
										</Button>
									</div>
									{SettingsSidebarStore.overrideContent}
								</motion.div>
							) : (
								<motion.div
									key="global"
									initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									animate={{opacity: 1}}
									exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
									transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
								>
									{SettingsSidebarStore.hasOverride && (
										<div className={styles.sidebarButtonWrapper}>
											<Button
												variant="secondary"
												rightIcon={<ArrowRightIcon className={styles.sidebarButtonIcon} />}
												onClick={() => SettingsSidebarStore.setUseOverride(true)}
											>
												{selectedTab === 'roles' ? t`Back to Roles` : t`Back to Overrides`}
											</Button>
										</div>
									)}
									{Object.entries(groupedSettingsTabs).map(([category, tabs]) => (
										<SettingsModalSidebarCategory key={category}>
											{category !== 'guild_settings' && (
												<SettingsModalSidebarCategoryTitle>
													{CATEGORY_LABELS[category as keyof typeof CATEGORY_LABELS]}
												</SettingsModalSidebarCategoryTitle>
											)}
											{tabs.map((tab) => {
												const tabId = `guild-settings-tab-${tab.type}`;
												const panelId = `guild-settings-tabpanel-${tab.type}`;
												return (
													<SettingsModalSidebarItem
														key={tab.type}
														icon={tab.icon}
														iconWeight={tab.iconWeight}
														label={tab.label}
														selected={tab.type === selectedTab}
														onClick={() => handleTabSelect(tab.type)}
														onRequestContentFocus={focusContentPanel}
														id={tabId}
														controlsId={panelId}
													/>
												);
											})}
										</SettingsModalSidebarCategory>
									))}
									{guild.isOwner(AuthenticationStore.currentUserId) && (
										<SettingsModalSidebarItem
											icon={TrashIcon}
											label={t`Delete Community`}
											danger={true}
											onClick={handleDeleteGuild}
										/>
									)}
								</motion.div>
							)}
						</AnimatePresence>
					</SettingsModalSidebarNav>
				</SettingsModalDesktopSidebar>
				<SettingsModalDesktopContent ref={contentRef} tabpanelId={activeTabPanelId} labelledBy={activeTabId}>
					<SettingsModalHeader
						title={currentTab?.label || t`Community Settings`}
						showUnsavedBanner={showUnsavedBanner}
						flashBanner={flashBanner}
						tabData={tabData}
						onClose={handleClose}
					/>
					<SettingsModalDesktopScroll scrollKey={scrollKey}>
						{currentTab && <currentTab.component guildId={guild.id} />}
					</SettingsModalDesktopScroll>
				</SettingsModalDesktopContent>
			</>
		);
	},
);

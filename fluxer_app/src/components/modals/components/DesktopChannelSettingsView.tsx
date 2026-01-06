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
import {ChannelTypes} from '~/Constants';
import {ChannelDeleteModal} from '~/components/modals/ChannelDeleteModal';
import {Button} from '~/components/uikit/Button/Button';
import type {ChannelRecord} from '~/records/ChannelRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
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
import type {ChannelSettingsTab, ChannelSettingsTabType} from '../utils/channelSettingsConstants';

interface DesktopChannelSettingsViewProps {
	channel: ChannelRecord;
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
		const {t} = useLingui();
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(selectedTab);
		const prefersReducedMotion = AccessibilityStore.useReducedMotion;
		const contentRef = React.useRef<HTMLDivElement>(null);
		const focusContentPanel = React.useCallback(() => {
			contentRef.current?.focus();
		}, []);

		const channelPermissionsOverrideOwnerId = React.useMemo(
			() => `channel-permissions-${channel.id}`,
			[channel.id],
		);

		const handleTabSelect = React.useCallback(
			(tabType: ChannelSettingsTabType) => {
				if (checkUnsavedChanges()) return;
				if (
					tabType === 'permissions' &&
					SettingsSidebarStore.ownerId === channelPermissionsOverrideOwnerId &&
					SettingsSidebarStore.isDismissed(channelPermissionsOverrideOwnerId)
				) {
					SettingsSidebarStore.activateOverride(channelPermissionsOverrideOwnerId);
				}
				onTabSelect(tabType);
			},
			[checkUnsavedChanges, onTabSelect, channelPermissionsOverrideOwnerId],
		);

		const handleDeleteChannel = React.useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalActionCreators.push(modal(() => <ChannelDeleteModal channelId={channel.id} />));
		}, [channel.id, checkUnsavedChanges]);

		const handleClose = React.useCallback(() => {
			if (checkUnsavedChanges()) return;
			ModalActionCreators.pop();
		}, [checkUnsavedChanges]);

		const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
		const useOverride = SettingsSidebarStore.useOverride;
		const activeTabPanelId = selectedTab ? `channel-settings-tabpanel-${selectedTab}` : undefined;
		const activeTabId = selectedTab ? `channel-settings-tab-${selectedTab}` : undefined;
		const hasSelectedTabInSidebar = React.useMemo(() => {
			if (!selectedTab || useOverride) {
				return false;
			}
			return Object.values(groupedSettingsTabs).some((tabs) => tabs.some((tab) => tab.type === selectedTab));
		}, [groupedSettingsTabs, selectedTab, useOverride]);
		const scrollKey = React.useMemo(
			() => `channel-settings-${channel.id}-${selectedTab ?? 'none'}`,
			[channel.id, selectedTab],
		);

		return (
			<>
				<SettingsModalDesktopSidebar>
					<div className={styles.sidebarHeader}>
						<div className={styles.guildName}>{channel.name}</div>
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
											onClick={() => SettingsSidebarStore.dismissOverride()}
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
									{SettingsSidebarStore.hasOverride &&
										SettingsSidebarStore.ownerId === channelPermissionsOverrideOwnerId &&
										!SettingsSidebarStore.isDismissed(channelPermissionsOverrideOwnerId) && (
										<div className={styles.sidebarButtonWrapper}>
											<Button
												variant="secondary"
												rightIcon={<ArrowRightIcon className={styles.sidebarButtonIcon} />}
												onClick={() => SettingsSidebarStore.activateOverride(channelPermissionsOverrideOwnerId)}
											>
												{t`Back to Overrides`}
											</Button>
										</div>
									)}
									{Object.entries(groupedSettingsTabs).map(([category, tabs]) => (
										<SettingsModalSidebarCategory key={category}>
											{category !== 'channel_settings' && (
												<SettingsModalSidebarCategoryTitle>
													{CATEGORY_LABELS[category as keyof typeof CATEGORY_LABELS]}
												</SettingsModalSidebarCategoryTitle>
											)}
											{tabs.map((tab) => {
												const tabId = `channel-settings-tab-${tab.type}`;
												const panelId = `channel-settings-tabpanel-${tab.type}`;
												return (
													<SettingsModalSidebarItem
														key={tab.type}
														icon={tab.icon}
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
									{!useOverride && (
										<SettingsModalSidebarItem
											icon={TrashIcon}
											label={isCategory ? t`Delete Category` : t`Delete Channel`}
											danger={true}
											onClick={handleDeleteChannel}
										/>
									)}
								</motion.div>
							)}
						</AnimatePresence>
					</SettingsModalSidebarNav>
				</SettingsModalDesktopSidebar>
				<SettingsModalDesktopContent ref={contentRef} tabpanelId={activeTabPanelId} labelledBy={activeTabId}>
					<SettingsModalHeader
						title={currentTab?.label || (isCategory ? t`Category Settings` : t`Channel Settings`)}
						showUnsavedBanner={showUnsavedBanner}
						flashBanner={flashBanner}
						tabData={tabData}
						onClose={handleClose}
					/>
					<SettingsModalDesktopScroll scrollKey={scrollKey}>
						{currentTab && <currentTab.component channelId={channel.id} />}
					</SettingsModalDesktopScroll>
				</SettingsModalDesktopContent>
			</>
		);
	},
);

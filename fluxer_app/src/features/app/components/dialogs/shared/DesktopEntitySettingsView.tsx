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
} from '@app/features/app/components/dialogs/shared/SettingsModalLayout';
import styles from '@app/features/guild/components/modals/GuildSettingsModal.module.css';
import {openMessageHistoryThresholdSettings} from '@app/features/guild/utils/guild_tabs/GuildOverviewTabUtils';
import {BACK_TO_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {ArrowLeftIcon, ArrowRightIcon} from '@app/features/ui/icons/DirectionalIcons';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {Icon, IconWeight} from '@phosphor-icons/react';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

const MESSAGE_HISTORY_THRESHOLD_DESCRIPTOR = msg({
	message: 'Message history threshold',
	comment: 'Settings link for configuring how much message history new members can see.',
});
interface EntitySettingsTab {
	type: string;
	label: string;
	icon: Icon;
	iconWeight?: IconWeight;
}
interface TabPresentation {
	label: React.ReactNode;
	requiresExplicitKeyboardActivation: boolean;
	focusContent?: boolean;
}
interface DesktopEntitySettingsViewProps<T extends EntitySettingsTab> {
	groupedSettingsTabs: Record<string, Array<T>>;
	selectedTab: T['type'];
	onTabSelect: (tab: T['type']) => void;
	overrideOwnerId: string;
	overrideTab: T['type'];
	overrideBackLabel: React.ReactNode;
	sidebarTitle: React.ReactNode;
	settingsTitle: React.ReactNode;
	pageLinkHref?: string | null;
	scrollKey: string;
	idPrefix: string;
	hiddenCategory: string;
	categoryLabel: (category: string) => React.ReactNode;
	presentTab: (tab: T) => TabPresentation;
	content: React.ReactNode;
	dangerAction: (checkUnsavedChanges: () => boolean) => React.ReactNode;
	thresholdTargetId: string | null;
}
export const DesktopEntitySettingsView = observer(function DesktopEntitySettingsView<T extends EntitySettingsTab>({
	groupedSettingsTabs,
	selectedTab,
	onTabSelect,
	overrideOwnerId,
	overrideTab,
	overrideBackLabel,
	sidebarTitle,
	settingsTitle,
	pageLinkHref,
	scrollKey,
	idPrefix,
	hiddenCategory,
	categoryLabel,
	presentTab,
	content,
	dangerAction,
	thresholdTargetId,
}: DesktopEntitySettingsViewProps<T>) {
	const {i18n} = useLingui();
	const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(selectedTab);
	const prefersReducedMotion = Accessibility.useReducedMotion;
	const contentRef = useRef<HTMLDivElement>(null);
	const focusContentPanel = useCallback(() => {
		contentRef.current?.focus();
	}, []);
	const handleTabSelect = useCallback(
		(tabType: T['type']) => {
			if (checkUnsavedChanges()) return;
			if (
				tabType === overrideTab &&
				SettingsSidebar.ownerId === overrideOwnerId &&
				SettingsSidebar.isDismissed(overrideOwnerId)
			) {
				SettingsSidebar.activateOverride(overrideOwnerId);
			}
			onTabSelect(tabType);
		},
		[checkUnsavedChanges, onTabSelect, overrideOwnerId, overrideTab],
	);
	const handleClose = useCallback(() => {
		if (checkUnsavedChanges()) return;
		ModalCommands.pop();
	}, [checkUnsavedChanges]);
	const useOverride = SettingsSidebar.useOverride;
	const activeTabPanelId = selectedTab ? `${idPrefix}-tabpanel-${selectedTab}` : undefined;
	const activeTabId = selectedTab ? `${idPrefix}-tab-${selectedTab}` : undefined;
	return (
		<>
			<SettingsModalDesktopSidebar data-flx="app.desktop-entity-settings-view.settings-modal-desktop-sidebar">
				<div className={styles.sidebarHeader} data-flx="app.desktop-entity-settings-view.sidebar-header">
					<div className={styles.guildName} data-flx="app.desktop-entity-settings-view.guild-name">
						{sidebarTitle}
					</div>
				</div>
				<SettingsModalSidebarNav data-flx="app.desktop-entity-settings-view.settings-modal-sidebar-nav">
					<AnimatePresence mode="wait" initial={false} data-flx="app.desktop-entity-settings-view.animate-presence">
						{SettingsSidebar.hasOverride && useOverride ? (
							<motion.div
								key="custom"
								initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
								animate={{opacity: 1}}
								exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
								transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2, ease: 'easeOut'}}
								data-flx="app.desktop-entity-settings-view.div"
							>
								<div
									className={styles.sidebarButtonWrapper}
									data-flx="app.desktop-entity-settings-view.sidebar-button-wrapper"
								>
									<Button
										variant="secondary"
										leftIcon={
											<ArrowLeftIcon
												className={styles.sidebarButtonIcon}
												data-flx="app.desktop-entity-settings-view.sidebar-button-icon"
											/>
										}
										onClick={() => SettingsSidebar.dismissOverride()}
										data-flx="app.desktop-entity-settings-view.button.dismiss-override"
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
								data-flx="app.desktop-entity-settings-view.div--2"
							>
								{SettingsSidebar.hasOverride &&
									SettingsSidebar.ownerId === overrideOwnerId &&
									!SettingsSidebar.isDismissed(overrideOwnerId) && (
										<div
											className={styles.sidebarButtonWrapper}
											data-flx="app.desktop-entity-settings-view.sidebar-button-wrapper--2"
										>
											<Button
												variant="secondary"
												rightIcon={
													<ArrowRightIcon
														className={styles.sidebarButtonIcon}
														data-flx="app.desktop-entity-settings-view.sidebar-button-icon--2"
													/>
												}
												onClick={() => SettingsSidebar.activateOverride(overrideOwnerId)}
												data-flx="app.desktop-entity-settings-view.button.activate-override"
											>
												{overrideBackLabel}
											</Button>
										</div>
									)}
								{Object.entries(groupedSettingsTabs).map(([category, tabs]) => (
									<SettingsModalSidebarCategory
										key={category}
										data-flx="app.desktop-entity-settings-view.settings-modal-sidebar-category"
									>
										{category !== hiddenCategory && (
											<SettingsModalSidebarCategoryTitle data-flx="app.desktop-entity-settings-view.settings-modal-sidebar-category-title">
												{categoryLabel(category)}
											</SettingsModalSidebarCategoryTitle>
										)}
										{tabs.map((tab) => {
											const tabId = `${idPrefix}-tab-${tab.type}`;
											const panelId = `${idPrefix}-tabpanel-${tab.type}`;
											const presentation = presentTab(tab);
											const requiresExplicitKeyboardActivation = presentation.requiresExplicitKeyboardActivation;
											return (
												<SettingsModalSidebarItem
													key={tab.type}
													icon={tab.icon}
													label={presentation.label}
													iconWeight={tab.iconWeight}
													selected={tab.type === selectedTab}
													autoSelectOnKeyboardNavigation={!requiresExplicitKeyboardActivation}
													onClick={() => handleTabSelect(tab.type)}
													onRequestContentFocus={presentation.focusContent === false ? undefined : focusContentPanel}
													id={tabId}
													controlsId={panelId}
													data-flx="app.desktop-entity-settings-view.settings-modal-sidebar-item.tab-select"
												/>
											);
										})}
									</SettingsModalSidebarCategory>
								))}
								{dangerAction(checkUnsavedChanges)}
							</motion.div>
						)}
					</AnimatePresence>
				</SettingsModalSidebarNav>
				{thresholdTargetId && (
					<SettingsModalSidebarFooter data-flx="app.desktop-entity-settings-view.settings-modal-sidebar-footer">
						<Button
							variant="secondary"
							small={true}
							onClick={() => openMessageHistoryThresholdSettings(thresholdTargetId!)}
							data-flx="app.desktop-entity-settings-view.button.open-message-history-threshold-settings"
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
				data-flx="app.desktop-entity-settings-view.settings-modal-desktop-content"
			>
				<SettingsModalHeader
					title={settingsTitle}
					pageLinkHref={pageLinkHref}
					showUnsavedBanner={showUnsavedBanner}
					flashBanner={flashBanner}
					tabData={tabData}
					onClose={handleClose}
					data-flx="app.desktop-entity-settings-view.settings-modal-header"
				/>
				<SettingsModalDesktopScroll
					scrollKey={scrollKey}
					data-flx="app.desktop-entity-settings-view.settings-modal-desktop-scroll"
				>
					{content}
				</SettingsModalDesktopScroll>
			</SettingsModalDesktopContent>
		</>
	);
});

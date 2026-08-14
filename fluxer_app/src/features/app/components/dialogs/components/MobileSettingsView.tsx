// SPDX-License-Identifier: AGPL-3.0-or-later

import {ClientInfo} from '@app/features/app/components/dialogs/components/ClientInfo';
import {LogoutModal} from '@app/features/app/components/dialogs/components/LogoutModal';
import styles from '@app/features/app/components/dialogs/components/MobileSettingsView.module.css';
import {
	MobileHeader,
	MobileHeaderWithBanner,
	MobileSettingsDangerItem,
} from '@app/features/app/components/dialogs/shared/MobileSettingsComponents';
import {SettingsCurrentTabProvider} from '@app/features/app/components/dialogs/shared/SettingsCurrentTabContext';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {hasWhatsNewEntries} from '@app/features/app/components/whats_new/WhatsNewEntries';
import {openWhatsNewModal} from '@app/features/app/components/whats_new/WhatsNewModal';
import {usePressable} from '@app/features/app/hooks/usePressable';
import {SETTINGS_DESCRIPTOR, SIGN_OUT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {activateLatestServiceWorker} from '@app/features/platform/types/Versioning';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {MentionBadgeAnimated} from '@app/features/ui/components/MentionBadge';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import ApplicationsTabState from '@app/features/user/components/modals/tabs/applications_tab/ApplicationsTabState';
import userSettingsStyles from '@app/features/user/components/modals/UserSettingsModal.module.css';
import {getSettingsTabComponent} from '@app/features/user/components/settings_utils/DesktopSettingsTabs';
import type {SettingsTab} from '@app/features/user/components/settings_utils/SettingsConstants';
import {
	getCategoryLabel,
	getUserSettingsTabLabel,
} from '@app/features/user/components/settings_utils/SettingsConstants';
import {isSettingsItemNew} from '@app/features/user/components/settings_utils/SettingsMetadata';
import {
	PRIMARY_SETTINGS_NAV_HIDDEN_TAB_TYPES,
	PROFILE_SETTINGS_TAB,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import {getAdvancedSettingItems} from '@app/features/user/components/settings_utils/SettingsSearchIndex';
import type {UserSettingsTabType} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {SettingsStatusBadge} from '@app/features/user/components/settings_utils/SettingsStatusBadge';
import {buildUserSettingsDeepLink} from '@app/features/user/components/settings_utils/UserSettingsDeepLinks';
import type {MobileNavigationState} from '@app/features/user/hooks/useMobileNavigation';
import {useSettingsContentKey} from '@app/features/user/hooks/useSettingsContentKey';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon, ArrowLeftIcon, type IconWeight, MegaphoneIcon, SignOutIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React, {type UIEvent, useCallback, useEffect, useMemo, useRef, useState} from 'react';

const UPDATE_SERVICE_WORKER_DESCRIPTOR = msg({
	message: 'Update service worker',
	comment: 'Short label in the settings dialog mobile settings view.',
});
const WHAT_S_NEW_DESCRIPTOR = msg({
	message: "What's new",
	comment: 'Short label in the settings dialog mobile settings view.',
});
const DEBUG_DESCRIPTOR = msg({
	message: 'Debug',
	comment: 'Short label in the settings dialog mobile settings view.',
});
const APPLICATION_DETAILS_DESCRIPTOR = msg({
	message: 'Application details',
	comment: 'Settings content title shown while a developer application detail page is loading.',
});
const logger = new Logger('MobileSettingsView');
const LONG_PRESS_CLICK_SUPPRESS_MS = 750;

type SettingsPageLongPressEvent = React.PointerEvent<HTMLElement> | React.TouchEvent<HTMLElement>;

function useSettingsPageDeepLinkLongPress(tabType: UserSettingsTabType): {
	handleLongPressCopy: (event: SettingsPageLongPressEvent) => void;
	consumeSuppressedClick: (event: React.MouseEvent<HTMLElement>) => boolean;
} {
	const {i18n} = useLingui();
	const suppressNextClickRef = useRef(false);
	const suppressClickTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const clearSuppressedClick = useCallback(() => {
		suppressNextClickRef.current = false;
		if (suppressClickTimeoutRef.current) {
			clearTimeout(suppressClickTimeoutRef.current);
			suppressClickTimeoutRef.current = null;
		}
	}, []);
	const suppressNextClick = useCallback(() => {
		suppressNextClickRef.current = true;
		if (suppressClickTimeoutRef.current) {
			clearTimeout(suppressClickTimeoutRef.current);
		}
		suppressClickTimeoutRef.current = setTimeout(() => {
			suppressNextClickRef.current = false;
			suppressClickTimeoutRef.current = null;
		}, LONG_PRESS_CLICK_SUPPRESS_MS);
	}, []);
	useEffect(() => clearSuppressedClick, [clearSuppressedClick]);
	const handleLongPressCopy = useCallback(
		(event: SettingsPageLongPressEvent) => {
			event.preventDefault();
			event.stopPropagation();
			suppressNextClick();
			void TextCopyCommands.copy(i18n, buildUserSettingsDeepLink(tabType));
		},
		[i18n, suppressNextClick, tabType],
	);
	const consumeSuppressedClick = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			if (!suppressNextClickRef.current) return false;
			event.preventDefault();
			event.stopPropagation();
			clearSuppressedClick();
			return true;
		},
		[clearSuppressedClick],
	);
	return {handleLongPressCopy, consumeSuppressedClick};
}

interface MobileSettingsViewProps {
	groupedSettingsTabs: Record<string, Array<SettingsTab>>;
	currentTab: SettingsTab | undefined;
	mobileNav: MobileNavigationState;
	onBack: () => void;
	onTabSelect: (tab: string, title: string) => void;
	initialGuildId?: string;
	initialSubtab?: string;
	pendingSection?: string | null;
	onPendingSectionConsumed?: () => void;
}

interface PressableSettingsItemProps {
	tab: SettingsTab;
	onSelect: () => void;
	badge?: React.ReactNode;
}

const PressableSettingsItem: React.FC<PressableSettingsItemProps> = observer(({tab, onSelect, badge}) => {
	const {isPressed, pressableProps} = usePressable();
	const {handleLongPressCopy, consumeSuppressedClick} = useSettingsPageDeepLinkLongPress(tab.type);
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			if (consumeSuppressedClick(event)) return;
			onSelect();
		},
		[consumeSuppressedClick, onSelect],
	);
	const IconComponent = tab.icon;
	return (
		<LongPressable
			className={clsx(styles.settingsItem, isPressed && styles.settingsItemPressed)}
			role="button"
			tabIndex={0}
			onClick={handleClick}
			onLongPress={handleLongPressCopy}
			onKeyDown={(event) => {
				if (!isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				onSelect();
			}}
			data-flx="app.mobile-settings-view.pressable-settings-item.settings-item.select"
			{...pressableProps}
		>
			<IconComponent
				className={styles.settingsItemIcon}
				weight={tab.iconWeight ?? 'fill'}
				data-flx="app.mobile-settings-view.pressable-settings-item.settings-item-icon"
			/>
			<div
				className={styles.settingsItemContent}
				data-flx="app.mobile-settings-view.pressable-settings-item.settings-item-content"
			>
				<div
					className={styles.settingsItemLabelContainer}
					data-flx="app.mobile-settings-view.pressable-settings-item.settings-item-label-container"
				>
					<span
						className={styles.settingsItemLabel}
						data-flx="app.mobile-settings-view.pressable-settings-item.settings-item-label"
					>
						{tab.label}
					</span>
					{badge}
				</div>
			</div>
			<ArrowLeftIcon
				className={styles.settingsItemArrow}
				data-flx="app.mobile-settings-view.pressable-settings-item.settings-item-arrow"
			/>
		</LongPressable>
	);
});

interface MobileSettingsActionItemProps {
	icon: React.ComponentType<{className?: string; weight?: IconWeight}>;
	label: React.ReactNode;
	onClick: () => void;
	isLoading?: boolean;
	iconWeight?: IconWeight;
	showArrow?: boolean;
}

const MobileSettingsActionItem: React.FC<MobileSettingsActionItemProps> = observer(
	({icon: IconComponent, label, onClick, isLoading = false, iconWeight, showArrow = true}) => {
		const {isPressed, pressableProps} = usePressable();
		return (
			<LongPressable
				className={clsx(styles.settingsItem, isPressed && styles.settingsItemPressed)}
				role="button"
				tabIndex={0}
				onClick={onClick}
				onKeyDown={(event) => {
					if (!isKeyboardActivationKey(event.key)) return;
					event.preventDefault();
					onClick();
				}}
				data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item.click"
				{...pressableProps}
			>
				<IconComponent
					className={styles.settingsItemIcon}
					weight={iconWeight ?? 'regular'}
					data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item-icon"
				/>
				<div
					className={styles.settingsItemContent}
					data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item-content"
				>
					<div
						className={styles.settingsItemLabelContainer}
						data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item-label-container"
					>
						<span
							className={styles.settingsItemLabel}
							data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item-label"
						>
							{label}
						</span>
						{isLoading && (
							<Spinner
								size="small"
								className={styles.settingsItemSpinner}
								data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item-spinner"
							/>
						)}
					</div>
				</div>
				{showArrow && (
					<ArrowLeftIcon
						className={styles.settingsItemArrow}
						data-flx="app.mobile-settings-view.mobile-settings-action-item.settings-item-arrow"
					/>
				)}
			</LongPressable>
		);
	},
);
const ServiceWorkerUpdateButton = observer(() => {
	const {i18n} = useLingui();
	const [isUpdating, setIsUpdating] = useState(false);
	const handleUpdateServiceWorker = async () => {
		if (isUpdating) return;
		setIsUpdating(true);
		try {
			await activateLatestServiceWorker();
			window.location.reload();
		} catch (error) {
			logger.error('Failed to update service worker:', error);
		} finally {
			setIsUpdating(false);
		}
	};
	return (
		<MobileSettingsActionItem
			icon={ArrowClockwiseIcon}
			iconWeight="bold"
			label={i18n._(UPDATE_SERVICE_WORKER_DESCRIPTOR)}
			onClick={handleUpdateServiceWorker}
			isLoading={isUpdating}
			showArrow={false}
			data-flx="app.mobile-settings-view.service-worker-update-button.mobile-settings-action-item.update-service-worker"
		/>
	);
});
const MOBILE_HIDDEN_TAB_TYPES = new Set<UserSettingsTabType>(['keybinds', ...PRIMARY_SETTINGS_NAV_HIDDEN_TAB_TYPES]);

interface MobileProfileSettingsItemProps {
	onSelect: () => void;
}

const MobileProfileSettingsItem: React.FC<MobileProfileSettingsItemProps> = observer(({onSelect}) => {
	const {isPressed, pressableProps} = usePressable();
	const {handleLongPressCopy, consumeSuppressedClick} = useSettingsPageDeepLinkLongPress(PROFILE_SETTINGS_TAB);
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			if (consumeSuppressedClick(event)) return;
			onSelect();
		},
		[consumeSuppressedClick, onSelect],
	);
	const currentUser = Users.currentUser;
	if (!currentUser) return null;
	return (
		<LongPressable
			className={clsx(styles.settingsItem, isPressed && styles.settingsItemPressed)}
			role="button"
			tabIndex={0}
			onClick={handleClick}
			onLongPress={handleLongPressCopy}
			onKeyDown={(event) => {
				if (!isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				onSelect();
			}}
			data-flx="app.mobile-settings-view.mobile-profile-settings-item.settings-item.select"
			{...pressableProps}
		>
			<StatusAwareAvatar
				size={32}
				user={currentUser}
				data-flx="app.mobile-settings-view.mobile-profile-settings-item.status-aware-avatar"
			/>
			<div
				className={styles.settingsItemContent}
				data-flx="app.mobile-settings-view.mobile-profile-settings-item.settings-item-content"
			>
				<div
					className={styles.settingsItemLabelContainer}
					data-flx="app.mobile-settings-view.mobile-profile-settings-item.settings-item-label-container"
				>
					<span
						className={styles.settingsItemLabel}
						data-flx="app.mobile-settings-view.mobile-profile-settings-item.settings-item-label"
					>
						{currentUser.displayName}
					</span>
				</div>
			</div>
			<ArrowLeftIcon
				className={styles.settingsItemArrow}
				data-flx="app.mobile-settings-view.mobile-profile-settings-item.settings-item-arrow"
			/>
		</LongPressable>
	);
});
const MobileSettingsList = observer(
	({
		groupedTabs,
		onTabSelect,
		scrollRef,
		onScroll,
	}: {
		groupedTabs: Record<string, Array<SettingsTab>>;
		onTabSelect: (tab: string, title: string) => void;
		scrollRef?: React.Ref<ScrollerHandle>;
		onScroll?: (event: UIEvent<HTMLDivElement>) => void;
	}) => {
		const {i18n} = useLingui();
		const currentUser = Users.currentUser;
		const shouldShowWhatsNew = hasWhatsNewEntries();
		const advancedSettingsHasNew = useMemo(
			() =>
				getAdvancedSettingItems().some((item) => isSettingsItemNew(item, Date.now(), currentUser?.createdAt ?? null)),
			[currentUser?.createdAt],
		);
		const mobileVisibleTabs = useMemo(() => {
			const visibleTabs: Record<string, Array<SettingsTab>> = {};
			Object.entries(groupedTabs).forEach(([category, tabs]) => {
				const filteredCategoryTabs = tabs.filter((tab) => !MOBILE_HIDDEN_TAB_TYPES.has(tab.type));
				if (filteredCategoryTabs.length > 0) {
					visibleTabs[category] = filteredCategoryTabs;
				}
			});
			return visibleTabs;
		}, [groupedTabs]);
		const handleLogout = useCallback(() => {
			ModalCommands.push(modal(() => <LogoutModal data-flx="app.mobile-settings-view.handle-logout.logout-modal" />));
		}, []);
		const categories = Object.entries(mobileVisibleTabs);
		const lastCategoryIndex = categories.length - 1;
		return (
			<Scroller
				className={styles.scrollerContainer}
				key="mobile-settings-list-scroller"
				ref={scrollRef}
				onScroll={onScroll}
				data-flx="app.mobile-settings-view.mobile-settings-list.scroller-container"
			>
				<div className={styles.profileSection} data-flx="app.mobile-settings-view.mobile-settings-list.profile-section">
					<div className={styles.categoryList} data-flx="app.mobile-settings-view.mobile-settings-list.profile-list">
						<MobileProfileSettingsItem
							onSelect={() => onTabSelect(PROFILE_SETTINGS_TAB, getUserSettingsTabLabel(i18n, PROFILE_SETTINGS_TAB))}
							data-flx="app.mobile-settings-view.mobile-settings-list.mobile-profile-settings-item.profile-tab-select"
						/>
					</div>
				</div>
				{categories.map(([category, tabs], categoryIndex) => (
					<div
						key={category}
						className={styles.categorySection}
						data-flx="app.mobile-settings-view.mobile-settings-list.category-section"
					>
						<h2
							className={styles.categoryTitle}
							data-flx="app.mobile-settings-view.mobile-settings-list.category-title"
						>
							{getCategoryLabel(category as SettingsTab['category'])}
						</h2>
						<div className={styles.categoryList} data-flx="app.mobile-settings-view.mobile-settings-list.category-list">
							{tabs.map((tab, index) => {
								const isLastTab = index === tabs.length - 1;
								const isLastCategory = categoryIndex === lastCategoryIndex;
								const badge =
									tab.type === 'gift_inventory' && currentUser?.hasUnreadGiftInventory ? (
										<MentionBadgeAnimated
											mentionCount={currentUser.unreadGiftInventoryCount ?? 1}
											data-flx="app.mobile-settings-view.mobile-settings-list.mention-badge-animated"
										/>
									) : tab.type === 'advanced_settings' && advancedSettingsHasNew ? (
										<SettingsStatusBadge
											kind="new"
											data-flx="app.mobile-settings-view.mobile-settings-list.settings-status-badge"
										/>
									) : undefined;
								return (
									<div key={tab.type} data-flx="app.mobile-settings-view.mobile-settings-list.div">
										<PressableSettingsItem
											tab={tab}
											onSelect={() => onTabSelect(tab.type, tab.label)}
											badge={badge}
											data-flx="app.mobile-settings-view.mobile-settings-list.pressable-settings-item.tab-select"
										/>
										{(!isLastTab || isLastCategory) && (
											<div
												className={styles.divider}
												data-flx="app.mobile-settings-view.mobile-settings-list.divider"
											/>
										)}
									</div>
								);
							})}
							{categoryIndex === lastCategoryIndex && (
								<>
									{shouldShowWhatsNew && (
										<>
											<MobileSettingsActionItem
												icon={MegaphoneIcon}
												iconWeight="fill"
												label={i18n._(WHAT_S_NEW_DESCRIPTOR)}
												onClick={openWhatsNewModal}
												data-flx="app.mobile-settings-view.mobile-settings-list.mobile-settings-action-item.open-whats-new-modal"
											/>
											<div
												className={styles.divider}
												data-flx="app.mobile-settings-view.mobile-settings-list.divider--2"
											/>
										</>
									)}
									<MobileSettingsDangerItem
										icon={SignOutIcon}
										label={i18n._(SIGN_OUT_DESCRIPTOR)}
										onClick={handleLogout}
										data-flx="app.mobile-settings-view.mobile-settings-list.mobile-settings-danger-item.logout"
									/>
								</>
							)}
						</div>
					</div>
				))}
				<div
					className={styles.categorySection}
					data-flx="app.mobile-settings-view.mobile-settings-list.category-section--2"
				>
					<h2
						className={styles.categoryTitle}
						data-flx="app.mobile-settings-view.mobile-settings-list.category-title--2"
					>
						{i18n._(DEBUG_DESCRIPTOR)}
					</h2>
					<div
						className={styles.categoryList}
						data-flx="app.mobile-settings-view.mobile-settings-list.category-list--2"
					>
						<ServiceWorkerUpdateButton data-flx="app.mobile-settings-view.mobile-settings-list.service-worker-update-button" />
					</div>
				</div>
				<div
					className={styles.clientInfoContainer}
					data-flx="app.mobile-settings-view.mobile-settings-list.client-info-container"
				>
					<ClientInfo data-flx="app.mobile-settings-view.mobile-settings-list.client-info" />
				</div>
			</Scroller>
		);
	},
);
const contentFadeVariants = {
	enter: {opacity: 0},
	center: {opacity: 1},
	exit: {opacity: 0},
};
const headerFadeVariants = {
	enter: {opacity: 0},
	center: {opacity: 1},
	exit: {opacity: 0},
};

interface MobileContentWithScrollSpyProps {
	scrollKey: string;
	settingsTabType?: UserSettingsTabType;
	initialGuildId?: string;
	initialSubtab?: string;
	currentTabComponent: React.ComponentType<Record<string, unknown>> | null;
	pendingSection?: string | null;
	onPendingSectionConsumed?: () => void;
}

const MobileContentWithScrollSpy: React.FC<MobileContentWithScrollSpyProps> = observer(
	({
		scrollKey,
		settingsTabType,
		initialGuildId,
		initialSubtab,
		currentTabComponent,
		pendingSection,
		onPendingSectionConsumed,
	}) => {
		useEffect(() => {
			if (!pendingSection) return;
			const frame = window.requestAnimationFrame(() => {
				const element = document.getElementById(pendingSection);
				if (element) {
					element.scrollIntoView({behavior: 'auto', block: 'start'});
				}
				onPendingSectionConsumed?.();
			});
			return () => window.cancelAnimationFrame(frame);
		}, [pendingSection, onPendingSectionConsumed]);
		return (
			<Scroller
				className={styles.scrollerFlex}
				key={scrollKey}
				data-settings-scroll-container
				data-flx="app.mobile-settings-view.mobile-content-with-scroll-spy.scroller-flex"
			>
				<div
					className={styles.contentContainer}
					data-flx="app.mobile-settings-view.mobile-content-with-scroll-spy.content-container"
				>
					{currentTabComponent &&
						(settingsTabType ? (
							<SettingsCurrentTabProvider
								value={settingsTabType}
								data-flx="app.mobile-settings-view.mobile-content-with-scroll-spy.settings-current-tab-provider"
							>
								{React.createElement(currentTabComponent, {
									settingsTabType,
									...(initialGuildId ? {initialGuildId} : {}),
									...(initialSubtab ? {initialSubtab} : {}),
								} as Record<string, unknown>)}
							</SettingsCurrentTabProvider>
						) : (
							React.createElement(currentTabComponent, {
								...(initialGuildId ? {initialGuildId} : {}),
								...(initialSubtab ? {initialSubtab} : {}),
							} as Record<string, unknown>)
						))}
				</div>
			</Scroller>
		);
	},
);
export const MobileSettingsView: React.FC<MobileSettingsViewProps> = observer(
	({
		groupedSettingsTabs,
		currentTab,
		mobileNav,
		onBack,
		onTabSelect,
		initialGuildId,
		initialSubtab,
		pendingSection,
		onPendingSectionConsumed,
	}) => {
		const {i18n} = useLingui();
		const currentTabId = mobileNav.currentView?.tab;
		const {showUnsavedBanner, flashBanner, tabData, checkUnsavedChanges} = useUnsavedChangesFlash(currentTabId);
		const {contentKey} = useSettingsContentKey();
		const [initialPendingSection, setInitialPendingSection] = useState<string | null>(() => initialSubtab ?? null);
		useEffect(() => {
			setInitialPendingSection(initialSubtab ?? null);
		}, [initialSubtab]);
		const scrollKey = useMemo(() => {
			if (!currentTabId) {
				return 'user-settings-mobile-root';
			}
			const subtabKey = contentKey ?? initialSubtab ?? 'root';
			return `user-settings-${currentTabId}-${subtabKey}`;
		}, [contentKey, currentTabId, initialSubtab]);
		const handleBack = useCallback(() => {
			if (checkUnsavedChanges()) return;
			if (currentTabId === 'applications' && ApplicationsTabState.isDetailView) {
				void ApplicationsTabState.navigateToList();
				return;
			}
			onBack();
		}, [checkUnsavedChanges, currentTabId, onBack]);
		const handleTabSelect = useCallback(
			(tab: string, title: string) => {
				if (checkUnsavedChanges()) return;
				onTabSelect(tab, title);
			},
			[checkUnsavedChanges, onTabSelect],
		);
		const pendingScrollSection = pendingSection ?? initialPendingSection;
		const handlePendingSectionConsumed = useCallback(() => {
			if (pendingSection) {
				onPendingSectionConsumed?.();
			}
			if (initialPendingSection) {
				setInitialPendingSection(null);
			}
		}, [initialPendingSection, onPendingSectionConsumed, pendingSection]);
		const showMobileList = mobileNav.isRootView;
		const showMobileContent = !mobileNav.isRootView;
		const mobileContentTitle =
			currentTabId === 'applications' && ApplicationsTabState.isDetailView
				? (ApplicationsTabState.selectedApplication?.name ?? i18n._(APPLICATION_DETAILS_DESCRIPTOR))
				: currentTab?.label || mobileNav.currentView?.title;
		const mobilePageLinkHref =
			currentTabId && !(currentTabId === 'applications' && ApplicationsTabState.isDetailView)
				? buildUserSettingsDeepLink(currentTabId)
				: null;
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
		const currentTabComponent = currentTab ? getSettingsTabComponent(currentTab.type) : null;
		return (
			<div className={userSettingsStyles.mobileWrapper} data-flx="app.mobile-settings-view.div">
				<div className={userSettingsStyles.mobileHeaderContainer} data-flx="app.mobile-settings-view.div--2">
					<AnimatePresence
						mode="wait"
						custom={mobileNav.direction}
						data-flx="app.mobile-settings-view.animate-presence"
					>
						{showMobileList && (
							<motion.div
								key="mobile-list-header"
								variants={headerFadeVariants}
								initial="center"
								animate="center"
								exit="exit"
								transition={{duration: 0.08, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileHeaderContent}
								data-flx="app.mobile-settings-view.div--3"
							>
								<MobileHeader
									title={i18n._(SETTINGS_DESCRIPTOR)}
									onBack={handleBack}
									data-flx="app.mobile-settings-view.mobile-header"
								/>
							</motion.div>
						)}
						{showMobileContent && currentTab && (
							<motion.div
								key={`mobile-content-header-${mobileNav.currentView?.tab}`}
								variants={headerFadeVariants}
								initial="enter"
								animate="center"
								exit="exit"
								transition={{duration: 0.08, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileHeaderContent}
								data-flx="app.mobile-settings-view.div--4"
							>
								<MobileHeaderWithBanner
									title={mobileContentTitle || currentTab.label}
									pageLinkHref={mobilePageLinkHref}
									onBack={handleBack}
									showUnsavedBanner={showUnsavedBanner}
									flashBanner={flashBanner}
									tabData={tabData}
									data-flx="app.mobile-settings-view.mobile-header-with-banner"
								/>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
				<div className={userSettingsStyles.mobileContentContainer} data-flx="app.mobile-settings-view.div--5">
					<AnimatePresence
						mode="wait"
						custom={mobileNav.direction}
						data-flx="app.mobile-settings-view.animate-presence--2"
					>
						{showMobileList && (
							<motion.div
								key="mobile-list-content"
								custom={mobileNav.direction}
								variants={contentFadeVariants}
								initial="center"
								animate="center"
								exit="exit"
								transition={{duration: 0.15, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileContentPane}
								data-flx="app.mobile-settings-view.div--6"
							>
								<MobileSettingsList
									groupedTabs={groupedSettingsTabs}
									onTabSelect={handleTabSelect}
									scrollRef={listScrollerRef}
									onScroll={handleListScroll}
									data-flx="app.mobile-settings-view.mobile-settings-list"
								/>
							</motion.div>
						)}
						{showMobileContent && currentTab && (
							<motion.div
								key={`mobile-content-${mobileNav.currentView?.tab}`}
								custom={mobileNav.direction}
								variants={contentFadeVariants}
								initial="enter"
								animate="center"
								exit="exit"
								transition={{duration: 0.15, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileContentPane}
								data-flx="app.mobile-settings-view.div--7"
							>
								<MobileContentWithScrollSpy
									scrollKey={scrollKey}
									settingsTabType={currentTab.type}
									initialGuildId={initialGuildId}
									initialSubtab={initialSubtab}
									currentTabComponent={currentTabComponent}
									pendingSection={pendingScrollSection}
									onPendingSectionConsumed={handlePendingSectionConsumed}
									data-flx="app.mobile-settings-view.mobile-content-with-scroll-spy"
								/>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
			</div>
		);
	},
);

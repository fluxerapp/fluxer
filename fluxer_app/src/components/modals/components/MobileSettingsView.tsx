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
import {ArrowClockwiseIcon, ArrowLeftIcon, BellSlashIcon, type IconWeight, SignOutIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {LongPressable} from '~/components/LongPressable';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ClientInfo} from '~/components/modals/components/ClientInfo';
import {LogoutModal} from '~/components/modals/components/LogoutModal';
import styles from '~/components/modals/components/MobileSettingsView.module.css';
import {ScrollSpyProvider, useScrollSpyContext} from '~/components/modals/hooks/ScrollSpyContext';
import type {MobileNavigationState} from '~/components/modals/hooks/useMobileNavigation';
import {useSettingsContentKey} from '~/components/modals/hooks/useSettingsContentKey';
import {
	MobileSectionNav,
	MobileSettingsDangerItem,
	MobileHeader as SharedMobileHeader,
} from '~/components/modals/shared/MobileSettingsComponents';
import userSettingsStyles from '~/components/modals/UserSettingsModal.module.css';
import {getSettingsTabComponent} from '~/components/modals/utils/desktopSettingsTabs';
import {
	getCategoryLabel,
	getSectionIdsForTab,
	getSectionsForTab,
	type SettingsTab,
	tabHasSections,
	type UserSettingsTabType,
} from '~/components/modals/utils/settingsConstants';
import {filterSettingsTabsForDeveloperMode} from '~/components/modals/utils/settingsTabFilters';
import {Button} from '~/components/uikit/Button/Button';
import {MentionBadgeAnimated} from '~/components/uikit/MentionBadge';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {usePressable} from '~/hooks/usePressable';
import {usePushSubscriptions} from '~/hooks/usePushSubscriptions';
import {activateLatestServiceWorker} from '~/lib/versioning';
import * as PushSubscriptionService from '~/services/push/PushSubscriptionService';
import AccessibilityStore from '~/stores/AccessibilityStore';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import UnsavedChangesStore from '~/stores/UnsavedChangesStore';
import UserStore from '~/stores/UserStore';
import {isPwaOnMobileOrTablet} from '~/utils/PwaUtils';

interface MobileSettingsViewProps {
	groupedSettingsTabs: Record<string, Array<SettingsTab>>;
	currentTab: SettingsTab | undefined;
	mobileNav: MobileNavigationState;
	onBack: () => void;
	onTabSelect: (tab: string, title: string) => void;
	initialGuildId?: string;
	initialSubtab?: string;
}

const MobileHeaderWithBanner = observer(
	({
		title,
		onBack,
		showBackButton = true,
		showUnsavedBanner = false,
		flashBanner = false,
		tabData = {},
	}: {
		title: string;
		onBack?: () => void;
		showBackButton?: boolean;
		showUnsavedBanner?: boolean;
		flashBanner?: boolean;
		tabData?: any;
	}) => {
		const {t} = useLingui();
		const prefersReducedMotion = AccessibilityStore.useReducedMotion;

		return (
			<div
				className={`safe-area-top ${styles.header}`}
				style={{
					transitionDuration: prefersReducedMotion ? '0ms' : '200ms',
					backgroundColor: showUnsavedBanner
						? flashBanner
							? 'var(--status-danger)'
							: 'var(--background-primary)'
						: 'var(--background-primary)',
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
							className={styles.headerContent}
						>
							<div className={styles.bannerTextContainer}>
								<div
									className={`${styles.bannerText} ${flashBanner ? styles.bannerTextWhite : styles.bannerTextPrimary}`}
								>
									<Trans>Careful! You have unsaved changes.</Trans>
								</div>
							</div>
							<div className={styles.bannerActions}>
								<Button variant="secondary" small={true} onClick={tabData.onReset}>
									<Trans>Reset</Trans>
								</Button>
								<Button small={true} onClick={tabData.onSave} submitting={tabData.isSubmitting}>
									<Trans>Save</Trans>
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
							className={styles.headerContentRelative}
						>
							{showBackButton && onBack && (
								<button type="button" onClick={onBack} className={styles.backButton} aria-label={t`Go back`}>
									<ArrowLeftIcon className={styles.icon5} />
								</button>
							)}
							<h1 className={styles.headerTitle}>{title}</h1>
							<div className={styles.headerSpacer} />
						</motion.div>
					)}
				</AnimatePresence>
			</div>
		);
	},
);

interface PressableSettingsItemProps {
	tab: SettingsTab;
	onSelect: () => void;
	badge?: React.ReactNode;
}

const PressableSettingsItem: React.FC<PressableSettingsItemProps> = observer(({tab, onSelect, badge}) => {
	const {isPressed, pressableProps} = usePressable();
	const IconComponent = tab.icon;

	return (
		<LongPressable
			className={clsx(styles.settingsItem, isPressed && styles.settingsItemPressed)}
			role="button"
			tabIndex={0}
			onClick={onSelect}
			{...pressableProps}
		>
			<IconComponent className={styles.settingsItemIcon} weight={tab.iconWeight ?? 'fill'} />
			<div className={styles.settingsItemContent}>
				<div className={styles.settingsItemLabelContainer}>
					<span className={styles.settingsItemLabel}>{tab.label}</span>
					{badge}
				</div>
			</div>
			<ArrowLeftIcon className={styles.settingsItemArrow} />
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
				{...pressableProps}
			>
				<IconComponent className={styles.settingsItemIcon} weight={iconWeight ?? 'regular'} />
				<div className={styles.settingsItemContent}>
					<div className={styles.settingsItemLabelContainer}>
						<span className={styles.settingsItemLabel}>{label}</span>
						{isLoading && <Spinner size="small" className={styles.settingsItemSpinner} />}
					</div>
				</div>
				{showArrow && <ArrowLeftIcon className={styles.settingsItemArrow} />}
			</LongPressable>
		);
	},
);

const ServiceWorkerUpdateButton = observer(() => {
	const {t} = useLingui();
	const [isUpdating, setIsUpdating] = React.useState(false);

	const handleUpdateServiceWorker = async () => {
		if (isUpdating) return;
		setIsUpdating(true);
		try {
			await activateLatestServiceWorker();
			window.location.reload();
		} catch (error) {
			console.error('Failed to update service worker:', error);
		} finally {
			setIsUpdating(false);
		}
	};

	return (
		<MobileSettingsActionItem
			icon={ArrowClockwiseIcon}
			iconWeight="bold"
			label={t`Update Service Worker`}
			onClick={handleUpdateServiceWorker}
			isLoading={isUpdating}
			showArrow={false}
		/>
	);
});

const MOBILE_HIDDEN_TAB_TYPES = new Set<UserSettingsTabType>(['keybinds']);

const MobileSettingsList = observer(
	({
		groupedTabs,
		onTabSelect,
	}: {
		groupedTabs: Record<string, Array<SettingsTab>>;
		onTabSelect: (tab: string, title: string) => void;
	}) => {
		const {t} = useLingui();
		const currentUser = UserStore.currentUser;
		const isDeveloper = DeveloperModeStore.isDeveloper;

		const filteredTabs = React.useMemo(
			() => filterSettingsTabsForDeveloperMode(groupedTabs, isDeveloper),
			[groupedTabs, isDeveloper],
		);

		const mobileVisibleTabs = React.useMemo(() => {
			const visibleTabs: Record<string, Array<SettingsTab>> = {};
			Object.entries(filteredTabs).forEach(([category, tabs]) => {
				const filteredCategoryTabs = tabs.filter((tab) => !MOBILE_HIDDEN_TAB_TYPES.has(tab.type));
				if (filteredCategoryTabs.length > 0) {
					visibleTabs[category] = filteredCategoryTabs;
				}
			});
			return visibleTabs;
		}, [filteredTabs]);

		const handleLogout = React.useCallback(() => {
			ModalActionCreators.push(modal(() => <LogoutModal />));
		}, []);

		const isPwaMobile = isPwaOnMobileOrTablet();
		const {subscriptions, refresh: refreshPushSubscriptions} = usePushSubscriptions(isPwaMobile);
		const showForgetPushAction = isPwaMobile && subscriptions.length > 0;

		const handleForgetPushSubscriptions = React.useCallback(() => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Forget push subscriptions?`}
						description={
							<p>
								<Trans>Clearing subscriptions ensures the gateway stops sending messages to this installation.</Trans>
							</p>
						}
						primaryText={t`Forget`}
						primaryVariant="primary"
						secondaryText={t`Cancel`}
						onPrimary={async () => {
							await PushSubscriptionService.unregisterAllPushSubscriptions();
							await refreshPushSubscriptions();
							NagbarActionCreators.dismissNagbar('desktopNotificationDismissed');
						}}
						onSecondary={() => {
							ModalActionCreators.pop();
						}}
					/>
				)),
			);
		}, [refreshPushSubscriptions, t]);

		const debugActions = React.useMemo(() => {
			const actions: Array<{key: string; element: React.ReactElement}> = [
				{key: 'update', element: <ServiceWorkerUpdateButton key="service-worker" />},
			];
			if (showForgetPushAction) {
				actions.push({
					key: 'forget',
					element: (
						<MobileSettingsActionItem
							icon={BellSlashIcon}
							iconWeight="fill"
							label={t`Forget Push Subscriptions`}
							onClick={handleForgetPushSubscriptions}
							showArrow={false}
						/>
					),
				});
			}
			return actions;
		}, [handleForgetPushSubscriptions, showForgetPushAction, t]);
		const lastDebugActionIndex = debugActions.length - 1;

		const categories = Object.entries(mobileVisibleTabs);
		const lastCategoryIndex = categories.length - 1;

		return (
			<Scroller className={styles.scrollerContainer} key="mobile-settings-list-scroller">
				{categories.map(([category, tabs], categoryIndex) => (
					<div key={category} className={styles.categorySection}>
						<h2 className={styles.categoryTitle}>{getCategoryLabel(category as SettingsTab['category'])}</h2>
						<div className={styles.categoryList}>
							{tabs.map((tab, index) => {
								const isLastTab = index === tabs.length - 1;
								const isLastCategory = categoryIndex === lastCategoryIndex;
								const badge =
									tab.type === 'gift_inventory' && currentUser?.hasUnreadGiftInventory ? (
										<MentionBadgeAnimated mentionCount={currentUser.unreadGiftInventoryCount ?? 1} />
									) : undefined;
								return (
									<div key={tab.type}>
										<PressableSettingsItem tab={tab} onSelect={() => onTabSelect(tab.type, tab.label)} badge={badge} />
										{(!isLastTab || isLastCategory) && <div className={styles.divider} />}
									</div>
								);
							})}
							{categoryIndex === lastCategoryIndex && (
								<MobileSettingsDangerItem icon={SignOutIcon} label={t`Log Out`} onClick={handleLogout} />
							)}
						</div>
					</div>
				))}
				{debugActions.length > 0 && (
					<div className={styles.categorySection}>
						<h2 className={styles.categoryTitle}>{t`Debug`}</h2>
						<div className={styles.categoryList}>
							{debugActions.map((action, index) => (
								<div key={action.key}>
									{action.element}
									{index !== lastDebugActionIndex && <div className={styles.divider} />}
								</div>
							))}
						</div>
					</div>
				)}
				<div className={styles.clientInfoContainer}>
					<ClientInfo />
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

interface MobileSectionNavWrapperProps {
	tabType: UserSettingsTabType;
}

const MobileSectionNavWrapper: React.FC<MobileSectionNavWrapperProps> = observer(({tabType}) => {
	const {t} = useLingui();
	const scrollSpyContext = useScrollSpyContext();
	const sections = getSectionsForTab(tabType, t);

	if (!scrollSpyContext || sections.length === 0) {
		return null;
	}

	const {activeSectionId, scrollToSection} = scrollSpyContext;

	return <MobileSectionNav sections={sections} activeSectionId={activeSectionId} onSectionClick={scrollToSection} />;
});

interface MobileContentWithScrollSpyProps {
	tabType: UserSettingsTabType;
	scrollKey: string;
	initialGuildId?: string;
	initialSubtab?: string;
	currentTabComponent: React.ComponentType<any> | null;
}

const MobileContentWithScrollSpy: React.FC<MobileContentWithScrollSpyProps> = observer(
	({tabType, scrollKey, initialGuildId, initialSubtab, currentTabComponent}) => {
		const scrollerRef = React.useRef<ScrollerHandle | null>(null);
		const scrollContainerRef = React.useRef<HTMLElement | null>(null);
		const sectionIds = React.useMemo(() => getSectionIdsForTab(tabType), [tabType]);
		const hasSections = tabHasSections(tabType);

		React.useEffect(() => {
			if (scrollerRef.current) {
				scrollContainerRef.current = scrollerRef.current.getScrollerNode();
			}
		});

		const content = (
			<Scroller ref={scrollerRef} className={styles.scrollerFlex} key={scrollKey} data-settings-scroll-container>
				{hasSections && <MobileSectionNavWrapper tabType={tabType} />}
				<div className={styles.contentContainer}>
					{currentTabComponent &&
						React.createElement(currentTabComponent, {
							...(initialGuildId ? {initialGuildId} : {}),
							...(initialSubtab ? {initialSubtab} : {}),
						} as any)}
				</div>
			</Scroller>
		);

		if (hasSections) {
			return (
				<ScrollSpyProvider sectionIds={sectionIds} containerRef={scrollContainerRef}>
					{content}
				</ScrollSpyProvider>
			);
		}

		return content;
	},
);

export const MobileSettingsView: React.FC<MobileSettingsViewProps> = observer(
	({groupedSettingsTabs, currentTab, mobileNav, onBack, onTabSelect, initialGuildId, initialSubtab}) => {
		const {t} = useLingui();
		const unsavedChangesStore = UnsavedChangesStore;
		const [flashBanner, setFlashBanner] = React.useState(false);
		const [lastFlashTrigger, setLastFlashTrigger] = React.useState(0);

		const currentTabId = mobileNav.currentView?.tab || '';
		const showUnsavedBanner = unsavedChangesStore.unsavedChanges[currentTabId] || false;
		const flashTrigger = unsavedChangesStore.flashTriggers[currentTabId] || 0;
		const tabData = unsavedChangesStore.tabData[currentTabId] || {};
		const {contentKey} = useSettingsContentKey();
		const scrollKey = React.useMemo(() => {
			if (!currentTabId) {
				return 'user-settings-mobile-root';
			}

			const subtabKey = contentKey ?? initialSubtab ?? 'root';
			return `user-settings-${currentTabId}-${subtabKey}`;
		}, [contentKey, currentTabId, initialSubtab]);

		React.useEffect(() => {
			if (flashTrigger > lastFlashTrigger) {
				setFlashBanner(true);
				setLastFlashTrigger(flashTrigger);
				setTimeout(() => setFlashBanner(false), 300);
			}
		}, [flashTrigger, lastFlashTrigger]);

		const checkUnsavedChanges = (tabId?: string): boolean => {
			const checkTabId = tabId || mobileNav.currentView?.tab;
			if (!checkTabId) return false;
			if (unsavedChangesStore.unsavedChanges[checkTabId]) {
				UnsavedChangesActionCreators.triggerFlashEffect(checkTabId);
				return true;
			}
			return false;
		};

		const handleBack = () => {
			if (checkUnsavedChanges()) return;
			onBack();
		};

		const handleTabSelect = (tab: string, title: string) => {
			if (checkUnsavedChanges()) return;
			onTabSelect(tab, title);
		};

		const showMobileList = mobileNav.isRootView;
		const showMobileContent = !mobileNav.isRootView;
		const currentTabComponent = currentTab ? getSettingsTabComponent(currentTab.type) : null;

		return (
			<div className={userSettingsStyles.mobileWrapper}>
				<div className={userSettingsStyles.mobileHeaderContainer}>
					<AnimatePresence mode="wait" custom={mobileNav.direction}>
						{showMobileList && (
							<motion.div
								key="mobile-list-header"
								variants={headerFadeVariants}
								initial="center"
								animate="center"
								exit="exit"
								transition={{duration: 0.08, ease: 'easeInOut'}}
								className={userSettingsStyles.mobileHeaderContent}
							>
								<SharedMobileHeader title={t`Settings`} onBack={() => ModalActionCreators.pop()} />
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
							>
								<MobileHeaderWithBanner
									title={mobileNav.currentView?.title || currentTab.label}
									onBack={handleBack}
									showUnsavedBanner={showUnsavedBanner}
									flashBanner={flashBanner}
									tabData={tabData}
								/>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
				<div className={userSettingsStyles.mobileContentContainer}>
					<AnimatePresence mode="wait" custom={mobileNav.direction}>
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
								style={{willChange: 'transform'}}
							>
								<MobileSettingsList groupedTabs={groupedSettingsTabs} onTabSelect={handleTabSelect} />
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
								style={{willChange: 'transform'}}
							>
								<MobileContentWithScrollSpy
									tabType={currentTab.type}
									scrollKey={scrollKey}
									initialGuildId={initialGuildId}
									initialSubtab={initialSubtab}
									currentTabComponent={currentTabComponent}
								/>
							</motion.div>
						)}
					</AnimatePresence>
				</div>
			</div>
		);
	},
);

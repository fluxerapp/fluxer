// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import headerStyles from '@app/features/app/components/dialogs/components/MobileSettingsView.module.css';
import styles from '@app/features/app/components/dialogs/shared/MobileSettingsComponents.module.css';
import {SettingsHeadingLinkButton} from '@app/features/app/components/dialogs/shared/SettingsHeadingLinkButton';
import {UnsavedChangesBannerContent} from '@app/features/app/components/dialogs/shared/UnsavedChangesBannerContent';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {usePressable} from '@app/features/app/hooks/usePressable';
import {GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {TabData} from '@app/features/ui/state/UnsavedChanges';
import type {SettingsSectionConfig} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {Trans, useLingui} from '@lingui/react/macro';
import type {Icon, IconWeight} from '@phosphor-icons/react';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type UIEvent, useEffect, useRef} from 'react';

interface MobileHeaderProps {
	title: React.ReactNode;
	onBack: () => void;
}

export const MobileHeader: React.FC<MobileHeaderProps> = observer(({title, onBack}) => {
	const {i18n} = useLingui();
	return (
		<div
			className={`safe-area-top ${styles.header}`}
			data-flx="app.mobile-settings-components.mobile-header.safe-area-top"
		>
			<div className={styles.headerContent} data-flx="app.mobile-settings-components.mobile-header.header-content">
				<button
					type="button"
					onClick={onBack}
					className={styles.backButton}
					aria-label={i18n._(GO_BACK_DESCRIPTOR)}
					data-flx="app.mobile-settings-components.mobile-header.back-button"
				>
					<ArrowLeftIcon
						className={styles.backButtonIcon}
						data-flx="app.mobile-settings-components.mobile-header.back-button-icon"
					/>
				</button>
				<h1 className={styles.headerTitle} data-flx="app.mobile-settings-components.mobile-header.header-title">
					{title}
				</h1>
				<div className={styles.headerSpacer} data-flx="app.mobile-settings-components.mobile-header.header-spacer" />
			</div>
		</div>
	);
});

interface MobileHeaderWithBannerProps {
	title: React.ReactNode;
	pageLinkHref?: string | null;
	onBack?: () => void;
	showBackButton?: boolean;
	showUnsavedBanner?: boolean;
	flashBanner?: boolean;
	tabData?: TabData;
}

export const MobileHeaderWithBanner: React.FC<MobileHeaderWithBannerProps> = observer(
	({
		title,
		pageLinkHref,
		onBack,
		showBackButton = true,
		showUnsavedBanner = false,
		flashBanner = false,
		tabData = {},
	}) => {
		const {i18n} = useLingui();
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const headerBackground = showUnsavedBanner && flashBanner ? 'var(--status-danger)' : 'var(--background-primary)';
		return (
			<div
				className={`safe-area-top ${headerStyles.header}`}
				style={{
					transitionDuration: prefersReducedMotion ? '0ms' : '200ms',
					backgroundColor: headerBackground,
				}}
				data-flx="app.mobile-settings-components.mobile-header-with-banner.safe-area-top"
			>
				<AnimatePresence
					mode="wait"
					data-flx="app.mobile-settings-components.mobile-header-with-banner.animate-presence"
				>
					{showUnsavedBanner ? (
						<motion.div
							key="banner"
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : {duration: 0.25, ease: 'easeOut'}}
							className={headerStyles.headerContent}
							data-flx="app.mobile-settings-components.mobile-header-with-banner.div"
						>
							<UnsavedChangesBannerContent
								tabData={tabData}
								textContainerClassName={headerStyles.bannerTextContainer}
								textClassName={`${headerStyles.bannerText} ${
									flashBanner ? headerStyles.bannerTextWhite : headerStyles.bannerTextPrimary
								}`}
								actionsClassName={headerStyles.bannerActions}
								smallActions={true}
								defaultSaveLabel={<Trans>Save</Trans>}
								dataFlx={{
									textContainer: 'app.mobile-settings-components.mobile-header-with-banner.div--2',
									text: 'app.mobile-settings-components.mobile-header-with-banner.div--3',
									actions: 'app.mobile-settings-components.mobile-header-with-banner.div--4',
									resetButton: 'app.mobile-settings-components.mobile-header-with-banner.button.reset',
									saveButton: 'app.mobile-settings-components.mobile-header-with-banner.button.save',
								}}
								data-flx="app.mobile-settings-components.mobile-header-with-banner.unsaved-changes-banner-content"
							/>
						</motion.div>
					) : (
						<motion.div
							key="title"
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={prefersReducedMotion ? {duration: 0} : {duration: 0.25, ease: 'easeOut'}}
							className={headerStyles.headerContentRelative}
							data-flx="app.mobile-settings-components.mobile-header-with-banner.div--5"
						>
							{showBackButton && onBack && (
								<button
									type="button"
									onClick={onBack}
									className={headerStyles.backButton}
									aria-label={i18n._(GO_BACK_DESCRIPTOR)}
									data-flx="app.mobile-settings-components.mobile-header-with-banner.button.back"
								>
									<ArrowLeftIcon
										className={headerStyles.icon5}
										data-flx="app.mobile-settings-components.mobile-header-with-banner.arrow-left-icon"
									/>
								</button>
							)}
							<h1
								className={headerStyles.headerTitle}
								data-flx="app.mobile-settings-components.mobile-header-with-banner.h1"
							>
								{title}
							</h1>
							<div
								className={headerStyles.headerActionSlot}
								data-flx="app.mobile-settings-components.mobile-header-with-banner.header-action-slot"
							>
								{pageLinkHref ? (
									<SettingsHeadingLinkButton
										href={pageLinkHref}
										target="page"
										data-flx="app.mobile-settings-components.mobile-header-with-banner.heading-link-button"
									/>
								) : null}
							</div>
						</motion.div>
					)}
				</AnimatePresence>
			</div>
		);
	},
);

interface SettingsTab {
	type: string;
	label: string;
	icon: React.ComponentType<{className?: string; weight?: IconWeight}>;
	iconWeight?: IconWeight;
}

interface PressableTabItemProps {
	tab: SettingsTab;
	onSelect: () => void;
}

const PressableTabItem: React.FC<PressableTabItemProps> = observer(({tab, onSelect}) => {
	const {isPressed, pressableProps} = usePressable();
	const IconComponent = tab.icon;
	return (
		<LongPressable
			className={clsx(styles.tabButton, isPressed && styles.tabButtonPressed)}
			role="button"
			tabIndex={0}
			onClick={onSelect}
			onKeyDown={(event) => {
				if (!isKeyboardActivationKey(event.key)) return;
				event.preventDefault();
				onSelect();
			}}
			data-flx="app.mobile-settings-components.pressable-tab-item.tab-button.select"
			{...pressableProps}
		>
			<IconComponent
				className={styles.tabIcon}
				weight={tab.iconWeight ?? 'fill'}
				data-flx="app.mobile-settings-components.pressable-tab-item.tab-icon"
			/>
			<div className={styles.tabContent} data-flx="app.mobile-settings-components.pressable-tab-item.tab-content">
				<div className={styles.tabLabel} data-flx="app.mobile-settings-components.pressable-tab-item.tab-label">
					{tab.label}
				</div>
			</div>
			<ArrowLeftIcon
				className={styles.tabArrow}
				data-flx="app.mobile-settings-components.pressable-tab-item.tab-arrow"
			/>
		</LongPressable>
	);
});

interface MobileSettingsDangerItemProps {
	icon: Icon;
	label: React.ReactNode;
	onClick: () => void;
}

export const MobileSettingsDangerItem: React.FC<MobileSettingsDangerItemProps> = observer(
	({icon: IconComponent, label, onClick}) => {
		const {isPressed, pressableProps} = usePressable();
		return (
			<LongPressable
				className={clsx(styles.dangerButton, isPressed && styles.dangerButtonPressed)}
				role="button"
				tabIndex={0}
				onClick={onClick}
				onKeyDown={(event) => {
					if (!isKeyboardActivationKey(event.key)) return;
					event.preventDefault();
					onClick();
				}}
				data-flx="app.mobile-settings-components.mobile-settings-danger-item.danger-button.click"
				{...pressableProps}
			>
				<IconComponent
					className={styles.dangerIcon}
					weight="fill"
					data-flx="app.mobile-settings-components.mobile-settings-danger-item.danger-icon"
				/>
				<div
					className={styles.dangerContent}
					data-flx="app.mobile-settings-components.mobile-settings-danger-item.danger-content"
				>
					<span
						className={styles.dangerLabel}
						data-flx="app.mobile-settings-components.mobile-settings-danger-item.danger-label"
					>
						{label}
					</span>
				</div>
			</LongPressable>
		);
	},
);

interface MobileSettingsListProps<T extends SettingsTab> {
	groupedTabs: Record<string, Array<T>>;
	onTabSelect: (tab: string, title: string) => void;
	footer?: React.ReactNode;
	categoryLabels?: Record<string, string>;
	hiddenCategories?: Array<string>;
	additionalContent?: React.ReactNode;
	dangerContent?: React.ReactNode;
	scrollRef?: React.Ref<ScrollerHandle>;
	onScroll?: (event: UIEvent<HTMLDivElement>) => void;
}

export const MobileSettingsList = observer(function MobileSettingsList<T extends SettingsTab>({
	groupedTabs,
	onTabSelect,
	footer,
	categoryLabels = {},
	hiddenCategories = [],
	additionalContent,
	dangerContent,
	scrollRef,
	onScroll,
}: MobileSettingsListProps<T>) {
	const categories = Object.entries(groupedTabs);
	const visibleCategoryIndexes = categories
		.map(([category], index) => (hiddenCategories.includes(category) ? -1 : index))
		.filter((index) => index >= 0);
	const lastVisibleCategoryIndex = visibleCategoryIndexes.length
		? visibleCategoryIndexes[visibleCategoryIndexes.length - 1]
		: -1;
	return (
		<Scroller
			className={styles.settingsList}
			key="mobile-settings-list-shared-scroller"
			ref={scrollRef}
			onScroll={onScroll}
			data-flx="app.mobile-settings-components.mobile-settings-list.settings-list"
		>
			{categories.map(([category, tabs], categoryIndex) => {
				const shouldHideCategory = hiddenCategories.includes(category);
				const categoryLabel = categoryLabels[category];
				const isLastVisibleCategory = categoryIndex === lastVisibleCategoryIndex;
				return (
					<div
						key={category}
						className={styles.categoryContainer}
						data-flx="app.mobile-settings-components.mobile-settings-list.category-container"
					>
						{!shouldHideCategory && categoryLabel && (
							<h2
								className={styles.categoryHeader}
								data-flx="app.mobile-settings-components.mobile-settings-list.category-header"
							>
								{categoryLabel}
							</h2>
						)}
						<div
							className={styles.categoryTabs}
							data-flx="app.mobile-settings-components.mobile-settings-list.category-tabs"
						>
							{tabs.map((tab, index) => {
								const isLast = index === tabs.length - 1;
								return (
									<div key={tab.type} data-flx="app.mobile-settings-components.mobile-settings-list.div">
										<PressableTabItem
											tab={tab}
											onSelect={() => onTabSelect(tab.type, tab.label)}
											data-flx="app.mobile-settings-components.mobile-settings-list.pressable-tab-item.tab-select"
										/>
										{!isLast && (
											<div
												className={styles.tabDivider}
												data-flx="app.mobile-settings-components.mobile-settings-list.tab-divider"
											/>
										)}
									</div>
								);
							})}
							{isLastVisibleCategory && dangerContent && tabs.length > 0 && (
								<div
									className={styles.tabDivider}
									data-flx="app.mobile-settings-components.mobile-settings-list.tab-divider--2"
								/>
							)}
							{isLastVisibleCategory && dangerContent}
						</div>
					</div>
				);
			})}
			{lastVisibleCategoryIndex === -1 && dangerContent && (
				<div
					className={styles.categoryContainer}
					data-flx="app.mobile-settings-components.mobile-settings-list.category-container--2"
				>
					<div
						className={styles.categoryTabs}
						data-flx="app.mobile-settings-components.mobile-settings-list.category-tabs--2"
					>
						{dangerContent}
					</div>
				</div>
			)}
			{additionalContent && (
				<div
					className={styles.extraContent}
					data-flx="app.mobile-settings-components.mobile-settings-list.additional-content"
				>
					{additionalContent}
				</div>
			)}
			{footer && (
				<div className={styles.footer} data-flx="app.mobile-settings-components.mobile-settings-list.footer">
					{footer}
				</div>
			)}
		</Scroller>
	);
});

interface MobileSectionNavProps {
	sections: ReadonlyArray<SettingsSectionConfig>;
	activeSectionId: string | null;
	onSectionClick: (sectionId: string) => void;
}

export const MobileSectionNav: React.FC<MobileSectionNavProps> = observer(
	({sections, activeSectionId, onSectionClick}) => {
		const scrollerRef = useRef<ScrollerHandle | null>(null);
		useEffect(() => {
			if (!activeSectionId) return;
			const node = scrollerRef.current?.getScrollerNode();
			if (!node) return;
			const activeButton = node.querySelector(`[data-section-id="${activeSectionId}"]`);
			if (activeButton instanceof HTMLElement) {
				activeButton.scrollIntoView({behavior: 'auto', block: 'nearest', inline: 'center'});
			}
		}, [activeSectionId]);
		return (
			<div
				className={styles.sectionNavContainer}
				data-flx="app.mobile-settings-components.mobile-section-nav.section-nav-container"
			>
				<Scroller
					key="mobile-settings-section-nav-scroller"
					ref={scrollerRef}
					className={styles.sectionNavScroller}
					orientation="horizontal"
					fade={false}
					data-flx="app.mobile-settings-components.mobile-section-nav.section-nav-scroller"
				>
					<div
						className={styles.sectionNavContent}
						data-flx="app.mobile-settings-components.mobile-section-nav.section-nav-content"
					>
						{sections.map((section) => (
							<FocusRing
								key={section.id}
								offset={-2}
								data-flx="app.mobile-settings-components.mobile-section-nav.focus-ring"
							>
								<button
									type="button"
									className={clsx(styles.sectionNavItem, activeSectionId === section.id && styles.sectionNavItemActive)}
									onClick={() => onSectionClick(section.id)}
									data-section-id={section.id}
									data-flx="app.mobile-settings-components.mobile-section-nav.section-nav-item.section-click.button"
								>
									{section.label}
								</button>
							</FocusRing>
						))}
					</div>
				</Scroller>
			</div>
		);
	},
);

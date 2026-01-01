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
import type {Icon, IconWeight} from '@phosphor-icons/react';
import {ArrowLeftIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {LongPressable} from '~/components/LongPressable';
import type {SettingsSectionConfig} from '~/components/modals/utils/settingsConstants';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {usePressable} from '~/hooks/usePressable';
import styles from './MobileSettingsComponents.module.css';

interface MobileHeaderProps {
	title: string;
	onBack: () => void;
}

export const MobileHeader: React.FC<MobileHeaderProps> = observer(({title, onBack}) => {
	const {t} = useLingui();
	return (
		<div className={`safe-area-top ${styles.header}`}>
			<div className={styles.headerContent}>
				<button type="button" onClick={onBack} className={styles.backButton} aria-label={t`Go back`}>
					<ArrowLeftIcon className={styles.backButtonIcon} />
				</button>
				<h1 className={styles.headerTitle}>{title}</h1>
				<div className={styles.headerSpacer} />
			</div>
		</div>
	);
});

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
			{...pressableProps}
		>
			<IconComponent className={styles.tabIcon} weight={tab.iconWeight ?? 'fill'} />
			<div className={styles.tabContent}>
				<div className={styles.tabLabel}>{tab.label}</div>
			</div>
			<ArrowLeftIcon className={styles.tabArrow} />
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
				{...pressableProps}
			>
				<IconComponent className={styles.dangerIcon} weight="fill" />
				<div className={styles.dangerContent}>
					<span className={styles.dangerLabel}>{label}</span>
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
}

export const MobileSettingsList = observer(function MobileSettingsList<T extends SettingsTab>({
	groupedTabs,
	onTabSelect,
	footer,
	categoryLabels = {},
	hiddenCategories = [],
	additionalContent,
	dangerContent,
}: MobileSettingsListProps<T>) {
	const categories = Object.entries(groupedTabs);
	const visibleCategoryIndexes = categories
		.map(([category], index) => (hiddenCategories.includes(category) ? -1 : index))
		.filter((index) => index >= 0);
	const lastVisibleCategoryIndex = visibleCategoryIndexes.length
		? visibleCategoryIndexes[visibleCategoryIndexes.length - 1]
		: -1;

	return (
		<Scroller className={styles.settingsList} key="mobile-settings-list-shared-scroller">
			{categories.map(([category, tabs], categoryIndex) => {
				const shouldHideCategory = hiddenCategories.includes(category);
				const categoryLabel = categoryLabels[category];
				const isLastVisibleCategory = categoryIndex === lastVisibleCategoryIndex;

				return (
					<div key={category} className={styles.categoryContainer}>
						{!shouldHideCategory && categoryLabel && <h2 className={styles.categoryHeader}>{categoryLabel}</h2>}
						<div className={styles.categoryTabs}>
							{tabs.map((tab, index) => {
								const isLast = index === tabs.length - 1;
								return (
									<div key={tab.type}>
										<PressableTabItem tab={tab} onSelect={() => onTabSelect(tab.type, tab.label)} />
										{!isLast && <div className={styles.tabDivider} />}
									</div>
								);
							})}
							{isLastVisibleCategory && dangerContent && tabs.length > 0 && <div className={styles.tabDivider} />}
							{isLastVisibleCategory && dangerContent}
						</div>
					</div>
				);
			})}

			{lastVisibleCategoryIndex === -1 && dangerContent && (
				<div className={styles.categoryContainer}>
					<div className={styles.categoryTabs}>{dangerContent}</div>
				</div>
			)}

			{additionalContent && <div className={styles.additionalContent}>{additionalContent}</div>}

			{footer && <div className={styles.footer}>{footer}</div>}
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
		const scrollerRef = React.useRef<ScrollerHandle | null>(null);

		React.useEffect(() => {
			if (!activeSectionId) return;
			const node = scrollerRef.current?.getScrollerNode();
			if (!node) return;
			const activeButton = node.querySelector(`[data-section-id="${activeSectionId}"]`);
			if (activeButton instanceof HTMLElement) {
				activeButton.scrollIntoView({behavior: 'smooth', block: 'nearest', inline: 'center'});
			}
		}, [activeSectionId]);

		return (
			<div className={styles.sectionNavContainer}>
				<Scroller ref={scrollerRef} className={styles.sectionNavScroller} orientation="horizontal" fade={false}>
					<div className={styles.sectionNavContent}>
						{sections.map((section) => (
							<FocusRing key={section.id} offset={-2}>
								<button
									type="button"
									className={clsx(styles.sectionNavItem, activeSectionId === section.id && styles.sectionNavItemActive)}
									onClick={() => onSectionClick(section.id)}
									data-section-id={section.id}
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

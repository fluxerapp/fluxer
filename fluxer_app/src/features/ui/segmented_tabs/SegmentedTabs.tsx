// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/ui/segmented_tabs/SegmentedTabs.module.css';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import type React from 'react';
import {useRef} from 'react';

export interface SegmentedTab<T extends string = string> {
	id: T;
	label: string;
}

interface SegmentedTabsProps<T extends string = string> {
	tabs: Array<SegmentedTab<T>>;
	selectedTab: T;
	onTabChange: (tab: T) => void;
	ariaLabel?: string;
	className?: string;
	semantics?: 'tabs' | 'radio';
}

export function SegmentedTabs<T extends string = string>({
	tabs,
	selectedTab,
	onTabChange,
	ariaLabel,
	className,
	semantics = 'tabs',
}: SegmentedTabsProps<T>) {
	const radio = semantics === 'radio';
	const tabRefs = useRef(new Map<T, HTMLButtonElement>());
	const selectedIndex = tabs.findIndex((tab) => tab.id === selectedTab);
	const focusTab = (tabId: T) => {
		tabRefs.current.get(tabId)?.focus();
	};
	const handleKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>, tabId: T) => {
		const currentIndex = tabs.findIndex((tab) => tab.id === tabId);
		if (currentIndex < 0) return;
		const direction = getTabNavigationDirection(event.key, 'horizontal');
		if (!direction) return;
		const nextIndex = getNextTabIndex(currentIndex, tabs.length, direction);
		if (nextIndex == null) return;
		event.preventDefault();
		event.stopPropagation();
		const nextTab = tabs[nextIndex];
		if (!nextTab) return;
		onTabChange(nextTab.id);
		focusTab(nextTab.id);
	};
	return (
		<div className={clsx(styles.container, className)} data-flx="ui.segmented-tabs.segmented-tabs.container">
			<div
				className={styles.tabList}
				{...(radio
					? {role: 'radiogroup', 'aria-label': ariaLabel}
					: {role: 'tablist', 'aria-label': ariaLabel, 'aria-orientation': 'horizontal' as const})}
				data-flx="ui.segmented-tabs.segmented-tabs.tab-list"
			>
				{tabs.map((tab) => {
					const isSelected = selectedTab === tab.id;
					return (
						<button
							key={tab.id}
							ref={(element) => {
								if (element) {
									tabRefs.current.set(tab.id, element);
								} else {
									tabRefs.current.delete(tab.id);
								}
							}}
							type="button"
							{...(radio ? {role: 'radio', 'aria-checked': isSelected} : {role: 'tab', 'aria-selected': isSelected})}
							tabIndex={isSelected ? 0 : -1}
							onClick={() => onTabChange(tab.id)}
							onKeyDown={(event) => handleKeyDown(event, tab.id)}
							className={clsx(styles.tab, isSelected ? styles.tabActive : styles.tabInactive)}
							data-flx="ui.segmented-tabs.segmented-tabs.tab.button"
						>
							{tab.label}
						</button>
					);
				})}
				<motion.div
					className={styles.tabBackground}
					layout
					transition={
						Accessibility.useReducedMotion
							? {duration: 0}
							: {
									type: 'spring',
									stiffness: 500,
									damping: 35,
								}
					}
					style={{
						width: `calc((100% - 6px) / ${tabs.length})`,
						insetInlineStart: `calc(3px + (100% - 6px) * ${selectedIndex} / ${tabs.length})`,
					}}
					data-flx="ui.segmented-tabs.segmented-tabs.tab-background"
				/>
			</div>
		</div>
	);
}

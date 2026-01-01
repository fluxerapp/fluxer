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

import {clsx} from 'clsx';
import React from 'react';
import {Scroller} from '~/components/uikit/Scroller';
import styles from './Tabs.module.css';

export interface TabItem<T extends string> {
	key: T;
	label: string | React.ReactNode;
}

export interface TabsProps<T extends string> {
	tabs: Array<TabItem<T>>;
	activeTab: T;
	onTabChange: (tab: T) => void;
	className?: string;
	renderTabSibling?: (tab: T) => React.ReactNode;
}

export function Tabs<T extends string>({tabs, activeTab, onTabChange, className, renderTabSibling}: TabsProps<T>) {
	const tabRefs = React.useRef<Map<T, HTMLButtonElement>>(new Map());

	const focusTab = (key: T) => {
		tabRefs.current.get(key)?.focus();
	};

	const getNextKey = (currentKey: T, direction: 'next' | 'prev' | 'first' | 'last'): T => {
		const currentIndex = tabs.findIndex((tab) => tab.key === currentKey);
		switch (direction) {
			case 'next':
				return tabs[(currentIndex + 1) % tabs.length].key;
			case 'prev':
				return tabs[(currentIndex - 1 + tabs.length) % tabs.length].key;
			case 'first':
				return tabs[0].key;
			case 'last':
				return tabs[tabs.length - 1].key;
		}
	};

	const handleKeyDown = (event: React.KeyboardEvent, tabKey: T) => {
		let nextKey: T | null = null;

		switch (event.key) {
			case 'ArrowLeft':
				nextKey = getNextKey(tabKey, 'prev');
				break;
			case 'ArrowRight':
				nextKey = getNextKey(tabKey, 'next');
				break;
			case 'Home':
				nextKey = getNextKey(tabKey, 'first');
				break;
			case 'End':
				nextKey = getNextKey(tabKey, 'last');
				break;
			default:
				return;
		}

		event.preventDefault();
		onTabChange(nextKey);
		focusTab(nextKey);
	};

	return (
		<Scroller orientation="horizontal" fade key="tabs-horizontal-scroller">
			<div role="tablist" aria-orientation="horizontal" className={clsx(styles.container, className)}>
				{tabs.map(({key, label}) => {
					const isSelected = key === activeTab;
					const sibling = renderTabSibling?.(key);
					return (
						<React.Fragment key={key}>
							<button
								ref={(el) => {
									if (el) {
										tabRefs.current.set(key, el);
									} else {
										tabRefs.current.delete(key);
									}
								}}
								type="button"
								role="tab"
								aria-selected={isSelected}
								tabIndex={isSelected ? 0 : -1}
								className={clsx(styles.tab, isSelected && styles.selected)}
								onClick={() => onTabChange(key)}
								onKeyDown={(e) => handleKeyDown(e, key)}
							>
								{label}
							</button>
							{sibling}
						</React.Fragment>
					);
				})}
			</div>
		</Scroller>
	);
}

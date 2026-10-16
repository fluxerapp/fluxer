// SPDX-License-Identifier: AGPL-3.0-or-later

import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import type {Icon, IconWeight} from '@phosphor-icons/react';
import type React from 'react';
import {useEffect, useRef} from 'react';

export interface SettingsTreeApi {
	isExpanded: (tabId: string) => boolean;
	expand: (tabId: string) => void;
	collapse: (tabId: string) => void;
	toggle: (tabId: string) => void;
}

export function expandSettingsTreeTab(expandedTab: string | null, tabId: string): string {
	return expandedTab === tabId ? expandedTab : tabId;
}

export function collapseSettingsTreeTab(expandedTab: string | null, tabId: string): string | null {
	return expandedTab === tabId ? null : expandedTab;
}

export function toggleSettingsTreeTab(expandedTab: string | null, tabId: string): string | null {
	return expandedTab === tabId ? null : tabId;
}

export function syncSettingsTreeExpansionToActiveTab(
	expandedTab: string | null,
	activeTabId: string | null,
): string | null {
	return expandedTab === activeTabId ? expandedTab : activeTabId;
}

export interface SidebarKeyboardEvent {
	key: string;
	altKey: boolean;
	ctrlKey: boolean;
	metaKey: boolean;
	shiftKey: boolean;
	target: EventTarget | null;
	currentTarget: EventTarget | null;
	preventDefault: () => void;
	stopPropagation: () => void;
}

export interface SettingsModalSidebarItemProps {
	label: React.ReactNode;
	icon: Icon;
	iconWeight?: IconWeight;
	selected?: boolean;
	danger?: boolean;
	autoSelectOnKeyboardNavigation?: boolean;
	onClick?: () => void;
	onRequestContentFocus?: () => void;
	id?: string;
	controlsId?: string;
	expandableId?: string;
	sectionsGroupId?: string;
	toggleOnSelectedClick?: boolean;
	attentionLabel?: string;
}

export interface SettingsModalSidebarItemLogicState {
	tabIndex: number;
	buttonRef: React.RefObject<HTMLButtonElement | null>;
}

const SIDEBAR_LIST_SELECTOR = '[data-settings-sidebar-list]';
const SIDEBAR_ITEM_SELECTOR = '[data-settings-sidebar-item="true"]';
const SIDEBAR_TAB_SELECTOR = '[data-settings-tab="true"]';
const SIDEBAR_NAVIGATION_KEY_ALIASES: Record<string, string> = {
	Down: 'ArrowDown',
	Left: 'ArrowLeft',
	Right: 'ArrowRight',
	Up: 'ArrowUp',
};

const isTabItem = (element: HTMLElement): boolean => element.matches(SIDEBAR_TAB_SELECTOR);
const isSectionItem = (element: HTMLElement): boolean => element.hasAttribute('data-section-id');
const isSelectedTabItem = (element: HTMLElement): boolean => element.dataset.selected === 'true';
const isExpandableTabItem = (element: HTMLElement): boolean => element.dataset.expandable === 'true';
const isTabItemExpanded = (element: HTMLElement): boolean => element.getAttribute('aria-expanded') === 'true';

const isDisabledSidebarItem = (element: HTMLElement): boolean => {
	if (element.getAttribute('aria-disabled') === 'true') return true;
	if (element instanceof HTMLButtonElement) return element.disabled;
	return false;
};
const isHiddenSidebarItem = (element: HTMLElement): boolean => {
	if (element.hidden || element.closest('[hidden], [aria-hidden="true"], [inert]')) return true;
	const style = window.getComputedStyle(element);
	return style.display === 'none' || style.visibility === 'hidden';
};
const getSidebarList = (element: HTMLElement | null): HTMLElement | null => {
	if (!element) return null;
	if (element.matches(SIDEBAR_LIST_SELECTOR)) return element;
	return element.closest<HTMLElement>(SIDEBAR_LIST_SELECTOR);
};
const getSidebarItems = (list: HTMLElement): Array<HTMLElement> => {
	return Array.from(list.querySelectorAll<HTMLElement>(SIDEBAR_ITEM_SELECTOR)).filter(
		(element) => !isDisabledSidebarItem(element) && !isHiddenSidebarItem(element),
	);
};
const getSidebarItemFromElement = (list: HTMLElement, element: HTMLElement | null): HTMLElement | null => {
	const item = element?.closest<HTMLElement>(SIDEBAR_ITEM_SELECTOR) ?? null;
	if (!item || !list.contains(item) || isDisabledSidebarItem(item) || isHiddenSidebarItem(item)) return null;
	return item;
};
const getSelectedSidebarTab = (list: HTMLElement): HTMLElement | null => {
	return list.querySelector<HTMLElement>(`${SIDEBAR_TAB_SELECTOR}[data-selected="true"]`);
};
const getRovingFallbackItem = (list: HTMLElement): HTMLElement | null => {
	return getSidebarItems(list)[0] ?? null;
};
const getCurrentSidebarItem = (list: HTMLElement, eventTarget: HTMLElement | null): HTMLElement | null => {
	const targetItem = getSidebarItemFromElement(list, eventTarget);
	if (targetItem) return targetItem;
	const activeItem = getSidebarItemFromElement(list, document.activeElement as HTMLElement | null);
	if (activeItem) return activeItem;
	return getSelectedSidebarTab(list) ?? getRovingFallbackItem(list);
};
const findFirstChildItem = (items: Array<HTMLElement>, index: number): HTMLElement | null => {
	const next = items[index + 1];
	return next && isSectionItem(next) ? next : null;
};
const findParentTabItem = (items: Array<HTMLElement>, index: number): HTMLElement | null => {
	for (let i = index - 1; i >= 0; i--) {
		const candidate = items[i];
		if (candidate && !isSectionItem(candidate)) return candidate;
	}
	return null;
};
const focusSidebarItem = (target: HTMLElement): void => {
	target.focus({preventScroll: true});
	target.scrollIntoView({block: 'nearest', inline: 'nearest'});
};
const moveFocusToItem = (list: HTMLElement, target: HTMLElement): void => {
	syncSidebarTabStops(list, target);
	focusSidebarItem(target);
};
const moveFocusToTabAndSelect = (list: HTMLElement, target: HTMLElement): void => {
	moveFocusToItem(list, target);
	if (isTabItem(target) && !isSelectedTabItem(target) && target.dataset.autoSelectOnKeyboardNavigation !== 'false') {
		target.click();
	}
};

export function syncSidebarTabStops(list: HTMLElement | null, preferredItem?: HTMLElement | null): HTMLElement | null {
	if (!list) return null;
	const items = getSidebarItems(list);
	if (!items.length) return null;
	const preferred = preferredItem && items.includes(preferredItem) ? preferredItem : null;
	const active = getSidebarItemFromElement(list, document.activeElement as HTMLElement | null);
	const selected = getSelectedSidebarTab(list);
	const target = preferred ?? active ?? selected ?? getRovingFallbackItem(list) ?? items[0] ?? null;
	if (!target) return null;
	for (const item of items) {
		item.tabIndex = item === target ? 0 : -1;
	}
	return target;
}

const normalizeSidebarNavigationKey = (key: string): string => SIDEBAR_NAVIGATION_KEY_ALIASES[key] ?? key;
const isTextInputElement = (element: HTMLElement | null): element is HTMLInputElement | HTMLTextAreaElement => {
	if (!element) return false;
	if (element.isContentEditable) return true;
	if (element.tagName === 'TEXTAREA') return true;
	if (element.tagName === 'INPUT') {
		const type = ((element as HTMLInputElement).type || '').toLowerCase();
		return type === '' || type === 'text' || type === 'search' || type === 'url' || type === 'email' || type === 'tel';
	}
	return false;
};

export function useSettingsModalSidebarItemLogic({
	selected,
}: Pick<SettingsModalSidebarItemProps, 'selected'>): SettingsModalSidebarItemLogicState {
	const ref = useRef<HTMLButtonElement>(null);
	useEffect(() => {
		if (selected) {
			ref.current?.scrollIntoView({block: 'nearest'});
		}
	}, [selected]);
	return {
		tabIndex: selected ? 0 : -1,
		buttonRef: ref,
	};
}

export function handleSettingsTreeKeyDown(event: SidebarKeyboardEvent, treeApi?: SettingsTreeApi): boolean {
	if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return false;
	const key = normalizeSidebarNavigationKey(event.key);
	const eventTarget = event.target instanceof HTMLElement ? event.target : null;
	if (isTextInputElement(eventTarget)) return false;
	const currentTarget = event.currentTarget instanceof HTMLElement ? event.currentTarget : null;
	const list = getSidebarList(currentTarget);
	if (!list) return false;
	const items = getSidebarItems(list);
	if (!items.length) return false;
	const current = getCurrentSidebarItem(list, eventTarget);
	if (!current) return false;
	const currentIndex = items.indexOf(current);
	if (currentIndex === -1) return false;

	if (key === 'ArrowRight') {
		if (!isExpandableTabItem(current)) return false;
		if (isTabItemExpanded(current)) {
			const child = findFirstChildItem(items, currentIndex);
			if (!child) return false;
			event.preventDefault();
			event.stopPropagation();
			moveFocusToItem(list, child);
			return true;
		}
		const tabId = current.dataset.tabId;
		if (!tabId || !treeApi) return false;
		event.preventDefault();
		event.stopPropagation();
		treeApi.expand(tabId);
		return true;
	}

	if (key === 'ArrowLeft') {
		if (isSectionItem(current)) {
			const parent = findParentTabItem(items, currentIndex);
			if (!parent) return false;
			event.preventDefault();
			event.stopPropagation();
			moveFocusToItem(list, parent);
			return true;
		}
		if (!isExpandableTabItem(current) || !isTabItemExpanded(current)) return false;
		const tabId = current.dataset.tabId;
		if (!tabId || !treeApi) return false;
		event.preventDefault();
		event.stopPropagation();
		treeApi.collapse(tabId);
		return true;
	}

	const direction = getTabNavigationDirection(key, 'vertical');
	if (!direction) return false;
	const nextIndex = getNextTabIndex(currentIndex, items.length, direction);
	if (nextIndex == null) return false;
	const target = items[nextIndex];
	if (!target) return false;
	event.preventDefault();
	event.stopPropagation();
	moveFocusToTabAndSelect(list, target);
	return true;
}

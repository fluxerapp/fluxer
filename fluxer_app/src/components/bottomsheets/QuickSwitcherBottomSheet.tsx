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
import {MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as QuickSwitcherActionCreators from '~/actions/QuickSwitcherActionCreators';
import {QuickSwitcherResultTypes} from '~/Constants';
import {Input} from '~/components/form/Input';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {MentionBadge} from '~/components/uikit/MentionBadge';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {SegmentedTabs} from '~/components/uikit/SegmentedTabs/SegmentedTabs';
import * as Sheet from '~/components/uikit/Sheet/Sheet';
import {useListNavigation} from '~/hooks/useListNavigation';
import type {QuickSwitcherExecutableResult, QuickSwitcherResult} from '~/stores/QuickSwitcherStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import ReadStateStore from '~/stores/ReadStateStore';
import {FriendsListContent} from '~/utils/friends/FriendsListUtils';
import {
	createSections,
	getChannelId,
	getQuickSwitcherTabs,
	getResultKey,
	getViewContext,
	handleContextMenu,
	type QuickSwitcherSection,
	renderIcon,
	useQuickSwitcherInputFocus,
} from '~/utils/quick-switcher/QuickSwitcherModalUtils';
import styles from './QuickSwitcherBottomSheet.module.css';

type QuickSwitcherContainerStyle = React.CSSProperties & {
	'--quick-switcher-scroll-padding-bottom'?: string;
};

const QUICK_SWITCHER_SCROLL_PADDING_BOTTOM = 'calc(env(safe-area-inset-bottom, 0px) + 1.25rem)';

const ResultRow = observer(
	({
		result,
		index,
		isKeyboardSelected,
		isHovered,
		onHover,
		onMouseLeave,
		onConfirm,
		innerRef,
	}: {
		result: QuickSwitcherResult;
		index: number;
		isKeyboardSelected: boolean;
		isHovered: boolean;
		onHover: (index: number) => void;
		onMouseLeave: () => void;
		onConfirm: (result: QuickSwitcherExecutableResult) => void;
		innerRef?: React.Ref<HTMLButtonElement>;
	}) => {
		if (result.type === QuickSwitcherResultTypes.HEADER) {
			return (
				<div key={getResultKey(result)} className={styles.sectionHeader}>
					{result.title}
				</div>
			);
		}

		const executableResult = result as QuickSwitcherExecutableResult;
		const channelId = getChannelId(executableResult);
		const unreadCount = channelId ? ReadStateStore.getUnreadCount(channelId) : 0;
		const mentionCount = channelId ? ReadStateStore.getMentionCount(channelId) : 0;
		const hasUnread = unreadCount > 0 || mentionCount > 0;
		const isActive = isKeyboardSelected || isHovered;
		const isHighlight = hasUnread && !isActive;

		const handleMouseEnter = () => onHover(index);
		const handleClick = (event: React.MouseEvent) => {
			event.preventDefault();
			onConfirm(executableResult);
		};

		const iconRendered = renderIcon(executableResult, isHighlight, styles.optionIcon, styles.optionIconHighlight);

		const key = getViewContext(executableResult)
			? `${executableResult.type}-${getViewContext(executableResult)}-${executableResult.id}`
			: `${executableResult.type}-${executableResult.id}`;

		return (
			<FocusRing offset={-2} enabled={false}>
				<button
					type="button"
					className={styles.option}
					ref={innerRef}
					onMouseEnter={handleMouseEnter}
					onMouseLeave={onMouseLeave}
					onMouseDown={(event) => event.preventDefault()}
					onClick={handleClick}
					onContextMenu={(event) => handleContextMenu(event, executableResult)}
					key={key}
				>
					<div className={styles.optionContent}>
						{iconRendered.type === 'avatar' ? (
							<div className={styles.avatar}>{iconRendered.content}</div>
						) : iconRendered.type === 'guild' ? (
							<div className={styles.guildIcon}>{iconRendered.content}</div>
						) : (
							iconRendered.content
						)}
						<div className={clsx(styles.optionText, isHighlight && styles.optionHighlight)}>
							<div className={styles.optionTitle}>{executableResult.title}</div>
							{executableResult.subtitle && <div className={styles.optionDescription}>{executableResult.subtitle}</div>}
						</div>
						{mentionCount > 0 && !isActive && <MentionBadge mentionCount={mentionCount} size="small" />}
					</div>
				</button>
			</FocusRing>
		);
	},
);

interface QuickSwitcherBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const QuickSwitcherBottomSheet: React.FC<QuickSwitcherBottomSheetProps> = observer(({isOpen, onClose}) => {
	const {t} = useLingui();
	const {query, results, selectedIndex} = QuickSwitcherStore;
	const inputRef = React.useRef<HTMLInputElement>(null);
	const scrollerRef = React.useRef<ScrollerHandle>(null);
	const rowRefs = React.useRef<Array<HTMLButtonElement | null>>([]);
	const shouldScrollToSelection = React.useRef(false);
	const [activeTab, setActiveTab] = React.useState<'search' | 'friends'>('search');
	const [friendsSearchQuery, setFriendsSearchQuery] = React.useState('');

	const {
		keyboardFocusIndex,
		hoverIndexForRender,
		handleMouseEnter: handleHoverIndex,
		handleMouseLeave,
		setSelectedIndex: setKeyboardIndex,
	} = useListNavigation({
		itemCount: results.length,
		initialIndex: selectedIndex >= 0 ? selectedIndex : 0,
		loop: true,
	});

	if (rowRefs.current.length !== results.length) {
		rowRefs.current = Array(results.length).fill(null);
	}

	React.useEffect(() => {
		if (results.length === 0) {
			setKeyboardIndex(-1);
			handleMouseLeave();
			return;
		}
		const clamped = selectedIndex >= 0 ? Math.min(selectedIndex, results.length - 1) : 0;
		setKeyboardIndex(clamped);
	}, [handleMouseLeave, results.length, selectedIndex, setKeyboardIndex]);

	React.useEffect(() => {
		handleMouseLeave();
	}, [handleMouseLeave, results.length]);

	useQuickSwitcherInputFocus(isOpen, true, activeTab, inputRef);

	React.useEffect(() => {
		if (!isOpen || activeTab !== 'search') return;
		const timeout = window.setTimeout(() => {
			if (document.activeElement !== inputRef.current) {
				inputRef.current?.focus();
				inputRef.current?.select();
			}
		}, 0);
		return () => window.clearTimeout(timeout);
	}, [activeTab, isOpen]);

	const handleQueryChange = React.useCallback((value: string) => {
		QuickSwitcherActionCreators.search(value);
	}, []);

	const handleKeyDown = React.useCallback(async (event: React.KeyboardEvent<HTMLInputElement>) => {
		switch (event.key) {
			case 'ArrowDown':
				event.preventDefault();
				shouldScrollToSelection.current = true;
				QuickSwitcherActionCreators.moveSelection('down');
				break;
			case 'ArrowUp':
				event.preventDefault();
				shouldScrollToSelection.current = true;
				QuickSwitcherActionCreators.moveSelection('up');
				break;
			case 'Tab':
			case 'Enter':
				event.preventDefault();
				await QuickSwitcherActionCreators.confirmSelection();
				break;
			case 'Escape':
				event.preventDefault();
				QuickSwitcherActionCreators.hide();
				break;
			default:
				break;
		}
	}, []);

	const handleSelect = React.useCallback(
		(index: number) => {
			shouldScrollToSelection.current = false;
			handleHoverIndex(index);
		},
		[handleHoverIndex],
	);

	const handleConfirm = React.useCallback((result: QuickSwitcherExecutableResult) => {
		void QuickSwitcherActionCreators.switchTo(result);
	}, []);

	React.useEffect(() => {
		if (!shouldScrollToSelection.current || keyboardFocusIndex < 0) {
			shouldScrollToSelection.current = false;
			return;
		}
		if (activeTab !== 'search') {
			shouldScrollToSelection.current = false;
			return;
		}
		shouldScrollToSelection.current = false;
		const node = rowRefs.current[keyboardFocusIndex];
		if (node) {
			scrollerRef.current?.scrollIntoViewNode({node: node as HTMLElement, padding: 32});
		}
	}, [keyboardFocusIndex, activeTab]);

	const sections = React.useMemo(() => createSections(results), [results]);

	React.useEffect(() => {
		if (!isOpen) {
			setActiveTab('search');
			setFriendsSearchQuery('');
		}
	}, [isOpen]);

	const friendsInputRef = React.useRef<HTMLInputElement>(null);
	const isSearchTab = activeTab === 'search';
	const inputPlaceholder = isSearchTab ? t`Search for channels, people, or communities` : t`Search friends`;

	const handleTabChange = React.useCallback((tab: 'search' | 'friends') => {
		setActiveTab(tab);
	}, []);

	const handleInputChange = (value: string) => {
		if (isSearchTab) {
			handleQueryChange(value);
		} else {
			setFriendsSearchQuery(value);
		}
	};

	const handleInputKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
		if (isSearchTab) {
			handleKeyDown(event);
		}
	};

	const handleInputClear = () => {
		handleInputChange('');
		if (isSearchTab) {
			inputRef.current?.focus();
			inputRef.current?.select();
		} else {
			friendsInputRef.current?.focus();
			friendsInputRef.current?.select();
		}
	};

	const clearButtonLabel = isSearchTab ? t`Clear search input` : t`Clear friends search`;

	const renderClearButton = () => (
		<button type="button" className={styles.searchClearButton} onClick={handleInputClear} aria-label={clearButtonLabel}>
			<XIcon size={16} weight="bold" color="currentColor" />
		</button>
	);

	const containerStyle: QuickSwitcherContainerStyle = React.useMemo(
		() => ({'--quick-switcher-scroll-padding-bottom': QUICK_SWITCHER_SCROLL_PADDING_BOTTOM}),
		[],
	);

	return (
		<Sheet.Root isOpen={isOpen} onClose={onClose} snapPoints={[0, 1]} initialSnap={1}>
			<Sheet.Handle />
			<Sheet.Content padding="none">
				<div className={styles.container} style={containerStyle}>
					<div className={styles.tabsContainer}>
						<SegmentedTabs
							tabs={getQuickSwitcherTabs(t)}
							selectedTab={activeTab}
							onTabChange={handleTabChange}
							ariaLabel={t`Quick switcher tabs`}
						/>
					</div>
					<div className={styles.panels}>
						<div className={clsx(styles.searchPanel, !isSearchTab && styles.panelHidden)}>
							<div className={styles.searchContainer}>
								<Input
									ref={inputRef}
									value={query}
									onChange={(event) => handleInputChange(event.target.value)}
									onKeyDown={handleInputKeyDown}
									placeholder={inputPlaceholder}
									className={styles.searchInput}
									leftIcon={<MagnifyingGlassIcon size={18} weight="bold" color="currentColor" />}
									rightElement={query.length > 0 ? renderClearButton() : undefined}
									spellCheck={false}
									autoComplete="off"
									inputMode="search"
								/>
							</div>
							<Scroller ref={scrollerRef} className={styles.scroller} key="quick-switcher-sheet-scroller">
								<div className={styles.scrollContent}>
									{results.length === 0 ? (
										<div className={styles.emptyState}>
											<div className={styles.emptyStateTitle}>{t`No matches found`}</div>
											<div className={styles.emptyStateHint}>
												{t`Try a different name or use @ / # / ! / * prefixes to filter results.`}
											</div>
										</div>
									) : (
										sections.map((section: QuickSwitcherSection, sidx: number) => (
											<div key={`section-${sidx}`} className={styles.section}>
												{section.header && <div className={styles.sectionHeader}>{section.header.title}</div>}
												<div className={styles.sectionList}>
													{section.rows.map(({result, index}) => (
														<ResultRow
															key={getResultKey(result)}
															result={result}
															index={index}
															isKeyboardSelected={index === keyboardFocusIndex}
															isHovered={index === hoverIndexForRender}
															onHover={handleSelect}
															onMouseLeave={handleMouseLeave}
															onConfirm={handleConfirm}
															innerRef={(node) => {
																rowRefs.current[index] = node;
															}}
														/>
													))}
												</div>
											</div>
										))
									)}
								</div>
							</Scroller>
						</div>
						<div className={clsx(styles.friendsPanel, isSearchTab && styles.panelHidden)}>
							<div className={styles.searchContainer}>
								<Input
									ref={friendsInputRef}
									value={friendsSearchQuery}
									onChange={(event) => handleInputChange(event.target.value)}
									placeholder={t`Search friends`}
									className={styles.searchInput}
									leftIcon={<MagnifyingGlassIcon size={18} weight="bold" color="currentColor" />}
									rightElement={friendsSearchQuery.length > 0 ? renderClearButton() : undefined}
									spellCheck={false}
									autoComplete="off"
								/>
							</div>
							<FriendsListContent
								variant="embedded"
								className={styles.friendsContent}
								showSearch={false}
								showHeader={false}
								searchQuery={friendsSearchQuery}
								onSearchChange={setFriendsSearchQuery}
							/>
						</div>
					</div>
				</div>
			</Sheet.Content>
		</Sheet.Root>
	);
});

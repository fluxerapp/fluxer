// SPDX-License-Identifier: AGPL-3.0-or-later

import {useListNavigation} from '@app/features/app/hooks/useListNavigation';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {SEARCH_FRIENDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {shouldDisableAutofocusOnMobile} from '@app/features/platform/utils/AutofocusUtils';
import {FriendsListContent} from '@app/features/relationship/utils/FriendsListUtils';
import * as QuickSwitcherCommands from '@app/features/search/commands/QuickSwitcherCommands';
import styles from '@app/features/search/components/bottomsheets/QuickSwitcherBottomSheet.module.css';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import type {QuickSwitcherExecutableResult, QuickSwitcherResult} from '@app/features/search/state/QuickSwitcherTypes';
import {
	createSections,
	dismissQuickSwitcher,
	getQuickSwitcherResultAccessibilityMetadata,
	getQuickSwitcherTabs,
	getResultKey,
	getViewContext,
	handleContextMenu,
	type QuickSwitcherSection,
	renderIcon,
	useQuickSwitcherInputFocus,
} from '@app/features/search/utils/QuickSwitcherModalUtils';
import {CloseIcon, SearchIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {Input} from '@app/features/ui/components/form/FormInput';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {SegmentedTabs} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import * as Sheet from '@app/features/ui/sheet/Sheet';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const SEARCH_FOR_CHANNELS_PEOPLE_OR_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Search for channels, people, or communities',
	comment: 'Placeholder text in the mobile quick switcher search input.',
});
const SEARCHING_PEOPLE_DESCRIPTOR = msg({
	message: 'Searching people...',
	comment: 'Loading state shown while the quick switcher fetches matching people. Trailing ellipsis is intentional.',
});
const NO_MATCHES_FOUND_DESCRIPTOR = msg({
	message: 'No matches found',
	comment: 'Empty state title in the quick switcher when no results match the query.',
});
const MESSAGE_1_RESULT_AVAILABLE_DESCRIPTOR = msg({
	message: '1 result available',
	comment: 'Screen-reader live region announcement when exactly one quick switcher result is available.',
});
const RESULTS_AVAILABLE_DESCRIPTOR = msg({
	message: '{resultCount} results available',
	comment: 'Screen-reader live region announcement listing the quick switcher result count.',
});
const NO_AUTOCOMPLETE_SUGGESTION_DESCRIPTOR = msg({
	message: 'No autocomplete suggestion',
	comment: 'Screen-reader announcement in the quick switcher when no autocomplete suggestion is active.',
});
const AUTOCOMPLETE_SUGGESTION_FOR_OF_DESCRIPTOR = msg({
	message: 'Autocomplete suggestion for {trimmedQuery}: {label}, {selectedOptionPosition} of {resultCount}',
	comment:
		'Screen-reader announcement for the highlighted quick switcher autocomplete suggestion including its position.',
});
const AUTOCOMPLETE_SUGGESTION_FOR_DESCRIPTOR = msg({
	message: 'Autocomplete suggestion for {trimmedQuery}: {label}',
	comment: 'Screen-reader announcement for the highlighted quick switcher autocomplete suggestion.',
});
const AUTOCOMPLETE_SUGGESTION_OF_DESCRIPTOR = msg({
	message: 'Autocomplete suggestion: {label}, {selectedOptionPosition} of {resultCount}',
	comment:
		'Screen-reader announcement for the highlighted autocomplete suggestion including position, without query echo.',
});
const AUTOCOMPLETE_SUGGESTION_DESCRIPTOR = msg({
	message: 'Autocomplete suggestion: {label}',
	comment: 'Screen-reader announcement for the highlighted autocomplete suggestion, without query echo.',
});
const CLEAR_SEARCH_INPUT_DESCRIPTOR = msg({
	message: 'Clear search input',
	comment: 'Accessible label for the clear button in the quick switcher main search input.',
});
const CLEAR_FRIENDS_SEARCH_DESCRIPTOR = msg({
	message: 'Clear friends search',
	comment: 'Accessible label for the clear button in the friends-tab search input of the quick switcher.',
});
const QUICK_SWITCHER_TABS_DESCRIPTOR = msg({
	message: 'Quick switcher tabs',
	comment: 'Accessible label for the tablist in the mobile quick switcher.',
});
const QUICK_SWITCHER_SEARCH_DESCRIPTOR = msg({
	message: 'Quick switcher search',
	comment: 'Accessible label for the quick switcher search input region.',
});
const USE_UP_AND_DOWN_ARROWS_TO_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Arrow keys navigate. Enter opens, Escape closes.',
	comment: 'Screen-reader instructions describing keyboard navigation in the quick switcher result list.',
});
const TRY_A_DIFFERENT_NAME_OR_USE_PREFIXES_TO_DESCRIPTOR = msg({
	message: 'Try a different name or use @ / # / ! / * prefixes to filter results.',
	comment:
		'Empty state hint in the quick switcher describing the @, #, !, and * prefix filters. Keep the literal prefix characters.',
});
const QUICK_SWITCHER_RESULTS_DESCRIPTOR = msg({
	message: 'Quick switcher results',
	comment: 'Accessible label for the quick switcher result listbox.',
});

type QuickSwitcherContainerStyle = React.CSSProperties & {
	'--quick_switcher-scroll-padding-bottom'?: string;
};

const QUICK_SWITCHER_SCROLL_PADDING_BOTTOM = 'calc(env(safe-area-inset-bottom, 0px) + 1.25rem)';
const getQuickSwitcherOptionId = (baseId: string, index: number): string => `${baseId}-option-${index}`;
const ResultRow = observer(
	({
		result,
		index,
		isKeyboardSelected,
		isHovered,
		onHover,
		onMouseLeave,
		onConfirm,
		optionId,
		positionInSet,
		setSize,
		innerRef,
	}: {
		result: QuickSwitcherResult;
		index: number;
		isKeyboardSelected: boolean;
		isHovered: boolean;
		onHover: (index: number) => void;
		onMouseLeave: () => void;
		onConfirm: (result: QuickSwitcherExecutableResult) => void;
		optionId: string;
		positionInSet: number;
		setSize: number;
		innerRef?: React.Ref<HTMLButtonElement>;
	}) => {
		const {i18n} = useLingui();
		if (result.type === QuickSwitcherResultTypes.HEADER) {
			return (
				<div
					key={getResultKey(result)}
					className={styles.sectionHeader}
					data-flx="search.quick-switcher-bottom-sheet.result-row.section-header"
				>
					{result.title}
				</div>
			);
		}
		const executableResult = result as QuickSwitcherExecutableResult;
		const resultMetadata = getQuickSwitcherResultAccessibilityMetadata(executableResult, i18n);
		const {label: optionLabel, mentionCount, unreadCount} = resultMetadata;
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
			<FocusRing offset={-2} enabled={false} data-flx="search.quick-switcher-bottom-sheet.result-row.focus-ring">
				<button
					id={optionId}
					type="button"
					role="option"
					className={styles.option}
					aria-label={optionLabel}
					aria-selected={isKeyboardSelected}
					aria-posinset={positionInSet}
					aria-setsize={setSize}
					ref={innerRef}
					onMouseEnter={handleMouseEnter}
					onMouseLeave={onMouseLeave}
					onMouseDown={(event) => {
						if (event.button === 0) event.preventDefault();
					}}
					onClick={handleClick}
					onContextMenu={(event) => handleContextMenu(event, executableResult)}
					key={key}
					tabIndex={-1}
					data-flx="search.quick-switcher-bottom-sheet.result-row.option.click.button"
				>
					<div className={styles.optionContent} data-flx="search.quick-switcher-bottom-sheet.result-row.option-content">
						{iconRendered.type === 'avatar' ? (
							<div className={styles.avatar} data-flx="search.quick-switcher-bottom-sheet.result-row.avatar">
								{iconRendered.content}
							</div>
						) : iconRendered.type === 'guild' ? (
							<div className={styles.guildIcon} data-flx="search.quick-switcher-bottom-sheet.result-row.guild-icon">
								{iconRendered.content}
							</div>
						) : (
							iconRendered.content
						)}
						<div
							className={clsx(styles.optionText, isHighlight && styles.optionHighlight)}
							data-flx="search.quick-switcher-bottom-sheet.result-row.option-text"
						>
							<div className={styles.optionTitle} data-flx="search.quick-switcher-bottom-sheet.result-row.option-title">
								{executableResult.title}
							</div>
							{executableResult.subtitle && (
								<div
									className={styles.optionDescription}
									data-flx="search.quick-switcher-bottom-sheet.result-row.option-description"
								>
									{executableResult.subtitle}
								</div>
							)}
						</div>
						{mentionCount > 0 && !isActive && (
							<MentionBadge
								mentionCount={mentionCount}
								size="small"
								data-flx="search.quick-switcher-bottom-sheet.result-row.mention-badge"
							/>
						)}
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
	const {i18n} = useLingui();
	const {query, results, selectedIndex} = QuickSwitcher;
	const quickSwitcherId = useId();
	const inputRef = useRef<HTMLInputElement>(null);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const rowRefs = useRef<Array<HTMLButtonElement | null>>([]);
	const shouldScrollToSelection = useRef(false);
	const directMessagesDisabled = RuntimeConfig.directMessagesDisabled;
	const [activeTab, setActiveTab] = useState<'search' | 'friends'>('search');
	const [friendsSearchQuery, setFriendsSearchQuery] = useState('');
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
	useEffect(() => {
		if (results.length === 0) {
			setKeyboardIndex(-1);
			handleMouseLeave();
			return;
		}
		const clamped = selectedIndex >= 0 ? Math.min(selectedIndex, results.length - 1) : 0;
		setKeyboardIndex(clamped);
	}, [handleMouseLeave, results.length, selectedIndex, setKeyboardIndex]);
	useEffect(() => {
		handleMouseLeave();
	}, [handleMouseLeave, results.length]);
	useQuickSwitcherInputFocus(isOpen, true, activeTab, inputRef);
	useEffect(() => {
		if (shouldDisableAutofocusOnMobile()) {
			return;
		}
		if (!isOpen || activeTab !== 'search') return;
		const timeout = window.setTimeout(() => {
			if (document.activeElement !== inputRef.current) {
				inputRef.current?.focus();
				inputRef.current?.select();
			}
		}, 0);
		return () => window.clearTimeout(timeout);
	}, [activeTab, isOpen]);
	const handleQueryChange = useCallback((value: string) => {
		QuickSwitcherCommands.search(value);
	}, []);
	const handleKeyDown = useCallback(async (event: React.KeyboardEvent<HTMLInputElement>) => {
		if (isIMEComposing(event)) {
			return;
		}
		switch (event.key) {
			case 'ArrowDown':
				event.preventDefault();
				shouldScrollToSelection.current = true;
				QuickSwitcherCommands.moveSelection('down');
				break;
			case 'ArrowUp':
				event.preventDefault();
				shouldScrollToSelection.current = true;
				QuickSwitcherCommands.moveSelection('up');
				break;
			case 'Tab':
			case 'Enter':
				event.preventDefault();
				await QuickSwitcherCommands.confirmSelection();
				break;
			case 'Escape':
				event.preventDefault();
				event.stopPropagation();
				dismissQuickSwitcher({isEscape: true});
				break;
			default:
				break;
		}
	}, []);
	const handleHover = useCallback(
		(index: number) => {
			shouldScrollToSelection.current = false;
			handleHoverIndex(index);
		},
		[handleHoverIndex],
	);
	const handleConfirm = useCallback((result: QuickSwitcherExecutableResult) => {
		void QuickSwitcherCommands.switchTo(result);
	}, []);
	useEffect(() => {
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
			scrollerRef.current?.revealElement({node: node as HTMLElement, padding: 32});
		}
	}, [keyboardFocusIndex, activeTab]);
	const sections = useMemo(() => createSections(results), [results]);
	useEffect(() => {
		if (!isOpen) {
			setActiveTab('search');
			setFriendsSearchQuery('');
		}
	}, [isOpen]);
	const friendsInputRef = useRef<HTMLInputElement>(null);
	const isSearchTab = activeTab === 'search' || directMessagesDisabled;
	const inputPlaceholder = isSearchTab
		? i18n._(SEARCH_FOR_CHANNELS_PEOPLE_OR_COMMUNITIES_DESCRIPTOR)
		: i18n._(SEARCH_FRIENDS_DESCRIPTOR);
	const listboxId = `${quickSwitcherId}-results`;
	const statusId = `${quickSwitcherId}-status`;
	const suggestionStatusId = `${quickSwitcherId}-suggestion-status`;
	const hintId = `${quickSwitcherId}-hint`;
	const selectableIndices = useMemo(
		() =>
			results.reduce<Array<number>>((indices, result, index) => {
				if (result.type !== QuickSwitcherResultTypes.HEADER) {
					indices.push(index);
				}
				return indices;
			}, []),
		[results],
	);
	const selectedOptionPosition = keyboardFocusIndex >= 0 ? selectableIndices.indexOf(keyboardFocusIndex) + 1 : 0;
	const resultCount = useMemo(
		() => results.filter((result) => result.type !== QuickSwitcherResultTypes.HEADER).length,
		[results],
	);
	const hasSearchIntent = query.trim().length > 0;
	const activeDescendant =
		isSearchTab && keyboardFocusIndex >= 0 && results[keyboardFocusIndex]?.type !== QuickSwitcherResultTypes.HEADER
			? getQuickSwitcherOptionId(listboxId, keyboardFocusIndex)
			: undefined;
	const resultStatus = QuickSwitcher.isLoadingMemberResults
		? i18n._(SEARCHING_PEOPLE_DESCRIPTOR)
		: resultCount === 0
			? hasSearchIntent
				? i18n._(NO_MATCHES_FOUND_DESCRIPTOR)
				: ''
			: resultCount === 1
				? i18n._(MESSAGE_1_RESULT_AVAILABLE_DESCRIPTOR)
				: i18n._(RESULTS_AVAILABLE_DESCRIPTOR, {resultCount});
	const activeSuggestionStatus = useMemo(() => {
		if (!isSearchTab) {
			return '';
		}
		const result = results[keyboardFocusIndex];
		if (!result || result.type === QuickSwitcherResultTypes.HEADER) {
			return resultCount === 0 && hasSearchIntent ? i18n._(NO_AUTOCOMPLETE_SUGGESTION_DESCRIPTOR) : '';
		}
		const label = getQuickSwitcherResultAccessibilityMetadata(result as QuickSwitcherExecutableResult, i18n).label;
		const trimmedQuery = query.trim();
		if (trimmedQuery.length > 0) {
			return selectedOptionPosition > 0
				? i18n._(AUTOCOMPLETE_SUGGESTION_FOR_OF_DESCRIPTOR, {trimmedQuery, label, selectedOptionPosition, resultCount})
				: i18n._(AUTOCOMPLETE_SUGGESTION_FOR_DESCRIPTOR, {trimmedQuery, label});
		}
		return selectedOptionPosition > 0
			? i18n._(AUTOCOMPLETE_SUGGESTION_OF_DESCRIPTOR, {label, selectedOptionPosition, resultCount})
			: i18n._(AUTOCOMPLETE_SUGGESTION_DESCRIPTOR, {label});
	}, [
		hasSearchIntent,
		i18n.locale,
		isSearchTab,
		keyboardFocusIndex,
		query,
		resultCount,
		results,
		selectedOptionPosition,
	]);
	const handleTabChange = useCallback((tab: 'search' | 'friends') => {
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
	const clearButtonLabel = isSearchTab
		? i18n._(CLEAR_SEARCH_INPUT_DESCRIPTOR)
		: i18n._(CLEAR_FRIENDS_SEARCH_DESCRIPTOR);
	const renderClearButton = () => (
		<button
			type="button"
			className={styles.searchClearButton}
			onClick={handleInputClear}
			aria-label={clearButtonLabel}
			data-flx="search.quick-switcher-bottom-sheet.render-clear-button.search-clear-button.input-clear"
		>
			<CloseIcon size={16} data-flx="search.quick-switcher-bottom-sheet.render-clear-button.close-icon" />
		</button>
	);
	const containerStyle: QuickSwitcherContainerStyle = useMemo(
		() => ({'--quick_switcher-scroll-padding-bottom': QUICK_SWITCHER_SCROLL_PADDING_BOTTOM}),
		[],
	);
	return (
		<Sheet.Root
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			data-flx="search.quick-switcher-bottom-sheet.sheet-root"
		>
			<Sheet.Handle data-flx="search.quick-switcher-bottom-sheet.sheet-handle" />
			<Sheet.Content padding="none" data-flx="search.quick-switcher-bottom-sheet.sheet-content">
				<div
					className={styles.container}
					style={containerStyle}
					data-flx="search.quick-switcher-bottom-sheet.container"
				>
					{!directMessagesDisabled && (
						<div className={styles.tabsContainer} data-flx="search.quick-switcher-bottom-sheet.tabs-container">
							<SegmentedTabs
								tabs={getQuickSwitcherTabs(i18n)}
								selectedTab={activeTab}
								onTabChange={handleTabChange}
								ariaLabel={i18n._(QUICK_SWITCHER_TABS_DESCRIPTOR)}
								data-flx="search.quick-switcher-bottom-sheet.segmented-tabs"
							/>
						</div>
					)}
					<div className={styles.panels} data-flx="search.quick-switcher-bottom-sheet.panels">
						<div
							className={clsx(styles.searchPanel, !isSearchTab && styles.panelHidden)}
							data-flx="search.quick-switcher-bottom-sheet.search-panel"
						>
							<div className={styles.searchContainer} data-flx="search.quick-switcher-bottom-sheet.search-container">
								<Input
									ref={inputRef}
									value={query}
									onChange={(event) => handleInputChange(event.target.value)}
									onKeyDown={handleInputKeyDown}
									placeholder={inputPlaceholder}
									className={styles.searchInput}
									leftIcon={<SearchIcon size={18} data-flx="search.quick-switcher-bottom-sheet.search-icon" />}
									rightElement={query['length'] > 0 ? renderClearButton() : undefined}
									spellCheck={false}
									autoComplete="off"
									inputMode="search"
									role="combobox"
									aria-autocomplete="list"
									aria-haspopup="listbox"
									aria-controls={resultCount > 0 ? listboxId : undefined}
									aria-describedby={`${statusId} ${suggestionStatusId} ${hintId}`}
									aria-expanded={resultCount > 0}
									aria-label={i18n._(QUICK_SWITCHER_SEARCH_DESCRIPTOR)}
									aria-activedescendant={activeDescendant}
									data-flx="search.quick-switcher-bottom-sheet.search-input.input-change"
								/>
								<div id={hintId} className={styles.srOnly} data-flx="search.quick-switcher-bottom-sheet.sr-only">
									{i18n._(USE_UP_AND_DOWN_ARROWS_TO_CHOOSE_A_DESCRIPTOR)}
								</div>
								<div
									id={statusId}
									className={styles.srOnly}
									role="status"
									aria-live="polite"
									aria-atomic="true"
									data-flx="search.quick-switcher-bottom-sheet.sr-only--2"
								>
									{resultStatus}
								</div>
								<div
									id={suggestionStatusId}
									className={styles.srOnly}
									role="status"
									aria-live="polite"
									aria-atomic="true"
									data-flx="search.quick-switcher-bottom-sheet.sr-only--3"
								>
									{activeSuggestionStatus}
								</div>
							</div>
							{results.length === 0 && !hasSearchIntent ? null : (
								<Scroller
									ref={scrollerRef}
									className={styles.scroller}
									key="quick_switcher-sheet-scroller"
									data-flx="search.quick-switcher-bottom-sheet.scroller"
								>
									<div className={styles.scrollContent} data-flx="search.quick-switcher-bottom-sheet.scroll-content">
										{results.length === 0 ? (
											<div className={styles.emptyState} data-flx="search.quick-switcher-bottom-sheet.empty-state">
												<div
													className={styles.emptyStateTitle}
													data-flx="search.quick-switcher-bottom-sheet.empty-state-title"
												>
													{i18n._(NO_MATCHES_FOUND_DESCRIPTOR)}
												</div>
												<div
													className={styles.emptyStateHint}
													data-flx="search.quick-switcher-bottom-sheet.empty-state-hint"
												>
													{i18n._(TRY_A_DIFFERENT_NAME_OR_USE_PREFIXES_TO_DESCRIPTOR)}
												</div>
											</div>
										) : (
											<div
												id={listboxId}
												role="listbox"
												aria-label={i18n._(QUICK_SWITCHER_RESULTS_DESCRIPTOR)}
												data-flx="search.quick-switcher-bottom-sheet.listbox"
											>
												{sections.map((section: QuickSwitcherSection) => (
													<div
														key={section.key}
														className={styles.section}
														role={section.header ? 'group' : 'presentation'}
														data-flx="search.quick-switcher-bottom-sheet.section"
														{...(section.header ? {'aria-labelledby': `${listboxId}-${section.key}`} : {})}
													>
														{section.header && (
															<div
																id={`${listboxId}-${section.key}`}
																className={styles.sectionHeader}
																data-flx="search.quick-switcher-bottom-sheet.section-header"
															>
																{section.header.title}
															</div>
														)}
														<div
															className={styles.sectionList}
															role="presentation"
															data-flx="search.quick-switcher-bottom-sheet.section-list"
														>
															{section.rows.map(
																({result, index}: {result: QuickSwitcherExecutableResult; index: number}) => (
																	<ResultRow
																		key={getResultKey(result)}
																		result={result}
																		index={index}
																		isKeyboardSelected={index === keyboardFocusIndex}
																		isHovered={index === hoverIndexForRender}
																		onHover={handleHover}
																		onMouseLeave={handleMouseLeave}
																		onConfirm={handleConfirm}
																		optionId={getQuickSwitcherOptionId(listboxId, index)}
																		positionInSet={selectableIndices.indexOf(index) + 1}
																		setSize={resultCount}
																		innerRef={(node) => {
																			rowRefs.current[index] = node;
																		}}
																		data-flx="search.quick-switcher-bottom-sheet.result-row"
																	/>
																),
															)}
														</div>
													</div>
												))}
											</div>
										)}
									</div>
								</Scroller>
							)}
						</div>
						{!directMessagesDisabled && (
							<div
								className={clsx(styles.friendsPanel, isSearchTab && styles.panelHidden)}
								data-flx="search.quick-switcher-bottom-sheet.friends-panel"
							>
								<div
									className={styles.searchContainer}
									data-flx="search.quick-switcher-bottom-sheet.search-container--2"
								>
									<Input
										ref={friendsInputRef}
										value={friendsSearchQuery}
										onChange={(event) => handleInputChange(event.target.value)}
										placeholder={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
										className={styles.searchInput}
										leftIcon={<SearchIcon size={18} data-flx="search.quick-switcher-bottom-sheet.search-icon--2" />}
										rightElement={friendsSearchQuery.length > 0 ? renderClearButton() : undefined}
										spellCheck={false}
										autoComplete="off"
										aria-label={i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
										data-flx="search.quick-switcher-bottom-sheet.search-input.input-change--2"
									/>
								</div>
								<FriendsListContent
									variant="embedded"
									className={styles.friendsContent}
									showSearch={false}
									showHeader={false}
									searchQuery={friendsSearchQuery}
									onSearchChange={setFriendsSearchQuery}
									data-flx="search.quick-switcher-bottom-sheet.friends-content"
								/>
							</div>
						)}
					</div>
				</div>
			</Sheet.Content>
		</Sheet.Root>
	);
});

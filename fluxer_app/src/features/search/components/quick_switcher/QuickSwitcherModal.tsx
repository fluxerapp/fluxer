// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import * as QuickSwitcherCommands from '@app/features/search/commands/QuickSwitcherCommands';
import {QuickSwitcherBottomSheet} from '@app/features/search/components/bottomsheets/QuickSwitcherBottomSheet';
import quickStyles from '@app/features/search/components/quick_switcher/QuickSwitcherModal.module.css';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {consumeEscapeIntent} from '@app/features/search/state/QuickSwitcherEscapeIntent';
import type {QuickSwitcherExecutableResult, QuickSwitcherResult} from '@app/features/search/state/QuickSwitcherTypes';
import {
	createSections,
	dismissQuickSwitcher,
	getQuickSwitcherResultAccessibilityMetadata,
	getResultKey,
	getViewContext,
	handleContextMenu,
	PREFIX_HINTS,
	type QuickSwitcherSection,
	renderIcon,
	useQuickSwitcherEscapeIntent,
	useQuickSwitcherInputFocus,
	useQuickSwitcherKeyboardHandling,
} from '@app/features/search/utils/QuickSwitcherModalUtils';
import {Input} from '@app/features/ui/components/form/FormInput';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {msg, ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const NO_AUTOCOMPLETE_SUGGESTION_DESCRIPTOR = msg({
	message: 'No autocomplete suggestion',
	comment: 'Screen-reader announcement in the desktop quick switcher when no autocomplete suggestion is active.',
});
const AUTOCOMPLETE_SUGGESTION_FOR_OF_DESCRIPTOR = msg({
	message: 'Autocomplete suggestion for {trimmedQuery}: {label}, {selectedOptionPosition} of {resultCount}',
	comment: 'Screen-reader announcement for the highlighted quick switcher autocomplete suggestion including position.',
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
const SEARCHING_PEOPLE_DESCRIPTOR = msg({
	message: 'Searching people...',
	comment:
		'Loading state in the desktop quick switcher while fetching people results. Trailing ellipsis is intentional.',
});
const NO_MATCHES_FOUND_DESCRIPTOR = msg({
	message: 'No matches found',
	comment: 'Empty state title in the desktop quick switcher when no results match the query.',
});
const MESSAGE_1_RESULT_AVAILABLE_DESCRIPTOR = msg({
	message: '1 result available',
	comment: 'Screen-reader live region announcement when exactly one quick switcher result is available.',
});
const RESULTS_AVAILABLE_DESCRIPTOR = msg({
	message: '{resultCount} results available',
	comment: 'Screen-reader live region announcement listing the quick switcher result count.',
});
const QUICK_SWITCHER_DESCRIPTOR = msg({
	message: 'Quick switcher',
	comment: 'Accessible label for the desktop quick switcher modal.',
});
const SEARCH_FOR_CHANNELS_PEOPLE_OR_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Search for channels, people, or communities',
	comment: 'Placeholder text in the desktop quick switcher search input.',
});
const QUICK_SWITCHER_SEARCH_DESCRIPTOR = msg({
	message: 'Quick switcher search',
	comment: 'Accessible label for the desktop quick switcher search input region.',
});
const USE_UP_AND_DOWN_ARROWS_TO_CHOOSE_A_DESCRIPTOR = msg({
	message: 'Arrow keys navigate. Enter opens, Escape closes.',
	comment: 'Screen-reader instructions describing keyboard navigation in the desktop quick switcher result list.',
});
const TRY_A_DIFFERENT_NAME_OR_USE_PREFIXES_TO_DESCRIPTOR = msg({
	message: 'Try a different name or use @ / # / ! / * prefixes to filter results.',
	comment:
		'Empty state hint in the desktop quick switcher describing the @, #, !, and * prefix filters. Keep the literal prefix characters.',
});
const QUICK_SWITCHER_RESULTS_DESCRIPTOR = msg({
	message: 'Quick switcher results',
	comment: 'Accessible label for the desktop quick switcher result listbox.',
});
const SELECTED_ROW_SCROLL_PADDING = 10;
const getQuickSwitcherOptionId = (baseId: string, index: number): string => `${baseId}-option-${index}`;
type PrefixHint = (typeof PREFIX_HINTS)[number];
const PrefixHintToken: React.FC<{
	hint: PrefixHint;
	children: React.ReactNode;
	tooltipDataFlx: string;
	codeDataFlx: string;
}> = ({hint, children, tooltipDataFlx, codeDataFlx}) => {
	const {i18n} = useLingui();
	return (
		<Tooltip text={i18n._(hint.label)} position="top" data-flx={tooltipDataFlx}>
			<span className={quickStyles.footerCode} data-flx={codeDataFlx}>
				{children}
			</span>
		</Tooltip>
	);
};
const ResultRow = observer(
	({
		result,
		index,
		isKeyboardSelected,
		onHover,
		onConfirm,
		optionId,
		positionInSet,
		setSize,
		innerRef,
	}: {
		result: QuickSwitcherResult;
		index: number;
		isKeyboardSelected: boolean;
		onHover: (index: number) => void;
		onConfirm: (result: QuickSwitcherExecutableResult) => void;
		optionId: string;
		positionInSet: number;
		setSize: number;
		innerRef?: React.Ref<HTMLDivElement>;
	}) => {
		const {i18n} = useLingui();
		if (result.type === QuickSwitcherResultTypes.HEADER) {
			return (
				<div
					key={getResultKey(result)}
					className={quickStyles.sectionHeader}
					data-flx="search.quick-switcher.quick-switcher-modal.result-row.div"
				>
					{result.title}
				</div>
			);
		}
		const executableResult = result as QuickSwitcherExecutableResult;
		const resultMetadata = getQuickSwitcherResultAccessibilityMetadata(executableResult, i18n);
		const {guildName, isChannel, isUser, label: optionLabel, mentionCount, subtext, unreadCount} = resultMetadata;
		const hasUnread = unreadCount > 0 || mentionCount > 0;
		const isActive = isKeyboardSelected;
		const isHighlight =
			hasUnread &&
			(executableResult.type === QuickSwitcherResultTypes.TEXT_CHANNEL ||
				executableResult.type === QuickSwitcherResultTypes.VOICE_CHANNEL ||
				executableResult.type === QuickSwitcherResultTypes.GROUP_DM);
		const handleMouseEnter = () => onHover(index);
		const handleClick = (event: React.MouseEvent) => {
			event.preventDefault();
			onConfirm(executableResult);
		};
		const handleOptionKeyDown = (event: React.KeyboardEvent) => {
			if (!isKeyboardActivationKey(event.key)) {
				return;
			}
			event.preventDefault();
			onConfirm(executableResult);
		};
		const iconRendered = renderIcon(
			executableResult,
			isHighlight,
			quickStyles.optionIcon,
			quickStyles.optionIconHighlight,
		);
		const key = getViewContext(executableResult)
			? `${executableResult.type}-${getViewContext(executableResult)}-${executableResult.id}`
			: `${executableResult.type}-${executableResult.id}`;
		return (
			<FocusRing offset={-2} data-flx="search.quick-switcher.quick-switcher-modal.result-row.focus-ring">
				<div
					id={optionId}
					role="option"
					aria-label={optionLabel}
					aria-selected={isKeyboardSelected}
					aria-posinset={positionInSet}
					aria-setsize={setSize}
					className={clsx(quickStyles.option, isActive && quickStyles.optionActive)}
					ref={innerRef}
					onMouseEnter={handleMouseEnter}
					onMouseDown={(event) => {
						if (event.button === 0) event.preventDefault();
					}}
					onClick={handleClick}
					onKeyDown={handleOptionKeyDown}
					onContextMenu={(event) => handleContextMenu(event, executableResult)}
					key={key}
					tabIndex={-1}
					data-flx="search.quick-switcher.quick-switcher-modal.result-row.option.click"
				>
					<div
						className={quickStyles.optionContent}
						data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--2"
					>
						{iconRendered.type === 'avatar' ? (
							<div
								className={quickStyles.avatar}
								data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--3"
							>
								{iconRendered.content}
							</div>
						) : iconRendered.type === 'guild' ? (
							<div
								className={quickStyles.guildIcon}
								data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--4"
							>
								{iconRendered.content}
							</div>
						) : (
							iconRendered.content
						)}
						<div
							className={clsx(quickStyles.optionText, isHighlight && quickStyles.optionHighlight)}
							data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--5"
						>
							<div
								className={quickStyles.optionPrimary}
								data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--6"
							>
								<div
									className={quickStyles.optionTitle}
									data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--7"
								>
									{executableResult.title}
								</div>
								{mentionCount > 0 && (
									<span
										className={quickStyles.optionMention}
										data-flx="search.quick-switcher.quick-switcher-modal.result-row.span"
									>
										<MentionBadge
											mentionCount={mentionCount}
											size="small"
											data-flx="search.quick-switcher.quick-switcher-modal.result-row.mention-badge"
										/>
									</span>
								)}
								{subtext && (
									<div
										className={clsx(
											quickStyles.optionDescription,
											isChannel && quickStyles.optionCategory,
											isUser && quickStyles.optionUserTag,
										)}
										data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--8"
									>
										{subtext}
									</div>
								)}
							</div>
							{guildName && (
								<div
									className={quickStyles.optionMeta}
									data-flx="search.quick-switcher.quick-switcher-modal.result-row.div--9"
								>
									{guildName}
								</div>
							)}
						</div>
					</div>
				</div>
			</FocusRing>
		);
	},
);
const QuickSwitcherModalComponent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const {isOpen, query, results, selectedIndex} = QuickSwitcher;
	const quickSwitcherId = useId();
	const inputRef = useRef<HTMLInputElement>(null);
	const scrollerRef = useRef<ScrollerHandle>(null);
	const rowRefs = useRef<Array<HTMLDivElement | null>>([]);
	const previousQueryRef = useRef(query);
	const [isKeyboardNavigating, setIsKeyboardNavigating] = useState(true);
	const isMobile = MobileLayout.isMobileLayout();
	const inputId = `${quickSwitcherId}-input`;
	const listboxId = `${quickSwitcherId}-results`;
	const statusId = `${quickSwitcherId}-status`;
	const suggestionStatusId = `${quickSwitcherId}-suggestion-status`;
	const hintId = `${quickSwitcherId}-hint`;
	if (rowRefs.current.length !== results.length) {
		rowRefs.current = Array(results.length).fill(null);
	}
	useQuickSwitcherKeyboardHandling(isOpen, isMobile, inputRef, query);
	useQuickSwitcherInputFocus(isOpen, isMobile, undefined, inputRef);
	const escapeIntentRef = useQuickSwitcherEscapeIntent(isOpen && !isMobile);
	const handleChange = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
		QuickSwitcherCommands.search(event.target.value);
	}, []);
	const handleKeyDown = useCallback(async (event: React.KeyboardEvent<HTMLInputElement>) => {
		if (isIMEComposing(event)) {
			return;
		}
		setIsKeyboardNavigating(true);
		switch (event.key) {
			case 'ArrowDown':
			case 'ArrowUp':
				event.preventDefault();
				event.stopPropagation();
				QuickSwitcherCommands.moveSelection(event.key === 'ArrowDown' ? 'down' : 'up');
				break;
			case 'Enter':
				event.preventDefault();
				await QuickSwitcherCommands.confirmSelection();
				break;
			default:
				break;
		}
	}, []);
	const handleClose = useCallback(() => {
		if (LayerManager.hasType('contextmenu')) {
			return;
		}
		dismissQuickSwitcher({isEscape: consumeEscapeIntent(escapeIntentRef)});
	}, [escapeIntentRef]);
	const handlePointerMove = useCallback(() => {
		setIsKeyboardNavigating(false);
	}, []);
	const handleHover = useCallback(
		(index: number) => {
			if (isKeyboardNavigating) {
				return;
			}
			QuickSwitcherCommands.select(index);
		},
		[isKeyboardNavigating],
	);
	const handleConfirm = useCallback((result: QuickSwitcherExecutableResult) => {
		void QuickSwitcherCommands.switchTo(result);
	}, []);
	useEffect(() => {
		const scroller = scrollerRef.current;
		if (!scroller) {
			return;
		}
		if (previousQueryRef.current !== query) {
			previousQueryRef.current = query;
			scroller.scrollTo({to: 0});
			return;
		}
		if (!isKeyboardNavigating || selectedIndex < 0) {
			return;
		}
		const node = rowRefs.current[selectedIndex];
		if (node) {
			scroller.revealElement({node, padding: SELECTED_ROW_SCROLL_PADDING});
		}
	}, [isKeyboardNavigating, query, results, selectedIndex]);
	const sections = useMemo(() => createSections(results), [results]);
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
	const selectedOptionPosition = selectedIndex >= 0 ? selectableIndices.indexOf(selectedIndex) + 1 : 0;
	const hasSearchIntent = query.trim().length > 0;
	const resultCount = useMemo(
		() => results.filter((result) => result.type !== QuickSwitcherResultTypes.HEADER).length,
		[results],
	);
	const activeDescendant =
		selectedIndex >= 0 && results[selectedIndex]?.type !== QuickSwitcherResultTypes.HEADER
			? getQuickSwitcherOptionId(listboxId, selectedIndex)
			: undefined;
	const activeSuggestionStatus = useMemo(() => {
		const result = results[selectedIndex];
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
	}, [hasSearchIntent, i18n.locale, query, resultCount, results, selectedIndex, selectedOptionPosition]);
	const resultStatus = QuickSwitcher.isLoadingMemberResults
		? i18n._(SEARCHING_PEOPLE_DESCRIPTOR)
		: resultCount === 0
			? hasSearchIntent
				? i18n._(NO_MATCHES_FOUND_DESCRIPTOR)
				: ''
			: resultCount === 1
				? i18n._(MESSAGE_1_RESULT_AVAILABLE_DESCRIPTOR)
				: i18n._(RESULTS_AVAILABLE_DESCRIPTOR, {resultCount});
	if (!isOpen) {
		return null;
	}
	if (isMobile) {
		return (
			<QuickSwitcherBottomSheet
				isOpen={isOpen}
				onClose={QuickSwitcherCommands.hide}
				data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.quick-switcher-bottom-sheet"
			/>
		);
	}
	return (
		<Modal.Root
			centered
			className={quickStyles.modalRoot}
			onClose={handleClose}
			initialFocusRef={inputRef}
			transitionPreset="quick"
			data-quick-switcher-modal="true"
			data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.modal-root"
		>
			<Modal.ScreenReaderLabel
				text={i18n._(QUICK_SWITCHER_DESCRIPTOR)}
				data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.modal-screen-reader-label"
			/>
			{/* biome-ignore lint/a11y/noStaticElementInteractions: pointer arbitration surface for the result rows; it carries no affordance of its own. */}
			<div
				className={quickStyles.container}
				onMouseMove={handlePointerMove}
				data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div"
			>
				<div
					className={quickStyles.header}
					data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--2"
				>
					<Input
						id={inputId}
						ref={inputRef}
						value={query}
						onChange={handleChange}
						onKeyDown={handleKeyDown}
						placeholder={i18n._(SEARCH_FOR_CHANNELS_PEOPLE_OR_COMMUNITIES_DESCRIPTOR)}
						spellCheck={false}
						className={quickStyles.inputBackground}
						role="combobox"
						aria-autocomplete="list"
						aria-haspopup="listbox"
						aria-controls={resultCount > 0 ? listboxId : undefined}
						aria-describedby={`${statusId} ${suggestionStatusId} ${hintId}`}
						aria-expanded={resultCount > 0}
						aria-label={i18n._(QUICK_SWITCHER_SEARCH_DESCRIPTOR)}
						aria-activedescendant={activeDescendant}
						data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.combobox.change"
					/>
					<div
						id={hintId}
						className={quickStyles.srOnly}
						data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--3"
					>
						{i18n._(USE_UP_AND_DOWN_ARROWS_TO_CHOOSE_A_DESCRIPTOR)}
					</div>
				</div>
				<div
					className={quickStyles.list}
					data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--4"
				>
					<div
						id={statusId}
						className={quickStyles.srOnly}
						role="status"
						aria-live="polite"
						aria-atomic="true"
						data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.status"
					>
						{resultStatus}
					</div>
					<div
						id={suggestionStatusId}
						className={quickStyles.srOnly}
						role="status"
						aria-live="polite"
						aria-atomic="true"
						data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.status--2"
					>
						{activeSuggestionStatus}
					</div>
					{results.length === 0 && !hasSearchIntent ? null : (
						<Scroller
							ref={scrollerRef}
							className={quickStyles.scrollerContainer}
							overflow="scroll"
							key="quick_switcher-desktop-scroller"
							data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.scroller"
						>
							{results.length === 0 ? (
								<div
									className={quickStyles.emptyState}
									role="status"
									aria-live="polite"
									data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.status--3"
								>
									<div
										className={quickStyles.emptyStateTitle}
										data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--5"
									>
										{i18n._(NO_MATCHES_FOUND_DESCRIPTOR)}
									</div>
									<div
										className={quickStyles.emptyStateHint}
										data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--6"
									>
										{i18n._(TRY_A_DIFFERENT_NAME_OR_USE_PREFIXES_TO_DESCRIPTOR)}
									</div>
								</div>
							) : (
								<div
									id={listboxId}
									role="listbox"
									aria-label={i18n._(QUICK_SWITCHER_RESULTS_DESCRIPTOR)}
									data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.listbox"
								>
									{sections.map((section: QuickSwitcherSection) => {
										const sectionHeaderId = section.header ? `${listboxId}-${section.key}` : undefined;
										const sectionAriaProps = sectionHeaderId
											? ({role: 'group', 'aria-labelledby': sectionHeaderId} as const)
											: ({role: 'presentation'} as const);
										return (
											<div
												key={section.key}
												className={quickStyles.section}
												data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--7"
												{...sectionAriaProps}
											>
												{section.header && (
													<div
														id={sectionHeaderId}
														className={quickStyles.sectionHeader}
														data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--8"
													>
														{section.header.title}
													</div>
												)}
												{section.rows.map(({result, index}: {result: QuickSwitcherExecutableResult; index: number}) => (
													<ResultRow
														key={getResultKey(result)}
														result={result}
														index={index}
														isKeyboardSelected={index === selectedIndex}
														onHover={handleHover}
														onConfirm={handleConfirm}
														optionId={getQuickSwitcherOptionId(listboxId, index)}
														positionInSet={selectableIndices.indexOf(index) + 1}
														setSize={resultCount}
														innerRef={(node) => {
															rowRefs.current[index] = node;
														}}
														data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.result-row"
													/>
												))}
											</div>
										);
									})}
								</div>
							)}
						</Scroller>
					)}
				</div>
				<div
					className={quickStyles.footer}
					data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.div--9"
				>
					<Trans>
						Start searches with{' '}
						<PrefixHintToken
							hint={PREFIX_HINTS[0]}
							tooltipDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.tooltip"
							codeDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.span"
							data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.prefix-hint-token"
						>
							{ph({peoplePrefix: '@'})}
						</PrefixHintToken>{' '}
						<PrefixHintToken
							hint={PREFIX_HINTS[1]}
							tooltipDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.tooltip--2"
							codeDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.span--2"
							data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.prefix-hint-token--2"
						>
							{ph({textChannelPrefix: '#'})}
						</PrefixHintToken>{' '}
						<PrefixHintToken
							hint={PREFIX_HINTS[2]}
							tooltipDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.tooltip--3"
							codeDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.span--3"
							data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.prefix-hint-token--3"
						>
							{ph({voiceChannelPrefix: '!'})}
						</PrefixHintToken>{' '}
						<PrefixHintToken
							hint={PREFIX_HINTS[3]}
							tooltipDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.tooltip--4"
							codeDataFlx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.span--4"
							data-flx="search.quick-switcher.quick-switcher-modal.quick-switcher-modal-component.prefix-hint-token--4"
						>
							{ph({communityPrefix: '*'})}
						</PrefixHintToken>{' '}
						to narrow down results.
					</Trans>
				</div>
			</div>
		</Modal.Root>
	);
});
export const QuickSwitcherModal = Object.assign(QuickSwitcherModalComponent, {
	disableBackdropOnMobile: true,
});

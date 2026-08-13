// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelsSection} from '@app/features/channel/components/message_search_bar/ChannelsSection';
import {DateSection} from '@app/features/channel/components/message_search_bar/DateSection';
import {FiltersSection} from '@app/features/channel/components/message_search_bar/FilterOption';
import {HistorySection} from '@app/features/channel/components/message_search_bar/HistorySection';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import type {SearchBarProps} from '@app/features/channel/components/message_search_bar/MessageSearchBarTypes';
import {SCOPE_ICON_COMPONENTS} from '@app/features/channel/components/message_search_bar/MessageSearchBarUtils';
import {UsersSection} from '@app/features/channel/components/message_search_bar/UsersSection';
import {useMessageSearchAutocomplete} from '@app/features/channel/components/message_search_bar/useMessageSearchAutocomplete';
import {ValuesSection} from '@app/features/channel/components/message_search_bar/ValuesSection';
import {DEFAULT_SCOPE_VALUE, getScopeOptionsForChannel} from '@app/features/channel/components/SearchScopeOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelSearch, {getChannelSearchContextId} from '@app/features/channel/state/ChannelSearch';
import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {LexicalSearchInput} from '@app/features/lexical/search/LexicalSearchInput';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import type {MessageSearchScope, SearchFilterOption} from '@app/features/search/utils/SearchUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {ContextMenuCloseProvider} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import {autoUpdate, FloatingPortal, flip, offset, size, useFloating} from '@floating-ui/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {HashIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo} from 'react';

export type {SearchBarProps} from '@app/features/channel/components/message_search_bar/MessageSearchBarTypes';

const SEARCH_SCOPE_DESCRIPTOR = msg({
	message: 'Search scope: {label}',
	comment:
		'Tooltip and aria-label for the search scope button in the message search bar. Sentence case. Preserve {label}; it is inserted by code.',
});
const SEARCH_SCOPE_FALLBACK_DESCRIPTOR = msg({
	message: 'Search scope',
	comment: 'Tooltip and aria-label for the search scope button when no scope label is available. Sentence case.',
});
const SEARCH_MESSAGES_PLACEHOLDER_DESCRIPTOR = msg({
	message: 'Search messages',
	comment: 'Placeholder shown inside the message search input. Sentence case.',
});
const SEARCH_SUGGESTIONS_DESCRIPTOR = msg({
	message: 'Search suggestions',
	comment: 'Aria-label for the search popout listbox; not visible. Sentence case.',
});
const SEARCH_SHORTCUTS_HINT_DESCRIPTOR = msg({
	message: 'Arrow keys review suggestions. Enter selects, Escape clears, Tab moves on.',
	comment:
		'Screen-reader-only instructions describing the keyboard shortcuts for the message search autocomplete. Announced when the input is focused. Sentence case.',
});
const ONE_SUGGESTION_AVAILABLE_DESCRIPTOR = msg({
	message: '1 suggestion available',
	comment: 'Screen-reader live-region announcement when exactly one message search suggestion is available.',
});
const SUGGESTIONS_AVAILABLE_DESCRIPTOR = msg({
	message: '{count} suggestions available',
	comment:
		'Screen-reader live-region announcement listing the number of message search suggestions. Preserve {count}; it is inserted by code.',
});
const ACTIVE_SUGGESTION_POSITION_DESCRIPTOR = msg({
	message: 'Suggestion {position} of {total}',
	comment:
		'Screen-reader live-region announcement of the currently highlighted message search suggestion position. Preserve {position} and {total}; they are inserted by code.',
});
export const MessageSearchBar = observer(
	({
		channel,
		value,
		onChange,
		onSearch,
		onClear,
		isResultsOpen = false,
		onCloseResults,
		inputRefExternal,
		highContrast = false,
	}: SearchBarProps) => {
		const {i18n} = useLingui();
		const {guildId: routeGuildId} = useParams() as {guildId?: string};
		const selectedGuildId = SelectedGuild.selectedGuildId;
		const channelGuildId = channel?.guildId ?? undefined;
		const isInGuildChannel = Boolean(channelGuildId);
		const currentGuildIdForScope = isInGuildChannel
			? (channelGuildId ?? routeGuildId ?? selectedGuildId ?? undefined)
			: undefined;
		const contextId = useMemo(
			() => getChannelSearchContextId(channel ?? null, selectedGuildId),
			[channel?.guildId, channel?.id, selectedGuildId],
		);
		const searchContext = contextId ? ChannelSearch.getContext(contextId) : null;
		const activeScope = searchContext?.scope ?? DEFAULT_SCOPE_VALUE;
		const scopeOptions = useMemo(
			() => getScopeOptionsForChannel(i18n, channel),
			[i18n.locale, channel?.id, channel?.type, channel?.guildId],
		);
		const scopeOptionValues = useMemo(() => new Set(scopeOptions.map((option) => option.value)), [scopeOptions]);
		useEffect(() => {
			if (!scopeOptions.length || !contextId) {
				return;
			}
			const fallbackScope = scopeOptions[0].value;
			const currentScope: MessageSearchScope = activeScope ?? fallbackScope;
			if (!scopeOptionValues.has(currentScope)) {
				ChannelSearch.setScope(contextId, fallbackScope);
			}
		}, [scopeOptions, scopeOptionValues, activeScope, contextId]);
		const handleScopeSelect = useCallback(
			(scope: MessageSearchScope) => {
				if (!contextId) {
					return;
				}
				ChannelSearch.setScope(contextId, scope);
			},
			[contextId],
		);
		const handleScopeMenuOpen = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
					<ContextMenuCloseProvider
						value={onClose}
						data-flx="channel.message-search-bar.message-search-bar.handle-scope-menu-open.context-menu-close-provider"
					>
						<MenuGroup data-flx="channel.message-search-bar.message-search-bar.handle-scope-menu-open.menu-group">
							{scopeOptions.map((option) => (
								<MenuItemRadio
									key={option.value}
									selected={activeScope === option.value}
									closeOnSelect
									onSelect={() => handleScopeSelect(option.value)}
									icon={React.createElement(SCOPE_ICON_COMPONENTS[option.value] ?? HashIcon, {
										size: 16,
										weight: 'bold',
									})}
									data-flx="channel.message-search-bar.message-search-bar.handle-scope-menu-open.menu-item-radio.scope-select"
								>
									{option.label}
								</MenuItemRadio>
							))}
						</MenuGroup>
					</ContextMenuCloseProvider>
				));
			},
			[handleScopeSelect, scopeOptions, activeScope],
		);
		const activeScopeOption = useMemo(() => {
			if (!scopeOptions.length) {
				return null;
			}
			return scopeOptions.find((opt) => opt.value === activeScope) ?? scopeOptions[0];
		}, [scopeOptions, activeScope]);
		const ScopeIconComponent = useMemo(() => {
			if (!activeScope) {
				return HashIcon;
			}
			return SCOPE_ICON_COMPONENTS[activeScope] ?? HashIcon;
		}, [activeScope]);
		const scopeTooltipText = useMemo(() => {
			if (activeScopeOption?.label) {
				return i18n._(SEARCH_SCOPE_DESCRIPTOR, {label: activeScopeOption.label});
			}
			return i18n._(SEARCH_SCOPE_FALLBACK_DESCRIPTOR);
		}, [i18n.locale, activeScopeOption?.label]);
		const {
			isFocused,
			setIsFocused,
			autocompleteType,
			inputRef,
			setInputRefs,
			setSuppressAutoOpen,
			filterOptions,
			listboxId,
			keyboardFocusIndex,
			hoverIndexForRender,
			getAutocompleteOptions,
			hasAnyOptions,
			getTotalOptions,
			getAriaActiveDescendant,
			handleAutocompleteSelect,
			handleKeyDown,
			handleInputValueChange,
			handleHistoryClear,
			handleFilterSelect,
			handleOptionMouseEnter,
			handleOptionMouseLeave,
			resetSelectedIndex,
			setHoverIndex,
			setHasInteracted,
		} = useMessageSearchAutocomplete({
			channel,
			value,
			onChange,
			onSearch,
			activeScope,
			currentGuildIdForScope,
			channelGuildId,
			routeGuildId,
			isInGuildChannel,
			contextId,
			inputRefExternal,
			isResultsOpen,
		});
		const portalHost = usePortalHost();
		const {refs, floatingStyles, isPositioned} = useFloating({
			placement: 'bottom-start',
			open: isFocused && autocompleteType !== null,
			whileElementsMounted: autoUpdate,
			middleware: [
				offset(8),
				flip({padding: 16}),
				size({
					apply({rects, elements}) {
						const minWidth = 380;
						const maxWidth = Math.min(window.innerWidth - 32, 480);
						const width = Math.min(maxWidth, Math.max(rects.reference.width, minWidth));
						Object.assign(elements.floating.style, {
							width: `${width}px`,
						});
					},
					padding: 16,
				}),
			],
		});
		const renderAutocompleteContent = () => {
			switch (autocompleteType) {
				case 'filters':
					return (
						<FiltersSection
							options={getAutocompleteOptions() as Array<SearchFilterOption>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							data-flx="channel.message-search-bar.message-search-bar.render-autocomplete-content.filters-section.autocomplete-select"
						/>
					);
				case 'history':
					return (
						<HistorySection
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							isInGuild={isInGuildChannel}
							channelId={channel?.id}
							onHistoryClear={handleHistoryClear}
							onFilterSelect={handleFilterSelect}
							onFilterMouseEnter={(index) => {
								setHoverIndex(index);
								setHasInteracted(true);
							}}
							onFilterMouseLeave={handleOptionMouseLeave}
							filterOptions={filterOptions}
							data-flx="channel.message-search-bar.message-search-bar.render-autocomplete-content.history-section.autocomplete-select"
						/>
					);
				case 'users':
					return (
						<UsersSection
							options={getAutocompleteOptions() as Array<User>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							guildId={currentGuildIdForScope}
							isInGuild={isInGuildChannel}
							data-flx="channel.message-search-bar.message-search-bar.render-autocomplete-content.users-section.autocomplete-select"
						/>
					);
				case 'channels':
					return (
						<ChannelsSection
							options={getAutocompleteOptions() as Array<Channel>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							data-flx="channel.message-search-bar.message-search-bar.render-autocomplete-content.channels-section.autocomplete-select"
						/>
					);
				case 'values':
					return (
						<ValuesSection
							options={getAutocompleteOptions() as Array<{value: string; label: string}>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							data-flx="channel.message-search-bar.message-search-bar.render-autocomplete-content.values-section.autocomplete-select"
						/>
					);
				case 'date':
					return (
						<DateSection
							options={getAutocompleteOptions() as Array<{value: string; label: string}>}
							selectedIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							onSelect={handleAutocompleteSelect}
							onMouseEnter={handleOptionMouseEnter}
							onMouseLeave={handleOptionMouseLeave}
							listboxId={listboxId}
							data-flx="channel.message-search-bar.message-search-bar.render-autocomplete-content.date-section.autocomplete-select"
						/>
					);
				default:
					return null;
			}
		};
		const hasValue = value.length > 0;
		const ariaActiveDescendant = getAriaActiveDescendant();
		const hintId = `${listboxId}-hint`;
		const suggestionsStatusId = `${listboxId}-status`;
		const activeSuggestionStatusId = `${listboxId}-active-status`;
		const totalOptions = getTotalOptions();
		const isPopoutOpen = isFocused && autocompleteType !== null && totalOptions > 0;
		const suggestionsAvailableStatus = !isPopoutOpen
			? ''
			: totalOptions === 1
				? i18n._(ONE_SUGGESTION_AVAILABLE_DESCRIPTOR)
				: i18n._(SUGGESTIONS_AVAILABLE_DESCRIPTOR, {count: totalOptions});
		const activeSuggestionStatus =
			isPopoutOpen && keyboardFocusIndex >= 0
				? i18n._(ACTIVE_SUGGESTION_POSITION_DESCRIPTOR, {position: keyboardFocusIndex + 1, total: totalOptions})
				: '';
		return (
			<>
				<div
					ref={refs.setReference}
					className={styles.anchor}
					data-flx="channel.message-search-bar.message-search-bar.anchor"
				>
					<div
						className={highContrast ? `${styles.inputContainer} ${styles.inputContainerOnCall}` : styles.inputContainer}
						data-flx="channel.message-search-bar.message-search-bar.input-container"
					>
						<Tooltip
							text={scopeTooltipText}
							position="bottom"
							data-flx="channel.message-search-bar.message-search-bar.tooltip"
						>
							<button
								type="button"
								className={styles.scopeButton}
								onClick={handleScopeMenuOpen}
								aria-label={scopeTooltipText}
								data-flx="channel.message-search-bar.message-search-bar.scope-button.scope-menu-open"
							>
								<MagnifyingGlassIcon
									className={styles.searchIcon}
									weight="bold"
									data-flx="channel.message-search-bar.message-search-bar.search-icon"
								/>
								<span
									className={styles.scopeBadge}
									data-flx="channel.message-search-bar.message-search-bar.scope-badge"
								>
									<ScopeIconComponent
										size={remFromPx(8)}
										weight="bold"
										data-flx="channel.message-search-bar.message-search-bar.scope-icon-component"
									/>
								</span>
							</button>
						</Tooltip>
						<LexicalSearchInput
							inputRef={setInputRefs}
							value={value}
							placeholder={i18n._(SEARCH_MESSAGES_PLACEHOLDER_DESCRIPTOR)}
							role="combobox"
							isAutocompleteOpen={isPopoutOpen}
							onValueChange={handleInputValueChange}
							onMouseDown={() => setSuppressAutoOpen(false)}
							onKeyDown={handleKeyDown}
							onFocus={() => {
								setIsFocused(true);
								if (KeyboardMode.keyboardModeEnabled) {
									setSuppressAutoOpen(false);
								}
							}}
							onBlur={() => {
								setIsFocused(false);
								if (isResultsOpen && value.trim().length === 0) {
									onCloseResults?.();
								}
							}}
							ariaProps={{
								...PASSWORD_MANAGER_IGNORE_ATTRIBUTES,
								'data-flx': 'channel.message-search-bar.message-search-bar.input.text',
								'aria-autocomplete': 'list',
								'aria-haspopup': 'listbox',
								'aria-expanded': isFocused && autocompleteType !== null,
								'aria-controls': isFocused && autocompleteType !== null ? listboxId : undefined,
								'aria-activedescendant': ariaActiveDescendant,
								'aria-keyshortcuts': 'ArrowDown ArrowUp Enter Escape',
								'aria-describedby': `${suggestionsStatusId} ${activeSuggestionStatusId} ${hintId}`,
							}}
						/>
						{hasValue && (
							<button
								type="button"
								onMouseDown={(ev) => {
									if (ev.button === 0) ev.preventDefault();
								}}
								onClick={() => {
									onClear();
									resetSelectedIndex();
									inputRef.current?.focus();
								}}
								className={styles.clearButton}
								aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
								data-flx="channel.message-search-bar.message-search-bar.clear-button"
							>
								<XIcon
									weight="bold"
									className={styles.optionMetaIcon}
									data-flx="channel.message-search-bar.message-search-bar.option-meta-icon"
								/>
							</button>
						)}
					</div>
					<span
						id={hintId}
						className={styles.srOnly}
						data-flx="channel.message-search-bar.message-search-bar.shortcuts-hint"
					>
						{i18n._(SEARCH_SHORTCUTS_HINT_DESCRIPTOR)}
					</span>
					<span
						id={suggestionsStatusId}
						role="status"
						aria-live="polite"
						aria-atomic="true"
						className={styles.srOnly}
						data-flx="channel.message-search-bar.message-search-bar.suggestions-status"
					>
						{suggestionsAvailableStatus}
					</span>
					<span
						id={activeSuggestionStatusId}
						role="status"
						aria-live="polite"
						aria-atomic="true"
						className={styles.srOnly}
						data-flx="channel.message-search-bar.message-search-bar.active-suggestion-status"
					>
						{activeSuggestionStatus}
					</span>
				</div>
				{isFocused && autocompleteType && hasAnyOptions() && (
					<FloatingPortal
						root={portalHost ?? undefined}
						data-flx="channel.message-search-bar.message-search-bar.floating-portal"
					>
						<div
							ref={refs.setFloating}
							role="presentation"
							style={{...floatingStyles, visibility: isPositioned ? 'visible' : 'hidden'}}
							className={styles.popoutContainer}
							onPointerDown={(e) => {
								if (e.button === 0) e.preventDefault();
							}}
							data-flx="channel.message-search-bar.message-search-bar.popout-container"
						>
							<div className={styles.popoutInner} data-flx="channel.message-search-bar.message-search-bar.popout-inner">
								<div
									className={`${styles.flex} ${styles.flexCol}`}
									data-flx="channel.message-search-bar.message-search-bar.flex"
								>
									<div
										id={listboxId}
										role="listbox"
										aria-label={i18n._(SEARCH_SUGGESTIONS_DESCRIPTOR)}
										className={styles.list}
										data-flx="channel.message-search-bar.message-search-bar.list"
									>
										{renderAutocompleteContent()}
									</div>
								</div>
							</div>
						</div>
					</FloatingPortal>
				)}
			</>
		);
	},
);

// SPDX-License-Identifier: AGPL-3.0-or-later

import {useListNavigation} from '@app/features/app/hooks/useListNavigation';
import styles from '@app/features/channel/components/Autocomplete.module.css';
import {AutocompleteChannel} from '@app/features/channel/components/AutocompleteChannel';
import {AutocompleteCommand} from '@app/features/channel/components/AutocompleteCommand';
import {AutocompleteCommandChoice} from '@app/features/channel/components/AutocompleteCommandChoice';
import {AutocompleteCommandOptionalAdd} from '@app/features/channel/components/AutocompleteCommandOptionalAdd';
import {AutocompleteEmoji} from '@app/features/channel/components/AutocompleteEmoji';
import {AutocompleteGif} from '@app/features/channel/components/AutocompleteGif';
import {AutocompleteMeme} from '@app/features/channel/components/AutocompleteMeme';
import {AutocompleteMention} from '@app/features/channel/components/AutocompleteMention';
import {AutocompleteSticker} from '@app/features/channel/components/AutocompleteSticker';
import {
	type AutocompleteOption,
	type AutocompleteType,
	getAutocompleteOptionId,
	isMentionMember,
	isMentionRole,
	isMentionUser,
} from '@app/features/channel/components/AutocompleteTypes';
import {
	MEDIA_DESCRIPTOR,
	MEMBERS_DESCRIPTOR,
	MENTIONS_DESCRIPTOR,
	ROLES_DESCRIPTOR,
	STICKERS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {autoUpdate, FloatingPortal, flip, offset, size, useFloating} from '@floating-ui/react';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useRef, useState} from 'react';

const SUGGESTIONS_DESCRIPTOR = msg({
	message: 'Suggestions',
	comment: 'Short label in the channel and chat autocomplete. Keep it concise.',
});
const CHANNELS_DESCRIPTOR = msg({
	message: 'Channels',
	comment: 'Section heading in the channel and chat autocomplete for channel suggestions.',
});
const COMMANDS_DESCRIPTOR = msg({
	message: 'Commands',
	comment: 'Section heading in the channel and chat autocomplete for command suggestions.',
});
const CHOICES_DESCRIPTOR = msg({
	message: 'Choices',
	comment: 'Section heading in the channel and chat autocomplete for slash-command argument choices.',
});
const OPTIONAL_ARGUMENTS_DESCRIPTOR = msg({
	message: 'Optional arguments',
	comment: 'Section heading in the channel and chat autocomplete for adding optional slash-command arguments.',
});
const USERS_DESCRIPTOR = msg({
	message: 'Users',
	comment: 'Section heading in the channel and chat autocomplete for user suggestions.',
});

function resolveMentionAutocompleteHeading(options: Array<AutocompleteOption>, i18n: I18n): string {
	if (options.some(isMentionMember)) {
		return i18n._(MEMBERS_DESCRIPTOR);
	}
	if (options.some(isMentionUser)) {
		return i18n._(USERS_DESCRIPTOR);
	}
	if (options.some(isMentionRole)) {
		return i18n._(ROLES_DESCRIPTOR);
	}
	return i18n._(MENTIONS_DESCRIPTOR);
}

function resolveAutocompleteHeading(
	type: AutocompleteType,
	options: Array<AutocompleteOption>,
	i18n: I18n,
): string | null {
	switch (type) {
		case 'mention':
			return resolveMentionAutocompleteHeading(options, i18n);
		case 'channel':
			return i18n._(CHANNELS_DESCRIPTOR);
		case 'command':
			return i18n._(COMMANDS_DESCRIPTOR);
		case 'commandChoice':
			return i18n._(CHOICES_DESCRIPTOR);
		case 'commandOptionalAdd':
			return i18n._(OPTIONAL_ARGUMENTS_DESCRIPTOR);
		case 'meme':
			return i18n._(MEDIA_DESCRIPTOR);
		case 'sticker':
			return i18n._(STICKERS_DESCRIPTOR);
		default:
			return null;
	}
}

function renderAutocompleteHeading(heading: string | null): React.ReactNode {
	if (heading == null) {
		return null;
	}
	return (
		<div className={styles.sectionHeading} aria-hidden={true} data-flx="channel.autocomplete.section-heading">
			{heading}
		</div>
	);
}

function resolveListboxId(listboxId: string | undefined, generatedListboxId: string): string {
	if (listboxId === undefined) {
		return generatedListboxId;
	}
	return listboxId;
}

function resolveKeyboardFocusIndex(externalIndex: number | undefined, internalIndex: number): number {
	if (externalIndex === undefined) {
		return internalIndex;
	}
	return externalIndex;
}

function resolveReferenceElement(referenceElement: HTMLElement | null | undefined): HTMLElement | null {
	if (referenceElement === undefined) {
		return null;
	}
	return referenceElement;
}

function resolvePortalRoot(portalHost: HTMLElement | null): HTMLElement | undefined {
	if (portalHost === null) {
		return undefined;
	}
	return portalHost;
}

function resolveZIndex(zIndex: number | undefined): number | undefined {
	if (zIndex === undefined) {
		return undefined;
	}
	return zIndex;
}

type ScrollerWithScrollableElement = ScrollerHandle & {
	getScrollableElement?: () => HTMLElement | null;
};

export type {AutocompleteOption, AutocompleteType} from '@app/features/channel/components/AutocompleteTypes';
export {
	getAutocompleteOptionId,
	isChannel,
	isCommand,
	isEmoji,
	isGif,
	isMeme,
	isMentionMember,
	isMentionRole,
	isMentionUser,
	isSpecialMention,
	isSticker,
} from '@app/features/channel/components/AutocompleteTypes';

const ATTACHED_AUTOCOMPLETE_GAP = 4;

export const Autocomplete = observer(
	({
		type,
		onSelect,
		selectedIndex: externalSelectedIndex,
		options,
		setSelectedIndex: externalSetSelectedIndex,
		referenceElement,
		zIndex,
		attached = false,
		listboxId,
		mainAxisOffset,
	}: {
		type: AutocompleteType;
		onSelect: (option: AutocompleteOption) => void;
		selectedIndex?: number;
		options: Array<AutocompleteOption>;
		setSelectedIndex?: React.Dispatch<React.SetStateAction<number>>;
		referenceElement?: HTMLElement | null;
		zIndex?: number;
		query?: string;
		attached?: boolean;
		listboxId?: string;
		mainAxisOffset?: number;
	}) => {
		const {i18n} = useLingui();
		const generatedListboxId = useId();
		const resolvedListboxId = resolveListboxId(listboxId, generatedListboxId);
		const getOptionId = useCallback(
			(index: number) => getAutocompleteOptionId(resolvedListboxId, index),
			[resolvedListboxId],
		);
		const {
			keyboardFocusIndex: internalKeyboardFocusIndex,
			hoverIndexForRender,
			handleKeyboardNavigation,
			handleMouseEnter,
			handleMouseLeave,
			reset,
		} = useListNavigation({
			itemCount: options.length,
			initialIndex: 0,
			loop: true,
		});
		const keyboardFocusIndex = resolveKeyboardFocusIndex(externalSelectedIndex, internalKeyboardFocusIndex);
		const [referenceState, setReferenceState] = useState<HTMLElement | null>(resolveReferenceElement(referenceElement));
		useEffect(() => {
			setReferenceState(resolveReferenceElement(referenceElement));
		}, [referenceElement]);
		const portalHost = usePortalHost();
		let resolvedMainAxisOffset = attached ? ATTACHED_AUTOCOMPLETE_GAP : 8;
		if (mainAxisOffset != null) {
			resolvedMainAxisOffset = mainAxisOffset;
		}
		const resolvedCrossAxisOffset = 0;
		const heading = resolveAutocompleteHeading(type, options, i18n);
		const {refs, floatingStyles} = useFloating({
			placement: 'top-start',
			open: true,
			whileElementsMounted: autoUpdate,
			elements: {reference: referenceState},
			middleware: [
				offset({mainAxis: resolvedMainAxisOffset, crossAxis: resolvedCrossAxisOffset}),
				flip({padding: 16}),
				size({
					apply({rects, elements}) {
						const width = rects.reference.width;
						Object.assign(elements.floating.style, {
							width: `${width}px`,
						});
					},
					padding: 16,
				}),
			],
		});
		const scrollerRef = useRef<ScrollerHandle>(null);
		const rowRefs = useRef<Array<HTMLButtonElement | null>>([]);
		if (rowRefs.current.length !== options.length) {
			rowRefs.current = Array(options.length).fill(null);
		}
		useEffect(() => {
			reset();
		}, [options.length, reset]);
		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent) => {
				if (isIMEComposing(event)) {
					return;
				}
				switch (event.key) {
					case 'ArrowDown': {
						event.preventDefault();
						handleKeyboardNavigation('down');
						if (externalSetSelectedIndex) {
							externalSetSelectedIndex((prev) => (prev + 1 >= options.length ? 0 : prev + 1));
						}
						break;
					}
					case 'Home': {
						event.preventDefault();
						handleKeyboardNavigation('home');
						if (externalSetSelectedIndex) {
							externalSetSelectedIndex(0);
						}
						break;
					}
					case 'End': {
						event.preventDefault();
						handleKeyboardNavigation('end');
						if (externalSetSelectedIndex) {
							externalSetSelectedIndex(Math.max(0, options.length - 1));
						}
						break;
					}
					case 'ArrowUp': {
						event.preventDefault();
						handleKeyboardNavigation('up');
						if (externalSetSelectedIndex) {
							externalSetSelectedIndex((prev) => (prev - 1 < 0 ? options.length - 1 : prev - 1));
						}
						break;
					}
					case 'Tab':
					case 'Enter': {
						if (event.key === 'Tab' && event.shiftKey) {
							break;
						}
						event.preventDefault();
						if (keyboardFocusIndex >= 0 && keyboardFocusIndex < options.length) {
							onSelect(options[keyboardFocusIndex]);
						}
						break;
					}
					default:
						break;
				}
			},
			[externalSetSelectedIndex, handleKeyboardNavigation, keyboardFocusIndex, onSelect, options],
		);
		const scrollChildIntoView = useCallback((node: HTMLElement | null, margin = 32) => {
			if (!node) return;
			const scroller = scrollerRef.current as ScrollerWithScrollableElement | null;
			if (scroller && typeof scroller.scrollIntoViewNode === 'function') {
				scroller.scrollIntoViewNode({node, padding: margin});
				return;
			}
			let scrollerEl: HTMLElement | null = null;
			if (scroller != null && typeof scroller.getScrollableElement === 'function') {
				scrollerEl = scroller.getScrollableElement();
			}
			if (scrollerEl == null) {
				scrollerEl = node.closest('[data-scrollable], .overflow-y-auto, .overflow-y-scroll');
			}
			if (scrollerEl == null) {
				scrollerEl = node.parentElement;
			}
			if (scrollerEl && scrollerEl instanceof HTMLElement) {
				const sRect = scrollerEl.getBoundingClientRect();
				const nRect = node.getBoundingClientRect();
				const outOfViewTop = nRect.top < sRect.top + margin;
				const outOfViewBottom = nRect.bottom > sRect.bottom - margin;
				if (outOfViewTop) {
					scrollerEl.scrollTop -= sRect.top + margin - nRect.top;
				} else if (outOfViewBottom) {
					scrollerEl.scrollTop += nRect.bottom - (sRect.bottom - margin);
				}
				return;
			}
			node.scrollIntoView({block: 'nearest'});
		}, []);
		useEffect(() => {
			const node = rowRefs.current[keyboardFocusIndex];
			if (!node) return;
			const raf = requestAnimationFrame(() => scrollChildIntoView(node, 32));
			return () => cancelAnimationFrame(raf);
		}, [keyboardFocusIndex, options.length, scrollChildIntoView]);
		return (
			<FloatingPortal root={resolvePortalRoot(portalHost)} data-flx="channel.autocomplete.floating-portal">
				<div
					ref={refs.setFloating}
					style={{...floatingStyles, zIndex: resolveZIndex(zIndex)}}
					className={`${styles.container} ${attached ? styles.containerAttached : styles.containerDetached}`}
					onKeyDown={handleKeyDown}
					role="listbox"
					id={resolvedListboxId}
					aria-label={i18n._(SUGGESTIONS_DESCRIPTOR)}
					data-flx="channel.autocomplete.container.key-down"
				>
					{type === 'gif' ? (
						<AutocompleteGif
							onSelect={onSelect}
							keyboardFocusIndex={keyboardFocusIndex}
							hoverIndex={hoverIndexForRender}
							options={options}
							onMouseEnter={handleMouseEnter}
							onMouseLeave={handleMouseLeave}
							rowRefs={rowRefs}
							getOptionId={getOptionId}
							data-flx="channel.autocomplete.autocomplete-gif.select"
						/>
					) : (
						<Scroller
							ref={scrollerRef}
							className={styles.scroller}
							key="autocomplete-scroller"
							data-flx="channel.autocomplete.scroller"
						>
							{renderAutocompleteHeading(heading)}
							{type === 'mention' ? (
								<AutocompleteMention
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
									data-flx="channel.autocomplete.autocomplete-mention.select"
								/>
							) : type === 'channel' ? (
								<AutocompleteChannel
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
									data-flx="channel.autocomplete.autocomplete-channel.select"
								/>
							) : type === 'command' ? (
								<AutocompleteCommand
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
									data-flx="channel.autocomplete.autocomplete-command.select"
								/>
							) : type === 'commandChoice' ? (
								<AutocompleteCommandChoice
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
								/>
							) : type === 'commandOptionalAdd' ? (
								<AutocompleteCommandOptionalAdd
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
								/>
							) : type === 'meme' ? (
								<AutocompleteMeme
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
									data-flx="channel.autocomplete.autocomplete-meme.select"
								/>
							) : type === 'sticker' ? (
								<AutocompleteSticker
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
									data-flx="channel.autocomplete.autocomplete-sticker.select"
								/>
							) : (
								<AutocompleteEmoji
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
									getOptionId={getOptionId}
									data-flx="channel.autocomplete.autocomplete-emoji.select"
								/>
							)}
						</Scroller>
					)}
				</div>
			</FloatingPortal>
		);
	},
);

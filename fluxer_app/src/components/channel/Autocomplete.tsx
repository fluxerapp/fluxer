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

import {autoUpdate, FloatingPortal, flip, offset, size, useFloating} from '@floating-ui/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import type {TenorGif} from '~/actions/TenorActionCreators';
import {Permissions} from '~/Constants';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {useListNavigation} from '~/hooks/useListNavigation';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {FavoriteMemeRecord} from '~/records/FavoriteMemeRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRoleRecord} from '~/records/GuildRoleRecord';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import type {UserRecord} from '~/records/UserRecord';
import type {Emoji} from '~/stores/EmojiStore';
import styles from './Autocomplete.module.css';
import {AutocompleteChannel} from './AutocompleteChannel';
import {AutocompleteCommand} from './AutocompleteCommand';
import {AutocompleteEmoji} from './AutocompleteEmoji';
import {AutocompleteGif} from './AutocompleteGif';
import {AutocompleteMeme} from './AutocompleteMeme';
import {AutocompleteMention} from './AutocompleteMention';
import {AutocompleteSticker} from './AutocompleteSticker';

export type AutocompleteType = 'mention' | 'channel' | 'emoji' | 'command' | 'meme' | 'gif' | 'sticker';

interface SimpleCommand {
	type: 'simple';
	name: string;
	content: string;
}

type ScrollerWithScrollableElement = ScrollerHandle & {
	getScrollableElement?: () => HTMLElement | null;
};

interface ActionCommand {
	type: 'action';
	name: string;
	permission?: bigint;
	requiresGuild?: boolean;
}

export type Command = SimpleCommand | ActionCommand;

export const COMMANDS: Array<Command> = [
	{type: 'simple', name: '/shrug', content: '¯\\_(ツ)_/¯'},
	{type: 'simple', name: '/tableflip', content: '(╯°□°)╯︵ ┻━┻'},
	{type: 'simple', name: '/unflip', content: '┬─┬ ノ( ゜-゜ノ)'},
	{type: 'action', name: '/me'},
	{type: 'action', name: '/spoiler'},
	{type: 'action', name: '/tts', permission: Permissions.SEND_TTS_MESSAGES},
	{type: 'action', name: '/nick', permission: Permissions.CHANGE_NICKNAME, requiresGuild: true},
	{type: 'action', name: '/kick', permission: Permissions.KICK_MEMBERS, requiresGuild: true},
	{type: 'action', name: '/ban', permission: Permissions.BAN_MEMBERS, requiresGuild: true},
	{type: 'action', name: '/msg'},
	{type: 'action', name: '/saved'},
	{type: 'action', name: '/sticker'},
	{type: 'action', name: '/gif'},
	{type: 'action', name: '/tenor'},
];

export type AutocompleteOption =
	| {type: 'mention'; kind: 'member'; member: GuildMemberRecord}
	| {type: 'mention'; kind: 'user'; user: UserRecord}
	| {type: 'mention'; kind: 'role'; role: GuildRoleRecord}
	| {type: 'mention'; kind: '@everyone' | '@here'}
	| {type: 'channel'; channel: ChannelRecord}
	| {type: 'emoji'; emoji: Emoji}
	| {type: 'command'; command: Command}
	| {type: 'meme'; meme: FavoriteMemeRecord}
	| {type: 'gif'; gif: TenorGif}
	| {type: 'sticker'; sticker: GuildStickerRecord};

export const isMentionMember = (
	o: AutocompleteOption,
): o is {type: 'mention'; kind: 'member'; member: GuildMemberRecord} => o.type === 'mention' && o.kind === 'member';
export const isMentionUser = (o: AutocompleteOption): o is {type: 'mention'; kind: 'user'; user: UserRecord} =>
	o.type === 'mention' && o.kind === 'user';
export const isMentionRole = (o: AutocompleteOption): o is {type: 'mention'; kind: 'role'; role: GuildRoleRecord} =>
	o.type === 'mention' && o.kind === 'role';
export const isSpecialMention = (o: AutocompleteOption): o is {type: 'mention'; kind: '@everyone' | '@here'} =>
	o.type === 'mention' && (o.kind === '@everyone' || o.kind === '@here');
export const isChannel = (o: AutocompleteOption): o is {type: 'channel'; channel: ChannelRecord} =>
	o.type === 'channel';
export const isEmoji = (o: AutocompleteOption): o is {type: 'emoji'; emoji: Emoji} => o.type === 'emoji';
export const isCommand = (o: AutocompleteOption): o is {type: 'command'; command: Command} => o.type === 'command';
export const isMeme = (o: AutocompleteOption): o is {type: 'meme'; meme: FavoriteMemeRecord} => o.type === 'meme';
export const isGif = (o: AutocompleteOption): o is {type: 'gif'; gif: TenorGif} => o.type === 'gif';
export const isSticker = (o: AutocompleteOption): o is {type: 'sticker'; sticker: GuildStickerRecord} =>
	o.type === 'sticker';

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
	}) => {
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

		const keyboardFocusIndex = externalSelectedIndex ?? internalKeyboardFocusIndex;

		const [referenceState, setReferenceState] = React.useState<HTMLElement | null>(referenceElement ?? null);

		React.useEffect(() => {
			setReferenceState(referenceElement ?? null);
		}, [referenceElement]);

		const {refs, floatingStyles} = useFloating({
			placement: 'top-start',
			open: true,
			whileElementsMounted: autoUpdate,
			elements: {reference: referenceState},
			middleware: [
				offset(attached ? 0 : 8),
				flip({padding: 16}),
				size({
					apply({rects, elements}) {
						Object.assign(elements.floating.style, {
							width: `${rects.reference.width}px`,
						});
					},
					padding: 16,
				}),
			],
		});

		const scrollerRef = React.useRef<ScrollerHandle>(null);
		const rowRefs = React.useRef<Array<HTMLButtonElement | null>>([]);

		if (rowRefs.current.length !== options.length) {
			rowRefs.current = Array(options.length).fill(null);
		}

		React.useEffect(() => {
			reset();
		}, [options.length, reset]);

		const handleKeyDown = React.useCallback(
			(event: React.KeyboardEvent) => {
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

		function scrollChildIntoView(node: HTMLElement | null, margin = 32) {
			if (!node) return;
			const scroller = scrollerRef.current as ScrollerWithScrollableElement | null;

			if (scroller && typeof scroller.scrollIntoViewNode === 'function') {
				scroller.scrollIntoViewNode({node, padding: margin});
				return;
			}

			const scrollerEl =
				scroller?.getScrollableElement?.() ||
				node.closest('[data-scrollable], .overflow-y-auto, .overflow-y-scroll') ||
				node.parentElement;

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
		}

		React.useLayoutEffect(() => {
			const node = rowRefs.current[keyboardFocusIndex] ?? null;
			if (!node) return;

			const raf = requestAnimationFrame(() => scrollChildIntoView(node, 32));
			return () => cancelAnimationFrame(raf);
		}, [keyboardFocusIndex, options.length]);

		return (
			<FloatingPortal>
				<div
					ref={refs.setFloating}
					style={{...floatingStyles, zIndex: zIndex ?? undefined}}
					className={`${styles.container} ${attached ? styles.containerAttached : styles.containerDetached}`}
					onKeyDown={handleKeyDown}
					role="listbox"
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
						/>
					) : (
						<Scroller
							ref={scrollerRef}
							className={styles.scroller}
							key="autocomplete-scroller"
							reserveScrollbarTrack={false}
						>
							{type === 'mention' ? (
								<AutocompleteMention
									onSelect={onSelect}
									keyboardFocusIndex={keyboardFocusIndex}
									hoverIndex={hoverIndexForRender}
									options={options}
									onMouseEnter={handleMouseEnter}
									onMouseLeave={handleMouseLeave}
									rowRefs={rowRefs}
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
								/>
							)}
						</Scroller>
					)}
				</div>
			</FloatingPortal>
		);
	},
);

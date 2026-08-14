// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {
	EMOJI_PICKER_CUSTOM_EMOJI_SIZE,
	getEmojiSpriteSheetLayout,
	getSpriteSheetBackground,
} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import type {Channel} from '@app/features/channel/models/Channel';
import * as EmojiPickerCommands from '@app/features/emoji/commands/EmojiPickerCommands';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {checkEmojiAvailability} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import {getEmojiDisplayDataWithSkinTone} from '@app/features/expressions/utils/SkinToneUtils';
import UnicodeEmojis, {EMOJI_SPRITES} from '@app/features/expressions/utils/UnicodeEmojis';
import {setUrlQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {EmojiContextMenuItems} from '@app/features/ui/action_menu/items/EmojiContextMenuItems';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {isFirefoxBrowser} from '@app/features/ui/utils/NativeUtils';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import React, {useEffect, useImperativeHandle, useRef} from 'react';

interface EmojiRendererProps {
	emoji: FlatEmoji;
	handleHover: (emoji: FlatEmoji | null) => void;
	handleSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	skinTone: string;
	channel: Channel | null;
	shouldAnimate: boolean;
	isHighlighted?: boolean;
	shouldScrollIntoView?: boolean;
}

export const EmojiRenderer = React.forwardRef<HTMLButtonElement, EmojiRendererProps>(
	(
		{
			emoji,
			handleHover,
			handleSelect,
			skinTone,
			channel,
			shouldAnimate,
			isHighlighted = false,
			shouldScrollIntoView = false,
			...props
		},
		forwardedRef,
	) => {
		const emojiRef = useRef<HTMLButtonElement | null>(null);
		const {i18n} = useLingui();
		useImperativeHandle(forwardedRef, () => emojiRef.current!);
		useEffect(() => {
			if (shouldScrollIntoView && emojiRef.current) {
				emojiRef.current.scrollIntoView({block: 'nearest', inline: 'nearest'});
			}
		}, [shouldScrollIntoView]);
		const availability = checkEmojiAvailability(i18n, emoji, channel);
		const customEmojiUrl = emoji.id
			? setUrlQueryParams(AvatarUtils.getEmojiURL({id: emoji.id, animated: Boolean(emoji.animated) && shouldAnimate}), {
					size: EMOJI_PICKER_CUSTOM_EMOJI_SIZE,
				})
			: (emoji.url ?? '');
		const handleClick = (e: React.MouseEvent) => {
			if (!availability.canUse) {
				e.preventDefault();
				e.stopPropagation();
				return;
			}
			if (e.altKey) {
				e.preventDefault();
				e.stopPropagation();
				EmojiPickerCommands.toggleFavorite(emoji);
				return;
			}
			handleSelect(emoji, e.shiftKey);
		};
		const handleContextMenu = (e: React.MouseEvent<HTMLButtonElement>) => {
			e.preventDefault();
			e.stopPropagation();
			ContextMenuCommands.openFromEvent(e, (props) => (
				<EmojiContextMenuItems
					emoji={emoji}
					onClose={props.onClose}
					data-flx="channel.emoji-picker.emoji-renderer.handle-context-menu.emoji-context-menu-items"
				/>
			));
		};
		const renderButton = (children: React.ReactNode) => {
			const isDisabled = !availability.canUse;
			const className = clsx(
				styles.emojiRenderer,
				isHighlighted && styles.selectedEmojiRenderer,
				isDisabled && 'cursor-not-allowed',
			);
			return (
				<FocusRing offset={-2} data-flx="channel.emoji-picker.emoji-renderer.render-button.focus-ring">
					<button
						type="button"
						tabIndex={-1}
						ref={emojiRef}
						onMouseEnter={() => handleHover(emoji)}
						onMouseLeave={() => handleHover(null)}
						onClick={handleClick}
						onContextMenu={handleContextMenu}
						className={className}
						aria-disabled={isDisabled}
						aria-selected={isHighlighted}
						role="option"
						data-flx="channel.emoji-picker.emoji-renderer.render-button.option.click.button"
						{...props}
					>
						{children}
					</button>
				</FocusRing>
			);
		};
		if (emoji.guildId || emoji.id) {
			const content = (
				<img
					src={customEmojiUrl}
					alt={emoji.name}
					className={styles.emojiImage}
					loading="lazy"
					data-flx="channel.emoji-picker.emoji-renderer.emoji-image"
				/>
			);
			return renderButton(content);
		}
		if (!emoji.useSpriteSheet) {
			return renderButton(
				<img
					src={emoji.url ?? ''}
					alt={emoji.name}
					className={styles.emojiImage}
					loading="lazy"
					data-flx="channel.emoji-picker.emoji-renderer.emoji-image--2"
				/>,
			);
		}
		const hasDiversity = emoji.hasDiversity && skinTone;
		const index = hasDiversity ? emoji.diversityIndex : emoji.index;
		if (isFirefoxBrowser()) {
			const {url} = getEmojiDisplayDataWithSkinTone(emoji, skinTone);
			if (url) {
				return renderButton(
					<img
						src={url}
						alt={emoji.name}
						className={styles.emojiImage}
						loading="lazy"
						data-flx="channel.emoji-picker.emoji-renderer.emoji-image--4"
					/>,
				);
			}
		}
		if (index === undefined) {
			return renderButton(
				<img
					src={emoji.url ?? ''}
					alt={emoji.name}
					className={styles.emojiImage}
					loading="lazy"
					data-flx="channel.emoji-picker.emoji-renderer.emoji-image--3"
				/>,
			);
		}
		const perRow = hasDiversity ? EMOJI_SPRITES.DiversityPerRow : EMOJI_SPRITES.NonDiversityPerRow;
		const rows = Math.ceil(
			(hasDiversity ? UnicodeEmojis.numDiversitySprites : UnicodeEmojis.numNonDiversitySprites) / perRow,
		);
		const spriteStyle = {
			backgroundImage: getSpriteSheetBackground(hasDiversity ? skinTone : ''),
			...getEmojiSpriteSheetLayout(index, perRow, rows),
		};
		return renderButton(
			<div
				className={styles.spriteEmoji}
				style={spriteStyle}
				data-flx="channel.emoji-picker.emoji-renderer.sprite-emoji"
			/>,
		);
	},
);

EmojiRenderer.displayName = 'EmojiRenderer';

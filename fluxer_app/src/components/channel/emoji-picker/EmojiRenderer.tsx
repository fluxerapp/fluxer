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
import {ClipboardIcon, StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as EmojiPickerActionCreators from '~/actions/EmojiPickerActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import styles from '~/components/channel/EmojiPicker.module.css';
import {EMOJI_SPRITE_SIZE, getSpriteSheetBackground} from '~/components/channel/emoji-picker/EmojiPickerConstants';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {EMOJI_SPRITES} from '~/lib/UnicodeEmojis';
import type {ChannelRecord} from '~/records/ChannelRecord';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import type {Emoji} from '~/stores/EmojiStore';
import {shouldUseNativeEmoji} from '~/utils/EmojiUtils';
import {checkEmojiAvailability} from '~/utils/ExpressionPermissionUtils';
import {shouldShowPremiumFeatures} from '~/utils/PremiumUtils';

interface EmojiRendererProps {
	emoji: Emoji;
	handleHover: (emoji: Emoji | null) => void;
	handleSelect: (emoji: Emoji, shiftKey?: boolean) => void;
	skinTone: string;
	spriteSheetSizes: {nonDiversitySize: string; diversitySize: string};
	channel: ChannelRecord | null;
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
			spriteSheetSizes,
			channel,
			isHighlighted = false,
			shouldScrollIntoView = false,
			...props
		},
		forwardedRef,
	) => {
		const emojiRef = React.useRef<HTMLButtonElement | null>(null);
		const {t, i18n} = useLingui();
		const isFavorite = EmojiPickerStore.isFavorite(emoji);

		React.useImperativeHandle(forwardedRef, () => emojiRef.current!);

		React.useEffect(() => {
			if (shouldScrollIntoView && emojiRef.current) {
				emojiRef.current.scrollIntoView({block: 'nearest', inline: 'nearest'});
			}
		}, [shouldScrollIntoView]);

		const availability = checkEmojiAvailability(i18n, emoji, channel);
		const isLocked = availability.isLockedByPremium;

		const handleClick = (e: React.MouseEvent) => {
			if (!availability.canUse) {
				e.preventDefault();
				e.stopPropagation();

				if (availability.isLockedByPremium && shouldShowPremiumFeatures()) {
					PremiumModalActionCreators.open();
				}

				return;
			}

			if (e.altKey) {
				e.preventDefault();
				e.stopPropagation();
				EmojiPickerActionCreators.toggleFavorite(emoji);
				return;
			}

			handleSelect(emoji, e.shiftKey);
		};

		const handleContextMenu = (e: React.MouseEvent<HTMLButtonElement>) => {
			e.preventDefault();
			e.stopPropagation();

			ContextMenuActionCreators.openFromEvent(e, (props) => (
				<>
					<MenuGroup>
						<MenuItem
							icon={<StarIcon className={styles.iconSmall} weight={isFavorite ? 'fill' : 'bold'} />}
							onClick={() => {
								EmojiPickerActionCreators.toggleFavorite(emoji);
							}}
						>
							{isFavorite ? t`Unfavorite Emoji` : t`Favorite Emoji`}
						</MenuItem>
						{emoji.id && (
							<MenuItem
								icon={<ClipboardIcon className={styles.iconSmall} />}
								onClick={() => {
									TextCopyActionCreators.copy(i18n, emoji.id!);
									props.onClose();
								}}
							>
								{t`Copy Emoji ID`}
							</MenuItem>
						)}
					</MenuGroup>
				</>
			));
		};

		const renderButton = (children: React.ReactNode, locked = false) => {
			const isDisabled = locked || !availability.canUse;
			const className = clsx(
				styles.emojiRenderer,
				isHighlighted && styles.selectedEmojiRenderer,
				isDisabled && 'cursor-not-allowed',
			);

			return (
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
					{...props}
				>
					{children}
				</button>
			);
		};

		if (emoji.guildId || emoji.id) {
			const content = (
				<img
					src={emoji.url ?? ''}
					alt={emoji.name}
					className={clsx(styles.emojiImage, isLocked && styles.emojiLocked)}
					loading="lazy"
				/>
			);

			if (isLocked) {
				return (
					<Tooltip text={availability.lockReason ?? t`Unlock external custom emojis with Plutonium`} position="top">
						{renderButton(content, true)}
					</Tooltip>
				);
			}

			return renderButton(content);
		}

		if (shouldUseNativeEmoji && emoji.surrogates) {
			const hasDiversity = emoji.hasDiversity && skinTone;
			const displayEmoji = hasDiversity ? emoji.surrogates + skinTone : emoji.surrogates;
			return renderButton(<span className={styles.nativeEmoji}>{displayEmoji}</span>);
		}

		if (!emoji.useSpriteSheet) {
			return renderButton(<img src={emoji.url ?? ''} alt={emoji.name} className={styles.emojiImage} loading="lazy" />);
		}

		const hasDiversity = emoji.hasDiversity && skinTone;
		const index = hasDiversity ? emoji.diversityIndex : emoji.index;

		if (index === undefined) {
			return renderButton(<img src={emoji.url ?? ''} alt={emoji.name} className={styles.emojiImage} loading="lazy" />);
		}

		const perRow = hasDiversity ? EMOJI_SPRITES.DiversityPerRow : EMOJI_SPRITES.NonDiversityPerRow;
		const x = -(index % perRow) * EMOJI_SPRITE_SIZE;
		const y = -Math.floor(index / perRow) * EMOJI_SPRITE_SIZE;

		const spriteStyle = {
			backgroundImage: getSpriteSheetBackground(hasDiversity ? skinTone : ''),
			backgroundPosition: `${x}px ${y}px`,
			backgroundSize: hasDiversity ? spriteSheetSizes.diversitySize : spriteSheetSizes.nonDiversitySize,
		};

		return renderButton(<div className={styles.spriteEmoji} style={spriteStyle} />);
	},
);

EmojiRenderer.displayName = 'EmojiRenderer';

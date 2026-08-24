// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {type AutocompleteOption, isEmoji, isMeme, isSticker} from '@app/features/channel/components/Autocomplete';
import styles from '@app/features/channel/components/AutocompleteEmoji.module.css';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {AutocompleteMemePreview} from '@app/features/channel/components/AutocompleteMemePreview';
import * as EmojiPickerCommands from '@app/features/emoji/commands/EmojiPickerCommands';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {getEmojiDisplayData} from '@app/features/expressions/utils/SkinToneUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {
	EMOJIS_DESCRIPTOR,
	MEDIA_DESCRIPTOR,
	STICKERS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DEFAULT_EMOJI_DESCRIPTOR = msg({
	message: 'Default emoji',
	comment: 'Short label in the channel and chat autocomplete emoji. Keep it concise.',
});
const SectionHeading = observer(({children}: {children: React.ReactNode}) => (
	<div className={styles.sectionHeading} data-flx="channel.autocomplete-emoji.section-heading.section-heading">
		{children}
	</div>
));
const AutocompleteEmojiIcon = observer(({emoji}: {emoji: FlatEmoji}) => {
	const isAnimatable = Boolean(emoji.animated);
	const shouldAnimate = useShouldAnimate({
		kind: 'emoji',
		isAnimated: isAnimatable,
	});
	const {url: fallbackDisplayUrl} = getEmojiDisplayData(emoji);
	const displayUrl =
		getEmojiRenderUrl({
			id: emoji.id,
			surrogateUrl: fallbackDisplayUrl ?? null,
			isAnimatable,
			animated: shouldAnimate,
			jumbo: false,
		}) ?? '';
	return (
		<img
			draggable={false}
			className={styles.emojiIcon}
			src={displayUrl}
			alt={emoji.name}
			aria-hidden={true}
			data-flx="channel.autocomplete-emoji.emoji-icon"
		/>
	);
});
const AutocompleteStickerIcon = observer(
	({sticker, isInteracting}: {sticker: GuildSticker; isInteracting: boolean}) => {
		const {shouldAnimate} = useStickerAnimation({isInteracting, isAnimated: sticker.animated});
		return (
			<div className={styles.stickerIconWrapper} data-flx="channel.autocomplete-emoji.sticker-icon-wrapper">
				<img
					draggable={false}
					className={styles.stickerIcon}
					src={AvatarUtils.getStickerURL({
						id: sticker.id,
						animated: shouldAnimate,
						isAnimatable: sticker.animated,
						size: 320,
					})}
					alt={sticker.name}
					aria-hidden={true}
					data-flx="channel.autocomplete-emoji.sticker-icon"
				/>
			</div>
		);
	},
);
export const AutocompleteEmoji = observer(
	({
		onSelect,
		keyboardFocusIndex,
		hoverIndex,
		options,
		onMouseEnter,
		onMouseLeave,
		rowRefs,
		getOptionId,
	}: {
		onSelect: (option: AutocompleteOption) => void;
		keyboardFocusIndex: number;
		hoverIndex: number;
		options: Array<AutocompleteOption>;
		onMouseEnter: (index: number) => void;
		onMouseLeave: () => void;
		rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
		getOptionId?: (index: number) => string;
	}) => {
		const {i18n} = useLingui();
		const emojis = options.filter(isEmoji);
		const stickers = options.filter(isSticker);
		const memes = options.filter(isMeme);
		const handleEmojiSelect = (option: AutocompleteOption) => {
			if (isEmoji(option)) EmojiPickerCommands.trackEmojiUsage(option.emoji);
			onSelect(option);
		};
		return (
			<>
				{emojis.length > 0 && (
					<>
						<SectionHeading data-flx="channel.autocomplete-emoji.section-heading">
							{i18n._(EMOJIS_DESCRIPTOR)}
						</SectionHeading>
						{emojis.map((option, index) => {
							return (
								<AutocompleteItem
									key={option.emoji.name}
									id={getOptionId?.(index)}
									name={`:${option.emoji.name}:`}
									description={
										option.emoji.guildId
											? Guilds.getGuild(option.emoji.guildId)?.name
											: i18n._(DEFAULT_EMOJI_DESCRIPTOR)
									}
									icon={
										<AutocompleteEmojiIcon
											emoji={option.emoji}
											data-flx="channel.autocomplete-emoji.autocomplete-emoji-icon"
										/>
									}
									isKeyboardSelected={index === keyboardFocusIndex}
									isHovered={index === hoverIndex}
									onSelect={() => handleEmojiSelect(option)}
									onMouseEnter={() => onMouseEnter(index)}
									onMouseLeave={onMouseLeave}
									innerRef={
										rowRefs
											? (node) => {
													rowRefs.current[index] = node;
												}
											: undefined
									}
									data-flx="channel.autocomplete-emoji.autocomplete-item.emoji-select"
								/>
							);
						})}
						{(stickers.length > 0 || memes.length > 0) && (
							<div className={styles.divider} aria-hidden={true} data-flx="channel.autocomplete-emoji.divider" />
						)}
					</>
				)}
				{stickers.length > 0 && (
					<>
						<SectionHeading data-flx="channel.autocomplete-emoji.section-heading--2">
							{i18n._(STICKERS_DESCRIPTOR)}
						</SectionHeading>
						{stickers.map((option, index) => {
							const currentIndex = emojis.length + index;
							return (
								<AutocompleteItem
									key={option.sticker.id}
									id={getOptionId?.(currentIndex)}
									name={option.sticker.name}
									description={
										option.sticker.tags.length > 0
											? option.sticker.tags.join(', ')
											: option.sticker.description || undefined
									}
									icon={
										<AutocompleteStickerIcon
											sticker={option.sticker}
											isInteracting={currentIndex === keyboardFocusIndex || currentIndex === hoverIndex}
											data-flx="channel.autocomplete-emoji.autocomplete-sticker-icon"
										/>
									}
									isKeyboardSelected={currentIndex === keyboardFocusIndex}
									isHovered={currentIndex === hoverIndex}
									onSelect={() => onSelect(option)}
									onMouseEnter={() => onMouseEnter(currentIndex)}
									onMouseLeave={onMouseLeave}
									innerRef={
										rowRefs
											? (node) => {
													rowRefs.current[currentIndex] = node;
												}
											: undefined
									}
									data-flx="channel.autocomplete-emoji.autocomplete-item.select"
								/>
							);
						})}
						{memes.length > 0 && (
							<div className={styles.divider} aria-hidden={true} data-flx="channel.autocomplete-emoji.divider--2" />
						)}
					</>
				)}
				{memes.length > 0 && (
					<>
						<SectionHeading data-flx="channel.autocomplete-emoji.section-heading--3">
							{i18n._(MEDIA_DESCRIPTOR)}
						</SectionHeading>
						{memes.map((option, index) => {
							const currentIndex = emojis.length + stickers.length + index;
							return (
								<AutocompleteItem
									key={option.meme.id}
									id={getOptionId?.(currentIndex)}
									name={option.meme.name}
									description={option.meme.tags.length > 0 ? option.meme.tags.join(', ') : undefined}
									icon={
										<div className={styles.memeIconWrapper} data-flx="channel.autocomplete-emoji.meme-icon-wrapper">
											<AutocompleteMemePreview
												meme={option.meme}
												data-flx="channel.autocomplete-emoji.autocomplete-meme-preview"
											/>
										</div>
									}
									isKeyboardSelected={currentIndex === keyboardFocusIndex}
									isHovered={currentIndex === hoverIndex}
									onSelect={() => onSelect(option)}
									onMouseEnter={() => onMouseEnter(currentIndex)}
									onMouseLeave={onMouseLeave}
									innerRef={
										rowRefs
											? (node) => {
													rowRefs.current[currentIndex] = node;
												}
											: undefined
									}
									data-flx="channel.autocomplete-emoji.autocomplete-item.select--2"
								/>
							);
						})}
					</>
				)}
			</>
		);
	},
);

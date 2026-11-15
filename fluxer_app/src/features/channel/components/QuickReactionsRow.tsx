// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/QuickReactionsRow.module.css';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {useExpressionImagesPreload} from '@app/features/expressions/utils/ExpressionImageCache';
import {getEmojiDisplayData} from '@app/features/expressions/utils/SkinToneUtils';
import {ReactionImage} from '@app/features/messaging/components/ReactionImage';
import {msg} from '@lingui/core/macro';
import type React from 'react';
import {useMemo} from 'react';

export const REACT_WITH_EMOJI_DESCRIPTOR = msg({
	message: 'React with {emojiShortcode}',
	comment:
		'Accessible label for a quick reaction emoji button. Preserve {emojiShortcode}; it is inserted by code, usually like :smile:.',
});

export const QuickReactionEmoji: React.FC<{emoji: FlatEmoji; 'data-flx'?: string}> = ({emoji, 'data-flx': dataFlx}) => {
	const emojiSrc = useReactionMenuEmojiSrc(emoji);
	return (
		<ReactionImage
			src={emojiSrc}
			alt={emoji.name}
			aria-hidden={true}
			draggable={false}
			className={styles.emojiImg}
			data-flx={dataFlx ?? 'channel.quick-reactions-row.render-quick-reaction-emoji.emoji-img'}
		/>
	);
};

function getReactionMenuEmojiSrc(emoji: FlatEmoji, animationAllowed: boolean): string {
	const {url: displayUrl} = getEmojiDisplayData(emoji);
	return emoji.id
		? buildCustomEmojiURL({id: emoji.id, animated: emoji.animated === true && animationAllowed})
		: (displayUrl ?? '');
}

export function useReactionMenuEmojiSrc(emoji: FlatEmoji): string {
	const animationAllowed = useShouldAnimate({kind: 'emoji', isAnimated: emoji.animated === true});
	return getReactionMenuEmojiSrc(emoji, animationAllowed);
}

const NO_SUBMENU_EMOJIS: ReadonlyArray<FlatEmoji> = [];

export function useReactionMenuImagePreload(
	quickReactionEmojis: ReadonlyArray<FlatEmoji>,
	submenuReactionEmojis: ReadonlyArray<FlatEmoji> = NO_SUBMENU_EMOJIS,
): void {
	const animationAllowed = useShouldAnimate({kind: 'emoji', isAnimated: true});
	const urls = useMemo(
		() =>
			Array.from(
				new Set(
					[...quickReactionEmojis, ...submenuReactionEmojis].map((emoji) =>
						getReactionMenuEmojiSrc(emoji, animationAllowed),
					),
				),
			).filter(Boolean),
		[quickReactionEmojis, submenuReactionEmojis, animationAllowed],
	);
	useExpressionImagesPreload(urls);
}

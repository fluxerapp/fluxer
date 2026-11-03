// SPDX-License-Identifier: AGPL-3.0-or-later
import premiumStyles from '@app/features/channel/components/PremiumUpsellBanner.module.css';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {getEmojiDisplayDataWithSkinTone} from '@app/features/expressions/utils/SkinToneUtils';
import {getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
export function renderEmojiPickerPreviewItem(emoji: FlatEmoji, skinTone: string, shouldAnimateEmoji: boolean) {
	const key = emoji.id ?? emoji.uniqueName ?? emoji.name ?? `${emoji.guildId ?? 'unicode'}-preview`;
	const {url: fallbackDisplayUrl} = getEmojiDisplayDataWithSkinTone(emoji, skinTone);
	const displayUrl = emoji.id
		? getEmojiRenderUrl({
				id: emoji.id,
				surrogateUrl: null,
				isAnimatable: Boolean(emoji.animated),
				animated: shouldAnimateEmoji,
				jumbo: false,
			})
		: fallbackDisplayUrl;
	const content = displayUrl ? (
		<img src={displayUrl} alt={emoji.name} data-flx="channel.emoji-picker.render-emoji-preview-item.img" />
	) : (
		<span className={premiumStyles.previewEmojiText} data-flx="channel.emoji-picker.render-emoji-preview-item.span">
			{emoji.name ?? emoji.uniqueName}
		</span>
	);
	return (
		<div className={premiumStyles.previewItem} key={key} data-flx="channel.emoji-picker.render-emoji-preview-item.div">
			{content}
		</div>
	);
}

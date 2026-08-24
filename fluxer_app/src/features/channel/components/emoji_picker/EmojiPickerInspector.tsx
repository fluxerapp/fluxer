// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {
	getEmojiSpriteSheetLayout,
	getSpriteSheetBackground,
} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {getEmojiDisplayDataWithSkinTone} from '@app/features/expressions/utils/SkinToneUtils';
import UnicodeEmojis, {EMOJI_SPRITES} from '@app/features/expressions/utils/UnicodeEmojis';
import Guilds from '@app/features/guild/state/Guilds';
import {getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import {isFirefoxBrowser} from '@app/features/ui/utils/NativeUtils';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface EmojiPickerInspectorProps {
	hoveredEmoji: FlatEmoji | null;
}

export const EmojiPickerInspector = observer(({hoveredEmoji}: EmojiPickerInspectorProps) => {
	const skinTone = Emoji.skinTone;
	const shouldAnimateEmoji = useShouldAnimate({
		kind: 'emoji',
		isAnimated: Boolean(hoveredEmoji?.animated),
		isHovering: Boolean(hoveredEmoji),
	});
	const getEmojiForDisplay = (
		emoji: FlatEmoji | null,
	): {useImg: boolean; url?: string; style?: React.CSSProperties} | null => {
		if (!emoji) return null;
		if (emoji.guildId || emoji.id) {
			return {
				url: emoji.id
					? (getEmojiRenderUrl({
							id: emoji.id,
							surrogateUrl: null,
							isAnimatable: Boolean(emoji.animated),
							animated: shouldAnimateEmoji,
							jumbo: false,
						}) ?? '')
					: (emoji.url ?? ''),
				useImg: true,
			};
		}
		if (!emoji.useSpriteSheet) {
			return {url: emoji.url, useImg: true};
		}
		if (isFirefoxBrowser()) {
			const {url} = getEmojiDisplayDataWithSkinTone(emoji, skinTone);
			if (url) return {url, useImg: true};
		}
		const hasSkinTones = emoji.hasSkinTones && skinTone;
		const index = hasSkinTones ? emoji.skinToneIndex : emoji.index;
		if (index === undefined) return {url: emoji.url, useImg: true};
		const perRow = hasSkinTones ? EMOJI_SPRITES.SkinTonePerRow : EMOJI_SPRITES.BasePerRow;
		const rows = Math.ceil((hasSkinTones ? UnicodeEmojis.skinToneSpriteCount : UnicodeEmojis.baseSpriteCount) / perRow);
		return {
			style: {
				backgroundImage: getSpriteSheetBackground(hasSkinTones ? skinTone : ''),
				...getEmojiSpriteSheetLayout(index, perRow, rows),
			},
			useImg: false,
		};
	};
	const emojiDisplay = getEmojiForDisplay(hoveredEmoji);
	const sourceGuild = hoveredEmoji?.guildId ? Guilds.getGuild(hoveredEmoji.guildId) : null;
	const renderEmoji = () => {
		if (!emojiDisplay || !hoveredEmoji) return null;
		if (emojiDisplay.useImg) {
			return (
				<img
					src={emojiDisplay.url ?? ''}
					alt={hoveredEmoji.name}
					className={styles.inspectorEmoji}
					data-flx="channel.emoji-picker.emoji-picker-inspector.render-emoji.inspector-emoji"
				/>
			);
		}
		return (
			<div
				className={styles.inspectorEmojiSprite}
				style={emojiDisplay.style}
				data-flx="channel.emoji-picker.emoji-picker-inspector.render-emoji.inspector-emoji-sprite"
			/>
		);
	};
	return (
		<div className={styles.inspector} data-flx="channel.emoji-picker.emoji-picker-inspector.inspector">
			{hoveredEmoji && (
				<>
					{renderEmoji()}
					<div
						className={styles.inspectorTextContainer}
						data-flx="channel.emoji-picker.emoji-picker-inspector.inspector-text-container"
					>
						<span
							className={styles.inspectorText}
							data-flx="channel.emoji-picker.emoji-picker-inspector.inspector-text"
						>
							{hoveredEmoji.allNamesString}
						</span>
						{sourceGuild && (
							<span
								className={styles.inspectorSourceText}
								data-flx="channel.emoji-picker.emoji-picker-inspector.inspector-source-text"
							>
								<Trans>
									from <strong data-flx="channel.emoji-picker.emoji-picker-inspector.strong">{sourceGuild.name}</strong>
								</Trans>
							</span>
						)}
					</div>
				</>
			)}
		</div>
	);
});

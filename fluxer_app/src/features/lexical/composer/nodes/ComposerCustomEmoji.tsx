// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import Guilds from '@app/features/guild/state/Guilds';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {getEmojiRenderUrl} from '@app/features/messaging/utils/markdown/EmojiDetector';
import {EmojiWithTooltip} from '@app/features/ui/emoji_tooltip_content/EmojiWithTooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useContext} from 'react';

const CUSTOM_EMOJI_DESCRIPTOR = msg({
	message: 'Custom emoji {emojiName} from a community',
	comment:
		'Accessible label for a custom emoji in the message composer when its community name is unavailable. Preserve {emojiName}.',
});
const CUSTOM_EMOJI_FROM_COMMUNITY_DESCRIPTOR = msg({
	message: 'Custom emoji {emojiName} from {communityName}',
	comment: 'Accessible label for a custom emoji in the message composer. Preserve {emojiName} and {communityName}.',
});

interface ComposerCustomEmojiProps {
	emojiId: string;
	animated: boolean;
	display: string;
}

export const ComposerCustomEmoji = observer(({emojiId, animated, display}: ComposerCustomEmojiProps) => {
	const {plainText} = useContext(ComposerMentionContext);
	const {i18n} = useLingui();
	const record = Emoji.getEmojiById(emojiId);
	const isAnimatable = record?.animated ?? animated;
	const shouldAnimate = useShouldAnimate({
		kind: 'emoji',
		isAnimated: isAnimatable,
	});
	if (plainText) {
		return (
			<span
				className={styles.plainText}
				contentEditable={false}
				data-flx="lexical.composer.nodes.composer-custom-emoji.plain-text"
			>
				{display}
			</span>
		);
	}
	const name = display.replace(/^:|:$/g, '');
	const emojiForSubtext: FlatEmoji =
		record == null
			? {
					id: emojiId,
					animated,
					name,
					uniqueName: name,
					allNamesString: display,
				}
			: record;
	const guildId = record == null ? null : record.guildId;
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const communityName = guild == null ? null : guild.name;
	const accessibleLabel = communityName
		? i18n._(CUSTOM_EMOJI_FROM_COMMUNITY_DESCRIPTOR, {emojiName: display, communityName})
		: i18n._(CUSTOM_EMOJI_DESCRIPTOR, {emojiName: display});
	const displayUrl =
		getEmojiRenderUrl({
			id: emojiId,
			surrogateUrl: null,
			isAnimatable,
			animated: shouldAnimate,
			jumbo: false,
		}) ?? '';
	return (
		<EmojiWithTooltip
			emojiUrl={displayUrl}
			emojiName={display}
			emojiForSubtext={emojiForSubtext}
			data-flx="lexical.composer.nodes.composer-custom-emoji.emoji-with-tooltip"
		>
			<img
				src={displayUrl}
				alt={accessibleLabel}
				className={styles.customEmoji}
				draggable={false}
				contentEditable={false}
				data-flx="lexical.composer.nodes.composer-custom-emoji.custom-emoji"
			/>
		</EmojiWithTooltip>
	);
});

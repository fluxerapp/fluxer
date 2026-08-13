// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import Guilds from '@app/features/guild/state/Guilds';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {setUrlQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {EmojiWithTooltip} from '@app/features/ui/emoji_tooltip_content/EmojiWithTooltip';
import * as AvatarSourceUtils from '@app/features/user/utils/AvatarSourceUtils';
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
	const shouldAnimate = useShouldAnimate({kind: 'emoji'});
	if (plainText) {
		return (
			<span className={styles.plainText} contentEditable={false}>
				{display}
			</span>
		);
	}
	const name = display.replace(/^:|:$/g, '');
	const record = Emoji.getEmojiById(emojiId);
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
	const displayUrl = AvatarSourceUtils.getEmojiURL({id: emojiId, animated: animated && shouldAnimate});
	const tooltipUrl = setUrlQueryParams(displayUrl, {size: 240, quality: 'lossless'});
	return (
		<EmojiWithTooltip emojiUrl={tooltipUrl} emojiName={display} emojiForSubtext={emojiForSubtext}>
			<img
				src={displayUrl}
				alt={accessibleLabel}
				className={styles.customEmoji}
				draggable={false}
				contentEditable={false}
			/>
		</EmojiWithTooltip>
	);
});

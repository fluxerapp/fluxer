// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {EmojiWithTooltip} from '@app/features/ui/emoji_tooltip_content/EmojiWithTooltip';
import {useContext} from 'react';

interface ComposerStandardEmojiProps {
	name: string;
	surrogate: string;
	url: string | null;
	display: string;
}

export const ComposerStandardEmoji = ({name, surrogate, url, display}: ComposerStandardEmojiProps) => {
	const {plainText} = useContext(ComposerMentionContext);
	if (plainText) {
		return (
			<span className={styles.plainText} contentEditable={false}>
				{display}
			</span>
		);
	}
	const emojiForSubtext: FlatEmoji = {
		name,
		uniqueName: name,
		allNamesString: display,
		surrogates: surrogate,
		animated: false,
		url: url == null ? undefined : url,
	};
	const image = url ? (
		<img src={url} alt={display} className={styles.customEmoji} draggable={false} contentEditable={false} />
	) : (
		<span className="emoji" role="img" aria-label={display} contentEditable={false}>
			{surrogate}
		</span>
	);
	return (
		<EmojiWithTooltip emojiUrl={url} emojiName={display} emojiForSubtext={emojiForSubtext}>
			{image}
		</EmojiWithTooltip>
	);
};

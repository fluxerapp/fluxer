// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import ExpressionInfoCardRollout from '@app/features/expressions/state/ExpressionInfoCardRollout';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {EXPRESSION_TOOLTIP_DELAY_MS} from '@app/features/expressions/utils/ExpressionPreviewConstants';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {EmojiWithTooltip} from '@app/features/ui/emoji_tooltip_content/EmojiWithTooltip';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {observer} from 'mobx-react-lite';
import {useContext} from 'react';

interface ComposerStandardEmojiProps {
	name: string;
	surrogate: string;
	url: string | null;
	display: string;
}

export const ComposerStandardEmoji = observer(({name, surrogate, url, display}: ComposerStandardEmojiProps) => {
	const {plainText} = useContext(ComposerMentionContext);
	if (plainText) {
		return (
			<span
				className={styles.plainText}
				contentEditable={false}
				data-flx="lexical.composer.nodes.composer-standard-emoji.plain-text"
			>
				{display}
			</span>
		);
	}
	const imageUrl = url == null ? EmojiUtils.getEmojiURL(surrogate) : url;
	if (!ExpressionInfoCardRollout.enabled) {
		const emojiForSubtext: FlatEmoji = {
			name,
			uniqueName: name,
			allNamesString: display,
			surrogates: surrogate,
			animated: false,
			url: imageUrl == null ? undefined : imageUrl,
		};
		return (
			<EmojiWithTooltip
				emojiUrl={imageUrl}
				emojiName={display}
				emojiForSubtext={emojiForSubtext}
				data-flx="lexical.composer.nodes.composer-standard-emoji.emoji-with-tooltip"
			>
				{imageUrl ? (
					<img
						src={imageUrl}
						alt={display}
						className={styles.customEmoji}
						draggable={false}
						contentEditable={false}
						data-flx="lexical.composer.nodes.composer-standard-emoji.custom-emoji.control"
					/>
				) : (
					<span
						className="emoji"
						role="img"
						aria-label={display}
						contentEditable={false}
						data-flx="lexical.composer.nodes.composer-standard-emoji.emoji.control"
					>
						{surrogate}
					</span>
				)}
			</EmojiWithTooltip>
		);
	}
	return (
		<Tooltip
			text={display}
			delay={EXPRESSION_TOOLTIP_DELAY_MS}
			data-flx="lexical.composer.nodes.composer-standard-emoji.tooltip"
		>
			{imageUrl ? (
				<img
					src={imageUrl}
					alt={display}
					aria-label={display}
					className={styles.customEmoji}
					draggable={false}
					contentEditable={false}
					data-flx="lexical.composer.nodes.composer-standard-emoji.custom-emoji"
				/>
			) : (
				<span
					className="emoji"
					role="img"
					aria-label={display}
					contentEditable={false}
					data-flx="lexical.composer.nodes.composer-standard-emoji.emoji"
				>
					{surrogate}
				</span>
			)}
		</Tooltip>
	);
});

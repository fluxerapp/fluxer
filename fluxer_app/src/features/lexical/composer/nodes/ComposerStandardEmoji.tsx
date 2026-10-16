// SPDX-License-Identifier: AGPL-3.0-or-later

import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {EXPRESSION_TOOLTIP_DELAY_MS} from '@app/features/expressions/utils/ExpressionPreviewConstants';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {useContext} from 'react';

interface ComposerStandardEmojiProps {
	surrogate: string;
	url: string | null;
	display: string;
}

export const ComposerStandardEmoji = ({surrogate, url, display}: ComposerStandardEmojiProps) => {
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
};

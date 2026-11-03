// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const EmojiPickerPopout = observer(
	({
		channelId,
		roleIconGuildId,
		handleSelect,
		onClose,
	}: {
		channelId: string | null;
		roleIconGuildId?: string;
		handleSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
		onClose?: () => void;
	}) => {
		const handleEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey?: boolean) => {
				handleSelect(emoji, shiftKey);
				if (!shiftKey && onClose) {
					onClose();
				}
			},
			[handleSelect, onClose],
		);
		return (
			<ExpressionPickerPopout
				channelId={channelId ?? undefined}
				roleIconGuildId={roleIconGuildId}
				onEmojiSelect={handleEmojiSelect}
				onClose={onClose}
				visibleTabs={['emojis']}
				data-flx="emoji.emoji-picker-popout.expression-picker-popout"
			/>
		);
	},
);

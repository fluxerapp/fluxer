/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import * as ExpressionPickerActionCreators from '~/actions/ExpressionPickerActionCreators';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import EmojiStore, {type Emoji} from '~/stores/EmojiStore';
import type {MentionSegment} from '~/utils/TextareaSegmentManager';

interface UseTextareaEmojiPickerReturn {
	handleEmojiSelect: (emoji: Emoji, shiftKey?: boolean) => void;
}

interface UseTextareaEmojiPickerParams {
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	insertSegment: (
		currentText: string,
		insertPosition: number,
		displayText: string,
		actualText: string,
		type: MentionSegment['type'],
		id: string,
	) => {newText: string};
	previousValueRef: React.MutableRefObject<string>;
	channelId?: string;
}

export function useTextareaEmojiPicker({
	setValue,
	textareaRef,
	insertSegment,
	previousValueRef,
	channelId,
}: UseTextareaEmojiPickerParams): UseTextareaEmojiPickerReturn {
	const handleEmojiSelect = React.useCallback(
		(emoji: Emoji, shiftKey?: boolean) => {
			const actualText = EmojiStore.getEmojiMarkdown(emoji);
			const displayText = `:${emoji.name}:`;

			setValue((prevValue) => {
				const needsSpace = prevValue.length > 0 && !prevValue.endsWith(' ');
				const prefix = prevValue.length === 0 ? '' : needsSpace ? ' ' : '';
				const insertPosition = prevValue.length + prefix.length;

				const {newText} = insertSegment(
					prevValue + prefix,
					insertPosition,
					`${displayText} `,
					`${actualText} `,
					'emoji',
					emoji.id ?? emoji.uniqueName,
				);

				previousValueRef.current = newText;
				return newText;
			});
			textareaRef.current?.focus();

			if (!shiftKey && channelId) {
				ExpressionPickerActionCreators.close();
				PopoutActionCreators.close(`expression-picker-${channelId}`);
			}
		},
		[insertSegment, setValue, textareaRef, previousValueRef, channelId],
	);

	return {
		handleEmojiSelect,
	};
}

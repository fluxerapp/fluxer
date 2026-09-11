// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import TextareaSelection from '@app/features/messaging/state/TextareaSelection';
import {focusTextareaWithSelection} from '@app/features/messaging/utils/TextareaSelectionUtils';
import {useEffect, useRef, useState} from 'react';

interface UseTextareaEditingOptions {
	channelId: string;
	editingMessageId: string | null;
	editingMessage: Message | null;
	isMobileEditMode: boolean;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	previousValueRef: React.RefObject<string>;
}

export const useTextareaEditing = ({
	channelId,
	editingMessageId,
	editingMessage,
	isMobileEditMode,
	value,
	setValue,
	textareaRef,
	previousValueRef,
}: UseTextareaEditingOptions) => {
	const [wasEditing, setWasEditing] = useState(false);
	const hasInitializedEditingRef = useRef(false);
	useEffect(() => {
		if (editingMessageId) {
			setWasEditing(true);
		} else if (wasEditing) {
			const textarea = textareaRef.current;
			if (textarea) {
				focusTextareaWithSelection(textarea, TextareaSelection.getChannelSelection(channelId), value.length);
			}
			setWasEditing(false);
		}
	}, [channelId, editingMessageId, wasEditing, value.length, textareaRef]);
	useEffect(() => {
		if (editingMessage && isMobileEditMode) {
			if (!hasInitializedEditingRef.current) {
				hasInitializedEditingRef.current = true;
				setValue(editingMessage.content);
				if (previousValueRef.current !== null) {
					previousValueRef.current = editingMessage.content;
				}
				requestAnimationFrame(() => {
					const textarea = textareaRef.current;
					if (!textarea) {
						return;
					}
					focusTextareaWithSelection(
						textarea,
						TextareaSelection.getEditingSelection(channelId, editingMessage.id),
						editingMessage.content.length,
					);
				});
			}
		} else {
			hasInitializedEditingRef.current = false;
		}
	}, [channelId, editingMessage, isMobileEditMode, previousValueRef, setValue, textareaRef]);
};

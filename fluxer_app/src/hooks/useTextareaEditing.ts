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
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import type {MessageRecord} from '~/records/MessageRecord';

interface UseTextareaEditingOptions {
	channelId: string;
	editingMessageId: string | null;
	editingMessage: MessageRecord | null;
	isMobileEditMode: boolean;
	replyingMessage: {messageId: string; mentioning: boolean} | null;
	value: string;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	previousValueRef: React.MutableRefObject<string>;
}

export const useTextareaEditing = ({
	editingMessageId,
	editingMessage,
	isMobileEditMode,
	replyingMessage,
	value,
	setValue,
	textareaRef,
	previousValueRef,
}: UseTextareaEditingOptions) => {
	const [wasEditing, setWasEditing] = React.useState(false);
	const hasInitializedEditingRef = React.useRef(false);
	const notifyLayoutResized = React.useCallback(() => {
		ComponentDispatch.dispatch('LAYOUT_RESIZED');
	}, []);

	React.useEffect(() => {
		if (editingMessageId) {
			setWasEditing(true);
		} else if (wasEditing) {
			textareaRef.current?.focus();
			textareaRef.current?.setSelectionRange(value.length, value.length);
			setWasEditing(false);
		}
	}, [editingMessageId, wasEditing, value.length, textareaRef]);

	React.useEffect(() => {
		if (editingMessage && isMobileEditMode) {
			if (!hasInitializedEditingRef.current) {
				hasInitializedEditingRef.current = true;
				setValue(editingMessage.content);
				if (previousValueRef.current !== null) {
					previousValueRef.current = editingMessage.content;
				}
				textareaRef.current?.focus();
				textareaRef.current?.setSelectionRange(editingMessage.content.length, editingMessage.content.length);
			}
		} else {
			hasInitializedEditingRef.current = false;
		}
	}, [editingMessage, isMobileEditMode, previousValueRef, setValue, textareaRef]);

	React.useEffect(() => {
		if (editingMessage && isMobileEditMode) {
			notifyLayoutResized();
		}
	}, [editingMessage, isMobileEditMode, notifyLayoutResized]);

	React.useEffect(() => {
		if (replyingMessage) {
			notifyLayoutResized();
		}
	}, [replyingMessage, notifyLayoutResized]);
};

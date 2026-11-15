// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {useEffect, useRef} from 'react';

interface UseTextareaEditingOptions {
	editingMessage: Message | null;
	isMobileEditMode: boolean;
	setValue: React.Dispatch<React.SetStateAction<string>>;
	previousValueRef: React.RefObject<string>;
}

export function useTextareaEditing({
	editingMessage,
	isMobileEditMode,
	setValue,
	previousValueRef,
}: UseTextareaEditingOptions) {
	const hasInitializedEditingRef = useRef(false);
	useEffect(() => {
		if (editingMessage && isMobileEditMode) {
			if (!hasInitializedEditingRef.current) {
				hasInitializedEditingRef.current = true;
				setValue(editingMessage.content);
				previousValueRef.current = editingMessage.content;
			}
		} else {
			hasInitializedEditingRef.current = false;
		}
	}, [editingMessage, isMobileEditMode, previousValueRef, setValue]);
}

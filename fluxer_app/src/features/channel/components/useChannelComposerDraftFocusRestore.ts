// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {canFocusTextarea} from '@app/features/platform/utils/InputFocusManager';
import type React from 'react';
import {useEffect} from 'react';

interface UseChannelComposerDraftFocusRestoreParams {
	handleRef: React.RefObject<ComposerHandle | null>;
	editableRef: React.RefObject<HTMLDivElement | null>;
	initialDraft: string;
	textareaInputDisabled: boolean;
	inlineEditActive: boolean;
}

export function useChannelComposerDraftFocusRestore({
	handleRef,
	editableRef,
	initialDraft,
	textareaInputDisabled,
	inlineEditActive,
}: UseChannelComposerDraftFocusRestoreParams): void {
	useEffect(() => {
		if (textareaInputDisabled) return;
		if (inlineEditActive) return;
		if (initialDraft.length === 0) return;
		const element = editableRef.current;
		if (!canFocusTextarea(element === null ? undefined : element)) return;
		const handle = handleRef.current;
		if (handle === null) return;
		handle.focus();
	}, []);
}

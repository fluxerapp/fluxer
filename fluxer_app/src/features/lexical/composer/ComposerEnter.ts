// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComposerTypeaheadActiveState} from '@app/features/lexical/composer/ComposerTypeaheadModifierGuard';
import {isOffsetInsideCodeBlock} from '@app/features/lexical/composer/codeBlockIndent';
import {$getComposerScanText, $getComposerSelectionRange} from '@app/features/lexical/composer/composerOffsets';
import {$focusFirstInvalidSlashSlot} from '@app/features/lexical/composer/slashSlots';
import {COMMAND_PRIORITY_HIGH, KEY_ENTER_COMMAND, type LexicalEditor} from 'lexical';

interface ComposerEnterOptions {
	typeaheadActiveState: ComposerTypeaheadActiveState;
	getSubmitOnEnter: () => boolean;
	getOnEnter: () => (() => void) | undefined;
}

function $isSelectionInCodeBlock(): boolean {
	const range = $getComposerSelectionRange();
	return range != null && isOffsetInsideCodeBlock($getComposerScanText(), range.start);
}

export function registerComposerEnter(
	editor: LexicalEditor,
	{typeaheadActiveState, getSubmitOnEnter, getOnEnter}: ComposerEnterOptions,
): () => void {
	return editor.registerCommand(
		KEY_ENTER_COMMAND,
		(event: KeyboardEvent | null) => {
			if (typeaheadActiveState.current || event == null) {
				return false;
			}
			const onEnter = getOnEnter();
			if (getSubmitOnEnter() && onEnter != null) {
				if (!event.shiftKey) {
					if (!event.ctrlKey && !event.metaKey && $isSelectionInCodeBlock()) {
						return false;
					}
					event.preventDefault();
					if ($focusFirstInvalidSlashSlot()) {
						return true;
					}
					onEnter();
					return true;
				}
				return false;
			}
			if ((event.metaKey || event.ctrlKey) && onEnter != null) {
				event.preventDefault();
				if ($focusFirstInvalidSlashSlot()) {
					return true;
				}
				onEnter();
				return true;
			}
			return false;
		},
		COMMAND_PRIORITY_HIGH,
	);
}

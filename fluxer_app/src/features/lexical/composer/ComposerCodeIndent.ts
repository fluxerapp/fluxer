// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComposerTypeaheadActiveState} from '@app/features/lexical/composer/ComposerTypeaheadModifierGuard';
import {analyzeCodeIndent} from '@app/features/lexical/composer/codeBlockIndent';
import {
	$captureSelectionOffsets,
	$getComposerDisplayText,
	$replaceComposerRange,
	$selectComposerRange,
} from '@app/features/lexical/composer/composerOffsets';
import {COMMAND_PRIORITY_HIGH, KEY_TAB_COMMAND, type LexicalEditor} from 'lexical';

export function registerComposerCodeIndent(
	editor: LexicalEditor,
	typeaheadActiveRef: ComposerTypeaheadActiveState,
): () => void {
	return editor.registerCommand(
		KEY_TAB_COMMAND,
		(event: KeyboardEvent | null) => {
			if (typeaheadActiveRef.current) {
				return false;
			}
			const offsets = $captureSelectionOffsets();
			if (offsets == null) {
				return false;
			}
			const backward = offsets.anchor > offsets.focus;
			const start = Math.min(offsets.anchor, offsets.focus);
			const end = Math.max(offsets.anchor, offsets.focus);
			const plan = analyzeCodeIndent($getComposerDisplayText(), start, end, event != null && event.shiftKey === true);
			if (plan == null) {
				return false;
			}
			if (event != null) {
				event.preventDefault();
			}
			for (const edit of [...plan.edits].sort((a, b) => b.start - a.start)) {
				$replaceComposerRange(edit.start, edit.end, {kind: 'text', text: edit.text}, {trailing: false});
			}
			$selectComposerRange(
				backward ? plan.selectionEnd : plan.selectionStart,
				backward ? plan.selectionStart : plan.selectionEnd,
			);
			return true;
		},
		COMMAND_PRIORITY_HIGH,
	);
}

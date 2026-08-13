// SPDX-License-Identifier: AGPL-3.0-or-later

import {$getComposerSelectionRange, $wrapComposerSelection} from '@app/features/lexical/composer/composerOffsets';
import {COMMAND_PRIORITY_HIGH, KEY_DOWN_COMMAND, type LexicalEditor} from 'lexical';

const SHORTCUTS = new Map([
	['b', {shift: false, wrapper: '**'}],
	['i', {shift: false, wrapper: '*'}],
	['u', {shift: false, wrapper: '__'}],
	['s', {shift: true, wrapper: '~~'}],
]);

export function registerComposerMarkdownShortcuts(editor: LexicalEditor): () => void {
	return editor.registerCommand(
		KEY_DOWN_COMMAND,
		(event) => {
			if (!(event.ctrlKey || event.metaKey) || event.altKey) {
				return false;
			}
			const shortcut = SHORTCUTS.get(event.key.toLowerCase());
			if (shortcut == null || event.shiftKey !== shortcut.shift) {
				return false;
			}
			const selection = $getComposerSelectionRange();
			if (selection == null || selection.start === selection.end) {
				return false;
			}
			event.preventDefault();
			$wrapComposerSelection(shortcut.wrapper, shortcut.wrapper);
			return true;
		},
		COMMAND_PRIORITY_HIGH,
	);
}

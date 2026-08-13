// SPDX-License-Identifier: AGPL-3.0-or-later

import {mergeRegister} from '@lexical/utils';
import {
	COMMAND_PRIORITY_HIGH,
	KEY_ARROW_DOWN_COMMAND,
	KEY_ARROW_UP_COMMAND,
	KEY_ENTER_COMMAND,
	KEY_TAB_COMMAND,
	type LexicalEditor,
} from 'lexical';

export interface ComposerTypeaheadActiveState {
	readonly current: boolean;
}

function hasModifier(event: KeyboardEvent | null): boolean {
	return event != null && (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey);
}

export function registerComposerTypeaheadModifierGuard(
	editor: LexicalEditor,
	activeState: ComposerTypeaheadActiveState,
): () => void {
	const guardModifiedKey = (event: KeyboardEvent | null): boolean => activeState.current && hasModifier(event);
	return mergeRegister(
		editor.registerCommand(KEY_TAB_COMMAND, guardModifiedKey, COMMAND_PRIORITY_HIGH),
		editor.registerCommand(KEY_ARROW_UP_COMMAND, guardModifiedKey, COMMAND_PRIORITY_HIGH),
		editor.registerCommand(KEY_ARROW_DOWN_COMMAND, guardModifiedKey, COMMAND_PRIORITY_HIGH),
		editor.registerCommand(
			KEY_ENTER_COMMAND,
			(event) =>
				activeState.current && event != null && !event.shiftKey && (event.altKey || event.ctrlKey || event.metaKey),
			COMMAND_PRIORITY_HIGH,
		),
	);
}

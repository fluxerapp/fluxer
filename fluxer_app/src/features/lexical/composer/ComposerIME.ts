// SPDX-License-Identifier: AGPL-3.0-or-later

import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {mergeRegister} from '@lexical/utils';
import {
	COMMAND_PRIORITY_CRITICAL,
	KEY_ARROW_DOWN_COMMAND,
	KEY_ARROW_UP_COMMAND,
	KEY_ENTER_COMMAND,
	KEY_ESCAPE_COMMAND,
	KEY_TAB_COMMAND,
	type LexicalEditor,
} from 'lexical';

export function registerComposerIMECommandGuard(editor: LexicalEditor): () => void {
	const preserveNativeIMEEvent = (event: KeyboardEvent | null): boolean => event != null && isIMEComposing(event);
	return mergeRegister(
		editor.registerCommand(KEY_ENTER_COMMAND, preserveNativeIMEEvent, COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(KEY_ARROW_UP_COMMAND, preserveNativeIMEEvent, COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(KEY_ARROW_DOWN_COMMAND, preserveNativeIMEEvent, COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(KEY_ESCAPE_COMMAND, preserveNativeIMEEvent, COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(KEY_TAB_COMMAND, preserveNativeIMEEvent, COMMAND_PRIORITY_CRITICAL),
	);
}

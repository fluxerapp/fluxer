// SPDX-License-Identifier: AGPL-3.0-or-later

import {mergeRegister} from '@lexical/utils';
import {CAN_REDO_COMMAND, CAN_UNDO_COMMAND, COMMAND_PRIORITY_LOW, type LexicalEditor} from 'lexical';

interface UndoRedoState {
	canUndo: boolean;
	canRedo: boolean;
}

const undoRedoStates = new WeakMap<LexicalEditor, UndoRedoState>();

export function registerContextMenuUndoRedo(editor: LexicalEditor): () => void {
	const state: UndoRedoState = {canUndo: false, canRedo: false};
	undoRedoStates.set(editor, state);
	return mergeRegister(
		editor.registerCommand(
			CAN_UNDO_COMMAND,
			(payload) => {
				state.canUndo = payload;
				return false;
			},
			COMMAND_PRIORITY_LOW,
		),
		editor.registerCommand(
			CAN_REDO_COMMAND,
			(payload) => {
				state.canRedo = payload;
				return false;
			},
			COMMAND_PRIORITY_LOW,
		),
		() => {
			undoRedoStates.delete(editor);
		},
	);
}

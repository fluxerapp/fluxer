// SPDX-License-Identifier: AGPL-3.0-or-later

import {mergeRegister} from '@lexical/utils';
import {
	CAN_REDO_COMMAND,
	CAN_UNDO_COMMAND,
	COMMAND_PRIORITY_LOW,
	getNearestEditorFromDOMNode,
	type LexicalEditor,
	REDO_COMMAND,
	UNDO_COMMAND,
} from 'lexical';

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

export interface LexicalUndoRedoTarget {
	editor: LexicalEditor;
	canUndo: boolean;
	canRedo: boolean;
}

export function resolveLexicalUndoRedoTarget(node: Node | null): LexicalUndoRedoTarget | null {
	const editor = getNearestEditorFromDOMNode(node);
	if (editor == null) {
		return null;
	}
	const state = undoRedoStates.get(editor);
	return {
		editor,
		canUndo: state == null ? false : state.canUndo,
		canRedo: state == null ? false : state.canRedo,
	};
}

export function dispatchLexicalUndo(editor: LexicalEditor): void {
	editor.dispatchCommand(UNDO_COMMAND, undefined);
}

export function dispatchLexicalRedo(editor: LexicalEditor): void {
	editor.dispatchCommand(REDO_COMMAND, undefined);
}

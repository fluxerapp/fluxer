// SPDX-License-Identifier: AGPL-3.0-or-later

import {$getSelection, $isNodeSelection, $isRangeSelection, type EditorState, type LexicalEditor} from 'lexical';

const MAX_SEARCH_FILTER_PILL_SELECTION_SUBSCRIBERS_PER_EDITOR = 2048;

interface SearchFilterPillSelectionSubscription {
	readonly editor: LexicalEditor;
	readonly nodeKey: string;
	readonly listener: () => void;
}

interface SearchFilterPillSelectionQuery {
	readonly editor: LexicalEditor;
	readonly nodeKey: string;
}

interface SearchFilterPillSelectionRelease {
	readonly editor: LexicalEditor;
	readonly selections: SearchFilterPillEditorSelections;
	readonly unsubscribe: () => void;
}

class DuplicateSearchFilterPillSelectionListenerError extends Error {
	constructor(nodeKey: string) {
		super(`Search filter pill selection listener is already registered for node ${nodeKey}`);
		this.name = 'DuplicateSearchFilterPillSelectionListenerError';
	}
}

class SearchFilterPillSelectionSubscriberCapacityError extends Error {
	constructor() {
		const message = [
			'Cannot register more than ',
			String(MAX_SEARCH_FILTER_PILL_SELECTION_SUBSCRIBERS_PER_EDITOR),
			' search filter pill selection subscribers per editor',
		].join('');
		super(message);
		this.name = 'SearchFilterPillSelectionSubscriberCapacityError';
	}
}

class SearchFilterPillEditorSelections {
	readonly #editor: LexicalEditor;
	readonly #listenersByNodeKey = new Map<string, Set<() => void>>();
	#selectedNodeKeys: ReadonlySet<string>;
	#subscriberCount = 0;
	#unregister: (() => void) | null = null;

	constructor(editor: LexicalEditor) {
		this.#editor = editor;
		this.#selectedNodeKeys = readSelectedNodeKeys(editor.getEditorState());
	}

	public get hasSubscribers(): boolean {
		return this.#subscriberCount > 0;
	}

	public isSelected(nodeKey: string): boolean {
		return this.#selectedNodeKeys.has(nodeKey);
	}

	public subscribe(nodeKey: string, listener: () => void): () => void {
		let listeners = this.#listenersByNodeKey.get(nodeKey);
		if (listeners == null) {
			listeners = new Set();
			this.#listenersByNodeKey.set(nodeKey, listeners);
		}
		if (listeners.has(listener)) {
			throw new DuplicateSearchFilterPillSelectionListenerError(nodeKey);
		}
		if (this.#subscriberCount >= MAX_SEARCH_FILTER_PILL_SELECTION_SUBSCRIBERS_PER_EDITOR) {
			throw new SearchFilterPillSelectionSubscriberCapacityError();
		}
		listeners.add(listener);
		this.#subscriberCount += 1;
		try {
			if (this.#subscriberCount === 1) {
				this.start();
			}
		} catch (error) {
			listeners.delete(listener);
			this.#subscriberCount -= 1;
			if (listeners.size === 0) {
				this.#listenersByNodeKey.delete(nodeKey);
			}
			throw error;
		}
		let disposed = false;
		return () => {
			if (disposed) return;
			disposed = true;
			listeners.delete(listener);
			this.#subscriberCount -= 1;
			if (listeners.size === 0) {
				this.#listenersByNodeKey.delete(nodeKey);
			}
			if (this.#subscriberCount === 0) {
				this.stop();
			}
		};
	}

	private start(): void {
		this.#selectedNodeKeys = readSelectedNodeKeys(this.#editor.getEditorState());
		this.#unregister = this.#editor.registerUpdateListener(({editorState}) => {
			this.handleEditorUpdate(editorState);
		});
	}

	private stop(): void {
		const unregister = this.#unregister;
		this.#unregister = null;
		if (unregister != null) {
			unregister();
		}
	}

	private handleEditorUpdate(editorState: EditorState): void {
		const nextSelectedNodeKeys = readSelectedNodeKeys(editorState);
		const previousSelectedNodeKeys = this.#selectedNodeKeys;
		if (
			nextSelectedNodeKeys.size === previousSelectedNodeKeys.size &&
			Array.from(nextSelectedNodeKeys).every((nodeKey) => previousSelectedNodeKeys.has(nodeKey))
		) {
			return;
		}
		this.#selectedNodeKeys = nextSelectedNodeKeys;
		const changedNodeKeys = new Set([...previousSelectedNodeKeys, ...nextSelectedNodeKeys]);
		const failures: Array<unknown> = [];
		for (const nodeKey of changedNodeKeys) {
			if (previousSelectedNodeKeys.has(nodeKey) === nextSelectedNodeKeys.has(nodeKey)) {
				continue;
			}
			const listeners = this.#listenersByNodeKey.get(nodeKey);
			if (listeners == null) {
				continue;
			}
			this.notifyListeners(listeners, failures);
		}
		if (failures.length > 0) {
			throw new AggregateError(failures, 'Search filter pill selection listener notification failed');
		}
	}

	private notifyListeners(listeners: ReadonlySet<() => void>, failures: Array<unknown>): void {
		for (const listener of [...listeners]) {
			try {
				listener();
			} catch (error) {
				failures.push(error);
			}
		}
	}
}

class SearchFilterPillSelectionOwner {
	readonly #editorSelections = new WeakMap<LexicalEditor, SearchFilterPillEditorSelections>();

	public subscribe({editor, nodeKey, listener}: SearchFilterPillSelectionSubscription): () => void {
		const selections = this.getOrCreate(editor);
		const unsubscribe = selections.subscribe(nodeKey, listener);
		let disposed = false;
		return () => {
			if (disposed) return;
			disposed = true;
			this.releaseSubscription({editor, selections, unsubscribe});
		};
	}

	public isSelected({editor, nodeKey}: SearchFilterPillSelectionQuery): boolean {
		const selections = this.#editorSelections.get(editor);
		if (selections != null) {
			return selections.isSelected(nodeKey);
		}
		return readSelectedNodeKeys(editor.getEditorState()).has(nodeKey);
	}

	private getOrCreate(editor: LexicalEditor): SearchFilterPillEditorSelections {
		const existing = this.#editorSelections.get(editor);
		if (existing != null) {
			return existing;
		}
		const selections = new SearchFilterPillEditorSelections(editor);
		this.#editorSelections.set(editor, selections);
		return selections;
	}

	private releaseSubscription({editor, selections, unsubscribe}: SearchFilterPillSelectionRelease): void {
		try {
			unsubscribe();
		} finally {
			if (!selections.hasSubscribers) {
				this.#editorSelections.delete(editor);
			}
		}
	}
}

function readSelectedNodeKeys(editorState: EditorState): ReadonlySet<string> {
	const selectedNodeKeys = new Set<string>();
	editorState.read(() => {
		$readSelectedNodeKeys(selectedNodeKeys);
	});
	return selectedNodeKeys;
}

function $readSelectedNodeKeys(selectedNodeKeys: Set<string>): void {
	const selection = $getSelection();
	if (!$isNodeSelection(selection) && (!$isRangeSelection(selection) || selection.isCollapsed())) {
		return;
	}
	for (const node of selection.getNodes()) {
		selectedNodeKeys.add(node.getKey());
	}
}

export const SearchFilterPillSelections = Object.freeze(new SearchFilterPillSelectionOwner());

// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerContextMenuUndoRedo} from '@app/features/lexical/LexicalUndoRedoRegistry';
import {SearchFilterNode} from '@app/features/lexical/nodes/SearchFilterNode';
import styles from '@app/features/lexical/search/LexicalSearchInput.module.css';
import {
	$applySearchSelectionRange,
	$getSearchQuery,
	$getSelectionRange,
	$insertSearchText,
	$replaceSearchDocumentFromQuery,
	$selectOffset,
	$selectSearchFilterBoundary,
	registerSearchPillTransform,
	resolveSearchFilterPointerBoundary,
	type SearchFilterPointerBoundary,
	SearchSelectionDirection,
	type SearchSelectionRange,
} from '@app/features/lexical/search/SearchEditorModel';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {flxElementClassName} from '@app/lib/react';
import {createEmptyHistoryState, type HistoryState, registerHistory} from '@lexical/history';
import type {InitialConfigType} from '@lexical/react/LexicalComposer';
import {LexicalComposer} from '@lexical/react/LexicalComposer';
import {useLexicalComposerContext} from '@lexical/react/LexicalComposerContext';
import {ContentEditable} from '@lexical/react/LexicalContentEditable';
import {LexicalErrorBoundary} from '@lexical/react/LexicalErrorBoundary';
import {PlainTextPlugin} from '@lexical/react/LexicalPlainTextPlugin';
import {mergeRegister} from '@lexical/utils';
import {clsx} from 'clsx';
import {
	$addUpdateTag,
	$getNearestNodeFromDOMNode,
	$setSelection,
	COMMAND_PRIORITY_HIGH,
	COMPOSITION_END_COMMAND,
	type EditorState,
	HISTORIC_TAG,
	KEY_ARROW_DOWN_COMMAND,
	KEY_ARROW_UP_COMMAND,
	KEY_ENTER_COMMAND,
	type LexicalEditor,
	PASTE_COMMAND,
	PASTE_TAG,
	type PasteCommandType,
	SKIP_DOM_SELECTION_TAG,
	type UpdateListenerPayload,
} from 'lexical';
import type React from 'react';
import {useEffect, useLayoutEffect, useMemo, useRef} from 'react';

const SEARCH_SELECTION_REQUEST_TAG = 'search-selection-request';
const TOUCH_TAP_MAX_DURATION_MS = 450;
const TOUCH_TAP_MAX_MOVEMENT_PX = 10;
const SEARCH_HISTORY_MERGE_DELAY_MS = 1_000;
const SEARCH_HISTORY_MAX_DEPTH = 100;
const COMPOSITION_EMITTED_VALUE_LIMIT = 64;
const DEFAULT_SEARCH_INPUT_ROLE = 'combobox';

export interface LexicalSearchInputHandle {
	readonly value: string;
	readonly selectionStart: number | null;
	readonly selectionEnd: number | null;
	readonly selectionDirection: SearchSelectionDirection | null;
	focus: () => void;
	blur: () => void;
	select: () => void;
	setSelectionRange: (start: number | null, end?: number | null, direction?: SearchSelectionDirection) => void;
}

export interface LexicalSearchInputProps {
	value: string;
	placeholder: string;
	className?: string;
	role?: string;
	isAutocompleteOpen?: boolean;
	inputRef: (node: LexicalSearchInputHandle | null) => void;
	onValueChange: (value: string) => void;
	onKeyDown?: React.KeyboardEventHandler<HTMLElement>;
	onFocus: () => void;
	onBlur: () => void;
	onMouseDown: () => void;
	ariaProps?: React.HTMLAttributes<HTMLElement> & {'aria-keyshortcuts'?: string; 'data-flx'?: string};
}

interface TouchPointerOrigin {
	readonly pointerId: number;
	readonly clientX: number;
	readonly clientY: number;
	readonly timeStamp: number;
}

interface MutableCell<T> {
	current: T;
}

interface SearchEditorSnapshot {
	readonly query: string;
	readonly selection: SearchSelectionRange | null;
}

interface SearchEditorUpdateOwnerRequest {
	readonly editor: LexicalEditor;
	readonly value: MutableCell<string>;
	readonly selection: MutableCell<SearchSelectionRange>;
	readonly requestedSelection: MutableCell<SearchSelectionRange | null>;
	readonly compositionEmittedValues: MutableCell<Array<string>>;
	readonly onValueChange: MutableCell<(value: string) => void>;
}

interface SearchKeyboardCommandOwnerRequest {
	readonly editor: LexicalEditor;
	readonly autocompleteOpen: MutableCell<boolean>;
}

interface SearchExternalValueOwnerRequest {
	readonly editor: LexicalEditor;
	readonly selection: MutableCell<SearchSelectionRange>;
	readonly requestedSelection: MutableCell<SearchSelectionRange | null>;
	readonly pendingExternalValue: MutableCell<{value: string} | null>;
	readonly compositionEmittedValues: MutableCell<Array<string>>;
	readonly focused: MutableCell<boolean>;
}

interface SearchFilterTargetRequest {
	readonly root: HTMLElement;
	readonly targetElement: Element | null;
	readonly filterElement: HTMLElement | null;
}

interface SearchFilterPlacementRequest {
	readonly root: HTMLElement;
	readonly view: Window & typeof globalThis;
	readonly filterElement: HTMLElement;
	readonly clientX: number;
}

function isMatchingTouchTap(origin: TouchPointerOrigin, event: React.PointerEvent<HTMLElement>): boolean {
	if (origin.pointerId !== event.pointerId) {
		return false;
	}
	if (event.timeStamp - origin.timeStamp > TOUCH_TAP_MAX_DURATION_MS) {
		return false;
	}
	const movement = Math.hypot(event.clientX - origin.clientX, event.clientY - origin.clientY);
	return movement <= TOUCH_TAP_MAX_MOVEMENT_PX;
}

function readSearchEditorSnapshot(editorState: EditorState): SearchEditorSnapshot {
	return editorState.read(() => ({
		query: $getSearchQuery(),
		selection: $getSelectionRange(),
	}));
}

function resolvePasteDataTransfer(event: PasteCommandType): DataTransfer | null {
	if ('clipboardData' in event) {
		return event.clipboardData;
	}
	if ('dataTransfer' in event) {
		return event.dataTransfer;
	}
	return null;
}

function readPastedSearchText(dataTransfer: DataTransfer): string {
	const plainText = dataTransfer.getData('text/plain');
	if (plainText.length > 0) {
		return plainText;
	}
	return dataTransfer.getData('text/uri-list');
}

class SearchPasteCommandOwner {
	public readonly handle = (event: PasteCommandType): boolean => {
		const dataTransfer = resolvePasteDataTransfer(event);
		if (dataTransfer == null) {
			return false;
		}
		const text = readPastedSearchText(dataTransfer);
		if (!$insertSearchText(text)) {
			return false;
		}
		event.preventDefault();
		$addUpdateTag(PASTE_TAG);
		return true;
	};
}

const SearchPasteCommand = Object.freeze(new SearchPasteCommandOwner());

class SearchEditorUpdateOwner {
	readonly #editor: LexicalEditor;
	readonly #value: MutableCell<string>;
	readonly #selection: MutableCell<SearchSelectionRange>;
	readonly #requestedSelection: MutableCell<SearchSelectionRange | null>;
	readonly #compositionEmittedValues: MutableCell<Array<string>>;
	readonly #onValueChange: MutableCell<(value: string) => void>;

	constructor({
		editor,
		value,
		selection,
		requestedSelection,
		compositionEmittedValues,
		onValueChange,
	}: SearchEditorUpdateOwnerRequest) {
		this.#editor = editor;
		this.#value = value;
		this.#selection = selection;
		this.#requestedSelection = requestedSelection;
		this.#compositionEmittedValues = compositionEmittedValues;
		this.#onValueChange = onValueChange;
	}

	public register(): () => void {
		return mergeRegister(
			registerSearchPillTransform(this.#editor),
			this.#editor.registerUpdateListener(this.handleUpdate),
			this.#editor.registerCommand(PASTE_COMMAND, SearchPasteCommand.handle, COMMAND_PRIORITY_HIGH),
		);
	}

	private readonly handleUpdate = ({editorState, tags}: UpdateListenerPayload): void => {
		const snapshot = readSearchEditorSnapshot(editorState);
		if (snapshot.selection != null) {
			this.#selection.current = snapshot.selection;
		}
		if (!tags.has(SEARCH_SELECTION_REQUEST_TAG)) {
			this.#requestedSelection.current = null;
		}
		if (snapshot.query === this.#value.current) {
			return;
		}
		this.recordCompositionValue(snapshot.query);
		this.#onValueChange.current(snapshot.query);
	};

	private recordCompositionValue(query: string): void {
		if (!this.#editor.isComposing()) {
			return;
		}
		const emittedValues = this.#compositionEmittedValues.current;
		if (emittedValues[emittedValues.length - 1] === query) {
			return;
		}
		emittedValues.push(query);
		if (emittedValues.length > COMPOSITION_EMITTED_VALUE_LIMIT) {
			emittedValues.shift();
		}
	}
}

class SearchKeyboardCommandOwner {
	readonly #editor: LexicalEditor;
	readonly #autocompleteOpen: MutableCell<boolean>;

	constructor({editor, autocompleteOpen}: SearchKeyboardCommandOwnerRequest) {
		this.#editor = editor;
		this.#autocompleteOpen = autocompleteOpen;
	}

	public register(): () => void {
		return mergeRegister(
			this.#editor.registerCommand(KEY_ENTER_COMMAND, this.handleEnter, COMMAND_PRIORITY_HIGH),
			this.#editor.registerCommand(KEY_ARROW_UP_COMMAND, this.handleArrow, COMMAND_PRIORITY_HIGH),
			this.#editor.registerCommand(KEY_ARROW_DOWN_COMMAND, this.handleArrow, COMMAND_PRIORITY_HIGH),
		);
	}

	private readonly handleEnter = (event: KeyboardEvent | null): boolean => {
		if (event == null) {
			return true;
		}
		if (isIMEComposing(event)) {
			return false;
		}
		event.preventDefault();
		return true;
	};

	private readonly handleArrow = (event: KeyboardEvent | null): boolean => {
		if (event != null && isIMEComposing(event)) {
			return false;
		}
		if (!this.#autocompleteOpen.current) {
			return false;
		}
		if (event != null) {
			event.preventDefault();
		}
		return true;
	};
}

class SearchExternalValueOwner {
	readonly #editor: LexicalEditor;
	readonly #selection: MutableCell<SearchSelectionRange>;
	readonly #requestedSelection: MutableCell<SearchSelectionRange | null>;
	readonly #pendingExternalValue: MutableCell<{value: string} | null>;
	readonly #compositionEmittedValues: MutableCell<Array<string>>;
	readonly #focused: MutableCell<boolean>;

	constructor({
		editor,
		selection,
		requestedSelection,
		pendingExternalValue,
		compositionEmittedValues,
		focused,
	}: SearchExternalValueOwnerRequest) {
		this.#editor = editor;
		this.#selection = selection;
		this.#requestedSelection = requestedSelection;
		this.#pendingExternalValue = pendingExternalValue;
		this.#compositionEmittedValues = compositionEmittedValues;
		this.#focused = focused;
	}

	public accept(nextValue: string): void {
		if (this.#editor.isComposing()) {
			this.stageComposingValue(nextValue);
			return;
		}
		this.apply(nextValue);
	}

	public registerCompositionEnd(): () => void {
		return this.#editor.registerCommand(COMPOSITION_END_COMMAND, this.handleCompositionEnd, COMMAND_PRIORITY_HIGH);
	}

	private apply(nextValue: string): void {
		const query = this.#editor.getEditorState().read($getSearchQuery);
		if (query === nextValue) {
			this.applyRequestedSelection();
			return;
		}
		this.replaceQuery(nextValue);
	}

	private applyRequestedSelection(): void {
		const requestedSelection = this.#requestedSelection.current;
		if (requestedSelection == null) {
			return;
		}
		this.#editor.update(
			() => {
				$applySearchSelectionRange(requestedSelection);
				this.addSkipDOMSelectionTagWhenUnfocused();
			},
			{discrete: true, tag: HISTORIC_TAG},
		);
		this.#requestedSelection.current = null;
	}

	private replaceQuery(nextValue: string): void {
		let nextSelection = this.#selection.current;
		const requestedSelection = this.#requestedSelection.current;
		if (requestedSelection != null) {
			nextSelection = requestedSelection;
		}
		this.#editor.update(
			() => {
				$replaceSearchDocumentFromQuery(nextValue, nextSelection);
				this.addSkipDOMSelectionTagWhenUnfocused();
			},
			{discrete: true, tag: 'replace-search-document'},
		);
		this.#requestedSelection.current = null;
	}

	private addSkipDOMSelectionTagWhenUnfocused(): void {
		if (!this.#focused.current) {
			$addUpdateTag(SKIP_DOM_SELECTION_TAG);
		}
	}

	private stageComposingValue(nextValue: string): void {
		if (this.#compositionEmittedValues.current.includes(nextValue)) {
			this.#pendingExternalValue.current = null;
			return;
		}
		this.#pendingExternalValue.current = {value: nextValue};
	}

	private readonly handleCompositionEnd = (): boolean => {
		queueMicrotask(this.consumePendingCompositionValue);
		return false;
	};

	private readonly consumePendingCompositionValue = (): void => {
		const pendingExternalValue = this.#pendingExternalValue.current;
		this.#pendingExternalValue.current = null;
		this.#compositionEmittedValues.current = [];
		if (pendingExternalValue != null) {
			this.apply(pendingExternalValue.value);
		}
	};
}

class SearchPointerSelectionOwner {
	constructor(private readonly editor: LexicalEditor) {}

	public place(target: EventTarget | null, clientX: number): boolean {
		const root = this.editor.getRootElement();
		if (root == null) {
			return false;
		}
		const view = root.ownerDocument.defaultView;
		if (view == null || !(target instanceof view.Node)) {
			return false;
		}
		const targetNode = target as Node;
		const targetElement = this.resolveTargetElement(view, targetNode);
		let filterElement: HTMLElement | null = null;
		if (targetElement != null) {
			filterElement = targetElement.closest<HTMLElement>('[data-lexical-search-filter="true"]');
		}
		const filterTarget = this.resolveFilterTarget({root, targetElement, filterElement});
		if (filterTarget != null) {
			return this.placeAtFilter({root, view, filterElement: filterTarget, clientX});
		}
		if (target === root || !root.contains(targetNode)) {
			return this.placeAtEnd();
		}
		return false;
	}

	private resolveTargetElement(view: Window & typeof globalThis, targetNode: Node): Element | null {
		if (targetNode instanceof view.Element) {
			return targetNode as Element;
		}
		return targetNode.parentElement;
	}

	private resolveFilterTarget({root, targetElement, filterElement}: SearchFilterTargetRequest): HTMLElement | null {
		if (filterElement == null || !root.contains(filterElement)) {
			return null;
		}
		if (targetElement != null && targetElement.closest('button') != null) {
			return null;
		}
		return filterElement;
	}

	private placeAtFilter({root, view, filterElement, clientX}: SearchFilterPlacementRequest): boolean {
		const filterRect = filterElement.getBoundingClientRect();
		let direction: 'ltr' | 'rtl' = 'ltr';
		if (view.getComputedStyle(root).direction === 'rtl') {
			direction = 'rtl';
		}
		const boundary = resolveSearchFilterPointerBoundary({
			direction,
			pointerX: clientX,
			left: filterRect.left,
			width: filterRect.width,
		});
		this.editor.update(() => this.selectFilterBoundary(filterElement, boundary), {discrete: true});
		this.editor.focus();
		return true;
	}

	private selectFilterBoundary(filterElement: HTMLElement, boundary: SearchFilterPointerBoundary): void {
		const node = $getNearestNodeFromDOMNode(filterElement);
		if (node != null) {
			$selectSearchFilterBoundary(node, boundary);
			return;
		}
		$selectOffset($getSearchQuery().length);
	}

	private placeAtEnd(): boolean {
		this.editor.update(() => $selectOffset($getSearchQuery().length), {discrete: true});
		this.editor.focus();
		return true;
	}
}

const EDITOR_CONFIG: Omit<InitialConfigType, 'editorState'> = {
	namespace: 'fluxer-search',
	onError: (error: Error) => {
		throw error;
	},
	nodes: [SearchFilterNode],
	theme: {
		searchFilter: styles.filterHost,
		paragraph: styles.paragraph,
	},
};

export const LexicalSearchInput = (props: LexicalSearchInputProps) => {
	const initialConfigRef = useRef<InitialConfigType | null>(null);
	if (initialConfigRef.current == null) {
		const initialValue = props.value;
		initialConfigRef.current = {
			...EDITOR_CONFIG,
			editorState: () => {
				$replaceSearchDocumentFromQuery(initialValue, null);
				$setSelection(null);
			},
		};
	}
	return (
		<LexicalComposer initialConfig={initialConfigRef.current}>
			<SearchEditorInner {...props} />
		</LexicalComposer>
	);
};

const SearchEditorInner = ({
	value,
	placeholder,
	className,
	role,
	isAutocompleteOpen,
	inputRef,
	onValueChange,
	onKeyDown,
	onFocus,
	onBlur,
	onMouseDown,
	ariaProps: ARIAProps,
}: LexicalSearchInputProps) => {
	const [editor] = useLexicalComposerContext();
	const valueRef = useRef(value);
	const selectionRef = useRef<SearchSelectionRange>({
		start: value.length,
		end: value.length,
		direction: SearchSelectionDirection.NONE,
	});
	const requestedSelectionRef = useRef<SearchSelectionRange | null>(null);
	const onValueChangeRef = useRef(onValueChange);
	const autocompleteOpenRef = useRef(isAutocompleteOpen === true);
	const pendingExternalValueRef = useRef<{value: string} | null>(null);
	const compositionEmittedValuesRef = useRef<Array<string>>([]);
	const focusedRef = useRef(false);
	const historyStateRef = useRef<HistoryState | null>(null);
	const touchPointerRef = useRef<TouchPointerOrigin | null>(null);
	const editorShellRef = useRef<HTMLElement | null>(null);
	const editableRef = useRef<HTMLDivElement | null>(null);
	onValueChangeRef.current = onValueChange;
	valueRef.current = value;
	autocompleteOpenRef.current = isAutocompleteOpen === true;
	if (historyStateRef.current == null) {
		historyStateRef.current = createEmptyHistoryState();
	}
	const historyState = historyStateRef.current;
	const editorUpdateOwner = useMemo(
		() =>
			new SearchEditorUpdateOwner({
				editor,
				value: valueRef,
				selection: selectionRef,
				requestedSelection: requestedSelectionRef,
				compositionEmittedValues: compositionEmittedValuesRef,
				onValueChange: onValueChangeRef,
			}),
		[editor],
	);
	const keyboardCommandOwner = useMemo(
		() => new SearchKeyboardCommandOwner({editor, autocompleteOpen: autocompleteOpenRef}),
		[editor],
	);
	const externalValueOwner = useMemo(
		() =>
			new SearchExternalValueOwner({
				editor,
				selection: selectionRef,
				requestedSelection: requestedSelectionRef,
				pendingExternalValue: pendingExternalValueRef,
				compositionEmittedValues: compositionEmittedValuesRef,
				focused: focusedRef,
			}),
		[editor],
	);
	const pointerSelectionOwner = useMemo(() => new SearchPointerSelectionOwner(editor), [editor]);

	useLayoutEffect(() => {
		return mergeRegister(
			registerHistory(
				editor,
				historyState,
				SEARCH_HISTORY_MERGE_DELAY_MS,
				Date.now,
				undefined,
				SEARCH_HISTORY_MAX_DEPTH,
			),
			registerContextMenuUndoRedo(editor),
		);
	}, [editor, historyState]);

	const setSelection = (
		start: number | null,
		end: number | null = start,
		direction: SearchSelectionDirection = SearchSelectionDirection.NONE,
	) => {
		let endOffset = valueRef.current.length;
		if (end != null) {
			endOffset = end;
		}
		const requestedEnd = Math.max(0, endOffset);
		let startOffset = requestedEnd;
		if (start != null) {
			startOffset = start;
		}
		let requestedStart = Math.max(0, startOffset);
		if (requestedEnd < requestedStart) {
			requestedStart = requestedEnd;
		}
		let selectionDirection = direction;
		if (requestedEnd === requestedStart) {
			selectionDirection = SearchSelectionDirection.NONE;
		}
		const range: SearchSelectionRange = {
			start: requestedStart,
			end: requestedEnd,
			direction: selectionDirection,
		};
		requestedSelectionRef.current = range;
		editor.update(
			() => {
				$applySearchSelectionRange(range);
			},
			{discrete: true, tag: [HISTORIC_TAG, SEARCH_SELECTION_REQUEST_TAG]},
		);
	};

	const shimRef = useRef<LexicalSearchInputHandle | null>(null);
	if (shimRef.current == null) {
		shimRef.current = {
			get value() {
				return valueRef.current;
			},
			get selectionStart() {
				return selectionRef.current.start;
			},
			get selectionEnd() {
				return selectionRef.current.end;
			},
			get selectionDirection() {
				return selectionRef.current.direction;
			},
			focus: () => editor.focus(),
			blur: () => editor.blur(),
			select: () => setSelection(0, valueRef.current.length, SearchSelectionDirection.FORWARD),
			setSelectionRange: setSelection,
		};
	}

	useLayoutEffect(() => {
		inputRef(shimRef.current);
		return () => inputRef(null);
	}, [inputRef]);

	useLayoutEffect(() => {
		return editorUpdateOwner.register();
	}, [editorUpdateOwner]);

	useEffect(() => {
		return keyboardCommandOwner.register();
	}, [keyboardCommandOwner]);

	useLayoutEffect(() => {
		externalValueOwner.accept(value);
	}, [externalValueOwner, value]);

	useLayoutEffect(() => {
		return externalValueOwner.registerCompositionEnd();
	}, [externalValueOwner]);

	let resolvedRole = DEFAULT_SEARCH_INPUT_ROLE;
	if (role != null) {
		resolvedRole = role;
	}
	return (
		<FocusRing offset={-2} focusTarget={editableRef} ringTarget={editorShellRef}>
			<flx-lexical-search-input
				ref={editorShellRef}
				className={flxElementClassName(styles.editorShell)}
				onPointerDown={(event) => {
					onMouseDown();
					if (!event.isPrimary || event.button !== 0) {
						return;
					}
					if (event.pointerType === 'touch') {
						touchPointerRef.current = {
							pointerId: event.pointerId,
							clientX: event.clientX,
							clientY: event.clientY,
							timeStamp: event.timeStamp,
						};
						return;
					}
					if (pointerSelectionOwner.place(event.target, event.clientX)) {
						event.preventDefault();
					}
				}}
				onPointerUp={(event) => {
					const pending = touchPointerRef.current;
					touchPointerRef.current = null;
					if (pending == null) return;
					if (!isMatchingTouchTap(pending, event)) return;
					if (pointerSelectionOwner.place(event.target, event.clientX)) {
						event.preventDefault();
					}
				}}
				onPointerCancel={() => {
					touchPointerRef.current = null;
				}}
			>
				<PlainTextPlugin
					contentEditable={
						<ContentEditable
							ref={editableRef}
							className={clsx(styles.editable, className)}
							role={resolvedRole}
							aria-multiline="false"
							spellCheck={false}
							onKeyDown={onKeyDown}
							onFocus={() => {
								focusedRef.current = true;
								onFocus();
							}}
							onBlur={() => {
								focusedRef.current = false;
								onBlur();
							}}
							aria-label={placeholder}
							aria-placeholder={placeholder}
							placeholder={<span className={styles.placeholder}>{placeholder}</span>}
							{...ARIAProps}
						/>
					}
					ErrorBoundary={LexicalErrorBoundary}
				/>
			</flx-lexical-search-input>
		</FocusRing>
	);
};

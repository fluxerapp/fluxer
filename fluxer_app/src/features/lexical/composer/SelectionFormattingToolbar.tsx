// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAntiShiftFloating} from '@app/features/app/hooks/useAntiShiftFloating';
import {
	$captureSelectionOffsets,
	$queryComposerSelectionWrappers,
	$selectComposerRange,
	$wrapComposerSelection,
	type ComposerSelectionOffsets,
} from '@app/features/lexical/composer/composerOffsets';
import styles from '@app/features/lexical/composer/SelectionFormattingToolbar.module.css';
import {useTooltipPortalRoot} from '@app/features/ui/tooltip/Tooltip';
import {flxElementClassName} from '@app/lib/react';
import type {ReferenceElement} from '@floating-ui/react';
import {FloatingPortal} from '@floating-ui/react';
import {useLexicalComposerContext} from '@lexical/react/LexicalComposerContext';
import {mergeRegister} from '@lexical/utils';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {Icon} from '@phosphor-icons/react';
import {
	CodeIcon,
	EyeSlashIcon,
	TextBIcon,
	TextItalicIcon,
	TextStrikethroughIcon,
	TextUnderlineIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {
	$getSelection,
	$isRangeSelection,
	BLUR_COMMAND,
	COMMAND_PRIORITY_HIGH,
	COMMAND_PRIORITY_LOW,
	KEY_DOWN_COMMAND,
	type LexicalEditor,
} from 'lexical';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

interface FormatButton {
	id: string;
	icon: Icon;
	wrapper: string;
	label: MessageDescriptor;
}

const FORMATS: ReadonlyArray<FormatButton> = [
	{
		id: 'bold',
		icon: TextBIcon,
		wrapper: '**',
		label: msg({message: 'Bold', comment: 'Selection formatting toolbar button.'}),
	},
	{
		id: 'italic',
		icon: TextItalicIcon,
		wrapper: '*',
		label: msg({message: 'Italic', comment: 'Selection formatting toolbar button.'}),
	},
	{
		id: 'underline',
		icon: TextUnderlineIcon,
		wrapper: '__',
		label: msg({message: 'Underline', comment: 'Selection formatting toolbar button.'}),
	},
	{
		id: 'strikethrough',
		icon: TextStrikethroughIcon,
		wrapper: '~~',
		label: msg({message: 'Strikethrough', comment: 'Selection formatting toolbar button.'}),
	},
	{
		id: 'code',
		icon: CodeIcon,
		wrapper: '`',
		label: msg({message: 'Inline code', comment: 'Selection formatting toolbar button.'}),
	},
	{
		id: 'spoiler',
		icon: EyeSlashIcon,
		wrapper: '||',
		label: msg({message: 'Spoiler', comment: 'Selection formatting toolbar button.'}),
	},
];

const FORMAT_WRAPPER_QUERIES = FORMATS.map(({wrapper}) => ({prefix: wrapper, suffix: wrapper}));

export function SelectionFormattingToolbarPlugin({enabled = true}: {enabled?: boolean}) {
	const [editor] = useLexicalComposerContext();
	const [rect, setRect] = useState<DOMRect | null>(null);
	const [active, setActive] = useState<Record<string, boolean>>({});
	const [focusRequest, setFocusRequest] = useState(0);
	const toolbarRef = useRef<HTMLElement | null>(null);
	const savedSelectionRef = useRef<ComposerSelectionOffsets | null>(null);
	const handleFocusRequest = useCallback(() => setFocusRequest(0), []);

	useEffect(() => {
		if (!enabled) {
			setRect(null);
			savedSelectionRef.current = null;
			return;
		}
		const recompute = () => {
			editor.getEditorState().read(
				() => {
					const selection = $getSelection();
					const root = editor.getRootElement();
					if (root == null) {
						setRect(null);
						return;
					}
					const activeElement = root.ownerDocument.activeElement;
					const toolbar = toolbarRef.current;
					const toolbarHasFocus = activeElement != null && toolbar != null && toolbar.contains(activeElement);
					if (!$isRangeSelection(selection) || selection.isCollapsed()) {
						if (!toolbarHasFocus || savedSelectionRef.current == null) {
							setRect(null);
						}
						return;
					}
					const {offsets, wrapped} = $queryComposerSelectionWrappers(FORMAT_WRAPPER_QUERIES);
					if (offsets == null) {
						setRect(null);
						return;
					}
					savedSelectionRef.current = offsets;
					if (activeElement !== root && !toolbarHasFocus) {
						setRect(null);
						return;
					}
					const nextActive: Record<string, boolean> = {};
					for (let index = 0; index < FORMATS.length; index += 1) {
						nextActive[FORMATS[index]!.id] = wrapped[index] == null ? false : wrapped[index]!;
					}
					setActive((previous) => (formatStatesEqual(previous, nextActive) ? previous : nextActive));
					if (toolbarHasFocus) {
						return;
					}
					const measured = getEditorSelectionRect(editor);
					if (measured == null) {
						setRect(null);
						return;
					}
					setRect((previous) => (selectionRectsEqual(previous, measured) ? previous : measured));
				},
				{editor},
			);
		};
		return mergeRegister(
			editor.registerUpdateListener(recompute),
			editor.registerCommand(
				BLUR_COMMAND,
				(event) => {
					const nextTarget = event.relatedTarget;
					if (isTargetWithin(toolbarRef.current, nextTarget)) {
						return false;
					}
					queueMicrotask(() => {
						const root = editor.getRootElement();
						const activeElement = root == null ? null : root.ownerDocument.activeElement;
						if (activeElement !== root && !isTargetWithin(toolbarRef.current, activeElement)) {
							setRect(null);
						}
					});
					return false;
				},
				COMMAND_PRIORITY_LOW,
			),
			editor.registerCommand(
				KEY_DOWN_COMMAND,
				(event) => {
					if (event.key !== 'F10' || !event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
						return false;
					}
					if (toolbarRef.current == null) {
						return false;
					}
					const selection = $getSelection();
					const hasSelection = $isRangeSelection(selection) && !selection.isCollapsed();
					if (hasSelection) {
						savedSelectionRef.current = $captureSelectionOffsets();
					}
					if (!hasSelection) {
						return false;
					}
					event.preventDefault();
					setFocusRequest((request) => request + 1);
					return true;
				},
				COMMAND_PRIORITY_HIGH,
			),
		);
	}, [editor, enabled]);

	useEffect(() => {
		if (!enabled) {
			return;
		}
		let currentRoot: HTMLElement | null = null;
		let addedShortcut = false;
		const unregister = editor.registerRootListener((nextRoot) => {
			if (currentRoot != null && addedShortcut) {
				removeKeyboardShortcut(currentRoot, 'Alt+F10');
			}
			currentRoot = nextRoot;
			addedShortcut = nextRoot != null && addKeyboardShortcut(nextRoot, 'Alt+F10');
		});
		return () => {
			unregister();
			if (currentRoot != null && addedShortcut) {
				removeKeyboardShortcut(currentRoot, 'Alt+F10');
			}
		};
	}, [editor, enabled]);

	if (!enabled || rect == null) {
		return null;
	}
	return (
		<SelectionToolbarSurface
			editor={editor}
			rect={rect}
			active={active}
			focusRequest={focusRequest}
			onFocusRequestHandled={handleFocusRequest}
			onDismiss={() => setRect(null)}
			toolbarRef={toolbarRef}
			savedSelectionRef={savedSelectionRef}
		/>
	);
}

function SelectionToolbarSurface({
	editor,
	rect,
	active,
	focusRequest,
	onFocusRequestHandled,
	onDismiss,
	toolbarRef,
	savedSelectionRef,
}: {
	editor: LexicalEditor;
	rect: DOMRect;
	active: Record<string, boolean>;
	focusRequest: number;
	onFocusRequestHandled: () => void;
	onDismiss: () => void;
	toolbarRef: React.RefObject<HTMLElement | null>;
	savedSelectionRef: React.MutableRefObject<ComposerSelectionOffsets | null>;
}) {
	const {i18n} = useLingui();
	const [focusIndex, setFocusIndex] = useState(0);
	const focusIndexRef = useRef(focusIndex);
	focusIndexRef.current = focusIndex;
	const rectRef = useRef(rect);
	rectRef.current = rect;
	const virtualReference = useMemo<ReferenceElement>(
		() => ({
			getBoundingClientRect: () => {
				const measured = getEditorSelectionRect(editor);
				return measured == null ? rectRef.current : measured;
			},
			contextElement: (() => {
				const root = editor.getRootElement();
				return root == null ? undefined : root;
			})(),
		}),
		[editor],
	);
	const {ref, state, style, updatePosition} = useAntiShiftFloating(virtualReference, true, {
		placement: 'top',
		offsetMainAxis: 8,
		shouldAutoUpdate: true,
		enableSmartBoundary: true,
	});
	useEffect(() => {
		updatePosition();
	}, [rect, updatePosition]);
	useLayoutEffect(() => {
		if (focusRequest === 0 || !state.isReady) {
			return;
		}
		const toolbar = toolbarRef.current;
		const button =
			toolbar == null
				? null
				: toolbar.querySelector<HTMLButtonElement>(`[data-format-index="${focusIndexRef.current}"]`);
		if (button != null) {
			button.focus();
		}
		if (button != null && button.ownerDocument.activeElement === button) {
			onFocusRequestHandled();
		}
	}, [focusRequest, onFocusRequestHandled, state.isReady, toolbarRef]);
	const portalRoot = useTooltipPortalRoot(true);
	const apply = (wrapper: string) => {
		editor.update(() => {
			const selection = $getSelection();
			if (!$isRangeSelection(selection) || selection.isCollapsed()) {
				const savedSelection = savedSelectionRef.current;
				if (savedSelection != null) {
					$selectComposerRange(savedSelection.anchor, savedSelection.focus);
				}
			}
			$wrapComposerSelection(wrapper, wrapper);
			savedSelectionRef.current = $captureSelectionOffsets();
		});
	};
	const focusButton = useCallback(
		(index: number) => {
			const nextIndex = (index + FORMATS.length) % FORMATS.length;
			setFocusIndex(nextIndex);
			const toolbar = toolbarRef.current;
			const button =
				toolbar == null ? null : toolbar.querySelector<HTMLButtonElement>(`[data-format-index="${nextIndex}"]`);
			if (button != null) {
				button.focus();
			}
		},
		[toolbarRef],
	);
	const returnFocusToEditor = useCallback(() => {
		editor.update(() => {
			const savedSelection = savedSelectionRef.current;
			if (savedSelection != null) {
				$selectComposerRange(savedSelection.anchor, savedSelection.focus);
			}
		});
		editor.focus();
	}, [editor, savedSelectionRef]);
	const handleToolbarKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLElement>) => {
			const targetIndex = Number((event.target as HTMLElement).dataset.formatIndex);
			const currentIndex = Number.isInteger(targetIndex) ? targetIndex : focusIndex;
			const defaultView = event.currentTarget.ownerDocument.defaultView;
			const direction = defaultView == null ? undefined : defaultView.getComputedStyle(event.currentTarget).direction;
			const rightStep = direction === 'rtl' ? -1 : 1;
			switch (event.key) {
				case 'ArrowRight':
					event.preventDefault();
					focusButton(currentIndex + rightStep);
					return;
				case 'ArrowLeft':
					event.preventDefault();
					focusButton(currentIndex - rightStep);
					return;
				case 'ArrowDown':
					event.preventDefault();
					focusButton(currentIndex + 1);
					return;
				case 'ArrowUp':
					event.preventDefault();
					focusButton(currentIndex - 1);
					return;
				case 'Home':
					event.preventDefault();
					focusButton(0);
					return;
				case 'End':
					event.preventDefault();
					focusButton(FORMATS.length - 1);
					return;
				case 'Escape':
					event.preventDefault();
					returnFocusToEditor();
					return;
				default:
					return;
			}
		},
		[focusButton, focusIndex, returnFocusToEditor],
	);
	return (
		<FloatingPortal root={portalRoot}>
			<flx-lexical-selection-formatting-toolbar
				ref={(element) => {
					(ref as React.MutableRefObject<HTMLElement | null>).current = element;
					toolbarRef.current = element;
				}}
				className={flxElementClassName(styles.toolbar)}
				role="toolbar"
				aria-label={i18n._(
					msg({message: 'Text formatting', comment: 'Accessible name of the composer selection toolbar.'}),
				)}
				aria-orientation="horizontal"
				aria-keyshortcuts="Alt+F10"
				style={{...style, zIndex: 'var(--z-index-tooltip)', visibility: state.isReady ? 'visible' : 'hidden'}}
				onBlur={(event) => {
					const root = editor.getRootElement();
					if (event.relatedTarget !== root && !isTargetWithin(event.currentTarget, event.relatedTarget)) {
						const toolbar = event.currentTarget;
						queueMicrotask(() => {
							const activeElement = toolbar.ownerDocument.activeElement;
							if (activeElement !== root && !isTargetWithin(toolbar, activeElement)) {
								onDismiss();
							}
						});
					}
				}}
				onKeyDown={handleToolbarKeyDown}
			>
				{FORMATS.map((format, index) => {
					const label = i18n._(format.label);
					const isActive = active[format.id] == null ? false : active[format.id];
					return (
						<button
							key={format.id}
							type="button"
							className={clsx(styles.button, isActive && styles.buttonActive)}
							aria-pressed={isActive}
							aria-label={label}
							title={label}
							tabIndex={focusIndex === index ? 0 : -1}
							data-format-index={index}
							onFocus={() => setFocusIndex(index)}
							onPointerDown={(event) => {
								setFocusIndex(index);
								event.preventDefault();
							}}
							onClick={() => apply(format.wrapper)}
						>
							<format.icon weight="bold" className={styles.icon} />
						</button>
					);
				})}
			</flx-lexical-selection-formatting-toolbar>
		</FloatingPortal>
	);
}

function isTargetWithin(container: HTMLElement | null, target: EventTarget | null): boolean {
	const ownerDocument = container == null ? null : container.ownerDocument;
	const defaultView = ownerDocument == null ? null : ownerDocument.defaultView;
	const NodeConstructor = defaultView == null ? undefined : defaultView.Node;
	return (
		container != null && NodeConstructor != null && target instanceof NodeConstructor && container.contains(target)
	);
}

function getEditorSelectionRect(editor: LexicalEditor): DOMRect | null {
	const root = editor.getRootElement();
	const selection = root == null ? null : root.ownerDocument.getSelection();
	if (root == null || selection == null || selection.rangeCount === 0) {
		return null;
	}
	const range = selection.getRangeAt(0);
	if (range.commonAncestorContainer !== root && !root.contains(range.commonAncestorContainer)) {
		return null;
	}
	const measured = range.getBoundingClientRect();
	return measured.width === 0 && measured.height === 0 ? null : measured;
}

function formatStatesEqual(a: Record<string, boolean>, b: Record<string, boolean>): boolean {
	return FORMATS.every(({id}) => (a[id] == null ? false : a[id]) === (b[id] == null ? false : b[id]));
}

function selectionRectsEqual(a: DOMRect | null, b: DOMRect): boolean {
	return a != null && a.x === b.x && a.y === b.y && a.width === b.width && a.height === b.height;
}

function addKeyboardShortcut(element: HTMLElement, shortcut: string): boolean {
	const attribute = element.getAttribute('aria-keyshortcuts');
	const shortcuts = new Set((attribute == null ? '' : attribute).split(/\s+/).filter(Boolean));
	if (shortcuts.has(shortcut)) {
		return false;
	}
	shortcuts.add(shortcut);
	element.setAttribute('aria-keyshortcuts', Array.from(shortcuts).join(' '));
	return true;
}

function removeKeyboardShortcut(element: HTMLElement, shortcut: string): void {
	const attribute = element.getAttribute('aria-keyshortcuts');
	const shortcuts = (attribute == null ? '' : attribute).split(/\s+/).filter(Boolean);
	const remainingShortcuts = shortcuts.filter((candidate) => candidate !== shortcut);
	if (remainingShortcuts.length === 0) {
		element.removeAttribute('aria-keyshortcuts');
		return;
	}
	element.setAttribute('aria-keyshortcuts', remainingShortcuts.join(' '));
}

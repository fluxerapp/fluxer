// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AutocompleteOption, AutocompleteType} from '@app/features/channel/components/AutocompleteTypes';
import {registerComposerClipboardCommands} from '@app/features/lexical/composer/ComposerClipboard';
import {registerComposerCodeIndent} from '@app/features/lexical/composer/ComposerCodeIndent';
import {
	type ComposerEmojiResolver,
	registerComposerEmojiShortcode,
} from '@app/features/lexical/composer/ComposerEmojiShortcode';
import type {ComposerHandle, ComposerSelectionRange} from '@app/features/lexical/composer/ComposerHandle';
import {resetComposerHistory} from '@app/features/lexical/composer/ComposerHistory';
import {registerComposerIMECommandGuard} from '@app/features/lexical/composer/ComposerIME';
import {registerComposerMarkdownHighlight} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {registerComposerMarkdownShortcuts} from '@app/features/lexical/composer/ComposerMarkdownShortcuts';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import {registerComposerPlainText} from '@app/features/lexical/composer/ComposerPlainText';
import {$hydrateComposerFromDraft, $projectComposer} from '@app/features/lexical/composer/ComposerSerialization';
import {
	type ComposerTypeaheadActiveState,
	registerComposerTypeaheadModifierGuard,
} from '@app/features/lexical/composer/ComposerTypeaheadModifierGuard';
import {ComposerTypeaheadPlugin} from '@app/features/lexical/composer/ComposerTypeaheadPlugin';
import {
	$captureSelectionOffsets,
	$getComposerDisplayText,
	$getComposerSelectionRange,
	$getTextUpToCursor,
	$isComposerEmpty,
	$replaceComposerRange,
	$selectComposerNodeBoundary,
	$selectComposerOffset,
	$selectComposerRange,
	$wrapComposerSelection,
} from '@app/features/lexical/composer/composerOffsets';
import styles from '@app/features/lexical/composer/LexicalMessageComposer.module.css';
import {ComposerCommandNode} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {ComposerCustomEmojiNode} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {ComposerStandardEmojiNode} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {SlashOptionalHintNode} from '@app/features/lexical/composer/nodes/SlashOptionalHintNode';
import {SlashSeparatorNode} from '@app/features/lexical/composer/nodes/SlashSeparatorNode';
import {$isSlashSlotNode, SlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {SlashSlotPlaceholderNode} from '@app/features/lexical/composer/nodes/SlashSlotPlaceholderNode';
import {SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {SelectionFormattingToolbarPlugin} from '@app/features/lexical/composer/SelectionFormattingToolbar';
import {SlashSlotAutocompletePlugin} from '@app/features/lexical/composer/SlashSlotAutocompletePlugin';
import {
	$applyOptionalChoice,
	$applySlotChoice,
	$applySlotPayload,
	$focusFirstInvalidSlashSlot,
	$getActiveOptionalContext,
	$getActiveSlotAutocompleteContext,
	$getActiveSlotChoiceContext,
	$insertSlashCommand,
	registerSlashSlotFocus,
	registerSlashSlotPlugin,
	type SlashCommandComposerState,
	type SlashOptionalContext,
	type SlashSlotAutocompleteContext,
	type SlashSlotChoiceContext,
} from '@app/features/lexical/composer/slashSlots';
import type {SlashSlotResolvers} from '@app/features/lexical/composer/slashSlotValidation';
import {registerContextMenuUndoRedo} from '@app/features/lexical/LexicalUndoRedoRegistry';
import ChatInputSettings from '@app/features/messaging/state/ChatInputSettings';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {flxElementClassName} from '@app/lib/react';
import type {InitialConfigType} from '@lexical/react/LexicalComposer';
import {LexicalComposer} from '@lexical/react/LexicalComposer';
import {useLexicalComposerContext} from '@lexical/react/LexicalComposerContext';
import {ContentEditable} from '@lexical/react/LexicalContentEditable';
import {LexicalErrorBoundary} from '@lexical/react/LexicalErrorBoundary';
import {HistoryPlugin} from '@lexical/react/LexicalHistoryPlugin';
import {PlainTextPlugin} from '@lexical/react/LexicalPlainTextPlugin';
import {mergeRegister} from '@lexical/utils';
import {clsx} from 'clsx';
import {
	$getNearestNodeFromDOMNode,
	$getRoot,
	$getSelection,
	$isRangeSelection,
	$setSelection,
	BLUR_COMMAND,
	COMMAND_PRIORITY_HIGH,
	COMMAND_PRIORITY_LOW,
	FOCUS_COMMAND,
	HISTORY_MERGE_TAG,
	KEY_ARROW_UP_COMMAND,
	KEY_ENTER_COMMAND,
} from 'lexical';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useImperativeHandle, useLayoutEffect, useMemo, useRef} from 'react';

const THEME: InitialConfigType['theme'] = {
	paragraph: styles.paragraph,
	syntaxMarker: styles.marker,
	composerMention: clsx(styles.mentionHost, markupStyles.inlineFormat),
	composerCustomEmoji: styles.emojiHost,
	composerCommand: styles.command,
	slashSlot: styles.slashSlot,
	slashSlotPlaceholder: styles.slashSlotPlaceholder,
	text: {
		bold: styles.bold,
		italic: styles.italic,
		underline: styles.underline,
		strikethrough: styles.strike,
		code: styles.code,
	},
};

export interface LexicalComposerInputProps {
	placeholder: string;
	disabled: boolean;
	handleRef: React.RefObject<ComposerHandle | null>;
	initialValue?: string;
	initialSegments?: ReadonlyArray<MentionSegment>;
	slotResolvers?: SlashSlotResolvers;
	markdown?: boolean;
	markdownParserFlags?: number;
	emojiShortcodeResolver?: ComposerEmojiResolver;
	channelId?: string;
	guildId?: string;
	selectionToolbar?: boolean;
	submitOnEnter?: boolean;
	focusRingTarget?: React.RefObject<Element | null>;
	focusRingEnabled?: boolean;
	className?: string;
	id?: string;
	ariaLabel?: string;
	ariaLabelledBy?: string;
	ariaDescribedBy?: string;
	ariaErrorMessage?: string;
	ariaInvalid?: React.AriaAttributes['aria-invalid'];
	autocompleteOptions: Array<AutocompleteOption>;
	autocompleteType: AutocompleteType;
	autocompleteQuery: string;
	autocompleteEnabled: boolean;
	slotMenuActive?: boolean;
	autocompleteReferenceElement: HTMLElement | null;
	autocompleteListboxId: string;
	onAutocompleteSelect: (option: AutocompleteOption) => void;
	onChange: (display: string, segments: Array<MentionSegment>, wire: string) => void;
	onCursorMove: () => void;
	onEnter?: () => void;
	onArrowUp: () => void;
	onKeyDown?: (event: React.KeyboardEvent<HTMLElement>) => void;
	onFocus?: () => void;
	onBlur?: () => void;
	onSlashCommandStateChange?: (state: SlashCommandComposerState) => void;
}

export const LexicalComposerInput = observer((props: LexicalComposerInputProps) => {
	const plainText = ChatInputSettings.renderComposerAsPlainText;
	const selectionToolbar = (props.selectionToolbar == null ? true : props.selectionToolbar) && !MobileLayout.enabled;
	const mentionContext = useMemo(
		() => ({guildId: props.guildId, channelId: props.channelId, plainText}),
		[plainText, props.guildId, props.channelId],
	);
	const config: InitialConfigType = {
		namespace: 'fluxer-composer',
		editable: !props.disabled,
		editorState: () => {
			$hydrateComposerFromDraft(
				props.initialValue == null ? '' : props.initialValue,
				props.initialSegments == null ? [] : props.initialSegments,
				plainText,
			);
		},
		onError: (error: Error) => {
			throw error;
		},
		nodes: [
			ComposerMentionNode,
			ComposerCustomEmojiNode,
			ComposerStandardEmojiNode,
			ComposerPlainSegmentNode,
			ComposerCommandNode,
			SlashSlotNode,
			SlashSlotPlaceholderNode,
			SlashSeparatorNode,
			SlashOptionalHintNode,
			SyntaxMarkerNode,
		],
		theme: THEME,
	};
	return (
		<LexicalComposer initialConfig={config}>
			<ComposerMentionContext.Provider value={mentionContext}>
				<ComposerInner {...props} plainText={plainText} selectionToolbar={selectionToolbar} />
			</ComposerMentionContext.Provider>
		</LexicalComposer>
	);
});

type ComposerInnerProps = LexicalComposerInputProps & {plainText: boolean};

const ComposerInner = ({
	placeholder,
	disabled,
	handleRef,
	slotResolvers,
	markdown = true,
	markdownParserFlags,
	emojiShortcodeResolver,
	selectionToolbar = true,
	submitOnEnter = true,
	focusRingTarget,
	focusRingEnabled = false,
	className,
	id,
	ariaLabel,
	ariaLabelledBy,
	ariaDescribedBy,
	ariaErrorMessage,
	ariaInvalid,
	autocompleteOptions,
	autocompleteType,
	autocompleteQuery,
	autocompleteEnabled,
	slotMenuActive = false,
	autocompleteReferenceElement,
	autocompleteListboxId,
	onAutocompleteSelect,
	onChange,
	onCursorMove,
	onEnter,
	onArrowUp,
	onKeyDown,
	onFocus,
	onBlur,
	onSlashCommandStateChange,
	plainText,
}: ComposerInnerProps) => {
	const [editor] = useLexicalComposerContext();
	const cb = useRef({onChange, onCursorMove, onEnter, onArrowUp});
	cb.current = {onChange, onCursorMove, onEnter, onArrowUp};
	const composerTypeaheadActiveRef = useRef(false);
	const slashSlotTypeaheadActiveRef = useRef(false);
	const typeaheadActiveState = useMemo<ComposerTypeaheadActiveState>(
		() => ({
			get current() {
				return composerTypeaheadActiveRef.current || slashSlotTypeaheadActiveRef.current;
			},
		}),
		[],
	);
	const slotResolversRef = useRef<SlashSlotResolvers>(slotResolvers == null ? {} : slotResolvers);
	slotResolversRef.current = slotResolvers == null ? {} : slotResolvers;
	const emojiResolverRef = useRef<ComposerEmojiResolver | undefined>(emojiShortcodeResolver);
	emojiResolverRef.current = emojiShortcodeResolver;
	const submitOnEnterRef = useRef(submitOnEnter);
	submitOnEnterRef.current = submitOnEnter;
	const plainTextRef = useRef(plainText);
	plainTextRef.current = plainText;
	const disabledRef = useRef(disabled);
	disabledRef.current = disabled;
	const onSlashCommandStateChangeRef = useRef(onSlashCommandStateChange);
	onSlashCommandStateChangeRef.current = onSlashCommandStateChange;
	const previousPlainTextRef = useRef(plainText);
	const lastSelectionRef = useRef<ComposerSelectionRange | null>(null);
	const pointerFocusRef = useRef(false);
	const selectLastSelectionOrEnd = () => {
		const saved = lastSelectionRef.current;
		if (saved != null) {
			$selectComposerRange(saved.start, saved.end);
			return;
		}
		$selectComposerOffset($getComposerDisplayText().length);
	};

	useImperativeHandle(
		handleRef,
		(): ComposerHandle => ({
			focus: () => {
				if (disabledRef.current) {
					return;
				}
				editor.update(
					() => {
						if (!$isRangeSelection($getSelection())) {
							selectLastSelectionOrEnd();
						}
					},
					{discrete: true},
				);
				editor.focus();
			},
			isFocused: () => editor.getRootElement() === document.activeElement,
			getEditor: () => editor,
			getDisplayValue: () => {
				let value = '';
				editor.getEditorState().read(
					() => {
						value = $projectComposer().display;
					},
					{editor},
				);
				return value;
			},
			getWireValue: () => {
				let value = '';
				editor.getEditorState().read(
					() => {
						value = $projectComposer().wire;
					},
					{editor},
				);
				return value;
			},
			getSegments: () => {
				let segments: Array<MentionSegment> = [];
				editor.getEditorState().read(
					() => {
						segments = $projectComposer().segments;
					},
					{editor},
				);
				return segments;
			},
			getTextUpToCursor: () => {
				let text = '';
				editor.getEditorState().read(
					() => {
						text = $getTextUpToCursor();
					},
					{editor},
				);
				return text;
			},
			getSelection: () => {
				let selection: ComposerSelectionRange | null = null;
				editor.getEditorState().read(
					() => {
						selection = $getComposerSelectionRange();
					},
					{editor},
				);
				if (selection != null) {
					lastSelectionRef.current = selection;
				}
				return selection == null ? lastSelectionRef.current : selection;
			},
			replaceRange: (start, end, payload, spacing) => {
				editor.update(() => {
					$replaceComposerRange(start, end, payload, spacing, plainTextRef.current);
				});
			},
			insertSlashCommand: (name, options, start, end) => {
				editor.update(() => {
					if (plainTextRef.current) {
						$replaceComposerRange(
							start,
							end,
							{kind: 'text', text: `${name} `},
							{leading: false, trailing: false},
							true,
						);
					} else {
						$insertSlashCommand(name, options, start, end);
					}
				});
			},
			insertTextAtCursor: (text) => {
				editor.update(() => {
					let selection = $getSelection();
					if (!$isRangeSelection(selection)) {
						selectLastSelectionOrEnd();
						selection = $getSelection();
					}
					if ($isRangeSelection(selection)) {
						selection.insertRawText(text);
					}
				});
			},
			wrapSelection: (prefix, suffix) => {
				editor.update(() => {
					$wrapComposerSelection(prefix, suffix);
				});
			},
			deleteSelection: () => {
				editor.update(() => {
					const selection = $getSelection();
					if ($isRangeSelection(selection)) {
						selection.insertText('');
					}
				});
			},
			clear: () => {
				lastSelectionRef.current = {start: 0, end: 0};
				editor.update(
					() => {
						$hydrateComposerFromDraft('', [], plainTextRef.current);
						$selectComposerOffset(0);
					},
					{discrete: true},
				);
				resetComposerHistory(editor);
			},
			getActiveSlotAutocompleteContext: () => {
				let context: SlashSlotAutocompleteContext | null = null;
				editor.getEditorState().read(
					() => {
						context = $getActiveSlotAutocompleteContext();
					},
					{editor},
				);
				return context;
			},
			getActiveSlotChoiceContext: () => {
				let context: SlashSlotChoiceContext | null = null;
				editor.getEditorState().read(
					() => {
						context = $getActiveSlotChoiceContext();
					},
					{editor},
				);
				return context;
			},
			applySlotChoice: (name) => {
				editor.update(() => {
					$applySlotChoice(name);
				});
			},
			applySlotPayload: (payload) => {
				editor.update(() => {
					$applySlotPayload(payload);
				});
			},
			getActiveOptionalContext: () => {
				let context: SlashOptionalContext | null = null;
				editor.getEditorState().read(
					() => {
						context = $getActiveOptionalContext();
					},
					{editor},
				);
				return context;
			},
			applyOptionalChoice: (name) => {
				editor.update(() => {
					$applyOptionalChoice(name);
				});
			},
			hydrate: (display, segments) => {
				lastSelectionRef.current = {start: display.length, end: display.length};
				editor.update(
					() => {
						$hydrateComposerFromDraft(display, segments, plainTextRef.current);
						$selectComposerOffset(display.length);
					},
					{discrete: true},
				);
				resetComposerHistory(editor);
			},
		}),
		[editor],
	);

	useLayoutEffect(() => {
		const cleanups = [
			editor.registerUpdateListener(({dirtyElements, dirtyLeaves, editorState}) => {
				editorState.read(() => {
					const selection = $getComposerSelectionRange();
					if (selection != null) {
						lastSelectionRef.current = selection;
					}
					if (dirtyElements.size > 0 || dirtyLeaves.size > 0) {
						const {display, segments, wire} = $projectComposer();
						cb.current.onChange(display, segments, wire);
					}
				});
				cb.current.onCursorMove();
			}),
		];
		if (plainText) {
			cleanups.push(registerComposerPlainText(editor));
		} else {
			cleanups.push(registerSlashSlotPlugin(editor, () => slotResolversRef.current, typeaheadActiveState));
			cleanups.push(registerSlashSlotFocus(editor, () => onSlashCommandStateChangeRef.current));
			if (markdown) {
				cleanups.push(registerComposerMarkdownHighlight(editor, markdownParserFlags));
			}
			cleanups.push(
				registerComposerEmojiShortcode(editor, (shortcodeName) => {
					const resolver = emojiResolverRef.current;
					return resolver == null ? null : resolver(shortcodeName);
				}),
			);
		}
		const modeChanged = previousPlainTextRef.current !== plainText;
		if (modeChanged) {
			editor.update(
				() => {
					const selection = $captureSelectionOffsets();
					const projection = $projectComposer();
					$hydrateComposerFromDraft(projection.display, projection.segments, plainText);
					if (selection != null) {
						$selectComposerRange(selection.anchor, selection.focus);
					}
				},
				{discrete: true, tag: 'composer-render-mode'},
			);
			resetComposerHistory(editor);
			previousPlainTextRef.current = plainText;
		}
		editor.update(
			() => {
				for (const node of $getRoot().getAllTextNodes()) {
					node.markDirty();
				}
			},
			{discrete: true, tag: HISTORY_MERGE_TAG},
		);
		return mergeRegister(...cleanups);
	}, [editor, markdown, markdownParserFlags, plainText]);

	useEffect(() => {
		return mergeRegister(
			registerContextMenuUndoRedo(editor),
			registerComposerIMECommandGuard(editor),
			registerComposerTypeaheadModifierGuard(editor, typeaheadActiveState),
			registerComposerMarkdownShortcuts(editor),
			registerComposerCodeIndent(editor, typeaheadActiveState),
			editor.registerCommand(
				FOCUS_COMMAND,
				() => {
					if (pointerFocusRef.current) {
						pointerFocusRef.current = false;
						return false;
					}
					if (disabledRef.current) {
						return false;
					}
					selectLastSelectionOrEnd();
					return false;
				},
				COMMAND_PRIORITY_LOW,
			),
			editor.registerCommand(
				BLUR_COMMAND,
				() => {
					pointerFocusRef.current = false;
					return false;
				},
				COMMAND_PRIORITY_LOW,
			),
			editor.registerCommand(
				KEY_ENTER_COMMAND,
				(event: KeyboardEvent | null) => {
					if (typeaheadActiveState.current || event == null) {
						return false;
					}
					if (submitOnEnterRef.current && cb.current.onEnter != null) {
						if (!event.shiftKey) {
							event.preventDefault();
							if ($focusFirstInvalidSlashSlot()) {
								return true;
							}
							cb.current.onEnter();
							return true;
						}
						return false;
					}
					if ((event.metaKey || event.ctrlKey) && cb.current.onEnter != null) {
						event.preventDefault();
						if ($focusFirstInvalidSlashSlot()) {
							return true;
						}
						cb.current.onEnter();
						return true;
					}
					return false;
				},
				COMMAND_PRIORITY_HIGH,
			),
			editor.registerCommand(
				KEY_ARROW_UP_COMMAND,
				(event: KeyboardEvent | null) => {
					if (typeaheadActiveState.current) {
						return false;
					}
					if (event != null && !event.altKey && !event.ctrlKey && !event.metaKey && !event.shiftKey) {
						if ($isComposerEmpty()) {
							cb.current.onArrowUp();
						}
					}
					return false;
				},
				COMMAND_PRIORITY_HIGH,
			),
		);
	}, [editor]);

	useEffect(
		() =>
			registerComposerClipboardCommands(editor, {
				getPlainText: () => plainTextRef.current,
				isEditable: () => !disabledRef.current && editor.isEditable(),
			}),
		[editor],
	);

	useEffect(() => {
		editor.setEditable(!disabled);
		if (disabled) {
			editor.update(() => {
				$setSelection(null);
			});
		}
	}, [editor, disabled]);
	const handleEditableKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLElement>) => {
			if (!event.defaultPrevented && !isIMEComposing(event)) {
				if (onKeyDown != null) {
					onKeyDown(event);
				}
			}
		},
		[onKeyDown],
	);
	const handleEditablePointerDown = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			pointerFocusRef.current = true;
			const target = event.target instanceof Element ? event.target : null;
			const root = editor.getRootElement();
			const slotHost = target == null ? null : target.closest<HTMLElement>('[data-lexical-composer-slot]');
			if (slotHost != null && root != null && root.contains(slotHost)) {
				let emptySlot = false;
				editor.getEditorState().read(
					() => {
						const node = $getNearestNodeFromDOMNode(slotHost);
						emptySlot = $isSlashSlotNode(node) && node.getTextContentSize() === 0;
					},
					{editor},
				);
				if (emptySlot) {
					event.preventDefault();
					editor.update(() => {
						const node = $getNearestNodeFromDOMNode(slotHost);
						if ($isSlashSlotNode(node)) {
							node.selectValueEnd();
						}
					});
					editor.focus();
				}
				return;
			}
			const host =
				target == null
					? null
					: target.closest<HTMLElement>(
							'[data-lexical-composer-mention], [data-lexical-composer-emoji], [data-lexical-composer-standard-emoji]',
						);
			if (host == null || root == null || !root.contains(host)) {
				return;
			}
			event.preventDefault();
			const rect = host.getBoundingClientRect();
			const clickedVisualStart = event.clientX < rect.left + rect.width / 2;
			const defaultView = host.ownerDocument.defaultView;
			const direction = defaultView == null ? undefined : defaultView.getComputedStyle(host).direction;
			const boundary = clickedVisualStart === (direction !== 'rtl') ? 'before' : 'after';
			editor.update(
				() => {
					const node = $getNearestNodeFromDOMNode(host);
					if (node != null) {
						$selectComposerNodeBoundary(node, boundary);
					}
				},
				{discrete: true},
			);
			editor.focus();
		},
		[editor],
	);
	const handleEditableContextMenu = useCallback(
		(event: React.MouseEvent<HTMLElement>) => {
			const target = event.target instanceof Element ? event.target : null;
			if (
				target != null &&
				target.closest(
					'[data-lexical-composer-mention], [data-lexical-composer-emoji], [data-lexical-composer-standard-emoji]',
				) != null
			) {
				return;
			}
			editor.focus();
			const doc = event.currentTarget.ownerDocument;
			const defaultView = doc.defaultView;
			const selection = defaultView == null ? null : defaultView.getSelection();
			if (selection == null || !selection.isCollapsed) {
				return;
			}
			const caretRangeFromPoint = doc.caretRangeFromPoint;
			const caret = caretRangeFromPoint == null ? null : caretRangeFromPoint.call(doc, event.clientX, event.clientY);
			if (caret == null) {
				return;
			}
			selection.removeAllRanges();
			selection.addRange(caret);
			if (selection.isCollapsed && typeof selection.modify === 'function') {
				selection.modify('move', 'backward', 'word');
				selection.modify('extend', 'forward', 'word');
			}
		},
		[editor],
	);

	return (
		<>
			<PlainTextPlugin
				contentEditable={
					<FocusRing offset={-2} ringTarget={focusRingTarget} enabled={focusRingEnabled}>
						<ContentEditable
							className={clsx(styles.editable, className)}
							id={id}
							spellCheck
							onKeyDown={handleEditableKeyDown}
							onPointerDown={handleEditablePointerDown}
							onContextMenu={handleEditableContextMenu}
							onFocus={onFocus}
							onBlur={onBlur}
							aria-label={ariaLabelledBy == null ? (ariaLabel == null ? placeholder : ariaLabel) : undefined}
							aria-labelledby={ariaLabelledBy}
							aria-describedby={ariaDescribedBy}
							aria-errormessage={ariaErrorMessage}
							aria-invalid={ariaInvalid}
							aria-disabled={disabled}
							aria-multiline="true"
							aria-autocomplete={autocompleteEnabled ? 'list' : 'none'}
							aria-haspopup={autocompleteEnabled ? 'listbox' : undefined}
							aria-placeholder={placeholder}
							placeholder={
								<flx-lexical-composer-input-placeholder className={flxElementClassName(styles.placeholder)}>
									{placeholder}
								</flx-lexical-composer-input-placeholder>
							}
							data-channel-textarea
							data-composer-render-mode={plainText ? 'plain' : 'rich'}
						/>
					</FocusRing>
				}
				ErrorBoundary={LexicalErrorBoundary}
			/>
			<HistoryPlugin />
			<ComposerTypeaheadPlugin
				options={autocompleteOptions}
				type={autocompleteType}
				query={autocompleteQuery}
				referenceElement={autocompleteReferenceElement}
				listboxId={autocompleteListboxId}
				enabled={autocompleteEnabled && !slotMenuActive}
				onSelect={onAutocompleteSelect}
				activeRef={composerTypeaheadActiveRef}
			/>
			<SlashSlotAutocompletePlugin
				options={autocompleteOptions}
				type={autocompleteType}
				query={autocompleteQuery}
				referenceElement={autocompleteReferenceElement}
				listboxId={autocompleteListboxId}
				enabled={autocompleteEnabled && slotMenuActive}
				onSelect={onAutocompleteSelect}
				activeRef={slashSlotTypeaheadActiveRef}
			/>
			<SelectionFormattingToolbarPlugin enabled={selectionToolbar && !plainText} />
		</>
	);
};

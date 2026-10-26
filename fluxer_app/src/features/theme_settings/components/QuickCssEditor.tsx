// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ColorPickerPopover} from '@app/features/app/components/floating/ColorPickerPopover';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import Theme from '@app/features/theme/state/Theme';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import styles from '@app/features/theme_settings/components/QuickCssEditor.module.css';
import {QuickCssEditorSettings} from '@app/features/theme_settings/components/QuickCssEditorSettings';
import {QuickCssOutline} from '@app/features/theme_settings/components/QuickCssOutline';
import {
	createEditorStatusStore,
	type EditorStatus,
	QuickCssStatusBar,
} from '@app/features/theme_settings/components/QuickCssStatusBar';
import {formatCssColor, parseCssColor, type RgbaColor} from '@app/features/theme_settings/editor/CssColorFormat';
import {
	type ColorSwatchTarget,
	cssColorSwatches,
	openColorAtCursor,
} from '@app/features/theme_settings/editor/CssColorSwatches';
import {cssCompletionConfig, cssCompletionSources} from '@app/features/theme_settings/editor/CssCompletions';
import {formatCssDocument} from '@app/features/theme_settings/editor/CssFormat';
import {cssHover, showInfoAtCursor} from '@app/features/theme_settings/editor/CssHover';
import {cssLint, relintCss} from '@app/features/theme_settings/editor/CssLint';
import {cssNumberStep} from '@app/features/theme_settings/editor/CssNumberStep';
import {cssRegionFolding} from '@app/features/theme_settings/editor/CssOutline';
import {editorPhrases} from '@app/features/theme_settings/editor/EditorPhrases';
import EditorPreferences from '@app/features/theme_settings/editor/EditorPreferences';
import {
	type CachedEditorState,
	registerLiveEditorCapture,
	writeCachedEditorState,
} from '@app/features/theme_settings/editor/EditorStateCache';
import {
	isElementPickerActive,
	startElementPicker,
	stopElementPicker,
	stopElementPickerFor,
	trackLastPointer,
} from '@app/features/theme_settings/editor/ElementPicker';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {getThemeBaseKind, type ThemeBaseKind} from '@app/features/theme_settings/utils/ThemeDefaultVariables';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {formatKeyHintText} from '@app/features/ui/keybind_hint/KeybindHint';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {closeBrackets, closeBracketsKeymap, completionKeymap} from '@codemirror/autocomplete';
import {defaultKeymap, history, historyField, historyKeymap, indentWithTab} from '@codemirror/commands';
import {css} from '@codemirror/lang-css';
import {
	bracketMatching,
	foldGutter,
	foldKeymap,
	HighlightStyle,
	indentOnInput,
	indentUnit,
	syntaxHighlighting,
} from '@codemirror/language';
import {forEachDiagnostic, lintKeymap, setDiagnosticsEffect} from '@codemirror/lint';
import {highlightSelectionMatches, openSearchPanel, searchKeymap} from '@codemirror/search';
import {Compartment, EditorState, type Extension, Prec, Transaction} from '@codemirror/state';
import {
	crosshairCursor,
	drawSelection,
	dropCursor,
	EditorView,
	highlightActiveLine,
	highlightActiveLineGutter,
	highlightSpecialChars,
	keymap,
	lineNumbers,
	placeholder,
	rectangularSelection,
	type ViewUpdate,
} from '@codemirror/view';
import {tags as t} from '@lezer/highlight';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	CursorClickIcon,
	DotsThreeIcon,
	GearSixIcon,
	type Icon,
	ListBulletsIcon,
	MagicWandIcon,
	MagnifyingGlassIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {reaction} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

const FORMAT_DESCRIPTOR = msg({
	message: 'Format',
	comment: 'Button in the quick CSS editor that tidies the indentation and spacing of the CSS.',
});
const OUTLINE_DESCRIPTOR = msg({
	message: 'Outline',
	comment: 'Button in the quick CSS editor that shows a list of the rules in the CSS.',
});
const EDITOR_SETTINGS_DESCRIPTOR = msg({
	message: 'Editor settings',
	comment: 'Button in the quick CSS editor that opens settings such as font size and line wrapping.',
});
const PICK_AN_ELEMENT_DESCRIPTOR = msg({
	message: 'Pick an element',
	comment:
		'Button in the quick CSS editor. After pressing it, clicking a part of the app adds a CSS selector for that part to the CSS.',
});
const PICKER_HINT_DESCRIPTOR = msg({
	message: 'Click an element to add its selector. {parentKey} picks its parent and Escape stops picking.',
	comment:
		'Shown at the bottom of the app while picking an element for the quick CSS editor. {parentKey} is a keyboard shortcut such as Alt + Up.',
});
const FIND_DESCRIPTOR = msg({
	message: 'Find and replace',
	comment: 'Button in the quick CSS editor that opens the search and replace bar.',
});
const MORE_DESCRIPTOR = msg({
	message: 'More',
	comment: 'Button in the quick CSS editor that opens a menu with more actions, such as importing and downloading CSS.',
});
const EMPTY_EDITOR_DESCRIPTOR = msg({
	message: 'Write CSS here. Press {completeKey} for suggestions, or {pickKey} to pick a part of the app.',
	comment:
		'Placeholder in the empty quick CSS editor. {completeKey} and {pickKey} are keyboard shortcuts such as Ctrl + Space.',
});
const COLOR_VALUE_DESCRIPTOR = msg({
	message: 'Color value',
	comment: 'Label of the text field in the color picker of the quick CSS editor, where a CSS color can be typed.',
});
const FIX_ERRORS_BEFORE_FORMATTING_DESCRIPTOR = msg({
	message: 'Fix the errors in your CSS before formatting it.',
	comment: 'Toast in the quick CSS editor when "Format" is used on CSS that cannot be parsed.',
});

const IMMEDIATE_WRITE_EVENTS = ['undo', 'redo', 'input.paste'];
const HISTORY_SAVE_DELAY_MS = 2000;
const OUTLINE_BESIDE_MIN_WIDTH = 720;
const COLOR_PICKER_POPOUT_KEY = 'quick-css-editor-color-picker';

const TOKEN_COLORS = {
	comment: 'var(--text-tertiary)',
	property: 'var(--text-link)',
	keyword: 'var(--accent-purple)',
	tag: 'var(--accent-success)',
	selector: 'var(--accent-warning)',
	number: 'var(--accent-success)',
	string: 'var(--accent-danger)',
	punctuation: 'var(--text-secondary)',
};

const LIGHT_TOKEN_SHARE: Readonly<Record<keyof typeof TOKEN_COLORS, number>> = {
	comment: 65,
	property: 55,
	keyword: 65,
	tag: 45,
	selector: 30,
	number: 45,
	string: 65,
	punctuation: 75,
};

function tokenVariables(baseTheme: ThemeBaseKind): Record<string, string> {
	return Object.fromEntries(
		(Object.keys(TOKEN_COLORS) as Array<keyof typeof TOKEN_COLORS>).map((token) => [
			`--qcss-${token}`,
			baseTheme === 'dark'
				? TOKEN_COLORS[token]
				: `color-mix(in oklab, ${TOKEN_COLORS[token]} ${LIGHT_TOKEN_SHARE[token]}%, var(--text-primary))`,
		]),
	);
}

function tint(color: string, percent: number): string {
	return `color-mix(in srgb, ${color} ${percent}%, transparent)`;
}

function buildEditorTheme(baseTheme: ThemeBaseKind, fontSize: number): Extension {
	const dark = baseTheme === 'dark';
	return EditorView.theme(
		{
			'&': {
				...tokenVariables(baseTheme),
				height: '100%',
				color: 'var(--text-primary)',
				backgroundColor: 'var(--background-secondary)',
				fontSize: `${fontSize}px`,
			},
			'.cm-scroller': {
				fontFamily: 'var(--font-mono, ui-monospace, monospace)',
				lineHeight: '1.6',
				padding: '4px 0',
			},
			'.cm-content': {caretColor: 'var(--text-primary)', padding: '8px 0'},
			'.cm-gutters': {backgroundColor: 'var(--background-secondary)', color: 'var(--qcss-comment)', border: 'none'},
			'.cm-activeLineGutter': {backgroundColor: 'transparent', color: 'var(--text-primary)'},
			'.cm-activeLine': {backgroundColor: tint('var(--text-primary)', 4)},
			'.cm-lineNumbers .cm-gutterElement': {padding: '0 8px 0 16px'},
			'&.cm-focused': {outline: 'none'},
			'.cm-cursor, .cm-dropCursor': {borderLeftColor: 'var(--text-primary)'},
			'.cm-placeholder': {color: 'var(--qcss-comment)'},
			'&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {
				backgroundColor: tint('var(--brand-primary)', 30),
			},
			'.cm-selectionMatch': {backgroundColor: tint('var(--brand-primary)', 18)},
			'.cm-searchMatch': {
				backgroundColor: tint('var(--status-warning)', 25),
				outline: '1px solid var(--status-warning)',
			},
			'.cm-searchMatch.cm-searchMatch-selected': {
				backgroundColor: tint('var(--status-warning)', 45),
			},
			'.cm-matchingBracket, &.cm-focused .cm-matchingBracket': {
				backgroundColor: tint('var(--brand-primary)', 25),
				outline: 'none',
			},
			'.cm-tooltip': {
				backgroundColor: 'var(--background-primary)',
				border: '1px solid var(--background-modifier-accent)',
				borderRadius: '6px',
				boxShadow: '0 8px 24px rgba(0, 0, 0, 0.28)',
				color: 'var(--text-primary)',
			},
			'.cm-tooltip-autocomplete > ul > li': {fontFamily: 'var(--font-mono, ui-monospace, monospace)'},
			'.cm-tooltip-autocomplete > ul > li[aria-selected]': {
				backgroundColor: 'var(--brand-primary)',
				color: 'var(--brand-primary-fill)',
			},
			'.cm-completionDetail': {
				fontFamily: 'var(--font-sans)',
				fontStyle: 'normal',
				opacity: '0.7',
				marginInlineStart: '12px',
			},
			'.cm-completionInfo': {padding: '8px 10px'},
			'.cm-panels': {backgroundColor: 'var(--background-secondary)', color: 'var(--text-primary)'},
			'.cm-panels.cm-panels-bottom': {borderTop: '1px solid var(--background-modifier-accent)'},
			'.cm-panels.cm-panels-top': {borderBottom: '1px solid var(--background-modifier-accent)'},
			'.cm-panel input, .cm-panel button, .cm-panel label': {fontFamily: 'var(--font-sans)', fontSize: '0.8125rem'},
			'.cm-panel.cm-search': {
				display: 'flex',
				flexWrap: 'wrap',
				alignItems: 'center',
				gap: '6px 8px',
				padding: '8px 40px 8px 10px',
			},
			'.cm-panel.cm-search br': {display: 'none'},
			'.cm-panel.cm-search [name=close]': {
				top: '8px',
				insetInlineEnd: '8px',
				width: '24px',
				height: '24px',
				borderRadius: '4px',
				color: 'var(--text-primary-muted)',
				fontSize: '18px',
				lineHeight: '22px',
			},
			'.cm-panel.cm-search [name=close]:hover': {
				color: 'var(--text-primary)',
				backgroundColor: 'var(--background-modifier-hover)',
			},
			'.cm-panel.cm-search label': {
				display: 'inline-flex',
				alignItems: 'center',
				gap: '4px',
				margin: '0',
				color: 'var(--text-primary-muted)',
				whiteSpace: 'nowrap',
				cursor: 'pointer',
			},
			'.cm-panel.cm-search input[type=checkbox]': {
				width: '14px',
				height: '14px',
				margin: '0',
				accentColor: 'var(--brand-primary)',
			},
			'.cm-textfield': {
				flex: '1 1 10rem',
				minWidth: '0',
				height: '28px',
				margin: '0',
				padding: '0 8px',
				backgroundColor: 'var(--background-primary)',
				color: 'var(--text-primary)',
				border: '1px solid var(--background-modifier-accent)',
				borderRadius: '6px',
				outline: 'none',
			},
			'.cm-textfield:focus': {borderColor: 'var(--brand-primary)'},
			'.cm-button': {
				height: '28px',
				margin: '0',
				padding: '0 10px',
				backgroundImage: 'none',
				backgroundColor: 'var(--background-modifier-hover)',
				color: 'var(--text-primary)',
				border: 'none',
				borderRadius: '6px',
				cursor: 'pointer',
			},
			'.cm-button:hover': {backgroundColor: 'var(--background-modifier-selected)'},
			'.cm-panel.cm-panel-lint': {padding: '0 0 4px'},
			'.cm-panel.cm-panel-lint ul': {maxHeight: '160px'},
			'.cm-panel.cm-panel-lint ul::before': {
				content: 'attr(aria-label)',
				display: 'block',
				position: 'sticky',
				top: '0',
				padding: '8px 12px 4px',
				backgroundColor: 'var(--background-secondary)',
				fontFamily: 'var(--font-sans)',
				fontSize: '0.8125rem',
				fontWeight: '600',
				color: 'var(--text-primary)',
			},
			'.cm-panel.cm-panel-lint ul [aria-selected]': {
				backgroundColor: 'var(--background-modifier-selected)',
				color: 'var(--text-primary)',
			},
			'.cm-panel.cm-panel-lint [name=close]': {
				top: '6px',
				insetInlineEnd: '8px',
				color: 'var(--text-primary-muted)',
			},
			'.cm-diagnostic': {fontFamily: 'var(--font-sans)'},
			'.cm-diagnostic-error': {borderLeftColor: 'var(--status-danger)'},
			'.cm-diagnostic-warning': {borderLeftColor: 'var(--status-warning)'},
			'.cm-lintRange-error': {
				backgroundImage: 'none',
				textDecoration: 'underline wavy var(--status-danger)',
				textUnderlineOffset: '3px',
			},
			'.cm-lintRange-warning': {
				backgroundImage: 'none',
				textDecoration: 'underline wavy var(--status-warning)',
				textUnderlineOffset: '3px',
			},
			'.cm-lintPoint-error:after': {borderBottomColor: 'var(--status-danger)'},
			'.cm-lintPoint-warning:after': {borderBottomColor: 'var(--status-warning)'},
			'.cm-foldPlaceholder': {
				backgroundColor: 'var(--background-modifier-hover)',
				border: '1px solid var(--background-modifier-accent)',
				color: 'var(--text-primary-muted)',
			},
		},
		{dark},
	);
}

const highlightStyle = HighlightStyle.define([
	{tag: t.comment, color: 'var(--qcss-comment)', fontStyle: 'italic'},
	{tag: [t.propertyName], color: 'var(--qcss-property)'},
	{tag: [t.keyword, t.modifier], color: 'var(--qcss-keyword)'},
	{tag: [t.tagName, t.typeName], color: 'var(--qcss-tag)'},
	{tag: [t.className, t.labelName], color: 'var(--qcss-selector)'},
	{tag: [t.atom, t.bool, t.unit], color: 'var(--qcss-number)'},
	{tag: [t.number], color: 'var(--qcss-number)'},
	{tag: [t.string, t.special(t.string)], color: 'var(--qcss-string)'},
	{tag: [t.variableName, t.definition(t.variableName)], color: 'var(--qcss-property)'},
	{tag: [t.function(t.variableName), t.macroName], color: 'var(--qcss-selector)'},
	{tag: [t.operator, t.punctuation, t.separator], color: 'var(--qcss-punctuation)'},
	{tag: [t.color], color: 'var(--qcss-string)'},
]);

function emptyEditorText(i18n: I18n): string {
	return i18n._(EMPTY_EDITOR_DESCRIPTOR, {
		completeKey: formatKeyHintText(i18n, {key: ' ', ctrl: true}),
		pickKey: formatKeyHintText(i18n, {key: 'c', ctrlOrMeta: true, shift: true}),
	});
}

function indentExtension(tabSize: number, indentWithTabs: boolean): Extension {
	return [EditorState.tabSize.of(tabSize), indentUnit.of(indentWithTabs ? '\t' : ' '.repeat(tabSize))];
}

function lineNumberExtension(enabled: boolean): Extension {
	return enabled ? [lineNumbers(), highlightActiveLineGutter()] : [];
}

function readStatus(state: EditorState, revision: number): EditorStatus {
	const head = state.selection.main.head;
	const line = state.doc.lineAt(head);
	let selected = 0;
	for (const range of state.selection.ranges) selected += range.to - range.from;
	let errors = 0;
	let warnings = 0;
	forEachDiagnostic(state, (diagnostic) => {
		if (diagnostic.severity === 'error') errors += 1;
		else if (diagnostic.severity === 'warning') warnings += 1;
	});
	return {
		line: line.number,
		column: head - line.from + 1,
		selected,
		lines: state.doc.lines,
		characters: state.doc.length,
		errors,
		warnings,
		revision,
	};
}

function toHexAlpha({r, g, b, a}: RgbaColor): string {
	return `#${[r, g, b, a * 255].map((channel) => Math.round(channel).toString(16).padStart(2, '0')).join('')}`;
}

const ColorSwatchPicker: React.FC<{view: EditorView; target: ColorSwatchTarget; onClose: () => void}> = ({
	view,
	target,
	onClose,
}) => {
	const {i18n} = useLingui();
	const sessionRef = useRef({from: target.from, to: target.to, original: target.value});
	const [color, setColor] = useState(() => parseCssColor(target.value));
	const [text, setText] = useState(target.value);
	const write = (next: string) => {
		const session = sessionRef.current;
		if (session.to > view.state.doc.length) return;
		view.dispatch({changes: {from: session.from, to: session.to, insert: next}, userEvent: 'input.color'});
		session.to = session.from + next.length;
		setText(next);
	};
	const commitText = () => {
		const value = text.trim();
		if (value === view.state.sliceDoc(sessionRef.current.from, sessionRef.current.to) || !CSS.supports('color', value))
			return;
		const parsed = parseCssColor(value);
		if (parsed) setColor(parsed);
		write(value);
	};
	return (
		<div className={styles.popoutSurface} data-flx="theme-settings.quick-css-editor.color-swatch-picker.surface">
			<ColorPickerPopover
				color={color ? toHexAlpha(color) : '#000000ff'}
				showAlpha
				onChange={(hexAlpha) => {
					const parsed = parseCssColor(hexAlpha);
					if (!parsed) return;
					setColor(parsed);
					write(formatCssColor(parsed, sessionRef.current.original));
				}}
				onReset={() => {
					write(sessionRef.current.original);
					onClose();
				}}
				data-flx="theme-settings.quick-css-editor.color-swatch-picker.color-picker-popover"
			>
				<Input
					className={styles.colorValueInput}
					value={text}
					dir="ltr"
					spellCheck={false}
					aria-label={i18n._(COLOR_VALUE_DESCRIPTOR)}
					onChange={(event) => setText(event.target.value)}
					onBlur={commitText}
					onKeyDown={(event) => {
						if (event.key !== 'Enter') return;
						event.preventDefault();
						commitText();
					}}
					data-flx="theme-settings.quick-css-editor.color-swatch-picker.value"
				/>
			</ColorPickerPopover>
		</div>
	);
};

interface ToolButtonProps {
	label: string;
	icon: Icon;
	onClick: (event: React.MouseEvent<HTMLButtonElement>) => void;
	pressed?: boolean;
	disabled?: boolean;
}

const ToolButton: React.FC<ToolButtonProps> = ({label, icon: IconComponent, onClick, pressed, disabled}) => (
	<Tooltip text={label} position="bottom" data-flx="theme-settings.quick-css-editor.tool-button.tooltip">
		<button
			type="button"
			className={clsx(styles.toolButton, pressed && styles.toolButtonPressed)}
			aria-label={label}
			aria-pressed={pressed}
			disabled={disabled}
			onClick={onClick}
			data-flx="theme-settings.quick-css-editor.tool-button.button"
		>
			<IconComponent size={remFromPx(18)} weight="bold" data-flx="theme-settings.quick-css-editor.tool-button.icon" />
		</button>
	</Tooltip>
);

interface QuickCssEditorProps {
	ariaLabel: string;
	value: string;
	onChange: (value: string, immediate: boolean) => void;
	onSave: () => void;
	readOnly?: boolean;
	cacheKey: string;
	cachedState: CachedEditorState | null;
	readStoredCss: () => string;
	viewRef?: React.MutableRefObject<EditorView | null>;
	filePicker: React.ReactNode;
	primaryAction: React.ReactNode;
	renderMenu: (onClose: () => void) => React.ReactNode;
	notes?: React.ReactNode;
	previewStatus: string | null;
}

const QuickCssEditor: React.FC<QuickCssEditorProps> = observer(
	({
		ariaLabel,
		value,
		onChange,
		onSave,
		readOnly,
		cacheKey,
		cachedState,
		readStoredCss,
		viewRef,
		filePicker,
		primaryAction,
		renderMenu,
		notes,
		previewStatus,
	}) => {
		const {i18n} = useLingui();
		const rootRef = useRef<HTMLDivElement | null>(null);
		const containerRef = useRef<HTMLDivElement | null>(null);
		const [view, setView] = useState<EditorView | null>(null);
		const onChangeRef = useRef(onChange);
		onChangeRef.current = onChange;
		const onSaveRef = useRef(onSave);
		onSaveRef.current = onSave;
		const readStoredCssRef = useRef(readStoredCss);
		readStoredCssRef.current = readStoredCss;
		const initialRef = useRef({cacheKey, cachedState, value, readOnly: Boolean(readOnly), ariaLabel, viewRef});
		const status = useMemo(() => createEditorStatusStore(), []);
		const [outlineOpen, setOutlineOpen] = useState(false);
		const [outlineFocusRequest, setOutlineFocusRequest] = useState(0);
		const [outlineBeside, setOutlineBeside] = useState(false);
		const baseTheme = getThemeBaseKind(Theme.effectiveTheme);
		const {
			fontSize,
			tabSize,
			indentWithTabs,
			wrapLines,
			lineNumbers: showLineNumbers,
			completeWhileTyping,
		} = EditorPreferences;

		const compartments = useMemo(
			() => ({
				theme: new Compartment(),
				contentAttributes: new Compartment(),
				placeholder: new Compartment(),
				readOnly: new Compartment(),
				phrases: new Compartment(),
				indent: new Compartment(),
				wrap: new Compartment(),
				lineNumbers: new Compartment(),
				completion: new Compartment(),
			}),
			[],
		);

		const runFormat = useCallback(
			(target: EditorView) => {
				void formatCssDocument(target, {
					useTabs: EditorPreferences.indentWithTabs,
					tabWidth: EditorPreferences.tabSize,
				}).then((result) => {
					if (result === 'has-errors') ToastCommands.error(i18n._(FIX_ERRORS_BEFORE_FORMATTING_DESCRIPTOR));
				});
				return true;
			},
			[i18n],
		);

		const openOutline = useCallback(() => {
			setOutlineOpen(true);
			setOutlineFocusRequest((request) => request + 1);
			return true;
		}, []);

		const togglePicker = useCallback(
			(target: EditorView, fromKeyboard: boolean) => {
				if (isElementPickerActive()) {
					stopElementPicker();
					return true;
				}
				if (target.state.readOnly) return false;
				startElementPicker(target, {
					hint: i18n._(PICKER_HINT_DESCRIPTOR, {parentKey: formatKeyHintText(i18n, {key: 'ArrowUp', alt: true})}),
					startAt: fromKeyboard ? ThemeSettingsCommands.getAppFocusBeforeEditor() : null,
					onDone: ThemeSettingsCommands.focusQuickCssEditor,
				});
				return true;
			},
			[i18n],
		);

		const runFormatRef = useRef(runFormat);
		const togglePickerRef = useRef(togglePicker);
		togglePickerRef.current = togglePicker;
		runFormatRef.current = runFormat;
		const openOutlineRef = useRef(openOutline);
		openOutlineRef.current = openOutline;

		useLayoutEffect(() => {
			const parent = containerRef.current;
			if (!parent) return;
			const initial = initialRef.current;
			const preferences = EditorPreferences;
			let historyTimer: ReturnType<typeof setTimeout> | null = null;
			let currentView: EditorView | null = null;
			let revision = 0;
			const capture = () => {
				if (historyTimer !== null) {
					clearTimeout(historyTimer);
					historyTimer = null;
				}
				if (currentView) {
					writeCachedEditorState(
						initial.cacheKey,
						readStoredCssRef.current(),
						currentView.state,
						currentView.scrollDOM.scrollTop,
					);
				}
			};
			const handleUpdate = (update: ViewUpdate) => {
				if (update.docChanged) {
					const immediate = update.transactions.some((transaction) =>
						IMMEDIATE_WRITE_EVENTS.some((event) => transaction.isUserEvent(event)),
					);
					onChangeRef.current(update.state.doc.toString(), immediate);
					if (historyTimer !== null) clearTimeout(historyTimer);
					historyTimer = setTimeout(capture, HISTORY_SAVE_DELAY_MS);
				}
				const diagnosticsChanged = update.transactions.some((transaction) =>
					transaction.effects.some((effect) => effect.is(setDiagnosticsEffect)),
				);
				if (update.docChanged) revision += 1;
				if (update.docChanged || update.selectionSet || diagnosticsChanged)
					status.set(readStatus(update.state, revision));
			};
			const extensions: Array<Extension> = [
				Prec.highest(
					keymap.of([
						indentWithTab,
						{
							key: 'Mod-s',
							run: () => {
								onSaveRef.current();
								return true;
							},
						},
						{key: 'Mod-Shift-f', run: (target) => runFormatRef.current(target)},
						{key: 'Mod-Shift-o', run: () => openOutlineRef.current()},
						{key: 'Mod-Shift-c', run: (target) => togglePickerRef.current(target, true)},
						{key: 'Alt-Enter', run: openColorAtCursor},
						{key: 'Mod-i', run: showInfoAtCursor},
					]),
				),
				cssNumberStep(),
				compartments.lineNumbers.of(lineNumberExtension(preferences.lineNumbers)),
				highlightSpecialChars(),
				history(),
				foldGutter(),
				drawSelection(),
				dropCursor(),
				EditorState.allowMultipleSelections.of(true),
				indentOnInput(),
				syntaxHighlighting(highlightStyle),
				bracketMatching(),
				closeBrackets(),
				rectangularSelection(),
				crosshairCursor(),
				highlightActiveLine(),
				highlightSelectionMatches(),
				keymap.of([
					...closeBracketsKeymap,
					...defaultKeymap,
					...searchKeymap,
					...historyKeymap,
					...foldKeymap,
					...completionKeymap,
					...lintKeymap,
				]),
				css(),
				cssCompletionSources(),
				compartments.completion.of(cssCompletionConfig(preferences.completeWhileTyping)),
				cssLint(),
				cssHover(),
				cssColorSwatches({
					pickColor: (target, swatch) => {
						PopoutCommands.open({
							key: COLOR_PICKER_POPOUT_KEY,
							position: 'bottom-start',
							target: swatch.anchor,
							keepOpenOnTargetUnmount: true,
							shouldAutoUpdate: false,
							animationType: 'none',
							returnFocusOnClose: false,
							onClose: () => target.focus(),
							render: ({onClose}) => (
								<ColorSwatchPicker
									view={target}
									target={swatch}
									onClose={onClose}
									data-flx="theme-settings.quick-css-editor.color-swatch-picker"
								/>
							),
						});
					},
					openVariable: (name) => {
						ThemeSettingsState.setTokenSearch(name);
						ThemeSettingsState.setEditorTab('colors');
						ThemeSettingsState.requestFocus('tab');
					},
				}),
				cssRegionFolding(),
				compartments.phrases.of(editorPhrases(i18n)),
				compartments.indent.of(indentExtension(preferences.tabSize, preferences.indentWithTabs)),
				compartments.wrap.of(preferences.wrapLines ? EditorView.lineWrapping : []),
				compartments.contentAttributes.of(EditorView.contentAttributes.of({'aria-label': initial.ariaLabel})),
				compartments.placeholder.of(placeholder(emptyEditorText(i18n))),
				compartments.readOnly.of(EditorState.readOnly.of(initial.readOnly)),
				compartments.theme.of(buildEditorTheme(getThemeBaseKind(Theme.effectiveTheme), preferences.fontSize)),
				EditorView.updateListener.of(handleUpdate),
			];
			const cached = initial.cachedState;
			const state =
				cached && cached.doc === initial.value
					? EditorState.fromJSON(cached.json, {extensions}, {history: historyField})
					: EditorState.create({doc: initial.value, extensions});
			const created = new EditorView({state, parent});
			currentView = created;
			if (initial.viewRef) initial.viewRef.current = created;
			if (cached) created.scrollDOM.scrollTop = cached.scrollTop;
			status.set(readStatus(created.state, revision));
			setView(created);
			const stopTrackingPointer = trackLastPointer();
			const unregisterCapture = registerLiveEditorCapture(capture);
			const unregisterView = ThemeSettingsCommands.registerQuickCssEditorView(created);
			return () => {
				unregisterView();
				unregisterCapture();
				stopTrackingPointer();
				stopElementPickerFor(created);
				capture();
				PopoutCommands.close(COLOR_PICKER_POPOUT_KEY);
				currentView = null;
				if (initial.viewRef?.current === created) initial.viewRef.current = null;
				created.destroy();
				setView(null);
			};
		}, [compartments, status]);

		useEffect(() => {
			view?.dispatch({effects: compartments.theme.reconfigure(buildEditorTheme(baseTheme, fontSize))});
		}, [view, baseTheme, fontSize, compartments]);

		useEffect(() => {
			view?.dispatch({effects: compartments.indent.reconfigure(indentExtension(tabSize, indentWithTabs))});
		}, [view, tabSize, indentWithTabs, compartments]);

		useEffect(() => {
			view?.dispatch({effects: compartments.wrap.reconfigure(wrapLines ? EditorView.lineWrapping : [])});
		}, [view, wrapLines, compartments]);

		useEffect(() => {
			view?.dispatch({effects: compartments.lineNumbers.reconfigure(lineNumberExtension(showLineNumbers))});
		}, [view, showLineNumbers, compartments]);

		useEffect(() => {
			view?.dispatch({effects: compartments.completion.reconfigure(cssCompletionConfig(completeWhileTyping))});
		}, [view, completeWhileTyping, compartments]);

		useEffect(() => {
			view?.dispatch({
				effects: [
					compartments.phrases.reconfigure(editorPhrases(i18n)),
					compartments.placeholder.reconfigure(placeholder(emptyEditorText(i18n))),
				],
			});
		}, [view, i18n, i18n.locale, compartments]);

		useEffect(() => {
			view?.dispatch({
				effects: compartments.contentAttributes.reconfigure(EditorView.contentAttributes.of({'aria-label': ariaLabel})),
			});
		}, [view, ariaLabel, compartments]);

		useEffect(() => {
			view?.dispatch({effects: compartments.readOnly.reconfigure(EditorState.readOnly.of(Boolean(readOnly)))});
		}, [view, readOnly, compartments]);

		useEffect(() => {
			if (!view) return;
			const current = view.state.doc.toString();
			if (current === value) return;
			view.dispatch({
				changes: {from: 0, to: current.length, insert: value},
				annotations: Transaction.addToHistory.of(false),
			});
		}, [view, value]);

		useEffect(() => {
			if (!view) return;
			return reaction(
				() => ThemeSettingsState.pendingFocus,
				(target) => {
					if (target !== 'editor' || !view.dom.isConnected || view.dom.closest('[hidden]')) return;
					ThemeSettingsState.requestFocus(null);
					view.focus();
				},
				{fireImmediately: true, scheduler: (run) => requestAnimationFrame(run)},
			);
		}, [view]);

		useEffect(() => {
			if (!view) return;
			view.scrollDOM.style.scrollBehavior = Accessibility.useSmoothScrolling ? 'smooth' : 'auto';
		}, [view, Accessibility.useSmoothScrolling]);

		useEffect(() => {
			if (!view) return;
			return reaction(
				() => [
					ThemeSettingsState.blockedUrls,
					ThemeSettingsState.missingReferences,
					ThemeLibrary.assets,
					ThemeLibrary.localFiles,
				],
				() => relintCss(view),
			);
		}, [view]);

		useEffect(() => {
			const root = rootRef.current;
			if (!root) return;
			const observer = new ResizeObserver(() => setOutlineBeside(root.clientWidth >= OUTLINE_BESIDE_MIN_WIDTH));
			observer.observe(root);
			return () => observer.disconnect();
		}, []);

		const outline =
			outlineOpen && view ? (
				<QuickCssOutline
					view={view}
					status={status}
					beside={outlineBeside}
					focusRequest={outlineFocusRequest}
					onClose={() => {
						setOutlineOpen(false);
						view.focus();
					}}
					data-flx="theme-settings.quick-css-editor.outline"
				/>
			) : null;

		return (
			<div ref={rootRef} className={styles.root} data-flx="theme-settings.quick-css-editor.root">
				<div className={styles.toolbar} data-flx="theme-settings.quick-css-editor.toolbar">
					{filePicker}
					{primaryAction}
					<div className={styles.tools} data-flx="theme-settings.quick-css-editor.tools">
						<ToolButton
							label={i18n._(FIND_DESCRIPTOR)}
							icon={MagnifyingGlassIcon}
							onClick={() => {
								if (view) openSearchPanel(view);
							}}
							data-flx="theme-settings.quick-css-editor.tool-button.find"
						/>
						<ToolButton
							label={i18n._(FORMAT_DESCRIPTOR)}
							icon={MagicWandIcon}
							disabled={readOnly}
							onClick={() => {
								if (view && !readOnly) runFormat(view);
							}}
							data-flx="theme-settings.quick-css-editor.tool-button.format"
						/>
						<ToolButton
							label={i18n._(PICK_AN_ELEMENT_DESCRIPTOR)}
							icon={CursorClickIcon}
							pressed={isElementPickerActive()}
							disabled={readOnly}
							onClick={() => {
								if (view) togglePicker(view, false);
							}}
							data-flx="theme-settings.quick-css-editor.tool-button.pick"
						/>
						<ToolButton
							label={i18n._(OUTLINE_DESCRIPTOR)}
							icon={ListBulletsIcon}
							pressed={outlineOpen}
							onClick={() => (outlineOpen ? setOutlineOpen(false) : openOutline())}
							data-flx="theme-settings.quick-css-editor.tool-button.outline"
						/>
						<ToolButton
							label={i18n._(EDITOR_SETTINGS_DESCRIPTOR)}
							icon={GearSixIcon}
							onClick={(event) =>
								PopoutCommands.open({
									key: 'quick-css-editor-settings',
									position: 'bottom-end',
									target: event.currentTarget,
									animationType: 'none',
									render: () => (
										<div className={styles.popoutSurface} data-flx="theme-settings.quick-css-editor.settings-surface">
											<QuickCssEditorSettings data-flx="theme-settings.quick-css-editor.quick-css-editor-settings" />
										</div>
									),
								})
							}
							data-flx="theme-settings.quick-css-editor.tool-button.settings"
						/>
						<ToolButton
							label={i18n._(MORE_DESCRIPTOR)}
							icon={DotsThreeIcon}
							onClick={(event) =>
								ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => renderMenu(onClose))
							}
							data-flx="theme-settings.quick-css-editor.tool-button.more"
						/>
					</div>
				</div>
				<div className={styles.notes} data-flx="theme-settings.quick-css-editor.notes">
					{notes}
				</div>
				<div
					className={clsx(styles.body, outlineBeside ? styles.bodyBeside : styles.bodyStacked)}
					data-flx="theme-settings.quick-css-editor.body"
				>
					{outline}
					<div
						ref={containerRef}
						className={styles.editor}
						dir="ltr"
						data-flx="theme-settings.quick-css-editor.editor"
					/>
				</div>
				<QuickCssStatusBar
					view={view}
					status={status}
					previewStatus={previewStatus}
					data-flx="theme-settings.quick-css-editor.quick-css-status-bar"
				/>
			</div>
		);
	},
);

export default QuickCssEditor;

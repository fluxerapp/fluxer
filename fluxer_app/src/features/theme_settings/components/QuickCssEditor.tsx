// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ColorPickerPopover} from '@app/features/app/components/floating/ColorPickerPopover';
import {formatKeyCombo} from '@app/features/input/utils/KeybindUtils';
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
import {type ColorSwatchTarget, cssColorSwatches} from '@app/features/theme_settings/editor/CssColorSwatches';
import {cssCompletionConfig, cssCompletionSources} from '@app/features/theme_settings/editor/CssCompletions';
import {formatCssDocument} from '@app/features/theme_settings/editor/CssFormat';
import {cssHover} from '@app/features/theme_settings/editor/CssHover';
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
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
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
import {highlightSelectionMatches, searchKeymap} from '@codemirror/search';
import {Compartment, EditorState, type Extension, Prec} from '@codemirror/state';
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
	rectangularSelection,
	type ViewUpdate,
} from '@codemirror/view';
import {tags as t} from '@lezer/highlight';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CursorClickIcon, GearSixIcon, type Icon, ListBulletsIcon, MagicWandIcon} from '@phosphor-icons/react';
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
		'Shown at the top of the app while picking an element for the quick CSS editor. {parentKey} is a keyboard shortcut such as Alt + Up.',
});
const FIX_ERRORS_BEFORE_FORMATTING_DESCRIPTOR = msg({
	message: 'Fix the errors in your CSS before formatting it.',
	comment: 'Toast in the quick CSS editor when "Format" is used on CSS that cannot be parsed.',
});

const IMMEDIATE_WRITE_EVENTS = ['undo', 'redo', 'input.paste'];
const HISTORY_SAVE_DELAY_MS = 2000;
const OUTLINE_BESIDE_MIN_WIDTH = 720;
const COLOR_PICKER_POPOUT_KEY = 'quick-css-editor-color-picker';

function buildEditorTheme(baseTheme: ThemeBaseKind, fontSize: number): Extension {
	const dark = baseTheme === 'dark';
	return EditorView.theme(
		{
			'&': {
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
			'.cm-gutters': {backgroundColor: 'var(--background-secondary)', color: 'var(--text-tertiary)', border: 'none'},
			'.cm-activeLineGutter': {backgroundColor: 'transparent', color: 'var(--text-primary)'},
			'.cm-activeLine': {backgroundColor: dark ? 'rgba(255, 255, 255, 0.03)' : 'rgba(0, 0, 0, 0.03)'},
			'.cm-lineNumbers .cm-gutterElement': {padding: '0 8px 0 16px'},
			'&.cm-focused': {outline: 'none'},
			'.cm-cursor, .cm-dropCursor': {borderLeftColor: 'var(--text-primary)'},
			'&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {
				backgroundColor: dark ? 'rgba(120, 160, 220, 0.28)' : 'rgba(80, 130, 210, 0.22)',
			},
			'.cm-selectionMatch': {backgroundColor: dark ? 'rgba(120, 160, 220, 0.18)' : 'rgba(80, 130, 210, 0.14)'},
			'.cm-searchMatch': {
				backgroundColor: dark ? 'rgba(250, 200, 80, 0.22)' : 'rgba(230, 170, 30, 0.26)',
				outline: '1px solid var(--status-warning)',
			},
			'.cm-searchMatch.cm-searchMatch-selected': {
				backgroundColor: dark ? 'rgba(250, 200, 80, 0.4)' : 'rgba(230, 170, 30, 0.45)',
			},
			'.cm-matchingBracket, &.cm-focused .cm-matchingBracket': {
				backgroundColor: dark ? 'rgba(120, 160, 220, 0.25)' : 'rgba(80, 130, 210, 0.2)',
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
			'.cm-panel input, .cm-panel button, .cm-panel label': {fontFamily: 'var(--font-sans)', fontSize: '13px'},
			'.cm-textfield': {
				backgroundColor: 'var(--background-primary)',
				color: 'var(--text-primary)',
				border: '1px solid var(--background-modifier-accent)',
				borderRadius: '4px',
			},
			'.cm-button': {
				backgroundImage: 'none',
				backgroundColor: 'var(--background-modifier-hover)',
				color: 'var(--text-primary)',
				border: '1px solid var(--background-modifier-accent)',
				borderRadius: '4px',
			},
			'.cm-panel.cm-panel-lint ul': {maxHeight: '160px'},
			'.cm-panel.cm-panel-lint ul [aria-selected]': {
				backgroundColor: 'var(--background-modifier-selected)',
				color: 'var(--text-primary)',
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
	{tag: t.comment, color: 'var(--text-tertiary)', fontStyle: 'italic'},
	{tag: [t.propertyName], color: 'var(--text-link)'},
	{tag: [t.keyword, t.modifier], color: 'var(--accent-purple)'},
	{tag: [t.tagName, t.typeName], color: 'var(--accent-success)'},
	{tag: [t.className, t.labelName], color: 'var(--accent-warning)'},
	{tag: [t.atom, t.bool, t.unit], color: 'var(--accent-success)'},
	{tag: [t.number], color: 'var(--accent-success)'},
	{tag: [t.string, t.special(t.string)], color: 'var(--accent-danger)'},
	{tag: [t.variableName, t.definition(t.variableName)], color: 'var(--text-link)'},
	{tag: [t.function(t.variableName), t.macroName], color: 'var(--accent-warning)'},
	{tag: [t.operator, t.punctuation, t.separator], color: 'var(--text-secondary)'},
	{tag: [t.color], color: 'var(--accent-danger)'},
]);

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

interface ColorPickerSession {
	from: number;
	to: number;
	original: string;
}

const ColorSwatchPicker: React.FC<{view: EditorView; target: ColorSwatchTarget; onClose: () => void}> = ({
	view,
	target,
	onClose,
}) => {
	const sessionRef = useRef<ColorPickerSession>({from: target.from, to: target.to, original: target.value});
	const [color, setColor] = useState(target.value);
	const write = (next: string) => {
		const session = sessionRef.current;
		if (session.to > view.state.doc.length) return;
		view.dispatch({changes: {from: session.from, to: session.to, insert: next}, userEvent: 'input.color'});
		session.to = session.from + next.length;
		setColor(next);
	};
	return (
		<div className={styles.popoutSurface} data-flx="theme-settings.quick-css-editor.color-swatch-picker.surface">
			<ColorPickerPopover
				color={color}
				onChange={(hex) => write(hex.toLowerCase())}
				onReset={() => {
					write(sessionRef.current.original);
					onClose();
				}}
				data-flx="theme-settings.quick-css-editor.color-swatch-picker.color-picker-popover"
			/>
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
	filePicker: React.ReactNode;
	actions: React.ReactNode;
	notes?: React.ReactNode;
	previewStatus: React.ReactNode;
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
		filePicker,
		actions,
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
		const initialRef = useRef({cacheKey, cachedState, value, readOnly: Boolean(readOnly), ariaLabel});
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
			(target: EditorView) => {
				if (isElementPickerActive()) {
					stopElementPicker();
					return true;
				}
				if (target.state.readOnly) return false;
				startElementPicker(target, {
					hint: i18n._(PICKER_HINT_DESCRIPTOR, {parentKey: formatKeyCombo(i18n, {key: 'ArrowUp', alt: true})}),
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
						{key: 'Mod-Shift-c', run: (target) => togglePickerRef.current(target)},
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
					},
				}),
				cssRegionFolding(),
				compartments.phrases.of(editorPhrases(i18n)),
				compartments.indent.of(indentExtension(preferences.tabSize, preferences.indentWithTabs)),
				compartments.wrap.of(preferences.wrapLines ? EditorView.lineWrapping : []),
				compartments.contentAttributes.of(EditorView.contentAttributes.of({'aria-label': initial.ariaLabel})),
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
			view?.dispatch({effects: compartments.phrases.reconfigure(editorPhrases(i18n))});
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
			view.dispatch({changes: {from: 0, to: current.length, insert: value}});
		}, [view, value]);

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
					<div className={styles.tools} data-flx="theme-settings.quick-css-editor.tools">
						<ToolButton
							label={i18n._(FORMAT_DESCRIPTOR)}
							icon={MagicWandIcon}
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
								if (view) togglePicker(view);
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
					</div>
					{actions}
				</div>
				{notes}
				<div
					className={clsx(styles.body, outlineBeside && styles.bodyBeside)}
					data-flx="theme-settings.quick-css-editor.body"
				>
					{outline}
					<div ref={containerRef} className={styles.editor} data-flx="theme-settings.quick-css-editor.editor" />
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

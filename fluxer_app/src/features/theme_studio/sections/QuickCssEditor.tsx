// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {getTokenVariableDefinition, TOKEN_GROUPS} from '@app/features/theme_studio/sections/TokenGroups';
import type {ThemeStudioBaseTheme} from '@app/features/theme_studio/utils/ThemeStudioPinnedVariables';
import type {CompletionContext, CompletionResult} from '@codemirror/autocomplete';
import {indentWithTab} from '@codemirror/commands';
import {css, cssLanguage} from '@codemirror/lang-css';
import {HighlightStyle, syntaxHighlighting} from '@codemirror/language';
import {Compartment, EditorState, type Extension, Prec} from '@codemirror/state';
import {EditorView, keymap} from '@codemirror/view';
import {tags as t} from '@lezer/highlight';
import {basicSetup} from 'codemirror';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useRef} from 'react';

const themeTokenCompletions = TOKEN_GROUPS.flatMap((group) =>
	group.variables.map((variable) => ({
		label: variable,
		type: getTokenVariableDefinition(variable)?.kind === 'color' ? 'variable' : 'constant',
		boost: 1,
	})),
);

function themeTokenCompletionSource(context: CompletionContext): CompletionResult | null {
	const match = context.matchBefore(/var\(\s*-{0,2}[\w-]*$/u);
	if (!match) return null;
	const token = /[-\w]*$/u.exec(match.text)?.[0] ?? '';
	const from = context.pos - token.length;
	if (!context.explicit && token.length === 0) return null;
	return {
		from,
		options: themeTokenCompletions,
		validFor: /^[-\w]*$/u,
	};
}

function buildEditorTheme(baseTheme: ThemeStudioBaseTheme): Extension {
	const dark = baseTheme === 'dark';
	return EditorView.theme(
		{
			'&': {
				height: '100%',
				color: 'var(--studio-fg-primary)',
				backgroundColor: 'var(--studio-bg-editor)',
				fontSize: '13px',
			},
			'.cm-scroller': {
				fontFamily: 'var(--studio-font-mono)',
				lineHeight: '1.6',
				padding: '4px 0',
			},
			'.cm-content': {
				caretColor: 'var(--studio-fg-primary)',
				padding: '8px 0',
			},
			'.cm-gutters': {
				backgroundColor: 'var(--studio-bg-editor)',
				color: 'var(--studio-fg-muted)',
				border: 'none',
			},
			'.cm-activeLineGutter': {
				backgroundColor: 'transparent',
				color: 'var(--studio-fg-primary)',
			},
			'.cm-activeLine': {
				backgroundColor: dark ? 'rgba(255, 255, 255, 0.03)' : 'rgba(0, 0, 0, 0.03)',
			},
			'.cm-lineNumbers .cm-gutterElement': {
				padding: '0 12px 0 16px',
			},
			'&.cm-focused': {
				outline: 'none',
			},
			'.cm-cursor, .cm-dropCursor': {
				borderLeftColor: 'var(--studio-fg-primary)',
			},
			'&.cm-focused .cm-selectionBackground, .cm-selectionBackground, ::selection': {
				backgroundColor: dark ? 'rgba(120, 160, 220, 0.28)' : 'rgba(80, 130, 210, 0.22)',
			},
			'.cm-selectionMatch': {
				backgroundColor: dark ? 'rgba(120, 160, 220, 0.18)' : 'rgba(80, 130, 210, 0.14)',
			},
			'.cm-matchingBracket, &.cm-focused .cm-matchingBracket': {
				backgroundColor: dark ? 'rgba(120, 160, 220, 0.25)' : 'rgba(80, 130, 210, 0.2)',
				outline: 'none',
			},
			'.cm-tooltip': {
				backgroundColor: 'var(--studio-bg-elevated)',
				border: '1px solid var(--studio-border)',
				borderRadius: '6px',
				boxShadow: '0 8px 24px rgba(0, 0, 0, 0.28)',
				color: 'var(--studio-fg-primary)',
			},
			'.cm-tooltip-autocomplete > ul > li[aria-selected]': {
				backgroundColor: 'var(--studio-accent, rgba(120, 160, 220, 0.3))',
				color: 'var(--studio-fg-primary)',
			},
			'.cm-tooltip-autocomplete > ul > li': {
				fontFamily: 'var(--studio-font-mono)',
			},
		},
		{dark},
	);
}

function buildHighlightStyle(): HighlightStyle {
	return HighlightStyle.define([
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
}

export interface QuickCssEditorProps {
	ariaLabel: string;
	baseTheme: ThemeStudioBaseTheme;
	className?: string;
	onChange: (value: string) => void;
	value: string;
}

const QuickCssEditor: React.FC<QuickCssEditorProps> = observer(({ariaLabel, baseTheme, className, onChange, value}) => {
	const containerRef = useRef<HTMLDivElement | null>(null);
	const viewRef = useRef<EditorView | null>(null);
	const onChangeRef = useRef(onChange);
	onChangeRef.current = onChange;
	const useSmoothScrolling = Accessibility.useSmoothScrolling;

	const themeCompartment = useMemo(() => new Compartment(), []);
	const contentAttributesCompartment = useMemo(() => new Compartment(), []);
	const initialDocumentRef = useRef(value);
	const initialAriaLabelRef = useRef(ariaLabel);
	const initialBaseThemeRef = useRef(baseTheme);

	useEffect(() => {
		const parent = containerRef.current;
		if (!parent) return;
		const state = EditorState.create({
			doc: initialDocumentRef.current,
			extensions: [
				Prec.highest(keymap.of([indentWithTab])),
				basicSetup,
				css(),
				cssLanguage.data.of({autocomplete: themeTokenCompletionSource}),
				EditorView.lineWrapping,
				contentAttributesCompartment.of(EditorView.contentAttributes.of({'aria-label': initialAriaLabelRef.current})),
				themeCompartment.of([buildEditorTheme(initialBaseThemeRef.current), syntaxHighlighting(buildHighlightStyle())]),
				EditorView.updateListener.of((update) => {
					if (update.docChanged) {
						onChangeRef.current(update.state.doc.toString());
					}
				}),
			],
		});
		const view = new EditorView({state, parent});
		viewRef.current = view;
		return () => {
			view.destroy();
			viewRef.current = null;
		};
	}, [contentAttributesCompartment, themeCompartment]);

	useEffect(() => {
		const view = viewRef.current;
		if (!view) return;
		view.dispatch({
			effects: themeCompartment.reconfigure([buildEditorTheme(baseTheme), syntaxHighlighting(buildHighlightStyle())]),
		});
	}, [baseTheme, themeCompartment]);

	useEffect(() => {
		const view = viewRef.current;
		if (!view) return;
		view.dispatch({
			effects: contentAttributesCompartment.reconfigure(EditorView.contentAttributes.of({'aria-label': ariaLabel})),
		});
	}, [ariaLabel, contentAttributesCompartment]);

	useEffect(() => {
		const view = viewRef.current;
		if (!view) return;
		const current = view.state.doc.toString();
		if (current === value) return;
		view.dispatch({changes: {from: 0, to: current.length, insert: value}});
	}, [value]);

	useEffect(() => {
		const view = viewRef.current;
		if (!view) return;
		view.scrollDOM.style.scrollBehavior = useSmoothScrolling ? 'smooth' : 'auto';
	}, [useSmoothScrolling]);

	return <div ref={containerRef} className={className} data-flx="theme-studio.quick-css-editor.div" />;
});

export default QuickCssEditor;

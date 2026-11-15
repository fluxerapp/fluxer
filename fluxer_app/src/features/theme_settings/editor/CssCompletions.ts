// SPDX-License-Identifier: AGPL-3.0-or-later

import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {THEME_VARIABLES} from '@app/features/theme/variables/ThemeVariableManifest';
import {showAppHighlights} from '@app/features/theme_settings/editor/AppHighlights';
import {
	findSelectorMatches,
	readOnScreenFlxValues,
	SEMANTIC_ATTRIBUTES,
} from '@app/features/theme_settings/editor/AppSelectors';
import editorStyles from '@app/features/theme_settings/editor/QuickCssEditorChrome.module.css';
import {
	createSwatchElement,
	getEnabledThemeVariables,
	getFileVariables,
	getVariableGroupLabel,
	isColorVariable,
	readCurrentVariableValue,
	readDefaultVariableValues,
	readDisplayVariableValue,
	readsAsColor,
} from '@app/features/theme_settings/editor/ThemeVariableInfo';
import {
	autocompletion,
	type Completion,
	type CompletionContext,
	type CompletionSource,
	completionStatus,
	selectedCompletion,
	snippetCompletion,
} from '@codemirror/autocomplete';
import {cssLanguage} from '@codemirror/lang-css';
import {syntaxTree} from '@codemirror/language';
import type {Extension} from '@codemirror/state';
import {ViewPlugin, type ViewUpdate} from '@codemirror/view';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const CURRENT_VALUE_DESCRIPTOR = msg({
	message: 'Current value {value}',
	comment: 'Shown for a CSS variable in the quick CSS editor. {value} is the value the app uses right now.',
});
const DARK_DEFAULT_DESCRIPTOR = msg({
	message: 'Default in dark themes {value}',
	comment: 'Shown for a CSS variable in the quick CSS editor. {value} is its default value in dark themes.',
});
const LIGHT_DEFAULT_DESCRIPTOR = msg({
	message: 'Default in light themes {value}',
	comment: 'Shown for a CSS variable in the quick CSS editor. {value} is its default value in light themes.',
});
const HIDE_AN_ELEMENT_DESCRIPTOR = msg({
	message: 'Hide an element',
	comment: 'Completion in the quick CSS editor that inserts a CSS rule hiding an element.',
});
const FONT_FROM_ASSET_DESCRIPTOR = msg({
	message: 'Load a font from your assets',
	comment:
		'Completion in the quick CSS editor that inserts an @font-face rule. Assets are files uploaded under "Assets".',
});
const MEDIA_QUERY_DESCRIPTOR = msg({
	message: 'Apply rules at some window sizes',
	comment: 'Completion in the quick CSS editor that inserts an @media rule.',
});
const SUPPORTS_DESCRIPTOR = msg({
	message: 'Apply rules when the browser supports a feature',
	comment: 'Completion in the quick CSS editor that inserts an @supports rule.',
});
const ROOT_DESCRIPTOR = msg({
	message: 'Change Fluxer variables',
	comment: 'Completion in the quick CSS editor that inserts a :root rule, where theme variables are set.',
});
const ASSET_DESCRIPTOR = msg({
	message: 'Asset',
	comment: 'Detail next to an uploaded theme asset in the completion list of the quick CSS editor.',
});
const LOCAL_FILE_DESCRIPTOR = msg({
	message: 'File on this computer',
	comment: 'Detail next to a linked local file in the completion list of the quick CSS editor.',
});

export function buildVariableInfo(ownerDocument: Document, name: string): HTMLElement {
	const root = ownerDocument.createElement('div');
	root.className = editorStyles.info;
	const current = readDisplayVariableValue(name);
	const defaults = readDefaultVariableValues(name);
	const rows: Array<[string, string | null]> = [
		[i18n._(CURRENT_VALUE_DESCRIPTOR, {value: current || '—'}), current],
		...(defaults.dark !== null ? [[i18n._(DARK_DEFAULT_DESCRIPTOR, {value: defaults.dark}), defaults.dark]] : []),
		...(defaults.light !== null ? [[i18n._(LIGHT_DEFAULT_DESCRIPTOR, {value: defaults.light}), defaults.light]] : []),
	] as Array<[string, string | null]>;
	const color = isColorVariable(name);
	for (const [text, value] of rows) {
		const row = ownerDocument.createElement('div');
		row.className = editorStyles.infoRow;
		if (color && value && readsAsColor(value)) {
			row.appendChild(createSwatchElement(ownerDocument, value, editorStyles.swatch));
		}
		row.appendChild(ownerDocument.createTextNode(text));
		root.appendChild(row);
	}
	return root;
}

function variableOptions(doc: string): Array<Completion> {
	const options: Array<Completion> = THEME_VARIABLES.map((definition) => ({
		label: definition.name,
		type: 'variable',
		detail: getVariableGroupLabel(definition),
		info: () => buildVariableInfo(document, definition.name),
		boost: 1,
	}));
	const known = new Set(options.map((option) => option.label));
	for (const name of [...getFileVariables(doc), ...getEnabledThemeVariables()]) {
		if (known.has(name)) continue;
		known.add(name);
		options.push({label: name, type: 'variable'});
	}
	return options;
}

type SyntaxNodeRef = ReturnType<typeof syntaxTree>['topNode'];

const DECLARATION_START = /(?:^|[{;])\s*(--[\w-]*)$/;

function isInsideDeclaration(context: CompletionContext): boolean {
	let node: SyntaxNodeRef | null = syntaxTree(context.state).resolveInner(context.pos, -1);
	while (node && node.name !== 'Block' && node.name !== 'StyleSheet') {
		if (node.name === 'Declaration') return true;
		node = node.parent;
	}
	return false;
}

const variableSource: CompletionSource = (context) => {
	const call = context.matchBefore(/var\(\s*[-\w]*$/);
	if (call) {
		const token = /[-\w]*$/.exec(call.text)?.[0] ?? '';
		if (!context.explicit && token.length === 0 && !call.text.endsWith('(')) return null;
		return {
			from: context.pos - token.length,
			options: variableOptions(context.state.doc.toString()),
			validFor: /^[-\w]*$/,
		};
	}
	const line = context.state.doc.lineAt(context.pos);
	const declaration = DECLARATION_START.exec(context.state.sliceDoc(line.from, context.pos));
	if (!declaration) return null;
	return {
		from: context.pos - declaration[1].length,
		options: variableOptions(context.state.doc.toString()).map((option) => ({...option, apply: `${option.label}: `})),
		validFor: /^[-\w]*$/,
	};
};

const FLX_COMPLETION_TYPE = 'constant data-flx';

function selectedFlxValue(update: ViewUpdate): string | null {
	if (completionStatus(update.state) !== 'active') return null;
	const selected = selectedCompletion(update.state);
	return selected?.type === FLX_COMPLETION_TYPE ? selected.label : null;
}

const flxOptionHighlight = ViewPlugin.fromClass(
	class {
		value: string | null = null;
		clear: (() => void) | null = null;

		update(update: ViewUpdate): void {
			const value = selectedFlxValue(update);
			if (value === this.value) return;
			this.value = value;
			this.clear?.();
			this.clear = null;
			if (value === null) return;
			const matches = findSelectorMatches(`[data-flx="${value.replace(/["\\]/g, '\\$&')}"]`);
			if (matches && matches.elements.length > 0) this.clear = showAppHighlights(matches.elements);
		}

		destroy(): void {
			this.clear?.();
		}
	},
);

const flxValueSource: CompletionSource = (context) => {
	const match = context.matchBefore(/\[data-flx\s*[~|^$*]?=\s*["']?[^"'\]\s]*$/);
	if (!match) return null;
	const token = /[^"'=\s]*$/.exec(match.text)?.[0] ?? '';
	return {
		from: context.pos - token.length,
		options: readOnScreenFlxValues().map((value) => ({label: value, type: FLX_COMPLETION_TYPE})),
		validFor: /^[^"'\]\s]*$/,
	};
};

const attributeSource: CompletionSource = (context) => {
	const match = context.matchBefore(/\[[\w-]*$/);
	if (!match || isInsideDeclaration(context)) return null;
	return {
		from: match.from + 1,
		options: [
			{label: 'data-flx', type: 'property', boost: 2},
			...SEMANTIC_ATTRIBUTES.map((attribute) => ({
				label: attribute.name,
				type: 'property',
				info: () => document.createTextNode(i18n._(attribute.description)),
			})),
		],
		validFor: /^[\w-]*$/,
	};
};

const referenceSource: CompletionSource = (context) => {
	const localFileCall = context.matchBefore(/fluxer-local-file\(\s*["']?[^"')]*$/);
	if (localFileCall) {
		const token = /[^"'(]*$/.exec(localFileCall.text)?.[0] ?? '';
		const quoted = /["'][^"']*$/.test(localFileCall.text);
		return {
			from: context.pos - token.length,
			options: ThemeLibrary.localFiles.map((file) => ({
				label: file.path,
				displayLabel: file.name,
				detail: i18n._(LOCAL_FILE_DESCRIPTOR),
				type: 'text',
				apply: quoted ? file.path : JSON.stringify(file.path),
			})),
		};
	}
	const urlCall = context.matchBefore(/url\(\s*["']?[^"')\s]*$/);
	if (!urlCall) return null;
	const token = /[^"'(\s]*$/.exec(urlCall.text)?.[0] ?? '';
	return {
		from: context.pos - token.length,
		options: [
			...ThemeLibrary.assets.map((asset) => ({
				label: `fluxer-theme-asset://${encodeURIComponent(asset.name)}`,
				displayLabel: asset.name,
				detail: i18n._(ASSET_DESCRIPTOR),
				type: 'text',
			})),
			...ThemeLibrary.localFiles.map((file) => ({
				label: `fluxer-local-file://${encodeURIComponent(file.path)}`,
				displayLabel: file.name,
				detail: i18n._(LOCAL_FILE_DESCRIPTOR),
				type: 'text',
			})),
		],
		validFor: /^[^"')\s]*$/,
	};
};

const snippetSource: CompletionSource = (context) => {
	const word = context.matchBefore(/[@:\w-]*$/);
	if (!word || (word.from === word.to && !context.explicit)) return null;
	if (isInsideDeclaration(context)) return null;
	const line = context.state.doc.lineAt(context.pos);
	if (!/^\s*$/.test(context.state.sliceDoc(line.from, word.from))) return null;
	const snippets: ReadonlyArray<Completion> = [
		snippetCompletion('@media (max-width: #{600px}) {\n\t#{}\n}', {
			label: '@media',
			detail: i18n._(MEDIA_QUERY_DESCRIPTOR),
			type: 'keyword',
		}),
		snippetCompletion('@supports (#{display: grid}) {\n\t#{}\n}', {
			label: '@supports',
			detail: i18n._(SUPPORTS_DESCRIPTOR),
			type: 'keyword',
		}),
		snippetCompletion(':root {\n\t#{}\n}', {label: ':root', detail: i18n._(ROOT_DESCRIPTOR), type: 'keyword'}),
		snippetCompletion('@font-face {\n\tfont-family: "#{name}";\n\tsrc: url(fluxer-theme-asset://#{file.woff2});\n}', {
			label: '@font-face',
			detail: i18n._(FONT_FROM_ASSET_DESCRIPTOR),
			type: 'keyword',
		}),
		snippetCompletion('[data-flx="#{}"] {\n\tdisplay: none;\n}', {
			label: 'hide',
			detail: i18n._(HIDE_AN_ELEMENT_DESCRIPTOR),
			type: 'keyword',
		}),
	];
	return {from: word.from, options: snippets, validFor: /^[@:\w-]*$/};
};

export function cssCompletionSources(): Extension {
	return [
		...[variableSource, flxValueSource, attributeSource, referenceSource, snippetSource].map((source) =>
			cssLanguage.data.of({autocomplete: source}),
		),
		flxOptionHighlight,
	];
}

export function cssCompletionConfig(activateOnTyping: boolean): Extension {
	return autocompletion({
		activateOnTyping,
		addToOptions: [
			{
				position: 20,
				render: (completion, _state, view) => {
					if (completion.type !== 'variable' || !isColorVariable(completion.label)) return null;
					const value = readCurrentVariableValue(completion.label);
					if (!value) return null;
					return createSwatchElement(view.dom.ownerDocument, value, editorStyles.swatch);
				},
			},
		],
	});
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import {findMissingThemeCssReferences} from '@app/features/theme/utils/ThemeCssUtils';
import {THEME_VARIABLE_NAMES} from '@app/features/theme/variables/ThemeVariableManifest';
import {getEnabledThemeVariables, getFileVariables} from '@app/features/theme_settings/editor/ThemeVariableInfo';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import {ensureSyntaxTree, syntaxTree} from '@codemirror/language';
import {type Diagnostic, linter} from '@codemirror/lint';
import {type EditorState, type Extension, StateEffect} from '@codemirror/state';
import type {EditorView} from '@codemirror/view';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PARSE_ERROR_DESCRIPTOR = msg({
	message: "This CSS can't be parsed.",
	comment: 'Problem shown in the quick CSS editor on a line with a CSS syntax error.',
});
const UNKNOWN_VARIABLE_DESCRIPTOR = msg({
	message: "--{name} isn't a Fluxer variable and isn't set anywhere in your CSS.",
	comment:
		'Problem shown in the quick CSS editor on var() of a CSS variable that does not exist. {name} is the variable name without its leading dashes.',
});
const HASHED_CLASS_DESCRIPTOR = msg({
	message: 'This class name changes with every Fluxer update. Use a [data-flx] selector.',
	comment:
		'Problem shown in the quick CSS editor on a generated class name. [data-flx] is CSS code and stays in English.',
});
const MISSING_ASSET_DESCRIPTOR = msg({
	message: 'Fluxer can\'t find the asset "{name}".',
	comment: 'Problem shown in the quick CSS editor on a reference to an uploaded theme asset that does not exist.',
});
const MISSING_FILE_DESCRIPTOR = msg({
	message: 'Fluxer can\'t find the file "{path}".',
	comment: 'Problem shown in the quick CSS editor on a reference to a file on the computer that Fluxer cannot read.',
});
const BLOCKED_URL_DESCRIPTOR = msg({
	message: 'Fluxer blocked loading {url}. Upload it as an asset.',
	comment:
		'Problem shown in the quick CSS editor on a web address that the app is not allowed to load. Assets are files uploaded under "Assets".',
});
const UNKNOWN_PROPERTY_DESCRIPTOR = msg({
	message: "This browser doesn't know the property {property}.",
	comment: 'Problem shown in the quick CSS editor on a CSS property name the browser does not support.',
});

const HASHED_CLASS_NAME_PATTERN = /^[A-Za-z][\w-]*__[\w-]+___[\w-]{6}$/;
const LINT_DELAY_MS = 300;
const PARSE_TIMEOUT_MS = 200;

const MANIFEST_VARIABLES: ReadonlySet<string> = new Set(THEME_VARIABLE_NAMES);
const propertySupport = new Map<string, boolean>();

function isKnownProperty(name: string): boolean {
	const key = name.toLowerCase();
	let known = propertySupport.get(key);
	if (known === undefined) {
		known = CSS.supports(key, 'initial');
		propertySupport.set(key, known);
	}
	return known;
}

export function lintCss(state: EditorState): Array<Diagnostic> {
	const tree = ensureSyntaxTree(state, state.doc.length, PARSE_TIMEOUT_MS) ?? syntaxTree(state);
	const doc = state.doc.toString();
	const diagnostics: Array<Diagnostic> = [];
	const errorLines = new Set<number>();
	const knownVariables = [MANIFEST_VARIABLES, getFileVariables(doc), getEnabledThemeVariables()];
	tree.iterate({
		enter: (node) => {
			if (node.type.isError) {
				const line = state.doc.lineAt(node.from);
				if (errorLines.has(line.number)) return false;
				errorLines.add(line.number);
				diagnostics.push({
					from: node.from,
					to: Math.max(node.to, Math.min(line.to, node.from + 1)),
					severity: 'error',
					message: i18n._(PARSE_ERROR_DESCRIPTOR),
				});
				return false;
			}
			switch (node.name) {
				case 'Comment':
				case 'StringLiteral':
					return false;
				case 'CallExpression': {
					const callee = node.node.getChild('Callee');
					if (!callee || state.sliceDoc(callee.from, callee.to) !== 'var') return undefined;
					const variable = node.node.getChild('ArgList')?.getChild('VariableName');
					if (!variable) return undefined;
					const name = state.sliceDoc(variable.from, variable.to);
					if (!knownVariables.some((set) => set.has(name))) {
						diagnostics.push({
							from: variable.from,
							to: variable.to,
							severity: 'warning',
							message: i18n._(UNKNOWN_VARIABLE_DESCRIPTOR, {name: name.slice(2)}),
						});
					}
					return undefined;
				}
				case 'ClassName': {
					if (HASHED_CLASS_NAME_PATTERN.test(state.sliceDoc(node.from, node.to))) {
						diagnostics.push({
							from: node.from,
							to: node.to,
							severity: 'warning',
							message: i18n._(HASHED_CLASS_DESCRIPTOR),
						});
					}
					return undefined;
				}
				case 'PropertyName': {
					const property = state.sliceDoc(node.from, node.to);
					if (!property.startsWith('--') && !isKnownProperty(property)) {
						diagnostics.push({
							from: node.from,
							to: node.to,
							severity: 'warning',
							message: i18n._(UNKNOWN_PROPERTY_DESCRIPTOR, {property}),
						});
					}
					return undefined;
				}
				default:
					return undefined;
			}
		},
	});
	const unreadable = new Set(ThemeSettingsState.missingReferences);
	for (const problem of findMissingThemeCssReferences(doc, ThemeLibrary.assets, ThemeLibrary.localFiles, unreadable)) {
		diagnostics.push({
			from: problem.from,
			to: problem.to,
			severity: 'error',
			message:
				problem.kind === 'asset'
					? i18n._(MISSING_ASSET_DESCRIPTOR, {name: problem.reference})
					: i18n._(MISSING_FILE_DESCRIPTOR, {path: problem.reference}),
		});
	}
	for (const url of ThemeSettingsState.blockedUrls) {
		let index = doc.indexOf(url);
		while (index !== -1) {
			diagnostics.push({
				from: index,
				to: index + url.length,
				severity: 'error',
				message: i18n._(BLOCKED_URL_DESCRIPTOR, {url}),
			});
			index = doc.indexOf(url, index + url.length);
		}
	}
	return diagnostics.sort((left, right) => left.from - right.from);
}

const relintEffect = StateEffect.define<null>();

export function relintCss(view: EditorView): void {
	view.dispatch({effects: relintEffect.of(null)});
}

export function cssLint(): Extension {
	return linter((view) => lintCss(view.state), {
		delay: LINT_DELAY_MS,
		needsRefresh: (update) =>
			update.transactions.some((transaction) => transaction.effects.some((effect) => effect.is(relintEffect))),
	});
}

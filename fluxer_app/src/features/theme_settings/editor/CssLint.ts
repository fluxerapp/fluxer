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
const MISSING_SEMICOLON_DESCRIPTOR = msg({
	message: 'Add ";" at the end of this line.',
	comment: 'Problem shown in the quick CSS editor on a CSS declaration that is missing the semicolon after it.',
});
const MISSING_CLOSING_BRACE_DESCRIPTOR = msg({
	message: 'This block is never closed. Add a closing brace after its last rule.',
	comment:
		'Problem shown in the quick CSS editor on an opening curly brace of a CSS block that has no closing curly brace.',
});
const STRAY_CLOSING_BRACE_DESCRIPTOR = msg({
	message: 'This closing brace has no opening brace.',
	comment: 'Problem shown in the quick CSS editor on a closing curly brace that closes nothing.',
});
const UNCLOSED_STRING_DESCRIPTOR = msg({
	message: 'This text is never closed. Add a {quote} at its end.',
	comment:
		'Problem shown in the quick CSS editor on a quoted CSS string with no closing quote. {quote} is the quote character, " or \'.',
});
const UNCLOSED_COMMENT_DESCRIPTOR = msg({
	message: 'This comment is never closed. Add "*/" at its end.',
	comment: 'Problem shown in the quick CSS editor on a CSS comment that has no closing */.',
});
const MISSING_COLON_DESCRIPTOR = msg({
	message: 'Add ":" between the property and its value.',
	comment: 'Problem shown in the quick CSS editor on a CSS declaration like "color red;" that is missing its colon.',
});
const UNKNOWN_PROPERTY_DESCRIPTOR = msg({
	message: "This browser doesn't know the property {property}.",
	comment: 'Problem shown in the quick CSS editor on a CSS property name the browser does not support.',
});

const HASHED_CLASS_NAME_PATTERN = /^[A-Za-z][\w-]*__[\w-]+___[\w-]{6}$/;
const LINT_DELAY_MS = 300;
const PARSE_TIMEOUT_MS = 200;

const MANIFEST_VARIABLES: ReadonlySet<string> = new Set(THEME_VARIABLE_NAMES);
const DESKTOP_ONLY_PROPERTIES: ReadonlySet<string> = new Set([
	'anchor-name',
	'anchor-scope',
	'app-region',
	'field-sizing',
	'interpolate-size',
	'position-anchor',
	'position-area',
	'position-try',
	'position-try-fallbacks',
	'position-try-order',
	'position-visibility',
	'reading-flow',
	'reading-order',
	'scroll-marker-group',
	'scroll-target-group',
	'text-box',
	'text-box-edge',
	'text-box-trim',
]);
const propertySupport = new Map<string, boolean>();

type SyntaxNode = ReturnType<typeof syntaxTree>['topNode'];

interface ParseProblem {
	from: number;
	to: number;
	message: string;
	hidesRestOfBlock?: boolean;
}

function isKnownProperty(name: string): boolean {
	const key = name.toLowerCase();
	if (key.startsWith('-') || DESKTOP_ONLY_PROPERTIES.has(key)) return true;
	let known = propertySupport.get(key);
	if (known === undefined) {
		known = CSS.supports(key, 'initial');
		propertySupport.set(key, known);
	}
	return known;
}

function isStyleDeclaration(property: SyntaxNode): boolean {
	const owner = property.parent?.parent?.parent?.name;
	return owner === 'RuleSet' || owner === 'KeyframeList';
}

function hasFallback(argList: SyntaxNode): boolean {
	return argList.getChild(',') !== null;
}

function unclosedBlockBrace(node: SyntaxNode): SyntaxNode | null {
	for (let parent = node.parent; parent; parent = parent.parent) {
		const isBlock = parent.name === 'Block' || parent.name === 'KeyframeList';
		if (isBlock && parent.lastChild?.name !== '}' && parent.firstChild?.name === '{') return parent.firstChild;
	}
	return null;
}

function describeParseError(state: EditorState, node: SyntaxNode): ParseProblem {
	const text = state.sliceDoc(node.from, node.to);
	const line = state.doc.lineAt(node.from);
	const fallback: ParseProblem = {
		from: node.from,
		to: Math.max(node.to, Math.min(line.to, node.from + 1)),
		message: i18n._(PARSE_ERROR_DESCRIPTOR),
	};
	if (node.from === node.to && state.sliceDoc(node.from).trim().length === 0) {
		const brace = unclosedBlockBrace(node);
		if (brace) return {from: brace.from, to: brace.to, message: i18n._(MISSING_CLOSING_BRACE_DESCRIPTOR)};
	}
	if (text === '}' && node.parent?.name === 'StyleSheet') {
		return {from: node.from, to: node.to, message: i18n._(STRAY_CLOSING_BRACE_DESCRIPTOR)};
	}
	if (text === '"' || text === "'") {
		return {
			from: node.from,
			to: line.to,
			message: i18n._(UNCLOSED_STRING_DESCRIPTOR, {quote: text}),
			hidesRestOfBlock: true,
		};
	}
	if (text === ':' && node.parent?.name === 'Declaration') {
		const value = node.prevSibling?.prevSibling;
		if (value && state.doc.lineAt(value.to).number < line.number) {
			return {from: value.from, to: value.to, message: i18n._(MISSING_SEMICOLON_DESCRIPTOR)};
		}
	}
	if (text === ';') {
		for (let parent = node.parent; parent; parent = parent.parent) {
			if (parent.name === 'Block') break;
			if (parent.name === 'RuleSet' && parent.parent?.name === 'Block') {
				const lineOfRule = state.doc.lineAt(parent.from);
				return {
					from: parent.from,
					to: lineOfRule.to,
					message: i18n._(MISSING_COLON_DESCRIPTOR),
					hidesRestOfBlock: true,
				};
			}
		}
	}
	return fallback;
}

function enclosingBlockEnd(node: SyntaxNode): number {
	for (let parent = node.parent; parent; parent = parent.parent) {
		if (parent.name === 'Block') return parent.to;
	}
	return Number.POSITIVE_INFINITY;
}

export function lintCss(state: EditorState): Array<Diagnostic> {
	const tree = ensureSyntaxTree(state, state.doc.length, PARSE_TIMEOUT_MS) ?? syntaxTree(state);
	const doc = state.doc.toString();
	const diagnostics: Array<Diagnostic> = [];
	const errorLines = new Set<number>();
	const errorParents = new Set<number>();
	let suppressedUntil = -1;
	const knownVariables = [MANIFEST_VARIABLES, getFileVariables(doc), getEnabledThemeVariables()];
	tree.iterate({
		enter: (node) => {
			if (node.type.isError) {
				if (node.from < suppressedUntil) return false;
				const parentStart = node.node.parent?.from ?? -1;
				if (node.from === node.to && errorParents.has(parentStart)) return false;
				const problem = describeParseError(state, node.node);
				const line = state.doc.lineAt(problem.from);
				errorParents.add(parentStart);
				if (errorLines.has(line.number)) return false;
				errorLines.add(line.number);
				if (problem.hidesRestOfBlock) suppressedUntil = enclosingBlockEnd(node.node);
				diagnostics.push({from: problem.from, to: problem.to, message: problem.message, severity: 'error'});
				return false;
			}
			switch (node.name) {
				case 'Comment': {
					const comment = state.sliceDoc(node.from, node.to);
					if (comment.length < 4 || !comment.endsWith('*/')) {
						diagnostics.push({
							from: node.from,
							to: Math.min(node.to, state.doc.lineAt(node.from).to),
							severity: 'error',
							message: i18n._(UNCLOSED_COMMENT_DESCRIPTOR),
						});
					}
					return false;
				}
				case 'StringLiteral':
					return false;
				case 'CallExpression': {
					const callee = node.node.getChild('Callee');
					if (!callee || state.sliceDoc(callee.from, callee.to) !== 'var') return undefined;
					const argList = node.node.getChild('ArgList');
					const variable = argList?.getChild('VariableName');
					if (!argList || !variable || hasFallback(argList)) return undefined;
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
					if (!property.startsWith('--') && isStyleDeclaration(node.node) && !isKnownProperty(property)) {
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

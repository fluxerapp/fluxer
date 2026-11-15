// SPDX-License-Identifier: AGPL-3.0-or-later

import {showAppHighlights} from '@app/features/theme_settings/editor/AppHighlights';
import {
	findSelectorMatches,
	SELECTOR_MATCH_LIMIT,
	type SelectorMatches,
} from '@app/features/theme_settings/editor/AppSelectors';
import {buildVariableInfo} from '@app/features/theme_settings/editor/CssCompletions';
import {selectorSpecificity} from '@app/features/theme_settings/editor/CssSpecificity';
import editorStyles from '@app/features/theme_settings/editor/QuickCssEditorChrome.module.css';
import {getVariableGroupLabel} from '@app/features/theme_settings/editor/ThemeVariableInfo';
import {getTokenVariableDefinition} from '@app/features/theme_settings/utils/TokenGroups';
import {syntaxTree} from '@codemirror/language';
import {type Extension, StateEffect, StateField} from '@codemirror/state';
import {type EditorView, hoverTooltip, keymap, showTooltip, type Tooltip} from '@codemirror/view';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const MATCH_COUNT_DESCRIPTOR = msg({
	message:
		'{count, plural, =0 {Matches nothing in the app right now} one {Matches # element in the app right now} other {Matches # elements in the app right now}}',
	comment: 'Shown when hovering a CSS selector in the quick CSS editor.',
});
const MATCH_LIMIT_DESCRIPTOR = msg({
	message: 'More than {limit} elements match',
	comment:
		'Shown when hovering a CSS selector in the quick CSS editor that matches very many elements. {limit} is a number such as 1,000.',
});
const SPECIFICITY_DESCRIPTOR = msg({
	message: 'Specificity {a}, {b}, {c}',
	comment:
		'Shown when hovering a CSS selector in the quick CSS editor. {a}, {b} and {c} are the three numbers of CSS specificity.',
});

export function describeSelectorMatches(matches: SelectorMatches): string {
	return matches.overLimit
		? i18n._(MATCH_LIMIT_DESCRIPTOR, {limit: i18n.number(SELECTOR_MATCH_LIMIT)})
		: i18n._(MATCH_COUNT_DESCRIPTOR, {count: matches.elements.length});
}

type SyntaxNodeRef = ReturnType<typeof syntaxTree>['topNode'];

function line(ownerDocument: Document, text: string, className?: string): HTMLElement {
	const element = ownerDocument.createElement('div');
	if (className) element.className = className;
	element.textContent = text;
	return element;
}

function variableTooltip(view: EditorView, node: SyntaxNodeRef): Tooltip {
	const name = view.state.sliceDoc(node.from, node.to);
	return {
		pos: node.from,
		end: node.to,
		above: true,
		create: () => {
			const ownerDocument = view.dom.ownerDocument;
			const dom = ownerDocument.createElement('div');
			dom.className = editorStyles.hover;
			dom.appendChild(line(ownerDocument, name, editorStyles.hoverTitle));
			const definition = getTokenVariableDefinition(name);
			if (definition) dom.appendChild(line(ownerDocument, getVariableGroupLabel(definition), editorStyles.hoverMuted));
			dom.appendChild(buildVariableInfo(ownerDocument, name));
			return {dom};
		},
	};
}

function selectorPartAt(ruleSet: SyntaxNodeRef, pos: number): SyntaxNodeRef | null {
	for (let child = ruleSet.firstChild; child && child.name !== 'Block'; child = child.nextSibling) {
		if (child.name !== ',' && child.from <= pos && pos <= child.to) return child;
	}
	return null;
}

function selectorTooltip(view: EditorView, ruleSet: SyntaxNodeRef, part: SyntaxNodeRef): Tooltip {
	const block = ruleSet.getChild('Block');
	const listText = view.state.sliceDoc(ruleSet.from, block?.from ?? ruleSet.to).trim();
	const partText = view.state.sliceDoc(part.from, part.to).trim();
	return {
		pos: part.from,
		end: part.to,
		above: true,
		create: () => {
			const ownerDocument = view.dom.ownerDocument;
			const dom = ownerDocument.createElement('div');
			dom.className = editorStyles.hover;
			dom.appendChild(line(ownerDocument, listText, editorStyles.hoverTitle));
			const matches = findSelectorMatches(listText);
			let clearHighlights: (() => void) | null = null;
			if (matches) {
				dom.appendChild(line(ownerDocument, describeSelectorMatches(matches)));
				if (matches.elements.length > 0) clearHighlights = showAppHighlights(matches.elements);
			}
			const [a, b, c] = selectorSpecificity(partText);
			dom.appendChild(line(ownerDocument, i18n._(SPECIFICITY_DESCRIPTOR, {a, b, c}), editorStyles.hoverMuted));
			return {dom, destroy: () => clearHighlights?.()};
		},
	};
}

function tooltipAt(view: EditorView, pos: number, side: -1 | 1): Tooltip | null {
	const inner = syntaxTree(view.state).resolveInner(pos, side);
	if (inner.name === 'VariableName') return variableTooltip(view, inner);
	for (let node: SyntaxNodeRef | null = inner; node; node = node.parent) {
		if (node.name === 'Block' || node.name === 'Declaration') return null;
		if (node.name === 'RuleSet') {
			const part = selectorPartAt(node, pos);
			return part ? selectorTooltip(view, node, part) : null;
		}
	}
	return null;
}

const setCursorTooltip = StateEffect.define<Tooltip | null>();

const cursorTooltipField = StateField.define<Tooltip | null>({
	create: () => null,
	update: (value, transaction) => {
		for (const effect of transaction.effects) {
			if (effect.is(setCursorTooltip)) return effect.value;
		}
		return transaction.docChanged || transaction.selection ? null : value;
	},
	provide: (field) => showTooltip.from(field),
});

export function showInfoAtCursor(view: EditorView): boolean {
	const head = view.state.selection.main.head;
	const tooltip = tooltipAt(view, head, 1) ?? tooltipAt(view, head, -1);
	if (!tooltip) return false;
	view.dispatch({effects: setCursorTooltip.of(tooltip)});
	return true;
}

export function cssHover(): Extension {
	return [
		hoverTooltip((view, pos, side) => tooltipAt(view, pos, side)),
		cursorTooltipField,
		keymap.of([
			{
				key: 'Escape',
				run: (view) => {
					if (!view.state.field(cursorTooltipField)) return false;
					view.dispatch({effects: setCursorTooltip.of(null)});
					return true;
				},
			},
		]),
	];
}

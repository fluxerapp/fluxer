// SPDX-License-Identifier: AGPL-3.0-or-later

import editorStyles from '@app/features/theme_settings/editor/QuickCssEditorChrome.module.css';
import {isColorVariable, readCurrentVariableValue} from '@app/features/theme_settings/editor/ThemeVariableInfo';
import {syntaxTree} from '@codemirror/language';
import {type Extension, Facet, RangeSetBuilder} from '@codemirror/state';
import {
	Decoration,
	type DecorationSet,
	type EditorView,
	ViewPlugin,
	type ViewUpdate,
	WidgetType,
} from '@codemirror/view';

type SyntaxNode = ReturnType<typeof syntaxTree>['topNode'];

const COLOR_FUNCTIONS = new Set(['rgb', 'rgba', 'hsl', 'hsla', 'hwb', 'lab', 'lch', 'oklab', 'oklch']);
const NOT_NAMED_COLORS = new Set(['inherit', 'initial', 'unset', 'revert', 'revert-layer', 'currentcolor']);
const SWATCH_FROM_ATTRIBUTE = 'data-swatch-from';
const namedColorSupport = new Map<string, boolean>();

interface ColorRange {
	from: number;
	to: number;
	color: string;
	variable: string | null;
}

function isNamedColor(value: string): boolean {
	const key = value.toLowerCase();
	if (NOT_NAMED_COLORS.has(key)) return false;
	let supported = namedColorSupport.get(key);
	if (supported === undefined) {
		supported = CSS.supports('color', key);
		namedColorSupport.set(key, supported);
	}
	return supported;
}

function isInsideDeclaration(node: SyntaxNode): boolean {
	for (let parent = node.parent; parent; parent = parent.parent) {
		if (parent.name === 'Declaration') return true;
		if (parent.name === 'Block') return false;
	}
	return false;
}

function colorRangeFor(view: EditorView, node: SyntaxNode): ColorRange | null | false {
	if (node.name === 'Comment' || node.name === 'StringLiteral') return false;
	if (node.name === 'ColorLiteral') {
		const value = view.state.sliceDoc(node.from, node.to);
		return CSS.supports('color', value) ? {from: node.from, to: node.to, color: value, variable: null} : false;
	}
	if (node.name === 'ValueName') {
		const value = view.state.sliceDoc(node.from, node.to);
		return isNamedColor(value) && isInsideDeclaration(node)
			? {from: node.from, to: node.to, color: value, variable: null}
			: false;
	}
	if (node.name !== 'CallExpression') return null;
	const callee = node.getChild('Callee');
	if (!callee) return null;
	const name = view.state.sliceDoc(callee.from, callee.to).toLowerCase();
	if (COLOR_FUNCTIONS.has(name)) {
		const value = view.state.sliceDoc(node.from, node.to);
		return CSS.supports('color', value) ? {from: node.from, to: node.to, color: value, variable: null} : false;
	}
	if (name !== 'var') return null;
	const variable = node.getChild('ArgList')?.getChild('VariableName');
	if (!variable) return null;
	const variableName = view.state.sliceDoc(variable.from, variable.to);
	if (!isColorVariable(variableName)) return null;
	const value = readCurrentVariableValue(variableName);
	return value ? {from: node.from, to: node.to, color: value, variable: variableName} : false;
}

export interface ColorSwatchTarget {
	from: number;
	to: number;
	value: string;
	anchor: HTMLElement;
}

export interface ColorSwatchHandlers {
	pickColor: (view: EditorView, target: ColorSwatchTarget) => void;
	openVariable: (name: string) => void;
}

const colorSwatchHandlers = Facet.define<ColorSwatchHandlers, ColorSwatchHandlers | null>({
	combine: (values) => values[0] ?? null,
});

class SwatchWidget extends WidgetType {
	constructor(
		readonly color: string,
		readonly from: number,
		readonly to: number,
		readonly variable: string | null,
	) {
		super();
	}

	override eq(other: SwatchWidget): boolean {
		return (
			other.color === this.color && other.from === this.from && other.to === this.to && other.variable === this.variable
		);
	}

	toDOM(view: EditorView): HTMLElement {
		const button = view.dom.ownerDocument.createElement('button');
		button.type = 'button';
		button.tabIndex = -1;
		button.className = editorStyles.inlineSwatch;
		button.style.setProperty('--swatch-color', this.color);
		button.setAttribute('aria-hidden', 'true');
		button.setAttribute(SWATCH_FROM_ATTRIBUTE, String(this.from));
		button.addEventListener('mousedown', (event) => event.preventDefault());
		button.addEventListener('click', (event) => {
			event.preventDefault();
			openSwatch(view, {from: this.from, to: this.to, color: this.color, variable: this.variable}, button);
		});
		return button;
	}

	override ignoreEvent(): boolean {
		return true;
	}
}

function openSwatch(view: EditorView, range: ColorRange, anchor: HTMLElement): void {
	const handlers = view.state.facet(colorSwatchHandlers);
	if (!handlers) return;
	if (range.variable) {
		handlers.openVariable(range.variable);
		return;
	}
	handlers.pickColor(view, {from: range.from, to: range.to, value: view.state.sliceDoc(range.from, range.to), anchor});
}

function buildSwatches(view: EditorView): DecorationSet {
	const builder = new RangeSetBuilder<Decoration>();
	const tree = syntaxTree(view.state);
	for (const {from, to} of view.visibleRanges) {
		tree.iterate({
			from,
			to,
			enter: (node) => {
				const range = colorRangeFor(view, node.node);
				if (range === null) return undefined;
				if (range === false) return false;
				builder.add(
					range.from,
					range.from,
					Decoration.widget({
						widget: new SwatchWidget(range.color, range.from, range.to, range.variable),
						side: -1,
					}),
				);
				return false;
			},
		});
	}
	return builder.finish();
}

export function openColorAtCursor(view: EditorView): boolean {
	const head = view.state.selection.main.head;
	for (const side of [1, -1] as const) {
		for (let node: SyntaxNode | null = syntaxTree(view.state).resolveInner(head, side); node; node = node.parent) {
			const range = colorRangeFor(view, node);
			if (range === false) break;
			if (range === null) continue;
			const anchor = view.contentDOM.querySelector<HTMLElement>(`[${SWATCH_FROM_ATTRIBUTE}="${range.from}"]`);
			if (!anchor) return false;
			openSwatch(view, range, anchor);
			return true;
		}
	}
	return false;
}

const swatchPlugin = ViewPlugin.fromClass(
	class {
		decorations: DecorationSet;

		constructor(view: EditorView) {
			this.decorations = buildSwatches(view);
		}

		update(update: ViewUpdate): void {
			if (update.docChanged || update.viewportChanged || syntaxTree(update.startState) !== syntaxTree(update.state)) {
				this.decorations = buildSwatches(update.view);
			}
		}
	},
	{decorations: (plugin) => plugin.decorations},
);

export function cssColorSwatches(handlers: ColorSwatchHandlers): Extension {
	return [colorSwatchHandlers.of(handlers), swatchPlugin];
}

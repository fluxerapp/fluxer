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
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PICK_COLOR_DESCRIPTOR = msg({
	message: 'Pick a color for {value}',
	comment: 'Accessible label of the color swatch next to a color in the quick CSS editor. {value} is the CSS color.',
});
const OPEN_VARIABLE_DESCRIPTOR = msg({
	message: 'Show {name} in "Colors and fonts"',
	comment:
		'Accessible label of the color swatch next to a CSS variable in the quick CSS editor. {name} is the variable name.',
});

const COLOR_FUNCTIONS = new Set(['rgb', 'rgba', 'hsl', 'hsla', 'hwb', 'lab', 'lch', 'oklab', 'oklch']);

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
		button.className = editorStyles.inlineSwatch;
		button.style.setProperty('--swatch-color', this.color);
		button.setAttribute(
			'aria-label',
			this.variable
				? i18n._(OPEN_VARIABLE_DESCRIPTOR, {name: this.variable})
				: i18n._(PICK_COLOR_DESCRIPTOR, {value: view.state.sliceDoc(this.from, this.to)}),
		);
		button.addEventListener('mousedown', (event) => event.preventDefault());
		button.addEventListener('click', (event) => {
			event.preventDefault();
			const handlers = view.state.facet(colorSwatchHandlers);
			if (!handlers) return;
			if (this.variable) {
				handlers.openVariable(this.variable);
				return;
			}
			handlers.pickColor(view, {
				from: this.from,
				to: this.to,
				value: view.state.sliceDoc(this.from, this.to),
				anchor: button,
			});
		});
		return button;
	}

	override ignoreEvent(): boolean {
		return true;
	}
}

function buildSwatches(view: EditorView): DecorationSet {
	const builder = new RangeSetBuilder<Decoration>();
	const tree = syntaxTree(view.state);
	for (const {from, to} of view.visibleRanges) {
		tree.iterate({
			from,
			to,
			enter: (node) => {
				if (node.name === 'Comment' || node.name === 'StringLiteral') return false;
				if (node.name === 'ColorLiteral') {
					const value = view.state.sliceDoc(node.from, node.to);
					if (CSS.supports('color', value)) {
						builder.add(
							node.from,
							node.from,
							Decoration.widget({widget: new SwatchWidget(value, node.from, node.to, null), side: -1}),
						);
					}
					return false;
				}
				if (node.name !== 'CallExpression') return undefined;
				const callee = node.node.getChild('Callee');
				if (!callee) return undefined;
				const name = view.state.sliceDoc(callee.from, callee.to).toLowerCase();
				if (COLOR_FUNCTIONS.has(name)) {
					const value = view.state.sliceDoc(node.from, node.to);
					if (CSS.supports('color', value)) {
						builder.add(
							node.from,
							node.from,
							Decoration.widget({widget: new SwatchWidget(value, node.from, node.to, null), side: -1}),
						);
					}
					return false;
				}
				if (name === 'var') {
					const variable = node.node.getChild('ArgList')?.getChild('VariableName');
					if (!variable) return undefined;
					const variableName = view.state.sliceDoc(variable.from, variable.to);
					if (!isColorVariable(variableName)) return undefined;
					const value = readCurrentVariableValue(variableName);
					if (value) {
						builder.add(
							node.from,
							node.from,
							Decoration.widget({widget: new SwatchWidget(value, node.from, node.to, variableName), side: -1}),
						);
					}
					return false;
				}
				return undefined;
			},
		});
	}
	return builder.finish();
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

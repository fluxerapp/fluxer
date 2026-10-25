// SPDX-License-Identifier: AGPL-3.0-or-later

import {syntaxTree} from '@codemirror/language';
import {EditorSelection, type Extension, Prec} from '@codemirror/state';
import {type Command, keymap} from '@codemirror/view';

const NUMBER_PATTERN = /^[-+]?(?:\d+\.?\d*|\.\d+)/;

function decimalsOf(value: string): number {
	const dot = value.indexOf('.');
	return dot === -1 ? 0 : value.length - dot - 1;
}

function stepNumbers(step: number): Command {
	return (view) => {
		const {state} = view;
		const tree = syntaxTree(state);
		const ranges = state.selection.ranges.map((range) => {
			for (const side of [-1, 1] as const) {
				const node = tree.resolveInner(range.head, side);
				if (node.name === 'NumberLiteral') return {range, node};
				if (node.name === 'Unit' && node.parent?.name === 'NumberLiteral') return {range, node: node.parent};
			}
			return {range, node: null};
		});
		if (ranges.some((entry) => entry.node === null)) return false;
		const changes = ranges.map(({node}) => {
			const text = state.sliceDoc(node!.from, node!.to);
			const number = NUMBER_PATTERN.exec(text)?.[0] ?? '';
			const decimals = Math.max(decimalsOf(number), decimalsOf(String(Math.abs(step))));
			const insert = (Number.parseFloat(number) + step).toFixed(decimals);
			return {from: node!.from, to: node!.from + number.length, insert};
		});
		const changeSet = state.changes(changes);
		view.dispatch({
			changes: changeSet,
			selection: EditorSelection.create(
				ranges.map(({range}) => EditorSelection.cursor(changeSet.mapPos(range.head, 1))),
				state.selection.mainIndex,
			),
			userEvent: 'input.step',
			scrollIntoView: true,
		});
		return true;
	};
}

export function cssNumberStep(): Extension {
	return Prec.high(
		keymap.of([
			{key: 'Alt-ArrowUp', run: stepNumbers(1)},
			{key: 'Alt-ArrowDown', run: stepNumbers(-1)},
			{key: 'Shift-Alt-ArrowUp', run: stepNumbers(10)},
			{key: 'Shift-Alt-ArrowDown', run: stepNumbers(-10)},
			{key: 'Ctrl-Alt-ArrowUp', run: stepNumbers(0.1)},
			{key: 'Ctrl-Alt-ArrowDown', run: stepNumbers(-0.1)},
		]),
	);
}

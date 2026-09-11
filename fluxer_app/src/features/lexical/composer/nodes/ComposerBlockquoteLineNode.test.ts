// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$createComposerBlockquoteLineNode,
	$getComposerLineNodes,
	$isComposerBlockquoteLineNode,
	ComposerBlockquoteLineNode,
} from '@app/features/lexical/composer/nodes/ComposerBlockquoteLineNode';
import {
	$createParagraphNode,
	$createRangeSelection,
	$createTextNode,
	$getRoot,
	createEditor,
	type EditorConfig,
	type LexicalEditor,
} from 'lexical';
import {describe, expect, it} from 'vitest';

const CONFIG: EditorConfig = {namespace: 'blockquote-line-test', theme: {composerBlockquoteLine: 'quote-line'}};

function createTestEditor(): LexicalEditor {
	return createEditor({
		namespace: 'blockquote-line-test',
		nodes: [ComposerBlockquoteLineNode],
		onError: (error) => {
			throw error;
		},
	});
}

describe('ComposerBlockquoteLineNode', () => {
	it('creates a span carrying the theme class', () => {
		const editor = createTestEditor();
		let dom: HTMLElement | null = null;
		editor.update(
			() => {
				dom = $createComposerBlockquoteLineNode().createDOM(CONFIG);
			},
			{discrete: true},
		);
		expect(dom).not.toBeNull();
		expect(dom!.tagName).toBe('SPAN');
		expect(dom!.className).toBe('quote-line');
	});

	it('is an inline node that cannot be left empty', () => {
		const editor = createTestEditor();
		let shape: {inline: boolean; canBeEmpty: boolean} | null = null;
		editor.update(
			() => {
				const wrapper = $createComposerBlockquoteLineNode();
				shape = {inline: wrapper.isInline(), canBeEmpty: wrapper.canBeEmpty()};
			},
			{discrete: true},
		);
		expect(shape).toEqual({inline: true, canBeEmpty: false});
	});

	it('inserts an attached empty sibling wrapper on insertNewAfter', () => {
		const editor = createTestEditor();
		let result: {isWrapper: boolean; attached: boolean; children: number; isNextSibling: boolean} | null = null;
		editor.update(
			() => {
				const paragraph = $createParagraphNode();
				const wrapper = $createComposerBlockquoteLineNode();
				wrapper.append($createTextNode('a'));
				paragraph.append(wrapper);
				$getRoot().clear().append(paragraph);
				const next = wrapper.insertNewAfter($createRangeSelection(), false);
				result = {
					isWrapper: $isComposerBlockquoteLineNode(next),
					attached: next.isAttached(),
					children: next.getChildrenSize(),
					isNextSibling: next.is(wrapper.getNextSibling()),
				};
			},
			{discrete: true},
		);
		expect(result).toEqual({isWrapper: true, attached: true, children: 0, isNextSibling: true});
	});
});

describe('$getComposerLineNodes', () => {
	it('expands wrappers in document order and leaves other children alone', () => {
		const editor = createTestEditor();
		let texts: Array<string> = [];
		let keysMatch = false;
		editor.update(
			() => {
				const paragraph = $createParagraphNode();
				const lead = $createTextNode('a');
				const wrapper = $createComposerBlockquoteLineNode();
				const marker = $createTextNode('> ');
				const body = $createTextNode('b');
				wrapper.append(marker, body);
				const tail = $createTextNode('c');
				paragraph.append(lead, wrapper, tail);
				$getRoot().clear().append(paragraph);
				const nodes = $getComposerLineNodes(paragraph);
				texts = nodes.map((node) => node.getTextContent());
				keysMatch = nodes.every((node, index) => node.is([lead, marker, body, tail][index]));
			},
			{discrete: true},
		);
		expect(texts).toEqual(['a', '> ', 'b', 'c']);
		expect(keysMatch).toBe(true);
	});

	it('returns the children unchanged when there is no wrapper', () => {
		const editor = createTestEditor();
		let texts: Array<string> = [];
		editor.update(
			() => {
				const paragraph = $createParagraphNode();
				paragraph.append($createTextNode('a'), $createTextNode('b'));
				$getRoot().clear().append(paragraph);
				texts = $getComposerLineNodes(paragraph).map((node) => node.getTextContent());
			},
			{discrete: true},
		);
		expect(texts).toEqual(['a', 'b']);
	});
});

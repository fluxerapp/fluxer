// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MarkdownParseOptions} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {parseMarkdownContent as parse} from '@app/features/messaging/utils/markdown/MarkdownParseCache';
import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {test} from 'vitest';

const SIMPLE_CONTENT = 'hello **world** with <@1234567890> and https://fluxer.app';
const RICH_CONTENT = [
	'# Release notes',
	'This is **bold**, *italic*, __underlined__, ~~removed~~, and ||spoilered||.',
	'> [!WARNING]',
	'> Check `inline code` and a larger block:',
	'```ts',
	'const value = 42;',
	'console.log(value);',
	'```',
	'- first item',
	'- second item with <#1500157798955385456>',
	'| name | value |',
	'| --- | ---: |',
	'| one | 1 |',
].join('\n');
const RENDER_OPTIONS = {
	context: MarkdownContext.STANDARD_WITHOUT_JUMBO,
	channelId: 'channel-1',
} satisfies MarkdownParseOptions;
const SIMPLE_AST = parse({content: SIMPLE_CONTENT, context: MarkdownContext.STANDARD_WITHOUT_JUMBO}).nodes;
const RICH_AST = parse({content: RICH_CONTENT, context: MarkdownContext.STANDARD_WITHOUT_JUMBO}).nodes;
let missSerial = 0;

test('Markdown rendering benchmarks', async ({bench}) => {
	await bench('parse cache hit for simple message content', () => {
		parse({content: SIMPLE_CONTENT, context: MarkdownContext.STANDARD_WITHOUT_JUMBO});
	}).run();

	await bench('parse cache miss for rich message content', () => {
		missSerial += 1;
		parse({content: `${RICH_CONTENT}\n${missSerial}`, context: MarkdownContext.STANDARD_WITHOUT_JUMBO});
	}).run();

	await bench('render simple parsed markdown AST to React nodes', () => {
		let count = 0;
		for (const node of SIMPLE_AST) {
			count += node.type.length;
		}
		(globalThis as {__markdownRenderBenchSink?: number}).__markdownRenderBenchSink = count + RENDER_OPTIONS.context;
	}).run();

	await bench('walk rich parsed markdown AST for render preparation', () => {
		let count = 0;
		const stack = [...RICH_AST];
		while (stack.length > 0) {
			const node = stack.pop()!;
			count += node.type.length;
			const children = (node as {children?: Array<Node>}).children;
			if (children) {
				for (const child of children) {
					stack.push(child);
				}
			}
		}
		(globalThis as {__markdownRenderBenchSink?: number}).__markdownRenderBenchSink = count + RENDER_OPTIONS.context;
	}).run();
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import type {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {isMarkdownParserReady} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import {WasmParser} from '@app/features/messaging/utils/markdown/parser/WasmParser';
import {LRUMap} from '@app/lib/list/ListLruMap';

interface ParseResult {
	nodes: Array<Node>;
}

export type RetainedMarkdownParse = readonly [context: MarkdownContext, content: string, nodes: Array<Node>];

const PARSE_CACHE_CAPACITY = 2048;
const MAX_RETAINED_MARKDOWN_PARSES = 256;
const MAX_RETAINED_MARKDOWN_PARSE_CHARACTERS = 512 * 1024;
const PARSE_CACHE_MAX_CHARACTERS = 2_000_000;
const parseCache = new LRUMap<string, ParseResult>(PARSE_CACHE_CAPACITY);
let cachedCharacters = 0;

ResettableStates.register(parseCache, () => {
	parseCache.clear();
	cachedCharacters = 0;
});

function getParseCacheKey(content: string, context: MarkdownContext): string {
	return `${context}\u0000${content}`;
}

function cacheParseResult(cacheKey: string, result: ParseResult): void {
	if (cacheKey.length > PARSE_CACHE_MAX_CHARACTERS) {
		return;
	}
	while (
		parseCache.size > 0 &&
		(parseCache.size >= PARSE_CACHE_CAPACITY || cachedCharacters + cacheKey.length > PARSE_CACHE_MAX_CHARACTERS)
	) {
		const oldest = parseCache.keys().next().value;
		if (oldest === undefined) {
			break;
		}
		parseCache.delete(oldest);
		cachedCharacters -= oldest.length;
	}
	parseCache.set(cacheKey, result);
	cachedCharacters += cacheKey.length;
}

export function parseMarkdownContent({content, context}: {content: string; context: MarkdownContext}): ParseResult {
	const flags = getParserFlagsForContext(context);
	if (!isMarkdownParserReady()) {
		return new WasmParser(content, flags).parse();
	}
	const cacheKey = getParseCacheKey(content, context);
	const cached = parseCache.get(cacheKey);
	if (cached !== undefined) {
		return cached;
	}
	const parser = new WasmParser(content, flags);
	const result = parser.parse();
	cacheParseResult(cacheKey, result);
	return result;
}

export function captureRetainedMarkdownParses(): Array<RetainedMarkdownParse> {
	const captured: Array<RetainedMarkdownParse> = [];
	let characters = 0;
	for (const [cacheKey, result] of Array.from(parseCache.entries()).reverse()) {
		characters += cacheKey.length + JSON.stringify(result.nodes).length;
		if (characters > MAX_RETAINED_MARKDOWN_PARSE_CHARACTERS || captured.length === MAX_RETAINED_MARKDOWN_PARSES) {
			break;
		}
		const separator = cacheKey.indexOf('\u0000');
		captured.push([
			Number(cacheKey.slice(0, separator)) as MarkdownContext,
			cacheKey.slice(separator + 1),
			result.nodes,
		]);
	}
	return captured.reverse();
}

export function seedRetainedMarkdownParses(parses: ReadonlyArray<RetainedMarkdownParse>): void {
	for (const [context, content, nodes] of parses) {
		const cacheKey = getParseCacheKey(content, context);
		if (!parseCache.has(cacheKey)) {
			cacheParseResult(cacheKey, {nodes});
		}
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {flattenAST} from './AstUtils';
import {getEmojiParserConfig} from './EmojiParsers';
import {MARKDOWN_PARSER_WASM_BASE64} from './MarkdownParserWasmBytes';
import type {Node} from './Nodes';
import * as StringUtils from './StringUtils';
import * as URLUtils from './UrlUtils';

interface WasmExports {
	memory: {buffer: ArrayBuffer};
	markdown_alloc(len: number): number;
	markdown_free(ptr: number, len: number): void;
	parse_markdown_ast(
		inputPtr: number,
		inputLen: number,
		flags: number,
		emojiContextPtr: number,
		emojiContextLen: number,
		outPtr: number,
	): number;
}

type WasmFunctionExportName = Exclude<keyof WasmExports, 'memory'>;
type CleanedTextNode = Extract<Node, {type: 'Text'}> & {__cleanedLinkText?: boolean};
type CleanableListItem = Omit<Extract<Node, {type: 'List'}>['items'][number], 'children'> & {
	children: Array<CleanableNode>;
};
type CleanableNode = Node & {
	content?: unknown;
	rawUrl?: unknown;
	source?: unknown;
	url?: unknown;
	text?: CleanableNode;
	children?: Array<CleanableNode>;
	items?: Array<CleanableListItem>;
	header?: CleanableNode;
	rows?: Array<CleanableNode>;
	cells?: Array<CleanableNode>;
};

const RESULT_HEADER_BYTES = 16;
const MAX_RETAINED_WASM_MEMORY_BYTES = 64 * 1024 * 1024;
const textEncoder = new TextEncoder();
const lenientTextDecoder = new TextDecoder('utf-8', {ignoreBOM: true, fatal: false});

let textDecoder = new TextDecoder('utf-8', {ignoreBOM: true, fatal: true});
let wasm: WasmExports | null = null;
let cachedMemory: Uint8Array | null = null;

declare const WebAssembly: {
	Module: new (bytes: Uint8Array) => unknown;
	Instance: new (module: unknown, imports?: Record<string, unknown>) => {exports: Record<string, unknown>};
};

function getWasmMemory(exports: Record<string, unknown>): WasmExports['memory'] {
	const memory = exports.memory;
	if (memory && typeof memory === 'object' && 'buffer' in memory && memory.buffer instanceof ArrayBuffer) {
		return memory as WasmExports['memory'];
	}
	throw new Error('markdown parser wasm memory export is missing');
}

function getWasmFunctionExport<TName extends WasmFunctionExportName>(
	exports: Record<string, unknown>,
	name: TName,
): WasmExports[TName] {
	const exportedValue = exports[name];
	if (typeof exportedValue === 'function') {
		return exportedValue as WasmExports[TName];
	}
	throw new Error(`markdown parser wasm function export "${name}" is missing`);
}

function createWasmExports(exports: Record<string, unknown>): WasmExports {
	return {
		memory: getWasmMemory(exports),
		markdown_alloc: getWasmFunctionExport(exports, 'markdown_alloc'),
		markdown_free: getWasmFunctionExport(exports, 'markdown_free'),
		parse_markdown_ast: getWasmFunctionExport(exports, 'parse_markdown_ast'),
	};
}

function decodeBase64(value: string): Uint8Array {
	if (typeof Buffer !== 'undefined') {
		return new Uint8Array(Buffer.from(value, 'base64'));
	}
	const binary = atob(value);
	const bytes = new Uint8Array(binary.length);
	for (let i = 0; i < binary.length; i++) {
		bytes[i] = binary.charCodeAt(i);
	}
	return bytes;
}

function getWasm(): WasmExports {
	if (!wasm) {
		const module = new WebAssembly.Module(decodeBase64(MARKDOWN_PARSER_WASM_BASE64));
		const instance = new WebAssembly.Instance(module, {});
		wasm = createWasmExports(instance.exports);
		cachedMemory = null;
	}
	return wasm;
}

function releaseOversizedWasmMemory(): void {
	if ((wasm?.memory.buffer.byteLength ?? 0) <= MAX_RETAINED_WASM_MEMORY_BYTES) {
		return;
	}
	wasm = null;
	cachedMemory = null;
}

function memoryU8(): Uint8Array {
	const exports = getWasm();
	if (!cachedMemory || cachedMemory.buffer !== exports.memory.buffer) {
		cachedMemory = new Uint8Array(exports.memory.buffer);
	}
	return cachedMemory;
}

function alloc(len: number): number {
	if (len === 0) return 0;
	const ptr = getWasm().markdown_alloc(len >>> 0);
	if (ptr === 0) throw new Error('markdown parser wasm allocation failed');
	return ptr >>> 0;
}

function free(ptr: number, len: number): void {
	if (ptr !== 0 && len !== 0) getWasm().markdown_free(ptr >>> 0, len >>> 0);
}

function passString(value: string): {ptr: number; len: number} {
	if (!value) return {ptr: 0, len: 0};
	const bytes = textEncoder.encode(value);
	const ptr = alloc(bytes.byteLength);
	memoryU8().set(bytes, ptr);
	return {ptr, len: bytes.byteLength};
}

function decodeText(ptr: number, len: number): string {
	if (len === 0) return '';
	try {
		return textDecoder.decode(memoryU8().subarray(ptr, ptr + len));
	} catch {
		textDecoder = lenientTextDecoder;
		return textDecoder.decode(memoryU8().subarray(ptr, ptr + len));
	}
}

function decodeJsonByteArray(value: ReadonlyArray<unknown>): string {
	const bytes = new Uint8Array(value.length);
	for (let index = 0; index < value.length; index++) {
		const byte = value[index];
		bytes[index] = typeof byte === 'number' && Number.isInteger(byte) ? byte & 0xff : 0;
	}
	return lenientTextDecoder.decode(bytes);
}

function coerceWasmString(value: unknown): string {
	if (typeof value === 'string') return value;
	if (Array.isArray(value)) return decodeJsonByteArray(value);
	if (value == null) return '';
	return String(value);
}

function readStringResult(outPtr: number): string {
	const header = new DataView(getWasm().memory.buffer, outPtr, RESULT_HEADER_BYTES);
	const dataPtr = header.getUint32(0, true);
	const dataLen = header.getUint32(4, true);
	const errPtr = header.getUint32(8, true);
	const errLen = header.getUint32(12, true);
	if (errPtr !== 0 || errLen !== 0) {
		const message = decodeText(errPtr, errLen) || 'markdown parser wasm call failed';
		free(errPtr, errLen);
		throw new Error(message);
	}
	const value = decodeText(dataPtr, dataLen);
	free(dataPtr, dataLen);
	return value;
}

class Utf8OffsetTracker {
	private index = 0;
	private byteOffset = 0;

	offsetFor(input: string, index: number): number {
		if (index < this.index) {
			this.index = 0;
			this.byteOffset = 0;
		}
		if (index > this.index) {
			this.byteOffset += textEncoder.encode(input.slice(this.index, index)).byteLength;
			this.index = index;
		}
		return this.byteOffset;
	}

	advance(characters: number, bytes: number): void {
		this.index += characters;
		this.byteOffset += bytes;
	}
}

function defaultCodepoints(emoji: string): string {
	const containsZwJ = emoji.includes('‍');
	const processed = containsZwJ ? emoji : emoji.replace(/️/g, '');
	return Array.from(processed)
		.map((char) => char.codePointAt(0)?.toString(16).replace(/^0+/, '') || '')
		.join('-');
}

const PLAINTEXT_SYMBOLS = new Set(['™', '™️', '©', '©️', '®', '®️']);
const SPECIAL_SHORTCODES: Record<string, string> = {
	tm: '™',
	copyright: '©',
	registered: '®',
};

function appendContextLine(parts: Array<string>): string {
	return `${parts.join('\t')}\n`;
}

function buildEmojiContext(input: string): string {
	const config = getEmojiParserConfig();
	const provider = config?.emojiProvider;
	let context = '';
	if (!provider) return context;
	const convertToCodePoints = config.convertToCodePoints || defaultCodepoints;
	const emojiRegex = config.emojiRegex;
	if (emojiRegex) {
		const offsetTracker = new Utf8OffsetTracker();
		emojiRegex.lastIndex = 0;
		let match: RegExpExecArray | null;
		while ((match = emojiRegex.exec(input)) !== null) {
			const candidate = match[0];
			if (!candidate || PLAINTEXT_SYMBOLS.has(candidate)) continue;
			const name = provider.getSurrogateName(candidate);
			if (!name) continue;
			const candidateBytes = textEncoder.encode(candidate).byteLength;
			const byteOffset = offsetTracker.offsetFor(input, match.index);
			context += appendContextLine([
				'S',
				String(byteOffset),
				String(candidateBytes),
				candidate,
				name,
				convertToCodePoints(candidate),
			]);
			offsetTracker.advance(candidate.length, candidateBytes);
		}
	}
	const shortcodeRegex = /:([\p{L}\p{N}_-]+):/gu;
	const seen = new Set<string>();
	let shortcodeMatch: RegExpExecArray | null;
	while ((shortcodeMatch = shortcodeRegex.exec(input)) !== null) {
		const name = shortcodeMatch[1];
		if (!name || SPECIAL_SHORTCODES[name]) continue;
		if (!seen.has(`C:${name}`)) {
			seen.add(`C:${name}`);
			const emoji = provider.findEmojiByName(name);
			if (emoji) {
				context += appendContextLine(['C', name, emoji.surrogates, convertToCodePoints(emoji.surrogates)]);
			}
		}
		if (config.skinToneSurrogates) {
			for (let tone = 1; tone <= 5; tone++) {
				const key = `K:${name}:${tone}`;
				if (seen.has(key)) continue;
				seen.add(key);
				const skinToneEmoji = provider.findEmojiWithSkinTone(name, config.skinToneSurrogates[tone - 1]);
				if (skinToneEmoji) {
					context += appendContextLine([
						'K',
						name,
						String(tone),
						skinToneEmoji.surrogates,
						convertToCodePoints(skinToneEmoji.surrogates),
					]);
				}
			}
		}
	}
	return context;
}

function textFromNode(node: Node | undefined): string {
	if (!node) return '';
	if (node.type === 'Text') return coerceWasmString(node.content);
	if ('children' in node && Array.isArray(node.children))
		return node.children.map((child) => textFromNode(child)).join('');
	return '';
}

function shouldTreatAsMaskedLink(trimmedLinkText: string, url: string): boolean {
	try {
		const normalizedUrl = URLUtils.normalizeUrl(url);
		const urlObj = new URL(normalizedUrl);
		const textUrl = new URL(trimmedLinkText.trim());
		return !(
			urlObj.origin === textUrl.origin &&
			urlObj.pathname === textUrl.pathname &&
			urlObj.search === textUrl.search &&
			urlObj.hash === textUrl.hash
		);
	} catch {
		return true;
	}
}

function makeCleanedTextNode(content: string): CleanedTextNode {
	return {type: 'Text', content, __cleanedLinkText: true};
}

function cleanNode(node: CleanableNode): Node {
	if (node.type === 'Text') {
		node.content = coerceWasmString(node.content);
		return node as Node;
	}
	if (node.type === 'CodeBlock' || node.type === 'InlineCode') {
		node.content = coerceWasmString(node.content);
	}
	if (node.type === 'Link') {
		const rawUrl = coerceWasmString(node.rawUrl ?? node.url);
		const source = coerceWasmString(node.source ?? rawUrl);
		const linkText = textFromNode(node.text);
		try {
			const normalizedUrl = URLUtils.normalizeUrl(rawUrl);
			if (
				!URLUtils.isValidUrl(normalizedUrl) ||
				(rawUrl.startsWith('/') && !rawUrl.startsWith('//')) ||
				(StringUtils.startsWithUrl(linkText.trim()) && shouldTreatAsMaskedLink(linkText, rawUrl))
			) {
				return makeCleanedTextNode(source);
			}
			let finalUrl = normalizedUrl;
			if (finalUrl.startsWith('tel:') || finalUrl.startsWith('sms:')) {
				const protocol = finalUrl.substring(0, finalUrl.indexOf(':') + 1);
				const phoneNumber = finalUrl.substring(finalUrl.indexOf(':') + 1);
				if (phoneNumber.startsWith('+')) finalUrl = protocol + URLUtils.normalizePhoneNumber(phoneNumber);
			} else {
				finalUrl = URLUtils.convertToAsciiUrl(finalUrl);
			}
			const cleaned: Extract<Node, {type: 'Link'}> = {
				type: 'Link',
				url: finalUrl,
				escaped: Boolean(node.escaped),
			};
			cleaned.text = node.text ? cleanNode(node.text) : undefined;
			return cleaned;
		} catch {
			return makeCleanedTextNode(source);
		}
	}
	if (node.type === 'CodeBlock' && !('language' in node)) {
		node.language = undefined;
	}
	if (node.type === 'Mention' && node.kind?.kind === 'Command') {
		if (!('subcommandGroup' in node.kind)) node.kind.subcommandGroup = undefined;
		if (!('subcommand' in node.kind)) node.kind.subcommand = undefined;
	}
	if (
		node.type === 'Mention' &&
		node.kind?.kind === 'GuildNavigation' &&
		node.kind.navigationType === 'LinkedRoles' &&
		!('id' in node.kind)
	) {
		node.kind.id = undefined;
	}
	if ('children' in node && Array.isArray(node.children)) {
		node.children = node.children.map((child) => cleanNode(child));
	}
	if (node.type === 'List' && Array.isArray(node.items)) {
		node.items = node.items.map((item) => ({
			...item,
			children: item.children.map((child) => cleanNode(child)),
		}));
	}
	if (node.type === 'Table') {
		node.header = cleanNode(node.header) as Extract<CleanableNode, {type: 'TableRow'}>;
		node.rows = node.rows.map((row) => cleanNode(row)) as Array<Extract<CleanableNode, {type: 'TableRow'}>>;
	}
	if (node.type === 'TableRow') {
		node.cells = node.cells.map((cell) => cleanNode(cell)) as Array<Extract<CleanableNode, {type: 'TableCell'}>>;
	}
	if (node.type === 'TableCell') {
		node.children = node.children.map((child) => cleanNode(child));
	}
	if (node.type === 'Sequence') {
		node.children = node.children.map((child) => cleanNode(child));
	}
	return node as Node;
}

function shouldKeepCleanedTextBoundary(left: string, right: string): boolean {
	return (
		isMalformedCleanedBlockText(left) ||
		isMalformedCleanedBlockText(right) ||
		(left.trim() === '' && left.includes('\n')) ||
		(right.trim() === '' && right.includes('\n')) ||
		(left.endsWith('\n') && right.startsWith('\n')) ||
		left.endsWith('\n\n') ||
		right.startsWith('\n\n') ||
		(right.includes('\n\n') && (left.endsWith('\n') || right.startsWith('\n')))
	);
}

function isMalformedCleanedBlockText(content: string): boolean {
	if (!content || (content[0] !== '#' && !(content[0] === '-' && content[1] === '#'))) return false;
	const trimmed = content.trim();
	return trimmed.startsWith('#') || trimmed.startsWith('-#');
}

function mergeCleanedTextNodes(nodes: Array<Node>): void {
	if (nodes.length <= 1) {
		for (const node of nodes) {
			if (node.type === 'Text') delete (node as CleanedTextNode).__cleanedLinkText;
		}
		return;
	}
	const merged: Array<Node> = [];
	for (const node of nodes) {
		const previous = merged[merged.length - 1];
		const shouldMergeCleanedText =
			previous?.type === 'Text' &&
			node.type === 'Text' &&
			((previous as CleanedTextNode).__cleanedLinkText || (node as CleanedTextNode).__cleanedLinkText) &&
			!shouldKeepCleanedTextBoundary(previous.content, node.content);
		if (shouldMergeCleanedText) {
			previous.content += node.content;
			(previous as CleanedTextNode).__cleanedLinkText ||= (node as CleanedTextNode).__cleanedLinkText;
		} else {
			merged.push(node);
		}
	}
	for (const node of merged) {
		if (node.type === 'Text') delete (node as CleanedTextNode).__cleanedLinkText;
	}
	nodes.length = 0;
	nodes.push(...merged);
}

export function parseMarkdownAstWithWasm(input: string, parserFlags: number): {nodes: Array<Node>} {
	const inputBytes = passString(input);
	const emojiContext = passString(buildEmojiContext(input));
	const outPtr = alloc(RESULT_HEADER_BYTES);
	try {
		const status = getWasm().parse_markdown_ast(
			inputBytes.ptr,
			inputBytes.len,
			parserFlags >>> 0,
			emojiContext.ptr,
			emojiContext.len,
			outPtr,
		);
		const result = JSON.parse(readStringResult(outPtr)) as {nodes: Array<CleanableNode>};
		if (status !== 0) throw new Error('markdown parser wasm call failed');
		const nodes = result.nodes.map((node) => cleanNode(node));
		mergeCleanedTextNodes(nodes);
		flattenAST(nodes);
		return {nodes};
	} finally {
		try {
			free(inputBytes.ptr, inputBytes.len);
			free(emojiContext.ptr, emojiContext.len);
			free(outPtr, RESULT_HEADER_BYTES);
		} finally {
			releaseOversizedWasmMemory();
		}
	}
}

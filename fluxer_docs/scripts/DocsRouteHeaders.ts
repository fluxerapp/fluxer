import assert from 'node:assert/strict';
import {parseFrontmatter} from '@astrojs/markdown-remark';
import {createProcessor} from '@mdx-js/mdx';
import type {Nodes, Root} from 'mdast';
import type {MdxJsxAttribute, MdxJsxFlowElement, MdxJsxTextElement} from 'mdast-util-mdx-jsx';
import remarkGfm from 'remark-gfm';
import {HTTP_METHODS, type MarkdownPage} from './DocsSource.ts';

export interface DocsRouteHeader {
	readonly method: string;
	readonly path: string;
	readonly file: string;
	readonly bot: boolean;
	readonly unauthenticated: boolean;
	readonly line: number;
	readonly endLine: number;
}

const STRING_ATTRIBUTES = new Set(['method', 'path', 'oauth2']);
const BOOLEAN_ATTRIBUTES = new Set(['bot', 'unauthenticated', 'auditReason', 'mfa']);
const markdownProcessor = createProcessor({format: 'md', remarkPlugins: [remarkGfm]});
const mdxProcessor = createProcessor({format: 'mdx', remarkPlugins: [remarkGfm]});

function readAttributeValue(attribute: MdxJsxAttribute, location: string): string | boolean {
	const value = attribute.value;
	if (value === null || value === undefined) return true;
	if (typeof value === 'string') return value;
	const body = value.data?.estree?.body;
	if (body?.length === 1 && body[0].type === 'ExpressionStatement') {
		const expression = body[0].expression;
		if (
			expression.type === 'Literal' &&
			(typeof expression.value === 'string' || typeof expression.value === 'boolean')
		) {
			return expression.value;
		}
		if (
			expression.type === 'TemplateLiteral' &&
			expression.expressions.length === 0 &&
			expression.quasis.length === 1 &&
			typeof expression.quasis[0].value.cooked === 'string'
		) {
			return expression.quasis[0].value.cooked;
		}
	}
	throw new Error(`${location}: RouteHeader ${attribute.name} requires a literal string or boolean`);
}

function readHeader(node: MdxJsxFlowElement | MdxJsxTextElement, file: string): DocsRouteHeader {
	assert(node.position !== undefined, 'Parsed RouteHeader has no source position');
	const line = node.position.start.line;
	const endLine = node.position.end.line;
	assert(Number.isInteger(line) && line > 0 && Number.isInteger(endLine) && endLine >= line);
	const location = `${file}:${line}`;
	const attributes = new Map<string, string | boolean>();
	for (const attribute of node.attributes) {
		if (attribute.type === 'mdxJsxExpressionAttribute') {
			throw new Error(`${location}: RouteHeader spread attributes are unsupported`);
		}
		const name = attribute.name;
		if (attributes.has(name)) {
			throw new Error(`${location}: duplicate RouteHeader attribute ${name}`);
		}
		const isString = STRING_ATTRIBUTES.has(name);
		if (!isString && !BOOLEAN_ATTRIBUTES.has(name)) {
			throw new Error(`${location}: unknown RouteHeader attribute ${name}`);
		}
		const value = readAttributeValue(attribute, location);
		if (isString ? typeof value !== 'string' : typeof value !== 'boolean') {
			throw new Error(`${location}: RouteHeader ${name} must be ${isString ? 'a string' : 'a boolean'}`);
		}
		attributes.set(name, value);
	}
	const method = attributes.get('method');
	const routePath = attributes.get('path');
	if (typeof method !== 'string' || typeof routePath !== 'string') {
		throw new Error(`${location}: RouteHeader requires method and path attributes`);
	}
	if (!HTTP_METHODS.has(method)) {
		throw new Error(`${location}: unsupported RouteHeader method ${method}`);
	}
	if (!routePath.startsWith('/')) {
		throw new Error(`${location}: RouteHeader path must be absolute`);
	}
	return {
		method,
		path: routePath,
		file,
		bot: attributes.get('bot') === true,
		unauthenticated: attributes.get('unauthenticated') === true,
		line,
		endLine,
	};
}

export function readRouteHeaders(page: MarkdownPage): Array<DocsRouteHeader> {
	let tree: Root;
	try {
		const {content} = parseFrontmatter(page.source, {frontmatter: 'empty-with-spaces'});
		const processor = page.file.endsWith('.mdx') ? mdxProcessor : markdownProcessor;
		tree = processor.parse({value: content, path: page.file});
	} catch (error) {
		throw new Error(`${page.relativePath}: ${String(error)}`, {cause: error});
	}
	const headers: Array<DocsRouteHeader> = [];
	const pending: Array<Nodes> = [tree];
	while (pending.length > 0) {
		const node = pending.pop();
		assert(node !== undefined);
		if ((node.type === 'mdxJsxFlowElement' || node.type === 'mdxJsxTextElement') && node.name === 'RouteHeader') {
			headers.push(readHeader(node, page.relativePath));
		}
		if ('children' in node) {
			for (let index = node.children.length - 1; index >= 0; index -= 1) {
				pending.push(node.children[index]);
			}
		}
	}
	return headers;
}

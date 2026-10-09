// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFile, writeFile} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';
import type {
	OpenAPIOperation as Operation,
	OpenAPISchema as SchemaNode,
	OpenAPIDocument as Spec,
} from '@fluxer/openapi/src/OpenAPITypes';
import {readRouteHeaders} from './DocsRouteHeaders.ts';
import {
	DOCS_ROOT,
	HTTP_METHODS,
	type MarkdownPage,
	readMarkdownPages,
	routeShape,
	slugifyHeading,
	splitTableRow,
} from './DocsSource.ts';

const REPO_ROOT = fileURLToPath(new URL('../../', import.meta.url));
const MAIN_SPEC = path.join(REPO_ROOT, 'fluxer_api/src/api/openapi/openapi.json');
const ADMIN_SPEC = path.join(REPO_ROOT, 'fluxer_admin/openapi-admin.json');
export interface Mismatch {
	readonly page: string;
	readonly operation: string;
	readonly kind: string;
	readonly detail: string;
}

export interface SchemaCounters {
	bodyTables: number;
	bodiesByReference: number;
	unionBodies: number;
	queryTables: number;
	responseTables: number;
	responseObjects: number;
	responseFieldsFound: number;
	nestedObjects: number;
	unlinkedObjects: number;
	typesCompared: number;
	optionalityCompared: number;
}

export interface SchemaVerification {
	readonly mismatches: ReadonlyArray<Mismatch>;
	readonly optionalityAdvisories: ReadonlyArray<string>;
	readonly counters: SchemaCounters;
}

interface DocField {
	readonly type: string | null;
	readonly typed: boolean;
	readonly optional: boolean;
	readonly anchors: ReadonlyArray<string>;
	readonly notes: ReadonlyArray<string>;
	readonly undeclared: boolean;
}

interface Heading {
	readonly level: number;
	readonly text: string;
	readonly line: number;
	readonly anchors: Array<string>;
	isObject: boolean;
}

interface Table {
	readonly line: number;
	readonly anchors: ReadonlyArray<string>;
	readonly stack: ReadonlyArray<Heading>;
	readonly header: ReadonlyArray<string>;
	readonly rows: ReadonlyArray<ReadonlyArray<string>>;
}

interface ProseLine {
	readonly line: number;
	readonly stack: ReadonlyArray<Heading>;
	readonly text: string;
}

interface ParsedPage {
	readonly page: MarkdownPage;
	readonly slug: string;
	readonly headings: ReadonlyArray<Heading>;
	readonly tables: ReadonlyArray<Table>;
	readonly prose: ReadonlyArray<ProseLine>;
}

interface ObjectUsage {
	readonly page: string;
	readonly operation: string;
	readonly anchors: string;
	readonly own: ReadonlyMap<string, DocField>;
	readonly properties: Set<string>;
	open: boolean;
}

interface Inheritance {
	readonly anchor: string;
	readonly except: ReadonlySet<string>;
}

interface ObjectBlock {
	readonly own: Map<string, DocField>;
	readonly tables: Array<ReadonlyMap<string, DocField>>;
	readonly inherits: Array<Inheritance>;
	readonly links: Array<Inheritance>;
	readonly undeclared: Set<string>;
	readonly declaredOnly: Set<string>;
}

function stripVersion(routePath: string): string {
	if (routePath.startsWith('/v1/')) {
		return routePath.slice(3);
	}
	return routePath;
}

function resolveSchemaPointer(spec: Spec, reference: string): SchemaNode | boolean {
	if (!reference.startsWith('#')) throw new Error(`Unsupported schema reference: ${reference}`);
	const pointer = decodeURIComponent(reference.slice(1));
	const prefix = '/components/schemas/';
	if (!pointer.startsWith(prefix)) throw new Error(`Unsupported schema reference: ${reference}`);
	let target: unknown = spec.components.schemas;
	for (const token of pointer.slice(prefix.length).split('/')) {
		if (/~(?:[^01]|$)/u.test(token)) throw new Error(`Invalid schema reference escape: ${reference}`);
		const key = token.replace(/~1/gu, '/').replace(/~0/gu, '~');
		if (
			target === null ||
			typeof target !== 'object' ||
			(Array.isArray(target) && !/^(0|[1-9][0-9]*)$/u.test(key)) ||
			!Object.hasOwn(target, key)
		) {
			throw new Error(`Missing schema reference: ${reference}`);
		}
		target = (target as Record<string, unknown>)[key];
	}
	if (typeof target === 'boolean') return target;
	if (target === null || typeof target !== 'object' || Array.isArray(target)) {
		throw new Error(`Reference does not identify a schema: ${reference}`);
	}
	return target as SchemaNode;
}

function resolveRef(spec: Spec, node: SchemaNode | boolean | undefined, depth = 0): SchemaNode | undefined {
	if (node == null || node === false) {
		return undefined;
	}
	if (node === true) {
		return {};
	}
	if (depth > 64) {
		throw new Error('OpenAPI reference chain exceeds the supported depth');
	}
	if (node.$ref != null) {
		const target = resolveSchemaPointer(spec, node.$ref);
		const resolved = resolveRef(spec, target, depth + 1);
		const {$ref, ...siblings} = node;
		if (resolved == null || Object.keys(siblings).length === 0) {
			return resolved;
		}
		if (Object.keys(resolved).length === 0) {
			return siblings;
		}
		return {...resolved, allOf: [...(resolved.allOf ?? []), siblings]};
	}
	return node;
}

function asSchema(node: unknown): SchemaNode | boolean | undefined {
	if (typeof node === 'boolean') return node;
	if (node == null || typeof node !== 'object' || Array.isArray(node)) return undefined;
	return node as SchemaNode;
}

function isSchemaNode(node: SchemaNode | boolean): node is SchemaNode {
	return typeof node !== 'boolean';
}

function branchesOf(resolved: SchemaNode): Array<SchemaNode | boolean> {
	return [...(resolved.allOf ?? []), ...(resolved.oneOf ?? []), ...(resolved.anyOf ?? [])];
}

function isUnion(resolved: SchemaNode | undefined): boolean {
	return resolved != null && ((resolved.oneOf ?? []).length > 0 || (resolved.anyOf ?? []).length > 0);
}

function collectRequired(spec: Spec, node: SchemaNode | boolean | undefined, depth = 0): Set<string> {
	const out = new Set<string>();
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null) {
		return out;
	}
	for (const name of resolved.required ?? []) {
		out.add(name);
	}
	for (const branch of resolved.allOf ?? []) {
		for (const name of collectRequired(spec, branch, depth + 1)) {
			out.add(name);
		}
	}
	for (const union of [resolved.oneOf ?? [], resolved.anyOf ?? []]) {
		const branchRequirements = union.map((branch) => collectRequired(spec, branch, depth + 1));
		for (const name of branchRequirements[0] ?? []) {
			if (branchRequirements.every((required) => required.has(name))) {
				out.add(name);
			}
		}
	}
	return out;
}

function collectTypes(spec: Spec, node: SchemaNode | boolean | undefined, depth = 0): Set<string> {
	const resolved = resolveRef(spec, node, depth);
	const out = new Set<string>();
	if (resolved == null) {
		return out;
	}
	if (resolved.format === 'snowflake') {
		out.add('string');
		return out;
	}
	for (const type of Array.isArray(resolved.type) ? resolved.type : [resolved.type]) {
		if (type != null && type !== 'null') {
			out.add(type);
		}
	}
	for (const branch of branchesOf(resolved)) {
		for (const type of collectTypes(spec, branch, depth + 1)) {
			out.add(type);
		}
	}
	return out;
}

function isDeprecatedProperty(property: unknown): boolean {
	if (property == null || typeof property !== 'object') {
		return false;
	}
	const node = property as {deprecated?: unknown; description?: unknown};
	if (node.deprecated === true) {
		return true;
	}
	return typeof node.description === 'string' && node.description.trimStart().toLowerCase().startsWith('deprecated');
}

function collectPropertySchemas(
	spec: Spec,
	node: SchemaNode | boolean | undefined,
	depth = 0,
	out = new Map<string, Array<SchemaNode | boolean>>(),
	includeDeprecated = false,
): Map<string, Array<SchemaNode | boolean>> {
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null) {
		return out;
	}
	for (const [key, property] of Object.entries(resolved.properties ?? {})) {
		if (!includeDeprecated && isDeprecatedProperty(property)) {
			continue;
		}
		const schema = asSchema(property);
		if (schema === undefined) {
			continue;
		}
		const list = out.get(key) ?? [];
		list.push(schema);
		out.set(key, list);
	}
	for (const branch of branchesOf(resolved)) {
		collectPropertySchemas(spec, branch, depth + 1, out, includeDeprecated);
	}
	return out;
}

function objectTarget(spec: Spec, node: SchemaNode | boolean | undefined, depth = 0): SchemaNode | undefined {
	if (depth > 32) {
		return undefined;
	}
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null) {
		return undefined;
	}
	if (resolved.properties != null) {
		return resolved;
	}
	const items = asSchema(resolved.items);
	if (items !== undefined && collectTypes(spec, resolved).has('array')) {
		return objectTarget(spec, items, depth + 1);
	}
	const values = asSchema(resolved.additionalProperties);
	if (values !== undefined && isSchemaNode(values)) {
		return objectTarget(spec, values, depth + 1);
	}
	const branches = branchesOf(resolved).filter((branch) => collectTypes(spec, branch).size > 0);
	if (branches.length === 1) {
		return objectTarget(spec, branches[0], depth + 1);
	}
	if (collectPropertySchemas(spec, resolved).size > 0) {
		return resolved;
	}
	const targets = new Map<string, SchemaNode>();
	for (const branch of branches) {
		const target = objectTarget(spec, branch, depth + 1);
		if (target != null) targets.set(JSON.stringify(target), target);
	}
	const [first, ...rest] = targets.values();
	return rest.length === 0 ? first : {anyOf: [first, ...rest]};
}

function acceptsUndeclaredProperties(spec: Spec, node: SchemaNode | boolean | undefined, depth = 0): boolean {
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null) return false;
	const additional = resolved.additionalProperties;
	if (resolved.properties != null && additional != null && additional !== false) return true;
	return branchesOf(resolved).some((branch) => acceptsUndeclaredProperties(spec, branch, depth + 1));
}

const DOC_TYPE_TO_JSON = new Map([
	['snowflake', 'string'],
	['string', 'string'],
	['integer', 'integer'],
	['boolean', 'boolean'],
	['iso8601 timestamp', 'string'],
	['base64 string', 'string'],
	['float', 'number'],
	['number', 'number'],
]);

function normaliseDocType(cell: string): string | null {
	const text = cell
		.replace(/<sup>.*?<\/sup>/gu, '')
		.replace(/\[([^\]]*)\]\([^)]*\)/gu, '$1')
		.replace(/`/gu, '')
		.trim()
		.replace(/^\?/u, '')
		.toLowerCase();
	if (text.startsWith('array')) {
		return 'array';
	}
	if (text.endsWith(' object') || text.includes('object')) {
		return 'object';
	}
	return DOC_TYPE_TO_JSON.get(text) ?? null;
}

function typeAgrees(docType: string, specTypes: ReadonlySet<string>): boolean {
	return specTypes.has(docType) || (docType === 'integer' && specTypes.has('number'));
}

function fieldNameText(cell: string): string {
	return cell
		.replace(/<sup>.*?<\/sup>/gu, '')
		.replace(/\*\*/gu, '')
		.replace(/`/gu, '')
		.replace(/\\/gu, '')
		.trim();
}

function cleanFieldName(cell: string): string | null {
	const name = fieldNameText(cell).replace(/\?$/u, '');
	if (name.length === 0) {
		return null;
	}
	if (!/^[a-z_][a-z0-9_.]*$/iu.test(name)) {
		return null;
	}
	return name;
}

function operationIndex(spec: Spec): Map<string, Operation> {
	const index = new Map<string, Operation>();
	for (const [routePath, item] of Object.entries(spec.paths)) {
		for (const [method, operation] of Object.entries(item)) {
			const upper = method.toUpperCase();
			if (!HTTP_METHODS.has(upper)) continue;
			const key = routeShape(upper, stripVersion(routePath));
			if (index.has(key)) throw new Error(`Duplicate OpenAPI operation: ${key}`);
			index.set(key, operation);
		}
	}
	return index;
}

function pageSlug(relativePath: string): string {
	return relativePath
		.replace(/\.(mdx|md)$/u, '')
		.replace(/\/index$/u, '')
		.replace(/^index$/u, '');
}

function documentReferences(page: string, line: string): Set<string> {
	const references = new Set<string>();
	for (const link of line.matchAll(/\]\(([^)\s]*)#([a-z0-9-]+)\)/gu)) {
		if (/^[a-z][a-z0-9+.-]*:/iu.test(link[1])) {
			continue;
		}
		const target =
			link[1].length === 0
				? page
				: link[1].startsWith('/')
					? link[1].slice(1)
					: path.posix.join(path.posix.dirname(page), link[1]);
		const slug = target
			.replace(/\.(mdx|md)$/u, '')
			.replace(/\/$/u, '')
			.replace(/\/index$/u, '');
		references.add(`${slug}#${link[2]}`);
	}
	return references;
}

function parsePage(page: MarkdownPage): ParsedPage {
	const slug = pageSlug(page.relativePath);
	const headings: Array<Heading> = [];
	const tables: Array<Table> = [];
	const prose: Array<ProseLine> = [];
	const stack: Array<Heading> = [];
	const pendingAnchors: Array<string> = [];
	const tableAnchors: Array<string> = [];
	let fence: string | null = null;
	let table: {line: number; anchors: Array<string>; stack: ReadonlyArray<Heading>; rows: Array<Array<string>>} | null =
		null;
	const closeTable = () => {
		if (table == null) return;
		const [header, , ...rows] = table.rows;
		tables.push({
			line: table.line,
			anchors: table.anchors,
			stack: table.stack,
			header: (header ?? []).map(fieldNameText),
			rows,
		});
		table = null;
	};
	for (let i = 0; i < page.lines.length; i += 1) {
		const line = page.lines[i];
		const fenceMatch = line.match(/^\s*(```|~~~)/u);
		if (fence != null) {
			if (fenceMatch != null && fenceMatch[1] === fence) fence = null;
			continue;
		}
		if (fenceMatch != null) {
			closeTable();
			fence = fenceMatch[1];
			continue;
		}
		if (line.startsWith('|')) {
			if (table == null) table = {line: i, anchors: tableAnchors.splice(0), stack: [...stack], rows: []};
			table.rows.push(splitTableRow(line));
			continue;
		}
		closeTable();
		for (const explicit of line.matchAll(/<a\s+id=["']([^"']+)["']/gu)) {
			const nextContent = page.lines.slice(i + 1).find((nextLine) => nextLine.trim().length > 0);
			if (nextContent != null && /^#{2,6}\s/u.test(nextContent)) {
				pendingAnchors.push(explicit[1]);
			} else if (nextContent?.startsWith('|')) {
				tableAnchors.push(explicit[1]);
			} else if (stack.length > 0) {
				stack[stack.length - 1].anchors.push(explicit[1]);
			}
		}
		const heading = line.match(/^(#{2,6})\s+(.+?)\s*$/u);
		if (heading != null) {
			const level = heading[1].length;
			const entry: Heading = {
				level,
				text: heading[2],
				line: i,
				anchors: [...new Set([slugifyHeading(heading[2]), ...pendingAnchors])],
				isObject: /\bobject\b/iu.test(heading[2]),
			};
			pendingAnchors.length = 0;
			while (stack.length > 0 && stack[stack.length - 1].level >= level) stack.pop();
			stack.push(entry);
			headings.push(entry);
			continue;
		}
		if (line.trim().length > 0) {
			prose.push({line: i, stack: [...stack], text: line});
		}
	}
	closeTable();
	return {page, slug, headings, tables, prose};
}

function tableFields(page: ParsedPage, table: Table): Map<string, DocField> {
	const out = new Map<string, DocField>();
	const nameColumn = table.header.indexOf('Field');
	if (nameColumn === -1) {
		return out;
	}
	const typeColumn = table.header.indexOf('Type');
	for (const cells of table.rows) {
		const nameCell = cells[nameColumn];
		if (nameCell === undefined) continue;
		const name = cleanFieldName(nameCell);
		if (name == null) continue;
		const typeCell = typeColumn === -1 ? undefined : cells[typeColumn];
		out.set(name, {
			type: typeCell === undefined ? null : normaliseDocType(typeCell),
			typed: typeCell !== undefined,
			optional: fieldNameText(nameCell).endsWith('?'),
			anchors: typeCell === undefined ? [] : [...documentReferences(page.slug, typeCell)],
			notes: [...nameCell.matchAll(/<sup>(\d+)<\/sup>/gu)].map((match) => match[1]),
			undeclared: false,
		});
	}
	return out;
}

function keyedValueAnchors(page: ParsedPage, table: Table): Array<string> {
	const nameColumn = table.header.indexOf('Field');
	const typeColumn = table.header.indexOf('Type');
	if (nameColumn === -1 || typeColumn === -1) {
		return [];
	}
	return table.rows
		.filter((cells) => /^(?:&#123;|\{)/u.test(fieldNameText(cells[nameColumn] ?? '')))
		.flatMap((cells) => [...documentReferences(page.slug, cells[typeColumn] ?? '')]);
}

const BODY_HEADING = /^JSON body$|\bresponse body$/iu;

const RESPONSE_BODY_HEADING = /\bresponse body$/iu;

const STRUCTURE_HEADING = /(?:^|\s)structure$|^(?:additional|value) fields?$/iu;

const INHERITANCE = /\b(?:extends|every (?:field|member) of)\b/iu;

function inheritedAnchors(slug: string, text: string, everyLink = false): Array<Inheritance> {
	const out: Array<Inheritance> = [];
	for (const sentence of text.split(/(?<=\.)\s+(?=[A-Z])/u)) {
		if (!everyLink && !INHERITANCE.test(sentence)) continue;
		const exceptClause = sentence.match(/\b(?:except|without)\b(.*)$/u)?.[1] ?? '';
		const except = new Set([...exceptClause.matchAll(/`([a-z_][a-z0-9_]*)`/giu)].map((match) => match[1]));
		for (const anchor of documentReferences(slug, sentence)) {
			out.push({anchor, except});
		}
	}
	return out;
}

const DECLARED_ONLY = /\bschema also declares\b/iu;

function declaredOnlySentences(text: string): Array<string> {
	return text.split(/(?<=\.)\s+(?=[A-Z])/u).filter((sentence) => DECLARED_ONLY.test(sentence));
}

function declaredOnlyNames(text: string): Array<string> {
	return declaredOnlySentences(text).flatMap((sentence) =>
		[...sentence.matchAll(/`([a-z_][a-z0-9_]*)`/giu)].map((match) => match[1]),
	);
}

const UNDECLARED_NOTE = /^<sup>(\d+)<\/sup>.*\bschema does not declare\b/u;

function markUndeclared(fields: ReadonlyMap<string, DocField>, notes: ReadonlySet<string>): Map<string, DocField> {
	const out = new Map<string, DocField>();
	for (const [name, field] of fields) {
		out.set(name, field.notes.some((note) => notes.has(note)) ? {...field, undeclared: true} : field);
	}
	return out;
}

function mergeGroups(groups: ReadonlyArray<ReadonlyMap<string, DocField>>): Map<string, DocField> {
	const out = new Map<string, DocField>();
	for (const group of groups) {
		for (const [field, docField] of group) {
			if (!out.has(field)) out.set(field, docField);
		}
	}
	return out;
}

function innermostObject(stack: ReadonlyArray<Heading>): Heading | undefined {
	for (let index = stack.length - 1; index >= 0; index -= 1) {
		if (stack[index].isObject) return stack[index];
	}
	return undefined;
}

class Verifier {
	readonly mismatches: Array<Mismatch> = [];
	readonly optionalityAdvisories: Array<string> = [];
	readonly counters: SchemaCounters = {
		bodyTables: 0,
		bodiesByReference: 0,
		unionBodies: 0,
		queryTables: 0,
		responseTables: 0,
		responseObjects: 0,
		responseFieldsFound: 0,
		nestedObjects: 0,
		unlinkedObjects: 0,
		typesCompared: 0,
		optionalityCompared: 0,
	};
	private readonly objects = new Map<string, ObjectBlock>();
	private readonly compared = new Set<string>();
	private readonly unlinked = new Set<string>();
	private readonly usages = new Map<string, ObjectUsage>();
	private readonly routeSections = new Map<ParsedPage, ReadonlySet<Heading>>();
	private readonly routeBodies = new Map<string, ReadonlyArray<ReadonlyMap<string, DocField>>>();
	private readonly identities = new Map<string, number>();
	private readonly parsed: ReadonlyArray<ParsedPage>;

	constructor(
		pages: ReadonlyArray<MarkdownPage>,
		private readonly mainSpec: Spec,
		private readonly adminSpec: Spec,
	) {
		this.parsed = pages.map(parsePage);
		const typeTargets = new Set<string>();
		for (const page of this.parsed) {
			for (const table of page.tables) {
				for (const field of tableFields(page, table).values()) {
					for (const anchor of field.anchors) typeTargets.add(anchor);
				}
				if (table.header[0] !== 'Status') continue;
				for (const row of table.rows) {
					for (const anchor of documentReferences(page.slug, row[1] ?? '')) typeTargets.add(anchor);
				}
			}
			for (const line of page.prose) {
				if (!line.stack.some((heading) => heading.level === 3 && BODY_HEADING.test(heading.text.trim()))) continue;
				for (const anchor of documentReferences(page.slug, line.text)) typeTargets.add(anchor);
			}
		}
		for (const page of this.parsed) {
			const routeLines = readRouteHeaders(page.page).map((header) => header.line);
			const sections = page.headings.filter((heading) => heading.level === 2);
			const routeSections = new Set(
				sections.filter((heading, index) => {
					const end = sections[index + 1]?.line ?? page.page.lines.length;
					return routeLines.some((line) => line > heading.line && line <= end);
				}),
			);
			this.routeSections.set(page, routeSections);
			for (const heading of page.headings) {
				if (routeSections.has(heading)) {
					heading.isObject = false;
					continue;
				}
				if (heading.anchors.some((anchor) => typeTargets.has(`${page.slug}#${anchor}`))) heading.isObject = true;
			}
		}
		for (const page of this.parsed) {
			this.indexObjects(page);
			for (const section of this.routeSections.get(page) ?? []) {
				const groups = page.tables
					.filter(
						(table) =>
							table.stack.includes(section) &&
							table.stack.some((heading) => heading.level === 3 && heading.text.trim() === 'JSON body') &&
							(innermostObject(table.stack)?.level ?? 0) <= 2,
					)
					.map((table) => tableFields(page, table))
					.filter((fields) => fields.size > 0);
				for (const anchor of section.anchors) this.routeBodies.set(`${page.slug}#${anchor}`, groups);
			}
		}
	}

	private schemaIdentity(node: SchemaNode): number {
		const shape = JSON.stringify(node);
		const known = this.identities.get(shape);
		if (known !== undefined) return known;
		const identity = this.identities.size;
		this.identities.set(shape, identity);
		return identity;
	}

	private indexObjects(page: ParsedPage): void {
		const blocks = new Map<Heading, ObjectBlock>();
		for (const heading of page.headings) {
			if (!heading.isObject) continue;
			const block: ObjectBlock = {
				own: new Map(),
				tables: [],
				inherits: [],
				links: [],
				undeclared: new Set(),
				declaredOnly: new Set(),
			};
			blocks.set(heading, block);
			for (const anchor of heading.anchors) {
				this.objects.set(`${page.slug}#${anchor}`, block);
			}
		}
		for (const table of page.tables) {
			if (table.anchors.length > 0) {
				const fields = tableFields(page, table);
				const block: ObjectBlock = {
					own: new Map(fields),
					tables: [fields],
					inherits: [],
					links: [],
					undeclared: new Set(),
					declaredOnly: new Set(),
				};
				for (const anchor of table.anchors) this.objects.set(`${page.slug}#${anchor}`, block);
			}
			const owner = innermostObject(table.stack);
			if (owner == null) continue;
			const inner = table.stack[table.stack.length - 1];
			if (inner !== owner && !STRUCTURE_HEADING.test(inner.text)) continue;
			const block = blocks.get(owner);
			for (const anchor of keyedValueAnchors(page, table)) block?.links.push({anchor, except: new Set()});
			const fields = tableFields(page, table);
			if (fields.size === 0) continue;
			block?.tables.push(fields);
			for (const [name, field] of fields) {
				if (!block?.own.has(name)) block?.own.set(name, field);
			}
		}
		for (const line of page.prose) {
			const owner = innermostObject(line.stack);
			const block = owner == null ? undefined : blocks.get(owner);
			if (block == null) continue;
			block.inherits.push(...inheritedAnchors(page.slug, line.text));
			const note = line.text.match(UNDECLARED_NOTE);
			if (note != null) block.undeclared.add(note[1]);
			for (const name of declaredOnlyNames(line.text)) block.declaredOnly.add(name);
			for (const anchor of documentReferences(page.slug, line.text)) {
				block.links.push({anchor, except: new Set()});
			}
		}
		for (const block of blocks.values()) {
			if (block.undeclared.size === 0) continue;
			for (const [name, field] of markUndeclared(block.own, block.undeclared)) block.own.set(name, field);
			block.tables.splice(
				0,
				block.tables.length,
				...block.tables.map((table) => markUndeclared(table, block.undeclared)),
			);
		}
	}

	private parents(block: ObjectBlock): Array<Inheritance> {
		const parents = block.own.size > 0 ? block.inherits : block.links;
		return parents.filter((parent) => this.isObjectAnchor(parent.anchor));
	}

	private isObjectAnchor(anchor: string): boolean {
		return this.objects.has(anchor);
	}

	private objectFields(anchors: Iterable<string>, seen = new Set<string>()): Map<string, DocField> {
		const out = new Map<string, DocField>();
		for (const anchor of anchors) {
			if (seen.has(anchor)) continue;
			seen.add(anchor);
			const block = this.objects.get(anchor);
			if (block == null) continue;
			for (const [name, field] of block.own) {
				if (!out.has(name)) out.set(name, field);
			}
			for (const parent of this.parents(block)) {
				for (const [name, field] of this.objectFields([parent.anchor], seen)) {
					if (!out.has(name) && !parent.except.has(name)) out.set(name, field);
				}
			}
		}
		return out;
	}

	private coverageFields(anchors: Iterable<string>, extra: Iterable<string> = []): Set<string> {
		const out = new Set<string>([...this.objectFields(anchors).keys(), ...extra]);
		for (const anchor of anchors) {
			for (const name of this.objects.get(anchor)?.declaredOnly ?? []) out.add(name);
		}
		return out;
	}

	private ownFields(anchors: Iterable<string>): Map<string, DocField> {
		const out = new Map<string, DocField>();
		for (const anchor of anchors) {
			const block = this.objects.get(anchor);
			if (block == null) continue;
			const own = block.own.size > 0 ? block.own : this.objectFields([anchor]);
			for (const [name, field] of own) {
				if (!out.has(name)) out.set(name, field);
			}
		}
		return out;
	}

	private push(page: string, operation: string, kind: string, detail: string): void {
		this.mismatches.push({page, operation, kind, detail});
	}

	private compareFields(
		spec: Spec,
		context: {page: string; operation: string; kind: string; label: string},
		documented: Iterable<readonly [string, DocField]>,
		coverage: ReadonlySet<string>,
		schema: SchemaNode | boolean | undefined,
		usages?: ReadonlyArray<ObjectUsage>,
	): number {
		const properties = collectPropertySchemas(spec, schema);
		const known = collectPropertySchemas(spec, schema, 0, new Map(), true);
		const prefix = context.label.length > 0 ? `${context.label}.` : '';
		const open = acceptsUndeclaredProperties(spec, schema);
		for (const usage of usages ?? []) {
			for (const field of known.keys()) usage.properties.add(field);
			usage.open ||= open;
		}
		for (const [field, docField] of documented) {
			const schemas = known.get(field);
			if (schemas == null) {
				if (usages == null && !open && !docField.undeclared) {
					this.push(context.page, context.operation, `${context.kind}-extra`, `${prefix}${field}`);
				}
				continue;
			}
			if (usages == null && docField.undeclared) {
				this.push(context.page, context.operation, `${context.kind}-stale-undeclared`, `${prefix}${field}`);
			}
			const specTypes = new Set<string>();
			for (const node of schemas) {
				for (const type of collectTypes(spec, node)) specTypes.add(type);
			}
			if (docField.type != null && specTypes.size > 0) {
				this.counters.typesCompared += 1;
				if (!typeAgrees(docField.type, specTypes)) {
					this.push(
						context.page,
						context.operation,
						'type-mismatch',
						`${prefix}${field}: documented ${docField.type}, schema ${[...specTypes].sort().join(' | ')}`,
					);
				}
			}
			const anchors = docField.anchors.filter((anchor) => this.isObjectAnchor(anchor));
			const target: SchemaNode | boolean = schemas.length === 1 ? schemas[0] : {anyOf: schemas.filter(isSchemaNode)};
			const nested = objectTarget(spec, target);
			if (anchors.length === 0 && nested != null) {
				this.unlinked.add(`${context.page} ${prefix}${field}`);
			}
			if (anchors.length > 0) {
				if (nested != null) {
					this.compareObject(spec, context.page, context.operation, anchors, target, `${prefix}${field}`);
				} else if (!docField.undeclared && this.ownFields(anchors).size > 0) {
					this.push(
						context.page,
						context.operation,
						'object-unresolved',
						`${prefix}${field}: the schema declares no members for (${anchors.join(', ')})`,
					);
				}
			}
		}
		let found = 0;
		for (const field of properties.keys()) {
			if (coverage.has(field)) {
				found += 1;
				continue;
			}
			this.push(context.page, context.operation, `${context.kind}-missing`, `${prefix}${field}`);
		}
		return found;
	}

	private compareObject(
		spec: Spec,
		page: string,
		operation: string,
		anchors: ReadonlyArray<string>,
		node: SchemaNode | boolean | undefined,
		label: string,
		extraCoverage: Iterable<string> = [],
	): number {
		const target = objectTarget(spec, node);
		if (target == null) {
			return 0;
		}
		const specName = spec === this.adminSpec ? 'admin' : 'main';
		const key = `${specName}|${[...anchors].sort().join(',')}|${this.schemaIdentity(target).toString()}`;
		if (this.compared.has(key)) {
			return 0;
		}
		this.compared.add(key);
		this.counters.nestedObjects += 1;
		const usages = anchors.map((anchor) => {
			let usage = this.usages.get(anchor);
			if (usage == null) {
				usage = {page, operation, anchors: anchor, own: this.ownFields([anchor]), properties: new Set(), open: false};
				this.usages.set(anchor, usage);
			}
			return usage;
		});
		const objectLabel = `${label.length > 0 ? `${label} ` : ''}(${anchors.join(', ')})`;
		this.compareObjectVariants(spec, page, operation, anchors, target, objectLabel);
		return this.compareFields(
			spec,
			{page, operation, kind: 'object', label: objectLabel},
			anchors.flatMap((anchor) => [...this.ownFields([anchor])]),
			this.coverageFields(anchors, extraCoverage),
			target,
			usages,
		);
	}

	private compareObjectVariants(
		spec: Spec,
		page: string,
		operation: string,
		anchors: ReadonlyArray<string>,
		target: SchemaNode,
		label: string,
	): void {
		const branches = [...(target.oneOf ?? []), ...(target.anyOf ?? [])];
		if (anchors.length < 2 || branches.length < 2) return;
		const branchProperties = branches.map((branch) => new Set(collectPropertySchemas(spec, branch).keys()));
		const documentedFor = branches.map(() => new Set<string>());
		const documented = new Set<string>();
		const matchedBranches = new Set<number>();
		for (const anchor of anchors) {
			const fields = [...this.ownFields([anchor]).keys()].filter((field) =>
				branchProperties.some((properties) => properties.has(field)),
			);
			const candidates = branchProperties
				.map((properties, index) => (fields.every((field) => properties.has(field)) ? index : -1))
				.filter((index) => index !== -1);
			if (candidates.length === 0) return;
			const coverage = this.coverageFields([anchor]);
			for (const field of coverage) documented.add(field);
			for (const index of candidates) {
				for (const field of coverage) documentedFor[index].add(field);
				if (candidates.length < branches.length) matchedBranches.add(index);
			}
		}
		for (const index of matchedBranches) {
			for (const field of branchProperties[index]) {
				if (documentedFor[index].has(field) || !documented.has(field)) continue;
				this.push(page, operation, 'object-missing', `${label}.${field} (variant ${(index + 1).toString()})`);
			}
		}
	}

	private objectTables(anchors: Iterable<string>, seen = new Set<string>()): Array<ReadonlyMap<string, DocField>> {
		const out: Array<ReadonlyMap<string, DocField>> = [];
		for (const anchor of anchors) {
			if (seen.has(anchor)) continue;
			seen.add(anchor);
			const block = this.objects.get(anchor);
			if (block == null) continue;
			out.push(...block.tables);
			for (const parent of this.parents(block)) {
				for (const table of this.objectTables([parent.anchor], seen)) {
					out.push(new Map([...table].filter(([name]) => !parent.except.has(name))));
				}
			}
		}
		return out;
	}

	private compareBodyBranches(
		spec: Spec,
		page: string,
		operation: string,
		schema: SchemaNode | boolean,
		groups: ReadonlyArray<ReadonlyMap<string, DocField>>,
	): void {
		const resolved = resolveRef(spec, schema);
		const allBranches = isUnion(resolved) ? [...(resolved?.oneOf ?? []), ...(resolved?.anyOf ?? [])] : [schema];
		const branches = allBranches;
		if (branches.length > 1) this.counters.unionBodies += 1;
		const branchProperties = branches.map((branch) => new Set(collectPropertySchemas(spec, branch).keys()));
		const branchRequired = branches.map((branch) => collectRequired(spec, branch));
		const allProperties = new Set(branchProperties.flatMap((properties) => [...properties]));
		const commonRequired = collectRequired(spec, schema);
		const documentedFor = branches.map(() => new Set<string>());
		const matchedBranches = new Set<number>();
		for (const group of groups) {
			const fields = [...group.keys()].filter((field) => allProperties.has(field));
			const candidates = branchProperties
				.map((properties, index) => (fields.every((field) => properties.has(field)) ? index : -1))
				.filter((index) => index !== -1);
			const specific = candidates.length > 0 && candidates.length < branches.length;
			for (const index of candidates) {
				for (const field of fields) documentedFor[index].add(field);
				if (specific) matchedBranches.add(index);
			}
			for (const [field, docField] of group) {
				if (!allProperties.has(field) || !docField.typed) continue;
				const specRequired =
					candidates.length > 0
						? candidates.every((index) => branchRequired[index].has(field))
						: commonRequired.has(field);
				this.counters.optionalityCompared += 1;
				if (specRequired === !docField.optional) continue;
				const detail = specRequired
					? `${field}: documented optional, schema marks it required`
					: `${field}: documented required, schema marks it optional`;
				this.optionalityAdvisories.push(`${page}  ${operation}  ${detail}`);
				this.push(page, operation, 'optionality', detail);
			}
		}
		const documented = new Set(groups.flatMap((group) => [...group.keys()]));
		for (const index of matchedBranches) {
			for (const field of branchProperties[index]) {
				if (documentedFor[index].has(field) || !documented.has(field)) continue;
				this.push(page, operation, 'body-missing', `${field} (variant ${(index + 1).toString()})`);
			}
		}
	}

	private reportObjectExtras(): void {
		for (const usage of this.usages.values()) {
			for (const [field, docField] of usage.own) {
				if (docField.undeclared && usage.properties.has(field)) {
					this.push(usage.page, usage.operation, 'object-stale-undeclared', `(${usage.anchors}).${field}`);
				}
			}
			if (usage.open) continue;
			for (const [field, docField] of usage.own) {
				if (usage.properties.has(field) || docField.undeclared) continue;
				this.push(usage.page, usage.operation, 'object-extra', `(${usage.anchors}).${field}`);
			}
			for (const field of this.objects.get(usage.anchors)?.declaredOnly ?? []) {
				if (!usage.properties.has(field)) {
					this.push(usage.page, usage.operation, 'object-stale-declared', `(${usage.anchors}).${field}`);
				}
			}
		}
	}

	run(): SchemaVerification {
		const mainIndex = operationIndex(this.mainSpec);
		const adminIndex = operationIndex(this.adminSpec);
		for (const page of this.parsed) {
			if (page.page.relativePath.startsWith('media-proxy/')) continue;
			const isAdmin = page.page.relativePath.startsWith('admin-api/');
			this.verifyPage(page, isAdmin ? this.adminSpec : this.mainSpec, isAdmin ? adminIndex : mainIndex);
		}
		this.reportObjectExtras();
		this.counters.unlinkedObjects = this.unlinked.size;
		return {mismatches: this.mismatches, optionalityAdvisories: this.optionalityAdvisories, counters: this.counters};
	}

	private verifyPage(page: ParsedPage, spec: Spec, index: ReadonlyMap<string, Operation>): void {
		const relative = page.page.relativePath;
		const routeHeaders = readRouteHeaders(page.page);
		const sections = page.headings.filter((heading) => heading.level === 2);
		for (let s = 0; s < sections.length; s += 1) {
			const section = sections[s];
			const end = sections[s + 1]?.line ?? page.page.lines.length;
			const header = routeHeaders.find((candidate) => candidate.line > section.line && candidate.line <= end);
			if (header == null) continue;
			if (header.endLine > end) {
				throw new Error(`${relative}:${header.line}: RouteHeader crosses a section boundary`);
			}
			const key = routeShape(header.method, stripVersion(header.path));
			const operation = index.get(key);
			if (operation == null) continue;
			this.verifyRoute(page, spec, section, end, key, operation);
		}
	}

	private verifyRoute(
		page: ParsedPage,
		spec: Spec,
		section: Heading,
		end: number,
		key: string,
		operation: Operation,
	): void {
		const relative = page.page.relativePath;
		const inSection = (stack: ReadonlyArray<Heading>) => stack.includes(section);
		const subsection = (stack: ReadonlyArray<Heading>) => {
			const text = stack.find((heading) => heading.level === 3 && stack.includes(section))?.text.trim() ?? null;
			return text != null && RESPONSE_BODY_HEADING.test(text) ? 'Response body' : text;
		};
		const sectionTables = page.tables.filter((table) => inSection(table.stack));
		const sectionProse = page.prose.filter((line) => inSection(line.stack));

		const fieldsUnder = (name: string) => {
			const groups: Array<ReadonlyMap<string, DocField>> = [];
			const undeclared = new Set<string>();
			for (const line of sectionProse) {
				const note = subsection(line.stack) === name ? line.text.match(UNDECLARED_NOTE) : null;
				if (note != null) undeclared.add(note[1]);
			}
			for (const table of sectionTables) {
				if (subsection(table.stack) !== name) continue;
				if ((innermostObject(table.stack)?.level ?? 0) > 2) continue;
				const fields = markUndeclared(tableFields(page, table), undeclared);
				if (fields.size > 0) groups.push(fields);
			}
			const tables = groups.length;
			if (tables > 0) {
				for (const line of sectionProse) {
					if (subsection(line.stack) !== name) continue;
					for (const parent of inheritedAnchors(page.slug, line.text)) {
						for (const table of this.objectTables([parent.anchor])) {
							groups.push(new Map([...table].filter(([field]) => !parent.except.has(field))));
						}
					}
				}
			}
			return {fields: mergeGroups(groups), groups, tables};
		};
		const referencesUnder = (name: string) => {
			const out: Array<Inheritance> = [];
			for (const line of sectionProse) {
				if (subsection(line.stack) !== name) continue;
				for (const parent of inheritedAnchors(page.slug, line.text, true)) {
					if (this.isObjectAnchor(parent.anchor)) out.push(parent);
				}
			}
			return out;
		};

		const jsonSchema = objectTarget(spec, operation.requestBody?.content?.['application/json']?.schema);
		const body = fieldsUnder('JSON body');
		const bodyReferences = referencesUnder('JSON body');
		const hasBodySection = page.headings.some(
			(heading) =>
				heading.level === 3 && heading.text.trim() === 'JSON body' && heading.line > section.line && heading.line < end,
		);
		if (jsonSchema == null) {
			for (const [field, docField] of body.fields) {
				if (!docField.undeclared) this.push(relative, key, 'body-extra', field);
			}
		}
		if (jsonSchema != null && !hasBodySection && collectPropertySchemas(spec, jsonSchema).size > 0) {
			this.push(relative, key, 'body-missing', 'no JSON body section documents the request body');
		}
		if (jsonSchema != null && hasBodySection && collectPropertySchemas(spec, jsonSchema).size > 0) {
			let documented = body.fields;
			let groups = body.groups;
			if (body.tables > 0) {
				this.counters.bodyTables += 1;
			} else if (bodyReferences.length > 0) {
				this.counters.bodiesByReference += 1;
				groups = bodyReferences.flatMap((parent) =>
					this.objectTables([parent.anchor]).map(
						(table) => new Map([...table].filter(([field]) => !parent.except.has(field))),
					),
				);
				documented = mergeGroups(groups);
			} else {
				const sameAs = sectionProse
					.filter((line) => subsection(line.stack) === 'JSON body')
					.flatMap((line) => [...documentReferences(page.slug, line.text)])
					.flatMap((anchor) => this.routeBodies.get(anchor) ?? []);
				if (sameAs.length > 0) {
					this.counters.bodiesByReference += 1;
					groups = sameAs;
					documented = mergeGroups(groups);
				}
			}
			this.compareFields(
				spec,
				{page: relative, operation: key, kind: 'body', label: ''},
				documented,
				new Set(documented.keys()),
				jsonSchema,
			);
			this.compareBodyBranches(spec, relative, key, jsonSchema, groups);
		}

		const query = fieldsUnder('Query parameters');
		const actualQuery = new Set((operation.parameters ?? []).filter((p) => p.in === 'query').map((p) => p.name));
		if (query.tables > 0) this.counters.queryTables += 1;
		for (const [field, docField] of query.fields) {
			if (!actualQuery.has(field) && !docField.undeclared) this.push(relative, key, 'query-extra', field);
			if (actualQuery.has(field) && docField.undeclared) this.push(relative, key, 'query-stale-undeclared', field);
		}
		for (const field of actualQuery) {
			if (!query.fields.has(field)) this.push(relative, key, 'query-missing', field);
		}

		const success = Object.entries(operation.responses ?? {}).find(([status]) => status.startsWith('2'));
		const responseSchema = success?.[1].content?.['application/json']?.schema;
		const responseTarget = objectTarget(spec, responseSchema);
		const responseBody = fieldsUnder('Response body');
		if (responseTarget == null) {
			for (const [field, docField] of responseBody.fields) {
				if (!docField.undeclared) this.push(relative, key, 'response-extra', field);
			}
			return;
		}
		const responseDeclared = {anchors: new Set<string>(), names: new Set<string>()};
		for (const line of sectionProse) {
			const name = subsection(line.stack);
			if (name !== 'Response' && name !== 'Response body') continue;
			for (const sentence of declaredOnlySentences(line.text)) {
				for (const reference of documentReferences(page.slug, sentence)) {
					if (this.isObjectAnchor(reference)) responseDeclared.anchors.add(reference);
				}
			}
			for (const field of declaredOnlyNames(line.text)) responseDeclared.names.add(field);
		}
		const responseKnown = collectPropertySchemas(spec, responseSchema, 0, new Map(), true);
		if (!acceptsUndeclaredProperties(spec, responseSchema)) {
			for (const field of responseDeclared.names) {
				if (!responseKnown.has(field)) this.push(relative, key, 'response-stale-declared', field);
			}
		}
		if (responseBody.tables > 0) {
			this.counters.responseTables += 1;
			this.counters.responseFieldsFound += this.compareFields(
				spec,
				{page: relative, operation: key, kind: 'response', label: ''},
				responseBody.fields,
				new Set([...responseBody.fields.keys(), ...responseDeclared.names]),
				responseTarget,
			);
			return;
		}
		const statusAnchors = new Set<string>(responseDeclared.anchors);
		for (const parent of referencesUnder('Response body')) statusAnchors.add(parent.anchor);
		for (const table of sectionTables) {
			if (subsection(table.stack) !== 'Response' || table.header[0] !== 'Status') continue;
			for (const row of table.rows) {
				if (!row[0]?.trim().startsWith('2')) continue;
				for (const reference of documentReferences(page.slug, row[1] ?? '')) {
					if (this.isObjectAnchor(reference)) statusAnchors.add(reference);
				}
			}
		}
		if (statusAnchors.size > 0) {
			this.counters.responseObjects += 1;
			this.counters.responseFieldsFound += this.compareObject(
				spec,
				relative,
				key,
				[...statusAnchors],
				responseSchema,
				'',
				responseDeclared.names,
			);
			return;
		}
		for (const field of collectPropertySchemas(spec, responseTarget).keys()) {
			if (!responseDeclared.names.has(field)) this.push(relative, key, 'response-missing', field);
		}
	}
}

export function verifyPages(pages: ReadonlyArray<MarkdownPage>, mainSpec: Spec, adminSpec: Spec): SchemaVerification {
	return new Verifier(pages, mainSpec, adminSpec).run();
}

const KIND_LABELS = new Map([
	['extra', 'documented but not in the schema'],
	['missing', 'in the schema but undocumented'],
	['declared', 'named by a "schema also declares" sentence but not in the schema'],
	['undeclared', 'footnoted as not declared by the schema but in the schema'],
]);

async function main(): Promise<void> {
	const mainSpec: Spec = JSON.parse(await readFile(MAIN_SPEC, 'utf8'));
	const adminSpec: Spec = JSON.parse(await readFile(ADMIN_SPEC, 'utf8'));
	const pages = await readMarkdownPages(DOCS_ROOT);
	const {mismatches, optionalityAdvisories, counters} = verifyPages(pages, mainSpec, adminSpec);

	if (process.env.FLUXER_DOCS_SCHEMA_JSON != null) {
		const payload: Record<string, Array<{operation: string; kind: string; field: string}>> = {};
		for (const m of mismatches) {
			const list = payload[m.page] ?? [];
			list.push({operation: m.operation, kind: m.kind, field: m.detail});
			payload[m.page] = list;
		}
		await writeFile(process.env.FLUXER_DOCS_SCHEMA_JSON, JSON.stringify(payload, null, 1));
		console.log(`wrote ${process.env.FLUXER_DOCS_SCHEMA_JSON}`);
	}

	const byKind = new Map<string, number>();
	for (const m of mismatches) {
		byKind.set(m.kind, (byKind.get(m.kind) ?? 0) + 1);
	}

	console.log(`request body tables checked: ${counters.bodyTables.toString()}`);
	console.log(`request bodies documented by reference to an object or route: ${counters.bodiesByReference.toString()}`);
	console.log(`union request bodies checked: ${counters.unionBodies.toString()}`);
	console.log(`query parameter tables checked: ${counters.queryTables.toString()}`);
	console.log(`success responses checked against a response body table: ${counters.responseTables.toString()}`);
	console.log(`success responses checked against a linked object section: ${counters.responseObjects.toString()}`);
	console.log(`response fields found documented: ${counters.responseFieldsFound.toString()}`);
	console.log(`linked object sections compared with their schema: ${counters.nestedObjects.toString()}`);
	console.log(
		`fields whose schema has members but whose row links no object section: ${counters.unlinkedObjects.toString()}`,
	);
	console.log(`request and response field types compared: ${counters.typesCompared.toString()}`);
	console.log(`request body optionality compared: ${counters.optionalityCompared.toString()}`);
	console.log(`optionality advisories: ${optionalityAdvisories.length.toString()}`);
	for (const entry of optionalityAdvisories) {
		console.log(`    ${entry}`);
	}
	for (const [kind, count] of [...byKind.entries()].sort()) {
		console.log(`  ${kind}: ${count.toString()}`);
	}
	if (mismatches.length > 0) {
		console.log('');
		for (const m of mismatches.slice(0, 200)) {
			let label = KIND_LABELS.get(m.kind.split('-').at(-1) ?? '') ?? m.kind;
			if (m.kind === 'type-mismatch') label = 'type disagreement';
			if (m.kind === 'optionality') label = 'optionality disagreement';
			console.log(`${m.page}  ${m.operation}  ${label}: ${m.detail}`);
		}
		if (mismatches.length > 200) {
			console.log(`... and ${(mismatches.length - 200).toString()} more`);
		}
		console.error(`FAIL: ${mismatches.length.toString()} field mismatches`);
		process.exit(1);
	}
	console.log('OK: no field mismatches found in the checked tables and checked-in OpenAPI schemas');
}

if (process.argv[1] != null && import.meta.url === pathToFileURL(process.argv[1]).href) {
	await main();
}

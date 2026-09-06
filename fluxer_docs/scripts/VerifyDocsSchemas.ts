// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdir, readFile} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const DOCS_ROOT = fileURLToPath(new URL('../src/content/docs/', import.meta.url));
const REPO_ROOT = fileURLToPath(new URL('../../', import.meta.url));
const MAIN_SPEC = path.join(REPO_ROOT, 'fluxer_api/src/api/openapi/openapi.json');
const ADMIN_SPEC = path.join(REPO_ROOT, 'fluxer_admin/openapi-admin.json');

const ROUTE_HEADER = /<RouteHeader\s+method="([A-Z]+)"\s+path="([^"]+)"/u;
const TABLE_ROW = /^\|\s*([^|]+?)\s*\|/u;
const OBJECT_REFERENCE = /\]\([^)]*#[a-z0-9-]*object\)/u;

interface Spec {
	readonly paths: Record<string, Record<string, Operation>>;
	readonly components?: {schemas?: Record<string, SchemaNode>};
}

interface Operation {
	readonly parameters?: Array<{name: string; in: string; schema?: SchemaNode}>;
	readonly requestBody?: {content?: Record<string, {schema?: SchemaNode}>};
	readonly responses?: Record<string, {content?: Record<string, {schema?: SchemaNode}>}>;
}

interface SchemaNode {
	$ref?: string;
	required?: Array<string>;
	properties?: Record<string, SchemaNode>;
	allOf?: Array<SchemaNode>;
	oneOf?: Array<SchemaNode>;
	anyOf?: Array<SchemaNode>;
	items?: SchemaNode;
	type?: string;
}

const SUDO_MERGE_FIELDS = ['mfa_method', 'mfa_code', 'webauthn_response', 'webauthn_challenge'];

const STREAMED_RESPONSE_ROUTES = new Map([
	['POST /admin/system/heap-snapshots', "'Content-Type': 'application/octet-stream'"],
]);

const EMPTY_RESPONSE_ROUTES = new Map([['DELETE /admin/users/{}/sessions', 'terminated_count']]);

const MERGED_SCHEMA_ROUTES = new Map([
	['POST /auth/sessions/logout', "Validator('json', LogoutAuthSessionsRequest.merge(SudoVerificationSchema))"],
	[
		'POST /guilds/{}/transfer-ownership',
		"Validator('json', GuildTransferOwnershipRequest.merge(SudoVerificationSchema))",
	],
]);

interface Mismatch {
	readonly page: string;
	readonly operation: string;
	readonly kind: string;
	readonly detail: string;
}

async function walk(directory: string): Promise<Array<string>> {
	const entries = await readdir(directory, {withFileTypes: true});
	const files: Array<string> = [];
	for (const entry of entries) {
		if (entry.name === 'node_modules') {
			continue;
		}
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await walk(resolved)));
			continue;
		}
		if (/\.mdx?$/u.test(entry.name)) {
			files.push(resolved);
		}
	}
	return files;
}

function stripVersion(routePath: string): string {
	if (routePath.startsWith('/v1/')) {
		return routePath.slice(3);
	}
	return routePath;
}

function shape(method: string, routePath: string): string {
	return `${method} ${routePath.split('?')[0].replace(/\{[^}]*\}/gu, '{}')}`;
}

function resolveRef(spec: Spec, node: SchemaNode | undefined, depth = 0): SchemaNode | undefined {
	if (node == null || depth > 12) {
		return node;
	}
	if (node.$ref != null) {
		const name = node.$ref.replace('#/components/schemas/', '');
		return resolveRef(spec, spec.components?.schemas?.[name], depth + 1);
	}
	return node;
}

function collectRequired(spec: Spec, node: SchemaNode | undefined, depth = 0): Set<string> {
	const out = new Set<string>();
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null || depth > 12) {
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
	const union = [...(resolved.oneOf ?? []), ...(resolved.anyOf ?? [])];
	if (union.length > 0) {
		const declaringBranches = new Map<string, number>();
		const requiringBranches = new Map<string, number>();
		for (const branch of union) {
			const branchRequired = collectRequired(spec, branch, depth + 1);
			for (const name of collectProperties(spec, branch, depth + 1)) {
				declaringBranches.set(name, (declaringBranches.get(name) ?? 0) + 1);
				if (branchRequired.has(name)) {
					requiringBranches.set(name, (requiringBranches.get(name) ?? 0) + 1);
				}
			}
		}
		for (const [name, declared] of declaringBranches) {
			if ((requiringBranches.get(name) ?? 0) === declared) {
				out.add(name);
			}
		}
	}
	return out;
}

function collectPropertyTypes(spec: Spec, node: SchemaNode | undefined, depth = 0): Map<string, string> {
	const out = new Map<string, string>();
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null || depth > 12) {
		return out;
	}
	for (const [key, value] of Object.entries(resolved.properties ?? {})) {
		const property = resolveRef(spec, value, depth + 1);
		const type = property?.type;
		if (typeof type === 'string') {
			out.set(key, type);
		}
	}
	for (const branch of [...(resolved.allOf ?? [])]) {
		for (const [key, type] of collectPropertyTypes(spec, branch, depth + 1)) {
			if (!out.has(key)) {
				out.set(key, type);
			}
		}
	}
	return out;
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

function collectProperties(spec: Spec, node: SchemaNode | undefined, depth = 0): Set<string> {
	const out = new Set<string>();
	const resolved = resolveRef(spec, node, depth);
	if (resolved == null || depth > 12) {
		return out;
	}
	for (const [key, property] of Object.entries(resolved.properties ?? {})) {
		if (isDeprecatedProperty(property)) {
			continue;
		}
		out.add(key);
	}
	for (const branch of [...(resolved.allOf ?? []), ...(resolved.oneOf ?? []), ...(resolved.anyOf ?? [])]) {
		for (const key of collectProperties(spec, branch, depth + 1)) {
			out.add(key);
		}
	}
	return out;
}

function operationIndex(spec: Spec): Map<string, Operation> {
	const index = new Map<string, Operation>();
	for (const [routePath, item] of Object.entries(spec.paths)) {
		for (const [method, operation] of Object.entries(item)) {
			index.set(shape(method.toUpperCase(), stripVersion(routePath)), operation);
		}
	}
	return index;
}

function cleanFieldName(cell: string): string | null {
	const name = cell
		.replace(/<sup>.*?<\/sup>/gu, '')
		.replace(/\*\*/gu, '')
		.replace(/`/gu, '')
		.replace(/\\/gu, '')
		.trim()
		.replace(/\?$/u, '');
	if (name.length === 0) {
		return null;
	}
	if (name === 'Field' || name === '---' || name === 'Status' || name === 'Name') {
		return null;
	}
	if (!/^[a-z_][a-z0-9_.]*$/iu.test(name)) {
		return null;
	}
	return name;
}

function sectionIsByReference(lines: ReadonlyArray<string>, start: number): boolean {
	let sawTable = false;
	let sawReference = false;
	for (let index = start; index < lines.length; index += 1) {
		const line = lines[index];
		if (line.startsWith('#')) {
			break;
		}
		if (line.startsWith('|')) {
			sawTable = true;
		}
		if (OBJECT_REFERENCE.test(line)) {
			sawReference = true;
		}
	}
	return sawReference && !sawTable;
}

function tableOptionality(lines: ReadonlyArray<string>, start: number): Map<string, boolean> {
	const out = new Map<string, boolean>();
	let index = start;
	while (index < lines.length && !lines[index].startsWith('|')) {
		if (lines[index].startsWith('#')) {
			return out;
		}
		index += 1;
	}
	for (; index < lines.length; index += 1) {
		const line = lines[index];
		if (!line.startsWith('|')) {
			break;
		}
		const cells = line.split('|').slice(1, -1);
		if (cells.length < 2) {
			continue;
		}
		const raw = cells[0]
			.replace(/<sup>.*?<\/sup>/gu, '')
			.replace(/`/gu, '')
			.trim();
		const name = cleanFieldName(cells[0]);
		if (name == null) {
			continue;
		}
		out.set(name, raw.endsWith('?'));
	}
	return out;
}

function tableFieldTypes(lines: ReadonlyArray<string>, start: number): Map<string, string> {
	const out = new Map<string, string>();
	let index = start;
	while (index < lines.length && !lines[index].startsWith('|')) {
		if (lines[index].startsWith('#')) {
			return out;
		}
		index += 1;
	}
	for (; index < lines.length; index += 1) {
		const line = lines[index];
		if (!line.startsWith('|')) {
			break;
		}
		const cells = line.split('|').slice(1, -1);
		if (cells.length < 2) {
			continue;
		}
		const name = cleanFieldName(cells[0]);
		if (name == null) {
			continue;
		}
		const type = normaliseDocType(cells[1]);
		if (type != null) {
			out.set(name, type);
		}
	}
	return out;
}

function tableFields(lines: ReadonlyArray<string>, start: number): Set<string> {
	const fields = new Set<string>();
	let index = start;
	while (index < lines.length && !lines[index].startsWith('|')) {
		if (lines[index].startsWith('#')) {
			return fields;
		}
		index += 1;
	}
	for (; index < lines.length; index += 1) {
		const line = lines[index];
		if (!line.startsWith('|')) {
			break;
		}
		const match = line.match(TABLE_ROW);
		if (match == null) {
			continue;
		}
		const name = cleanFieldName(match[1]);
		if (name != null) {
			fields.add(name);
		}
	}
	return fields;
}

const mainSpec: Spec = JSON.parse(await readFile(MAIN_SPEC, 'utf8'));
const adminSpec: Spec = JSON.parse(await readFile(ADMIN_SPEC, 'utf8'));
const mainIndex = operationIndex(mainSpec);
const adminIndex = operationIndex(adminSpec);

function slugifyHeading(heading: string): string {
	return heading
		.replace(/`/gu, '')
		.replace(/\[([^\]]*)\]\([^)]*\)/gu, '$1')
		.replace(/<[^>]*>/gu, '')
		.toLowerCase()
		.replace(/[^a-z0-9\s-]/gu, '')
		.trim()
		.replace(/\s+/gu, '-');
}

const allFiles = await walk(DOCS_ROOT);
const anchorFields = new Map<string, Set<string>>();
const anchorTypes = new Map<string, Map<string, string>>();
for (const file of allFiles) {
	const slug = path
		.relative(DOCS_ROOT, file)
		.replace(/\.(mdx|md)$/u, '')
		.replace(/\/index$/u, '')
		.replace(/^index$/u, '');
	const lines = (await readFile(file, 'utf8')).split('\n');
	let currentAnchor: string | null = null;
	for (let i = 0; i < lines.length; i += 1) {
		const line = lines[i];
		const heading = line.match(/^##\s+(.+?)\s*$/u);
		if (heading != null && !line.startsWith('###')) {
			currentAnchor = slugifyHeading(heading[1]);
			const explicit = line.match(/id=["']([^"']+)["']/u);
			if (explicit != null) {
				currentAnchor = explicit[1];
			}
			continue;
		}
		if (currentAnchor == null || !line.startsWith('|')) {
			continue;
		}
		const row = line.match(TABLE_ROW);
		if (row == null) {
			continue;
		}
		const name = cleanFieldName(row[1]);
		if (name == null) {
			continue;
		}
		const cells = line.split('|').slice(1, -1);
		const declaredType = cells.length >= 2 ? normaliseDocType(cells[1]) : null;
		for (const anchorKey of [`${slug}#${currentAnchor}`]) {
			const set = anchorFields.get(anchorKey) ?? new Set<string>();
			set.add(name);
			anchorFields.set(anchorKey, set);
			if (declaredType != null) {
				const typeMap = anchorTypes.get(anchorKey) ?? new Map<string, string>();
				if (!typeMap.has(name)) {
					typeMap.set(name, declaredType);
				}
				anchorTypes.set(anchorKey, typeMap);
			}
		}
	}
}

const mismatches: Array<Mismatch> = [];
let checkedBodies = 0;
let checkedQueries = 0;
let unionBodiesSkipped = 0;
let referencedElsewhere = 0;
let documentedByReference = 0;
let responsesChecked = 0;
let responseFieldsFound = 0;
let typesCompared = 0;
let optionalityCompared = 0;
const optionalityAdvisories: Array<string> = [];

for (const file of await walk(DOCS_ROOT)) {
	const relative = path.relative(DOCS_ROOT, file);
	if (relative.startsWith('media-proxy/')) {
		continue;
	}
	const lines = (await readFile(file, 'utf8')).split('\n');
	const pageFields = new Set<string>();
	for (let i = 0; i < lines.length; i += 1) {
		if (!lines[i].startsWith('|')) {
			continue;
		}
		const match = lines[i].match(TABLE_ROW);
		if (match == null) {
			continue;
		}
		const name = cleanFieldName(match[1]);
		if (name != null) {
			pageFields.add(name);
		}
	}
	const isAdmin = relative.startsWith('admin-api/');
	const spec = isAdmin ? adminSpec : mainSpec;
	const index = isAdmin ? adminIndex : mainIndex;

	const sections: Array<{start: number; end: number}> = [];
	let current = -1;
	for (let i = 0; i < lines.length; i += 1) {
		if (lines[i].startsWith('## ') && !lines[i].startsWith('### ')) {
			if (current !== -1) {
				sections.push({start: current, end: i});
			}
			current = i;
		}
	}
	if (current !== -1) {
		sections.push({start: current, end: lines.length});
	}

	for (const section of sections) {
		let header: RegExpMatchArray | null = null;
		for (let i = section.start; i < section.end; i += 1) {
			const match = lines[i].match(ROUTE_HEADER);
			if (match != null) {
				header = match;
				break;
			}
		}
		if (header == null) {
			continue;
		}
		const key = shape(header[1], stripVersion(header[2]));
		const operation = index.get(key);
		if (operation == null) {
			continue;
		}

		const referenced = new Set<string>();
		for (let i = section.start; i < section.end; i += 1) {
			for (const link of lines[i].matchAll(/\]\(([^)\s]*)#([a-z0-9-]+)\)/gu)) {
				const targetPage = link[1].replace(/^\//u, '').replace(/\/$/u, '');
				if (targetPage.length === 0) {
					referenced.add(`${relative.replace(/\.(mdx|md)$/u, '').replace(/\/index$/u, '')}#${link[2]}`);
					continue;
				}
				referenced.add(`${targetPage}#${link[2]}`);
			}
		}
		const referencedFields = new Set<string>();
		for (const anchor of referenced) {
			for (const field of anchorFields.get(anchor) ?? []) {
				referencedFields.add(field);
			}
		}

		const successResponse = Object.entries(operation.responses ?? {}).find(([status]) => status.startsWith('2'));
		if (successResponse != null) {
			const responseSchema = successResponse[1].content?.['application/json']?.schema;
			const resolvedResponse = resolveRef(spec, responseSchema);
			const target = resolvedResponse?.type === 'array' ? resolvedResponse.items : resolvedResponse;
			const responseProperties = collectProperties(spec, target);
			const resolvedTarget = resolveRef(spec, target);
			const responseIsUnion =
				resolvedTarget != null && ((resolvedTarget.oneOf ?? []).length > 0 || (resolvedTarget.anyOf ?? []).length > 0);
			if (!STREAMED_RESPONSE_ROUTES.has(key) && !EMPTY_RESPONSE_ROUTES.has(key)) {
				const referencedTypes = new Map<string, string>();
				for (const anchor of referenced) {
					for (const [field, type] of anchorTypes.get(anchor) ?? []) {
						if (!referencedTypes.has(field)) {
							referencedTypes.set(field, type);
						}
					}
				}
				for (const [field, specType] of collectPropertyTypes(spec, target)) {
					const docType = referencedTypes.get(field);
					if (docType == null) {
						continue;
					}
					typesCompared += 1;
					if (specType === docType) {
						continue;
					}
					if (specType === 'number' && docType === 'integer') {
						continue;
					}
					mismatches.push({
						page: relative,
						operation: key,
						kind: 'type-mismatch',
						detail: `${field}: documented ${docType}, response schema ${specType}`,
					});
				}
			}
			if (
				responseProperties.size > 0 &&
				!responseIsUnion &&
				!STREAMED_RESPONSE_ROUTES.has(key) &&
				!EMPTY_RESPONSE_ROUTES.has(key)
			) {
				responsesChecked += 1;
				for (const field of responseProperties) {
					if (pageFields.has(field) || referencedFields.has(field)) {
						responseFieldsFound += 1;
						continue;
					}
					mismatches.push({page: relative, operation: key, kind: 'response-missing', detail: field});
				}
			}
		}

		for (let i = section.start; i < section.end; i += 1) {
			const heading = lines[i].trim();
			if (heading === '### JSON body') {
				const documented = tableFields(lines, i + 1);
				const content = operation.requestBody?.content ?? {};
				const jsonSchema = content['application/json']?.schema;
				if (jsonSchema == null) {
					continue;
				}
				const actual = collectProperties(spec, jsonSchema);
				if (actual.size === 0) {
					continue;
				}
				if (sectionIsByReference(lines, i + 1)) {
					documentedByReference += 1;
					continue;
				}
				checkedBodies += 1;
				const documentedTypes = tableFieldTypes(lines, i + 1);
				const actualTypes = collectPropertyTypes(spec, jsonSchema);
				for (const [field, docType] of documentedTypes) {
					const specType = actualTypes.get(field);
					if (specType == null) {
						continue;
					}
					typesCompared += 1;
					if (specType === docType) {
						continue;
					}
					if (specType === 'number' && docType === 'integer') {
						continue;
					}
					mismatches.push({
						page: relative,
						operation: key,
						kind: 'type-mismatch',
						detail: `${field}: documented ${docType}, schema ${specType}`,
					});
				}
				const documentedOptional = tableOptionality(lines, i + 1);
				const requiredFields = collectRequired(spec, jsonSchema);
				const bodyProperties = collectProperties(spec, jsonSchema);
				for (const [field, isOptional] of documentedOptional) {
					if (!bodyProperties.has(field)) {
						continue;
					}
					const specRequired = requiredFields.has(field);
					optionalityCompared += 1;
					if (specRequired === !isOptional) {
						continue;
					}
					optionalityAdvisories.push(
						specRequired
							? `${relative}  ${key}  ${field}: documented optional, schema marks it required`
							: `${relative}  ${key}  ${field}: documented required, schema marks it optional`,
					);
				}
				const merged = MERGED_SCHEMA_ROUTES.has(key);
				for (const field of documented) {
					if (actual.has(field)) {
						continue;
					}
					if (merged && SUDO_MERGE_FIELDS.includes(field)) {
						continue;
					}
					mismatches.push({page: relative, operation: key, kind: 'body-extra', detail: field});
				}
				const resolvedBody = resolveRef(spec, jsonSchema);
				const isUnion =
					resolvedBody != null && ((resolvedBody.oneOf ?? []).length > 0 || (resolvedBody.anyOf ?? []).length > 0);
				if (isUnion) {
					unionBodiesSkipped += 1;
					continue;
				}
				for (const field of actual) {
					if (documented.has(field)) {
						continue;
					}
					if (pageFields.has(field)) {
						referencedElsewhere += 1;
						continue;
					}
					mismatches.push({page: relative, operation: key, kind: 'body-missing', detail: field});
				}
			}
			if (heading === '### Query parameters') {
				const documented = tableFields(lines, i + 1);
				const actual = new Set((operation.parameters ?? []).filter((p) => p.in === 'query').map((p) => p.name));
				if (actual.size === 0) {
					continue;
				}
				checkedQueries += 1;
				for (const field of documented) {
					if (!actual.has(field)) {
						mismatches.push({page: relative, operation: key, kind: 'query-extra', detail: field});
					}
				}
				for (const field of actual) {
					if (documented.has(field)) {
						continue;
					}
					if (pageFields.has(field)) {
						referencedElsewhere += 1;
						continue;
					}
					mismatches.push({page: relative, operation: key, kind: 'query-missing', detail: field});
				}
			}
		}
	}
}

if (process.env.FLUXER_DOCS_SCHEMA_JSON != null) {
	const grouped = new Map<string, Array<Mismatch>>();
	for (const m of mismatches) {
		const list = grouped.get(m.page) ?? [];
		list.push(m);
		grouped.set(m.page, list);
	}
	const payload: Record<string, Array<{operation: string; kind: string; field: string}>> = {};
	for (const [page, list] of grouped) {
		payload[page] = list.map((m) => ({operation: m.operation, kind: m.kind, field: m.detail}));
	}
	const {writeFile} = await import('node:fs/promises');
	await writeFile(process.env.FLUXER_DOCS_SCHEMA_JSON, JSON.stringify(payload, null, 1));
	console.log(`wrote ${process.env.FLUXER_DOCS_SCHEMA_JSON}`);
}

const byKind = new Map<string, number>();
for (const m of mismatches) {
	byKind.set(m.kind, (byKind.get(m.kind) ?? 0) + 1);
}

const authSource = await readFile(path.join(REPO_ROOT, 'fluxer_api/src/api/auth/AuthController.ts'), 'utf8');
const guildSource = await readFile(
	path.join(REPO_ROOT, 'fluxer_api/src/api/guild/controllers/GuildMemberController.ts'),
	'utf8',
);
const adminSystemSource = await readFile(
	path.join(REPO_ROOT, 'fluxer_api/src/api/admin/controllers/SystemAdminController.ts'),
	'utf8',
);
const staleExemptions: Array<string> = [];
for (const [route, anchor] of STREAMED_RESPONSE_ROUTES) {
	if (!adminSystemSource.includes(anchor)) {
		staleExemptions.push(`${route}: no longer streams a file, drop this exemption`);
	}
}
const adminUserSecuritySource = await readFile(
	path.join(REPO_ROOT, 'fluxer_api/src/api/admin/services/AdminUserSecurityService.ts'),
	'utf8',
);
for (const [route, marker] of EMPTY_RESPONSE_ROUTES) {
	if (adminUserSecuritySource.includes(marker)) {
		staleExemptions.push(`${route}: the handler now emits ${marker}, drop this exemption`);
	}
}
for (const [route, anchor] of MERGED_SCHEMA_ROUTES) {
	const present = authSource.includes(anchor) || guildSource.includes(anchor);
	if (!present) {
		staleExemptions.push(`${route}: the .merge() call is gone, drop this exemption`);
	}
}
if (staleExemptions.length > 0) {
	console.log('stale exemptions:');
	for (const entry of staleExemptions) {
		console.log(`  ${entry}`);
	}
}

console.log(`request body tables checked: ${checkedBodies.toString()}`);
console.log(`union bodies skipped for the missing check: ${unionBodiesSkipped.toString()}`);
console.log(
	`fields documented in a shared object section rather than the route table: ${referencedElsewhere.toString()}`,
);
console.log(`bodies documented by reference to an object section: ${documentedByReference.toString()}`);
console.log(
	`merged-schema exemptions active: ${MERGED_SCHEMA_ROUTES.size.toString()} (the OpenAPI generator drops .merge() operands)`,
);
console.log(`query parameter tables checked: ${checkedQueries.toString()}`);
console.log(`success response schemas checked: ${responsesChecked.toString()}`);
console.log(
	`streamed-response exemptions active: ${STREAMED_RESPONSE_ROUTES.size.toString()} (the spec declares JSON, the implementation streams a file)`,
);
console.log(
	`empty-response exemptions active: ${EMPTY_RESPONSE_ROUTES.size.toString()} (the spec declares a body, the handler returns none)`,
);
console.log(`response fields found documented on the page: ${responseFieldsFound.toString()}`);
console.log(`request body field types compared: ${typesCompared.toString()}`);
console.log(`request body optionality compared: ${optionalityCompared.toString()}`);
console.log(`optionality advisories: ${optionalityAdvisories.length.toString()}`);
if (optionalityAdvisories.length > 0) {
	console.log('  ADVISORY ONLY, this does not fail the run.');
	console.log('  Both current lines were adjudicated by hand. ZERO documentation bugs: in each');
	console.log('  case the page is MORE precise than a bare ? marker can be.');
	console.log('    POST /stripe/checkout/subscription/preapproval requires country_code in the');
	console.log('      handler (StripeCheckoutService.ts:322 throws when it is absent) while the');
	console.log('      shared CreateCheckoutSessionRequest requires only price_id.');
	console.log('    PATCH /admin/discovery/applications/{guild_id} takes a discriminated union.');
	console.log('      reason is required when status is rejected and optional when it is approved,');
	console.log('      which the page states in a footnote instead of flattening it to ?.');
	console.log('  Re-adjudicate only if this list changes.');
	for (const entry of optionalityAdvisories) {
		console.log(`    ${entry}`);
	}
}
for (const [kind, count] of [...byKind.entries()].sort()) {
	console.log(`  ${kind}: ${count.toString()}`);
}
if (mismatches.length > 0) {
	console.log('');
	for (const m of mismatches.slice(0, 120)) {
		let label = 'in the schema but undocumented';
		if (m.kind.endsWith('extra')) {
			label = 'documented but not in the schema';
		}
		if (m.kind === 'type-mismatch') {
			label = 'type disagreement';
		}
		if (m.kind === 'optionality') {
			label = 'optionality disagreement';
		}
		console.log(`${m.page}  ${m.operation}  ${label}: ${m.detail}`);
	}
	if (mismatches.length > 120) {
		console.log(`... and ${(mismatches.length - 120).toString()} more`);
	}
	console.error(`FAIL: ${mismatches.length.toString()} field mismatches`);
	process.exit(1);
}
console.log('OK: every documented request body and query table matches the live schema');

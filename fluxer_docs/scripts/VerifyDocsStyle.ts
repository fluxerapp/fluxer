// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdir, readFile} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {
	columnWidthPercents,
	extractTables,
	PX,
	TABLE_BUDGET_PX,
	TABLE_FLOOR_CAP_SHARE,
	TABLE_MAX_CELL_CHARS,
	TABLE_MAX_COLUMNS,
	TABLE_MAX_IDENT_CHARS,
	TABLE_WIDE_TIER_PX,
} from './DocsTableWidth.ts';

const DOCS_ROOT = fileURLToPath(new URL('../src/content/docs/', import.meta.url));
const STYLES_ROOT = fileURLToPath(new URL('../src/styles/', import.meta.url));
const STARLIGHT_STYLES = fileURLToPath(new URL('../node_modules/@astrojs/starlight/style/', import.meta.url));

const FORBIDDEN_MARKETING_WORDS = [
	'seamless',
	'effortless',
	'blazing',
	'cutting-edge',
	'game-changing',
	'powerful and',
	'just simply',
	'simply use',
	'simply call',
	'simply add',
	'simply set',
];

const FORBIDDEN_TOPICS = [
	{pattern: /mobile[- ]device/iu, reason: 'mobile notifications API does not exist in the live era'},
	{pattern: /push subscription/iu, reason: 'push API does not exist in the live era'},
	{pattern: /\/push\/events/u, reason: 'push events API does not exist in the live era'},
	{pattern: /voice[- ]public[- ]key/iu, reason: 'voice connection API does not exist in the live era'},
	{pattern: /match-public-key/u, reason: 'voice connection API does not exist in the live era'},
];

const CANONICAL_HEADINGS = new Map([
	['### JSON params', '### JSON body'],
	['### Query string parameters', '### Query parameters'],
	['### Response Body', '### Response body'],
	['### JSON Body', '### JSON body'],
	['### Query Parameters', '### Query parameters'],
]);

interface Finding {
	readonly file: string;
	readonly line: number;
	readonly rule: string;
	readonly detail: string;
}

async function walk(directory: string): Promise<Array<string>> {
	const entries = await readdir(directory, {withFileTypes: true});
	const files: Array<string> = [];
	for (const entry of entries) {
		const resolved = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await walk(resolved)));
			continue;
		}
		if (entry.name.endsWith('.mdx') || entry.name.endsWith('.md')) {
			files.push(resolved);
		}
	}
	return files;
}

function frontmatterOf(source: string): string | null {
	if (!source.startsWith('---\n')) {
		return null;
	}
	const end = source.indexOf('\n---', 4);
	if (end === -1) {
		return null;
	}
	return source.slice(4, end);
}

function insideFence(lines: ReadonlyArray<string>, index: number): boolean {
	let fenced = false;
	for (let cursor = 0; cursor < index; cursor += 1) {
		if (lines[cursor].startsWith('```')) {
			fenced = !fenced;
		}
	}
	return fenced;
}

const files = await walk(DOCS_ROOT);
const findings: Array<Finding> = [];

for (const file of files) {
	const relative = path.relative(DOCS_ROOT, file);
	const source = await readFile(file, 'utf8');
	const lines = source.split('\n');
	const frontmatter = frontmatterOf(source);

	if (frontmatter == null) {
		findings.push({file: relative, line: 1, rule: 'frontmatter', detail: 'missing frontmatter block'});
	} else {
		if (!/^# SPDX-License-Identifier: AGPL-3\.0-or-later$/mu.test(frontmatter)) {
			findings.push({file: relative, line: 2, rule: 'spdx', detail: 'missing SPDX comment in frontmatter'});
		}
		if (!/^title:\s*\S/mu.test(frontmatter)) {
			findings.push({file: relative, line: 1, rule: 'frontmatter', detail: 'missing title'});
		}
		if (!/^description:\s*\S/mu.test(frontmatter)) {
			findings.push({file: relative, line: 1, rule: 'frontmatter', detail: 'missing description'});
		}
	}

	const usesRouteHeader = source.includes('<RouteHeader');
	const importsRouteHeader = source.includes("import RouteHeader from '@/components/RouteHeader.astro'");
	if (usesRouteHeader && !importsRouteHeader) {
		findings.push({file: relative, line: 1, rule: 'import', detail: 'uses RouteHeader without importing it'});
	}
	if (usesRouteHeader && !file.endsWith('.mdx')) {
		findings.push({file: relative, line: 1, rule: 'extension', detail: 'uses RouteHeader but is not .mdx'});
	}

	for (let index = 0; index < lines.length; index += 1) {
		const line = lines[index];
		const number = index + 1;
		if (insideFence(lines, index)) {
			continue;
		}
		if (line.includes('—')) {
			findings.push({file: relative, line: number, rule: 'em-dash', detail: line.trim().slice(0, 100)});
		}
		for (const word of FORBIDDEN_MARKETING_WORDS) {
			if (line.toLowerCase().includes(word)) {
				findings.push({file: relative, line: number, rule: 'tone', detail: `contains "${word.trim()}"`});
			}
		}
		const canonical = CANONICAL_HEADINGS.get(line.trim());
		if (canonical != null) {
			findings.push({
				file: relative,
				line: number,
				rule: 'heading',
				detail: `use "${canonical}" for consistency with the rest of the corpus`,
			});
		}
		for (const topic of FORBIDDEN_TOPICS) {
			if (topic.pattern.test(line)) {
				findings.push({file: relative, line: number, rule: 'unshipped', detail: topic.reason});
			}
		}
	}
}

for (const file of files) {
	const relative = path.relative(DOCS_ROOT, file);
	const lines = (await readFile(file, 'utf8')).split('\n');
	const sectionStarts: Array<number> = [];
	for (let index = 0; index < lines.length; index += 1) {
		if (lines[index].startsWith('## ') && !lines[index].startsWith('### ')) {
			sectionStarts.push(index);
		}
	}
	sectionStarts.push(lines.length);
	for (let s = 0; s + 1 < sectionStarts.length; s += 1) {
		let responseBody = -1;
		let response = -1;
		for (let index = sectionStarts[s]; index < sectionStarts[s + 1]; index += 1) {
			const heading = lines[index].trim();
			if (heading === '### Response body' && responseBody === -1) {
				responseBody = index;
			}
			if (heading === '### Response' && response === -1) {
				response = index;
			}
		}
		if (responseBody !== -1 && response !== -1 && responseBody > response) {
			findings.push({
				file: relative,
				line: response + 1,
				rule: 'order',
				detail: '"### Response body" must precede "### Response" (conventions.md)',
			});
		}
	}
}

for (const file of files) {
	const relative = path.relative(DOCS_ROOT, file);
	const lines = (await readFile(file, 'utf8')).split('\n');
	let block = new Map<string, number>();
	let sinceFootnote = 0;
	for (let index = 0; index < lines.length; index += 1) {
		const definition = lines[index].match(/^<sup>(\d+)<\/sup>\s/u);
		if (definition == null) {
			if (lines[index].trim().length > 0) {
				sinceFootnote += 1;
			}
			if (sinceFootnote > 1) {
				block = new Map<string, number>();
			}
			continue;
		}
		sinceFootnote = 0;
		const previous = block.get(definition[1]);
		if (previous != null) {
			findings.push({
				file: relative,
				line: index + 1,
				rule: 'footnote',
				detail: `footnote ${definition[1]} is defined twice in the same block, first at line ${previous.toString()}`,
			});
			continue;
		}
		block.set(definition[1], index + 1);
	}
}

const NOTATION_EXAMPLE = 'A superscript marker such as <sup>1</sup> refers to the numbered footnote';

for (const file of files) {
	const relative = path.relative(DOCS_ROOT, file);
	const source = await readFile(file, 'utf8');
	const lines = source.split('\n');
	const explainsNotation = source.includes(NOTATION_EXAMPLE);
	const bounds: Array<number> = [0];
	for (let index = 0; index < lines.length; index += 1) {
		if (lines[index].startsWith('## ') && !lines[index].startsWith('### ')) {
			bounds.push(index);
		}
	}
	bounds.push(lines.length);
	for (let b = 0; b + 1 < bounds.length; b += 1) {
		const definitions = new Map<string, number>();
		const references = new Set<string>();
		for (let index = bounds[b]; index < bounds[b + 1]; index += 1) {
			const definition = lines[index].match(/^<sup>(\d+)<\/sup>\s/u);
			if (definition != null) {
				if (!definitions.has(definition[1])) {
					definitions.set(definition[1], index + 1);
				}
				continue;
			}
			for (const marker of lines[index].matchAll(/<sup>(\d+)<\/sup>/gu)) {
				references.add(marker[1]);
			}
		}
		for (const [number, line] of definitions) {
			if (!references.has(number)) {
				findings.push({
					file: relative,
					line,
					rule: 'footnote',
					detail: `footnote ${number} is defined but never referenced in its section`,
				});
			}
		}
		for (const number of references) {
			if (definitions.has(number)) {
				continue;
			}
			if (explainsNotation) {
				continue;
			}
			findings.push({
				file: relative,
				line: bounds[b] + 1,
				rule: 'footnote',
				detail: `footnote ${number} is referenced but never defined in its section`,
			});
		}
	}
}

type TableRule =
	| 'table-columns'
	| 'table-fit'
	| 'table-cell'
	| 'table-identifier'
	| 'table-rows'
	| 'table-parallel'
	| 'table-wide-tight';

const TABLE_RULES: ReadonlyArray<TableRule> = [
	'table-columns',
	'table-fit',
	'table-cell',
	'table-identifier',
	'table-rows',
	'table-parallel',
	'table-wide-tight',
];

const TABLE_COUNT_FLOOR = 1800;

const ACCEPTED_TABLE_FINDINGS = new Map<string, Readonly<Partial<Record<TableRule, number>>>>([
	['admin-api/applications.mdx', {'table-identifier': 1}],
	['admin-api/bulk-jobs.mdx', {'table-fit': 1}],
	['admin-api/discovery.mdx', {'table-identifier': 1}],
	['admin-api/guilds.mdx', {'table-identifier': 3}],
	['admin-api/index.mdx', {'table-fit': 1, 'table-identifier': 2}],
	['admin-api/instance.mdx', {'table-identifier': 5}],
	['admin-api/messages.mdx', {'table-identifier': 1}],
	['admin-api/reports.mdx', {'table-fit': 1, 'table-identifier': 2}],
	['admin-api/users.mdx', {'table-fit': 1, 'table-identifier': 1}],
	['admin-api/voice.mdx', {'table-identifier': 3}],
	['authentication.md', {'table-cell': 2, 'table-identifier': 1}],
	['conventions.md', {'table-cell': 1, 'table-parallel': 1}],
	['gateway/event-filtering.md', {'table-cell': 2}],
	['gateway/events.md', {'table-identifier': 1}],
	['gateway/opcodes-and-close-codes.md', {'table-cell': 1}],
	['gateway/overview.md', {'table-cell': 1}],
	['http-api/authentication.mdx', {'table-identifier': 1}],
	['http-api/billing.mdx', {'table-identifier': 5}],
	['http-api/calls.mdx', {'table-fit': 1, 'table-cell': 3}],
	['http-api/channels.mdx', {'table-cell': 6}],
	['http-api/connections.mdx', {'table-fit': 1, 'table-cell': 4, 'table-identifier': 1}],
	['http-api/deployment-availability.md', {'table-fit': 1}],
	['http-api/discovery.mdx', {'table-cell': 3}],
	['http-api/donations.mdx', {'table-cell': 2}],
	['http-api/entrance-sounds.mdx', {'table-cell': 3, 'table-parallel': 1}],
	['http-api/gifs.mdx', {'table-cell': 5}],
	['http-api/gifts.mdx', {'table-cell': 1}],
	['http-api/guild-audit-logs.mdx', {'table-identifier': 3}],
	['http-api/guild-channels.mdx', {'table-cell': 2}],
	['http-api/guild-emojis.mdx', {'table-cell': 4}],
	['http-api/guild-members.mdx', {'table-identifier': 2}],
	['http-api/guild-moderation.mdx', {'table-cell': 1}],
	['http-api/guild-stickers.mdx', {'table-cell': 3}],
	['http-api/guilds.mdx', {'table-fit': 1, 'table-identifier': 4}],
	['http-api/instance.mdx', {'table-identifier': 4}],
	['http-api/invites.mdx', {'table-cell': 6}],
	['http-api/messages.mdx', {'table-fit': 1, 'table-cell': 20}],
	['http-api/permissions.mdx', {'table-cell': 8}],
	['http-api/premium.mdx', {'table-identifier': 5}],
	['http-api/read-states.mdx', {'table-cell': 2}],
	['http-api/reports.mdx', {'table-cell': 5, 'table-identifier': 1}],
	['http-api/search.mdx', {'table-fit': 1, 'table-cell': 2, 'table-identifier': 1}],
	['http-api/streams.mdx', {'table-cell': 5}],
	['http-api/unfurl.mdx', {'table-cell': 2, 'table-parallel': 1}],
	['http-api/users.mdx', {'table-fit': 1, 'table-identifier': 4}],
	['http-api/users/content.mdx', {'table-cell': 4}],
	['http-api/users/current-user.mdx', {'table-cell': 1, 'table-identifier': 1}],
	['http-api/users/data-harvest.mdx', {'table-cell': 2}],
	['http-api/users/email-and-password.mdx', {'table-identifier': 1}],
	['http-api/users/mfa.mdx', {'table-cell': 3, 'table-parallel': 1}],
	['http-api/users/phone-verification.mdx', {'table-cell': 2}],
	['http-api/users/private-channels.mdx', {'table-cell': 3}],
	['http-api/users/relationships.mdx', {'table-fit': 2, 'table-cell': 3, 'table-identifier': 2}],
	['http-api/users/settings-protobuf.md', {'table-fit': 2, 'table-identifier': 7, 'table-parallel': 1}],
	['http-api/users/settings.mdx', {'table-fit': 1, 'table-cell': 2, 'table-identifier': 1, 'table-parallel': 1}],
	['http-api/webhooks.mdx', {'table-identifier': 1, 'table-parallel': 1}],
	['media-proxy/overview.md', {'table-parallel': 1}],
	['media-proxy/responses-and-limits.md', {'table-cell': 2}],
	['media-proxy/routes.mdx', {'table-cell': 1}],
	['media-proxy/transformations.md', {'table-parallel': 1}],
	['topics/uploads.md', {'table-fit': 1, 'table-identifier': 1}],
	['voice/index.md', {'table-parallel': 1}],
]);

const tableFindingsByPage = new Map<string, Map<TableRule, Array<Finding>>>();
let tablesMeasured = 0;
const overWideTier: Array<Finding> = [];

for (const file of files) {
	const relative = path.relative(DOCS_ROOT, file);
	const source = await readFile(file, 'utf8');
	for (const table of extractTables(source)) {
		tablesMeasured += 1;
		const raise = (rule: TableRule, detail: string): void => {
			const perRule = tableFindingsByPage.get(relative) ?? new Map<TableRule, Array<Finding>>();
			const list = perRule.get(rule) ?? [];
			list.push({file: relative, line: table.line, rule, detail});
			perRule.set(rule, list);
			tableFindingsByPage.set(relative, perRule);
		};
		const shape = table.header.join(' | ');
		if (table.columns > TABLE_MAX_COLUMNS) {
			raise(
				'table-columns',
				`[${shape}] has ${table.columns.toString()} columns, at most ${TABLE_MAX_COLUMNS.toString()} are allowed`,
			);
		}
		if (table.minDemandPx > TABLE_BUDGET_PX) {
			raise(
				'table-fit',
				`[${shape}] demands ${table.minDemandPx.toString()}px against the ${TABLE_BUDGET_PX.toString()}px budget, columns ${table.columnMinPx.join('/')}px`,
			);
		}
		if (table.minDemandPx > TABLE_WIDE_TIER_PX) {
			overWideTier.push({
				file: relative,
				line: table.line,
				rule: 'table-wide-tier',
				detail: `[${shape}] demands ${table.minDemandPx.toString()}px, over the ${TABLE_WIDE_TIER_PX.toString()}px width where the wide CSS tier switches to content sizing, so this table overflows its column instead of wrapping`,
			});
		}
		if (table.worstCellChars > TABLE_MAX_CELL_CHARS) {
			raise(
				'table-cell',
				`[${shape}] column ${(table.worstCellColumn + 1).toString()} holds ${table.worstCellChars.toString()} rendered characters, over ${TABLE_MAX_CELL_CHARS.toString()}: "${table.worstCellText.slice(0, 60)}..."`,
			);
		}
		if (table.col1IdentChars > TABLE_MAX_IDENT_CHARS) {
			raise(
				'table-identifier',
				`[${shape}] first column holds "${table.col1IdentText}", ${table.col1IdentChars.toString()} characters, over ${TABLE_MAX_IDENT_CHARS.toString()}`,
			);
		}
		if (table.rows < 2 && !table.canonical) {
			raise(
				'table-rows',
				`[${shape}] has ${table.rows.toString()} body row(s) and is not a canonical schema shape, so it is a sentence`,
			);
		}
		if (!table.parallel) {
			raise('table-parallel', `[${shape}] ${table.nonParallelReason}`);
		}
		const shares = columnWidthPercents(
			table.columnMinPx.map((minPx, index) => ({minPx, maxPx: table.columnMaxPx[index]})),
			TABLE_WIDE_TIER_PX,
		);
		const floorCap = TABLE_WIDE_TIER_PX * TABLE_FLOOR_CAP_SHARE;
		for (const [index, share] of shares.entries()) {
			const allotted = (share / 100) * TABLE_WIDE_TIER_PX;
			const unbreakable = table.columnMinPx[index] + PX.cellPad;
			if (unbreakable <= floorCap && allotted + PX.proseFallback < unbreakable) {
				raise(
					'table-wide-tight',
					`[${shape}] column ${(index + 1).toString()} is allotted ${Math.round(allotted).toString()}px but its longest unbreakable word needs ${Math.round(unbreakable).toString()}px, so that word breaks mid-word even at the widest tier`,
				);
			}
		}
	}
}

if (tablesMeasured < TABLE_COUNT_FLOOR) {
	findings.push({
		file: 'scripts/DocsTableWidth.ts',
		line: 1,
		rule: 'table-extractor',
		detail: `only ${tablesMeasured.toString()} tables were measured, floor is ${TABLE_COUNT_FLOOR.toString()}. The table rules have gone blind`,
	});
}

findings.push(...overWideTier);

const acceptedByRule = new Map<TableRule, number>();
for (const [relative, perRule] of tableFindingsByPage) {
	const allowance = ACCEPTED_TABLE_FINDINGS.get(relative) ?? {};
	for (const [rule, list] of perRule) {
		const allowed = allowance[rule] ?? 0;
		acceptedByRule.set(rule, (acceptedByRule.get(rule) ?? 0) + Math.min(allowed, list.length));
		for (const finding of list.slice(allowed)) {
			findings.push(finding);
		}
	}
}
for (const [relative, allowance] of ACCEPTED_TABLE_FINDINGS) {
	const perRule = tableFindingsByPage.get(relative);
	for (const rule of TABLE_RULES) {
		const allowed = allowance[rule] ?? 0;
		const actual = perRule?.get(rule)?.length ?? 0;
		if (allowed > actual) {
			findings.push({
				file: relative,
				line: 1,
				rule: 'table-ledger',
				detail: `the ledger allows ${allowed.toString()} ${rule} findings but the page now produces ${actual.toString()}. Lower the entry in VerifyDocsStyle.ts`,
			});
		}
	}
}

interface CssBlock {
	readonly selector: string;
	readonly body: string;
	readonly line: number;
}

function cssBlocks(source: string): Array<CssBlock> {
	const blocks: Array<CssBlock> = [];
	const pattern = /([^{}]+)\{([^{}]*)\}/gu;
	let match: RegExpExecArray | null = pattern.exec(source);
	while (match != null) {
		blocks.push({
			selector: match[1]
				.replace(/\/\*[\s\S]*?\*\//gu, ' ')
				.replace(/\s+/gu, ' ')
				.trim(),
			body: match[2],
			line: source.slice(0, match.index).split('\n').length,
		});
		match = pattern.exec(source);
	}
	return blocks;
}

const SIDEWAYS_SCROLL = /(?:^|;)\s*overflow(?:-x)?\s*:\s*(?:auto|scroll)\s*(?:;|$)/mu;
const SCROLL_FORBIDDEN_SELECTOR =
	/\btable\b|\bthead\b|\btbody\b|\btr\b|\.sl-markdown-content\s*(?:,|$)|table-wrapper|overflow-wrapper/u;

for (const entry of await readdir(STYLES_ROOT, {withFileTypes: true})) {
	if (!entry.name.endsWith('.css')) {
		continue;
	}
	const source = await readFile(path.join(STYLES_ROOT, entry.name), 'utf8');
	for (const block of cssBlocks(source)) {
		if (!SCROLL_FORBIDDEN_SELECTOR.test(block.selector) || !SIDEWAYS_SCROLL.test(block.body)) {
			continue;
		}
		findings.push({
			file: `src/styles/${entry.name}`,
			line: block.line,
			rule: 'table-scroll',
			detail: `"${block.selector}" makes a table or the markdown container a sideways scroller. No table may ever scroll horizontally`,
		});
	}
}

{
	const starlightTableScrollers: Array<string> = [];
	for (const entry of await readdir(STARLIGHT_STYLES, {withFileTypes: true})) {
		if (!entry.name.endsWith('.css')) {
			continue;
		}
		const source = await readFile(path.join(STARLIGHT_STYLES, entry.name), 'utf8');
		for (const block of cssBlocks(source)) {
			if (block.selector.includes('table') && SIDEWAYS_SCROLL.test(block.body)) {
				starlightTableScrollers.push(`${entry.name}:${block.line.toString()} ${block.selector}`);
			}
		}
	}
	const content = await readFile(path.join(STYLES_ROOT, 'content.css'), 'utf8');
	const neutralised = cssBlocks(content).some(
		(block) =>
			/\.sl-markdown-content\s+table\b/u.test(block.selector) &&
			/(?:^|;)\s*display\s*:\s*table\s*(?:;|$)/mu.test(block.body) &&
			/(?:^|;)\s*overflow\s*:\s*visible\s*(?:;|$)/mu.test(block.body),
	);
	if (starlightTableScrollers.length > 0 && !neutralised) {
		findings.push({
			file: 'src/styles/content.css',
			line: 1,
			rule: 'table-scroll',
			detail: `Starlight still ships a scrolling table (${starlightTableScrollers.join(', ')}) and content.css no longer overrides it with "display: table" and "overflow: visible"`,
		});
	}
}

const byRule = new Map<string, number>();
for (const finding of findings) {
	byRule.set(finding.rule, (byRule.get(finding.rule) ?? 0) + 1);
}

console.log(`pages checked: ${files.length.toString()}`);
console.log(`tables measured: ${tablesMeasured.toString()}`);

for (const rule of TABLE_RULES) {
	console.log(
		`  ${rule}: ${(byRule.get(rule) ?? 0).toString()} new, ${(acceptedByRule.get(rule) ?? 0).toString()} accepted`,
	);
}
for (const [rule, count] of [...byRule.entries()].sort()) {
	if ((TABLE_RULES as ReadonlyArray<string>).includes(rule)) {
		continue;
	}
	console.log(`  ${rule}: ${count.toString()}`);
}
if (findings.length > 0) {
	console.log('');
	for (const finding of findings.slice(0, 200)) {
		console.log(`${finding.file}:${finding.line.toString()}  [${finding.rule}] ${finding.detail}`);
	}
	if (findings.length > 200) {
		console.log(`... and ${(findings.length - 200).toString()} more`);
	}
	console.error(`FAIL: ${findings.length.toString()} style problems`);
	process.exit(1);
}
console.log('OK: house style clean');

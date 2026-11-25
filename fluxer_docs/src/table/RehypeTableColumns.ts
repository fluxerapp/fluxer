// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RehypePlugin} from '@astrojs/markdown-remark';
import type {Element, ElementContent, Root, RootContent} from 'hast';
import {CELL_BREAK_MIN, CELL_BREAKS, insertBreaks} from '../plugins/rehypeInlineCode.ts';
import {
	type ColumnDemandPx,
	columnWidthPercents,
	maxContentPx,
	minContentPx,
	type Piece,
	PX,
	TABLE_BUDGET_PX,
	TABLE_DESK_TIER_PX,
	TABLE_MIDDLE_TIER_PX,
	TABLE_PHONE_PX,
	TABLE_WIDE_TIER_PX,
} from './DocsTableMetrics.ts';

interface CellNode {
	readonly type?: string;
	readonly value?: string;
	readonly tagName?: string;
	readonly name?: string;
	readonly properties?: {readonly className?: unknown};
	readonly children?: ReadonlyArray<CellNode>;
}

const CELL_TAGS = new Set(['th', 'td']);
const SECTION_TAGS = new Set(['thead', 'tbody', 'tfoot']);

function isElement(node: RootContent | ElementContent, tagName: string): boolean {
	return node.type === 'element' && node.tagName === tagName;
}

function cellPieces(node: CellNode, code: boolean, out: Array<Piece>, nowrap = false): void {
	if (node.type === 'text' && typeof node.value === 'string') {
		out.push({text: node.value, code, nowrap});
		return;
	}
	if (node.type === 'raw' && typeof node.value === 'string') {
		out.push({text: node.value.replace(/<br\s*\/?>/giu, ' ').replace(/<[^>]+>/gu, ''), code, nowrap});
		return;
	}
	const tag = node.tagName ?? node.name;
	if (tag === 'br') {
		out.push({text: ' ', code});
		return;
	}
	if (node.children === undefined) {
		return;
	}
	const className = node.properties?.className;
	const kept = nowrap || (Array.isArray(className) && className.includes('flx-nowrap'));
	for (const child of node.children) {
		cellPieces(child, code || tag === 'code', out, kept);
	}
}

function readCell(cell: Element): Array<Piece> {
	const parts: Array<Piece> = [];
	for (const child of cell.children as ReadonlyArray<CellNode>) {
		cellPieces(child, false, parts);
	}

	return parts.filter((piece) => piece.text.length > 0);
}

interface TableRow {
	readonly cells: ReadonlyArray<Element>;
	readonly header: boolean;
}

function readRows(table: Element): Array<TableRow> {
	const rows: Array<TableRow> = [];
	const collect = (node: ElementContent, header: boolean): void => {
		if (node.type !== 'element') {
			return;
		}
		if (node.tagName === 'tr') {
			const cells = node.children.filter(
				(child): child is Element => child.type === 'element' && CELL_TAGS.has(child.tagName),
			);
			rows.push({cells, header});
			return;
		}
		if (SECTION_TAGS.has(node.tagName)) {
			for (const child of node.children) {
				collect(child, header || node.tagName === 'thead');
			}
		}
	};
	for (const child of table.children) {
		collect(child, false);
	}
	return rows;
}

function spans(cell: Element): boolean {
	const {colSpan, rowSpan} = cell.properties;
	return (typeof colSpan === 'number' && colSpan > 1) || (typeof rowSpan === 'number' && rowSpan > 1);
}

interface SolvedTable {
	readonly table: Element;
	readonly demands: Array<ColumnDemandPx>;
	readonly run: number;
	readonly headers: ReadonlyArray<string>;
}

interface TierPercents {
	narrow: Array<number>;
	desk: Array<number>;
	mid: Array<number>;
	wide: Array<number>;
}

function solveTable(table: Element): {demands: Array<ColumnDemandPx>; headers: Array<string>} | undefined {
	if (table.children.some((child) => isElement(child, 'colgroup'))) {
		return undefined;
	}
	const rows = readRows(table);
	if (rows.length === 0) {
		return undefined;
	}
	const columns = rows[0].cells.length;
	if (columns < 2) {
		return undefined;
	}
	const demands: Array<ColumnDemandPx> = [];
	const headerRow = rows.find((row) => row.header);
	for (let column = 0; column < columns; column += 1) {
		const headerCell = headerRow?.cells[column];
		const label = headerCell === undefined ? '' : textOf(headerCell).trim().toLowerCase();
		const mono = TERSE_HEADERS.has(label);
		const named = column === 0 && NAME_HEADERS.has(label);
		let minPx = 0;
		let maxPx = 0;
		let typicalPx = 0;
		for (const row of rows) {
			const cell = row.cells[column];
			if (cell === undefined) {
				continue;
			}
			if (spans(cell)) {
				return undefined;
			}

			const bold = row.header || cell.tagName === 'th';
			const parts = (mono || named) && !bold ? readCell(cell).map((piece) => ({...piece, mono: true})) : readCell(cell);
			const cellMaxPx = maxContentPx(parts, bold);
			minPx = Math.max(minPx, minContentPx(parts, bold));
			maxPx = Math.max(maxPx, cellMaxPx);
			if (cellMaxPx <= TYPE_OUTLIER_PX) {
				typicalPx = Math.max(typicalPx, cellMaxPx);
			}
		}
		if (label === 'type') {
			const typePx = Math.min(TYPE_COLUMN_PX - PX.cellPad, maxPx);
			demands.push({minPx: typePx, maxPx: typePx});
			continue;
		}
		const anchorPx = column === 0 && label === 'field' ? PX.fieldAnchor : 0;
		const fullPx = (mono && typicalPx > 0 ? Math.max(typicalPx, minPx) : maxPx) + anchorPx;
		demands.push({minPx: mono || named ? fullPx : minPx + anchorPx, maxPx: fullPx});
	}
	const header = rows.find((row) => row.header);
	if (header === undefined || header.cells.length !== columns) {
		return undefined;
	}
	const headers = header.cells.map((cell) =>
		readCell(cell)
			.map((piece) => piece.text)
			.join('')
			.replace(/\s+/gu, ' ')
			.trim(),
	);
	return {demands, headers};
}

const BLOCK_TAGS = new Set(['p', 'ul', 'ol', 'pre', 'blockquote', 'aside', 'details', 'figure', 'hr', 'dl']);
const RUN_SEPARATOR_LIMIT = 3;
const ALIGN_TOLERANCE_PERCENT = 10;
const ALIGNED_GROUP_SIZE = 3;

interface RunWalk {
	run: number;
	separators: number;
}

function collectTables(node: Root | RootContent, out: Array<SolvedTable>, walk: RunWalk): void {
	if (node.type === 'element' && node.tagName === 'table') {
		if (walk.separators > RUN_SEPARATOR_LIMIT) {
			walk.run += 1;
		}
		walk.separators = 0;
		const solved = solveTable(node);
		if (solved !== undefined) {
			out.push({table: node, demands: solved.demands, run: walk.run, headers: solved.headers});
		}
		return;
	}
	if (node.type !== 'root' && node.type !== 'element') {
		return;
	}
	if (node.type === 'element' && BLOCK_TAGS.has(node.tagName)) {
		walk.separators += 1;
	}
	for (const child of node.children) {
		collectTables(child, out, walk);
	}
}

function groupKey(entry: SolvedTable): string {
	return `${entry.run.toString()}\u0000${entry.headers.join('\u0000')}`;
}

function mergeDemands(tables: ReadonlyArray<SolvedTable>): Map<string, Array<ColumnDemandPx>> {
	const merged = new Map<string, Array<ColumnDemandPx>>();
	for (const entry of tables) {
		const current = merged.get(groupKey(entry));
		if (current === undefined) {
			merged.set(
				groupKey(entry),
				entry.demands.map((demand) => ({minPx: demand.minPx, maxPx: demand.maxPx})),
			);
			continue;
		}
		if (current.length !== entry.demands.length) {
			continue;
		}
		for (const [index, demand] of entry.demands.entries()) {
			current[index] = {
				minPx: Math.max(current[index].minPx, demand.minPx),
				maxPx: Math.max(current[index].maxPx, demand.maxPx),
			};
		}
	}
	return merged;
}

function alignTrailing(tables: ReadonlyArray<SolvedTable>, allocations: Map<string, TierPercents>): void {
	const buckets = new Map<string, Set<string>>();
	for (const entry of tables) {
		const last = entry.headers.at(-1);
		if (last === undefined) {
			continue;
		}
		const bucket = `${entry.run.toString()}\u0000${last}`;
		const keys = buckets.get(bucket) ?? new Set<string>();
		keys.add(groupKey(entry));
		buckets.set(bucket, keys);
	}
	for (const keys of buckets.values()) {
		if (keys.size < 2) {
			continue;
		}
		for (const tier of ['narrow', 'desk', 'mid', 'wide'] as const) {
			const leads: Array<{key: string; lead: number}> = [];
			for (const key of keys) {
				const row = allocations.get(key)?.[tier];
				if (row === undefined) {
					continue;
				}
				const lead = 100 - row[row.length - 1];
				if (lead > 0) {
					leads.push({key, lead});
				}
			}
			leads.sort((a, b) => a.lead - b.lead);
			let cluster: Array<{key: string; lead: number}> = [];
			const flush = (): void => {
				if (cluster.length > 1) {
					const target = cluster[cluster.length - 1].lead;
					for (const member of cluster) {
						const row = allocations.get(member.key)?.[tier];
						if (row === undefined) {
							continue;
						}
						const scale = target / member.lead;
						for (let index = 0; index < row.length - 1; index += 1) {
							row[index] = Math.round(row[index] * scale * 10) / 10;
						}
						row[row.length - 1] = Math.round((100 - row.slice(0, -1).reduce((a, b) => a + b, 0)) * 10) / 10;
					}
				}
				cluster = [];
			};
			for (const entry of leads) {
				if (cluster.length > 0 && entry.lead - cluster[0].lead > ALIGN_TOLERANCE_PERCENT) {
					flush();
				}
				cluster.push(entry);
			}
			flush();
		}
	}
}

function sizeTables(tree: Root): void {
	const tables: Array<SolvedTable> = [];
	collectTables(tree, tables, {run: 0, separators: 0});
	const merged = mergeDemands(tables);
	const allocations = new Map<string, TierPercents>();
	for (const [key, demands] of merged) {
		allocations.set(key, {
			narrow: columnWidthPercents(demands, TABLE_BUDGET_PX),
			desk: columnWidthPercents(demands, TABLE_DESK_TIER_PX),
			mid: columnWidthPercents(demands, TABLE_MIDDLE_TIER_PX),
			wide: columnWidthPercents(demands, TABLE_WIDE_TIER_PX),
		});
	}
	alignTrailing(tables, allocations);
	const groupSizes = new Map<string, number>();
	for (const entry of tables) {
		groupSizes.set(groupKey(entry), (groupSizes.get(groupKey(entry)) ?? 0) + 1);
	}
	for (const entry of tables) {
		const percents = allocations.get(groupKey(entry));
		if (percents === undefined) {
			continue;
		}
		if ((groupSizes.get(groupKey(entry)) ?? 0) >= ALIGNED_GROUP_SIZE) {
			entry.table.properties.dataAligned = '';
		}
		entry.table.children.unshift({
			type: 'element',
			tagName: 'colgroup',
			properties: {},
			children: percents.narrow.map((percent, index) => ({
				type: 'element' as const,
				tagName: 'col',
				properties: {
					style: `--flx-col-width:${percent.toString()}%;--flx-col-width-desk:${percents.desk[index].toString()}%;--flx-col-width-mid:${percents.mid[index].toString()}%;--flx-col-width-wide:${percents.wide[index].toString()}%`,
				},
				children: [],
			})),
		});
	}
}

const TERSE_HEADERS = new Set(['type', 'required']);
const TYPE_OUTLIER_PX = 150;
const TYPE_COLUMN_PX = 184;
const NAME_HEADERS = new Set([
	'field',
	'name',
	'parameter',
	'setting',
	'variable',
	'flag',
	'port',
	'command',
	'event',
	'value',
	'task',
]);
const PROSE_HEADERS = new Set([
	'description',
	'condition',
	'meaning',
	'what it does',
	'covers',
	'what to do',
	'cause',
	'what to use',
	'how fluxer runs there',
	'how the voice server runs',
	'what it sets',
	'what it sets the log level for',
]);
const HEADING_TAGS = new Set(['h2', 'h3']);
const PROSE_CELL_CHARS = 30;
const KEEP_LINK_CHARS = 20;
const NUMBER_CELL = /^[-+]?\d[\d.,_ ]*(%|ms|s|KiB|MiB|GiB|KB|MB|GB|px)?$/u;

function textOf(node: ElementContent | RootContent): string {
	if (node.type === 'text') {
		return node.value;
	}
	if (node.type === 'element') {
		if (node.tagName === 'sup') {
			return '';
		}
		return node.children.map(textOf).join('');
	}
	return '';
}

function slugify(text: string): string {
	return text
		.toLowerCase()
		.trim()
		.replace(/[^\p{L}\p{N}\s_-]/gu, '')
		.replace(/\s/gu, '-');
}

interface TableContext {
	heading: string;
	headingSlug: string;
	ids: Set<string>;
}

function uniqueId(base: string, ids: Set<string>): string {
	let id = base;
	let n = 2;
	while (ids.has(id)) {
		id = `${base}-${n.toString()}`;
		n += 1;
	}
	ids.add(id);
	return id;
}

function addClass(element: Element, name: string): void {
	const current = element.properties.className;
	const list = Array.isArray(current) ? current.map(String) : [];
	if (!list.includes(name)) {
		list.push(name);
	}
	element.properties.className = list;
}

function breakIdentifiers(element: Element): void {
	element.children = insertBreaks(element.children, CELL_BREAKS, CELL_BREAK_MIN);
	for (const child of element.children) {
		if (child.type === 'element' && child.tagName !== 'code') {
			breakIdentifiers(child);
		}
	}
}

function ignoreNotes(cell: Element | undefined): void {
	for (const child of cell?.children ?? []) {
		if (child.type === 'element' && child.tagName === 'sup') {
			child.properties.dataPagefindIgnore = '';
		} else if ((child.type as string) === 'mdxJsxTextElement' && (child as unknown as {name: string}).name === 'sup') {
			(child as unknown as {attributes: Array<{type: string; name: string; value: string}>}).attributes.push({
				type: 'mdxJsxAttribute',
				name: 'data-pagefind-ignore',
				value: '',
			});
		}
	}
}

function markColumns(table: Element, context: TableContext): boolean {
	const rows = readRows(table);
	const header = rows.find((row) => row.header);
	if (header === undefined || header.cells.length === 0) {
		return false;
	}
	const first = textOf(header.cells[0]).trim().toLowerCase();
	const body = rows.filter((row) => !row.header);
	const named = NAME_HEADERS.has(first) || first === 'type';
	for (const child of table.children) {
		if (child.type === 'element' && child.tagName === 'thead') {
			child.properties.dataPagefindIgnore = '';
		}
	}
	for (const row of body) {
		ignoreNotes(row.cells[0]);
	}
	for (const row of body) {
		const link = soleLink(row.cells[0]);
		if (link !== null && textOf(link).trim().length <= KEEP_LINK_CHARS) {
			addClass(link, 'flx-keep-link');
		}
	}
	const labels = header.cells.map((cell) => textOf(cell).trim());
	if (named) {
		for (const row of rows) {
			for (const [column, cell] of row.cells.entries()) {
				const label = labels[column];
				if (column === 0 && first !== 'type') {
					cell.properties.dataCol = 'name';
				} else if (label !== undefined && TERSE_HEADERS.has(label.toLowerCase())) {
					cell.properties.dataCol = 'terse';
				}
				if (!row.header && (column === 0 || (label !== undefined && NAME_HEADERS.has(label.toLowerCase())))) {
					breakIdentifiers(cell);
				}
				if (!row.header && label?.toLowerCase() === 'type') {
					breakTypeBrackets(cell);
				}
			}
		}
	}
	if (first === 'field') {
		for (const row of body) {
			const cell = row.cells[0];
			if (cell === undefined) {
				continue;
			}
			const field = textOf(cell).replace(/\?/gu, '').trim();
			if (field.length === 0) {
				continue;
			}
			const base = slugify(context.headingSlug.length > 0 ? `${context.headingSlug}-${field}` : field);
			const id = uniqueId(base, context.ids);
			cell.properties.id = id;
			cell.children.push({
				type: 'element',
				tagName: 'a',
				properties: {
					className: ['flx-field-anchor'],
					href: `#${id}`,
					ariaLabel: `Link to ${field}`,
					dataPagefindIgnore: '',
				},
				children: [{type: 'text', value: '#'}],
			});
		}
	}
	for (let column = 1; column < header.cells.length; column += 1) {
		const values = body.map((row) => (row.cells[column] === undefined ? '' : textOf(row.cells[column]).trim()));
		if (values.length === 0 || !values.every((value) => NUMBER_CELL.test(value))) {
			continue;
		}
		for (const row of rows) {
			const cell = row.cells[column];
			if (cell !== undefined) {
				addClass(cell, 'num');
				if (!row.header) {
					wrapDigits(cell);
				}
			}
		}
	}
	const stack = stacksOnPhone(rows, labels, named);
	if (!stack) {
		return false;
	}
	const last = labels.length - 1;
	const labelLast = last >= 1 && !PROSE_HEADERS.has((labels[last] ?? '').toLowerCase());
	for (const row of body) {
		for (const [column, cell] of row.cells.entries()) {
			const label = labels[column];
			if (column > 0 && (column < last || labelLast) && label !== undefined && label.length > 0) {
				cell.properties.dataLabel = label;
			}
		}
	}
	if (proseColumn(header.cells[1], body)) {
		addClass(table, 'flx-stack-prose');
	}
	return true;
}

function stacksOnPhone(rows: ReadonlyArray<TableRow>, labels: ReadonlyArray<string>, named: boolean): boolean {
	if (labels.length >= 3) {
		return true;
	}
	if (labels.length !== 2) {
		return false;
	}
	const minPx = [0, 0];
	const maxPx = [0, 0];
	for (const row of rows) {
		for (const [column, cell] of row.cells.slice(0, 2).entries()) {
			const bold = row.header || cell.tagName === 'th';
			const mono = !bold && column === 0 && named;
			const parts = mono ? readCell(cell).map((piece) => ({...piece, mono: true})) : readCell(cell);
			minPx[column] = Math.max(minPx[column], minContentPx(parts, bold) + PX.cellPad);
			maxPx[column] = Math.max(maxPx[column], maxContentPx(parts, bold) + PX.cellPad);
		}
	}
	const half = TABLE_PHONE_PX / 2;
	return minPx[0] + minPx[1] > TABLE_PHONE_PX || (maxPx[0] > half && maxPx[1] > half);
}

const DIGIT_RUN = /\d(?:[\d.,_]*\d)?/gu;

function wrapDigits(element: Element): void {
	const children: Array<ElementContent> = [];
	for (const child of element.children) {
		if (child.type === 'element') {
			wrapDigits(child);
			children.push(child);
			continue;
		}
		if (child.type !== 'text') {
			children.push(child);
			continue;
		}
		let last = 0;
		for (const match of child.value.matchAll(DIGIT_RUN)) {
			if (match.index > last) {
				children.push({type: 'text', value: child.value.slice(last, match.index)});
			}
			children.push({
				type: 'element',
				tagName: 'span',
				properties: {className: ['flx-num']},
				children: [{type: 'text', value: match[0]}],
			});
			last = match.index + match[0].length;
		}
		if (last < child.value.length) {
			children.push({type: 'text', value: child.value.slice(last)});
		}
	}
	element.children = children;
}

function breakTypeBrackets(cell: Element): void {
	const children: Array<ElementContent> = [];
	for (const [index, child] of cell.children.entries()) {
		children.push(child);
		if (child.type === 'text' && child.value.endsWith('[') && cell.children[index + 1]?.type === 'element') {
			children.push({type: 'element', tagName: 'wbr', properties: {}, children: []});
		}
	}
	cell.children = children;
}

function proseColumn(heading: Element, body: ReadonlyArray<TableRow>): boolean {
	if (TERSE_HEADERS.has(textOf(heading).trim().toLowerCase()) || body.length === 0) {
		return false;
	}
	const total = body.reduce(
		(sum, row) => sum + (row.cells[1] === undefined ? 0 : textOf(row.cells[1]).trim().length),
		0,
	);
	return total / body.length > PROSE_CELL_CHARS;
}

function soleLink(cell: Element | undefined): Element | null {
	const children = cell?.children.filter((child) => !isBlank(child)) ?? [];
	const link = children[0];
	return children.length === 1 && link?.type === 'element' && link.tagName === 'a' ? link : null;
}

function isBlank(node: ElementContent | RootContent): boolean {
	return node.type === 'text' && node.value.trim().length === 0;
}

function startsWithSup(node: ElementContent | RootContent): boolean {
	if (node.type !== 'element' || node.tagName !== 'p') {
		return false;
	}
	const first = node.children.find((child) => !isBlank(child));
	return first !== undefined && first.type === 'element' && first.tagName === 'sup';
}

function wrapTables(parent: Root | Element, context: TableContext): void {
	const children = parent.children as Array<RootContent | ElementContent>;
	for (let index = 0; index < children.length; index += 1) {
		const node = children[index];
		if (node.type !== 'element') {
			continue;
		}
		if (HEADING_TAGS.has(node.tagName)) {
			context.heading = textOf(node).trim();
			const id = node.properties.id;
			context.headingSlug = typeof id === 'string' ? id : slugify(context.heading);
			context.ids.add(context.headingSlug);
			continue;
		}
		if (node.tagName === 'table') {
			const stack = markColumns(node, context);
			if (stack) {
				addClass(node, 'flx-stack');
			}
			children[index] = {
				type: 'element',
				tagName: 'div',
				properties: {
					className: stack ? ['flx-table-scroll', 'flx-table-stack'] : ['flx-table-scroll'],
					dataScrollLabel: context.heading.length > 0 ? context.heading : 'Table',
				},
				children: [node],
			};
			for (let next = index + 1; next < children.length; next += 1) {
				const sibling = children[next];
				if (isBlank(sibling)) {
					continue;
				}
				if (!startsWithSup(sibling)) {
					break;
				}
				addClass(sibling as Element, 'flx-notes');
			}
			continue;
		}
		if (node.tagName === 'pre' || node.tagName === 'code') {
			continue;
		}
		wrapTables(node, context);
	}
}

export const rehypeTableColumnWidths = (): RehypePlugin => () => (tree) => {
	sizeTables(tree);
	wrapTables(tree, {heading: '', headingSlug: '', ids: new Set()});
};

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RehypePlugin} from '@astrojs/markdown-remark';
import type {Element, ElementContent, Root, RootContent} from 'hast';
import {
	type ColumnDemandPx,
	columnWidthPercents,
	maxContentPx,
	minContentPx,
	type Piece,
	TABLE_MIDDLE_TIER_PX,
	TABLE_NARROW_TIER_PX,
	TABLE_WIDE_TIER_PX,
} from './DocsTableMetrics.ts';

interface CellNode {
	readonly type?: string;
	readonly value?: string;
	readonly tagName?: string;
	readonly name?: string;
	readonly children?: ReadonlyArray<CellNode>;
}

const CELL_TAGS = new Set(['th', 'td']);
const SECTION_TAGS = new Set(['thead', 'tbody', 'tfoot']);

function isElement(node: RootContent | ElementContent, tagName: string): boolean {
	return node.type === 'element' && node.tagName === tagName;
}

function cellPieces(node: CellNode, code: boolean, out: Array<Piece>): void {
	if (node.type === 'text' && typeof node.value === 'string') {
		out.push({text: node.value, code});
		return;
	}
	if (node.type === 'raw' && typeof node.value === 'string') {
		out.push({text: node.value.replace(/<br\s*\/?>/giu, ' ').replace(/<[^>]+>/gu, ''), code});
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
	for (const child of node.children) {
		cellPieces(child, code || tag === 'code', out);
	}
}

function readCell(cell: Element, header: boolean): Array<Piece> {
	const parts: Array<Piece> = [];
	for (const child of cell.children as ReadonlyArray<CellNode>) {
		cellPieces(child, false, parts);
	}

	return parts
		.map((piece) => ({text: header ? piece.text.toUpperCase() : piece.text, code: piece.code}))
		.filter((piece) => piece.text.length > 0);
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
	for (let column = 0; column < columns; column += 1) {
		let minPx = 0;
		let maxPx = 0;
		for (const row of rows) {
			const cell = row.cells[column];
			if (cell === undefined) {
				continue;
			}
			if (spans(cell)) {
				return undefined;
			}

			const bold = row.header || cell.tagName === 'th';
			const parts = readCell(cell, bold);
			minPx = Math.max(minPx, minContentPx(parts, bold));
			maxPx = Math.max(maxPx, maxContentPx(parts, bold));
		}
		demands.push({minPx, maxPx});
	}
	const header = rows.find((row) => row.header);
	if (header === undefined || header.cells.length !== columns) {
		return undefined;
	}
	const headers = header.cells.map((cell) =>
		readCell(cell, true)
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
		for (const tier of ['narrow', 'mid', 'wide'] as const) {
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
			narrow: columnWidthPercents(demands, TABLE_NARROW_TIER_PX),
			mid: columnWidthPercents(demands, TABLE_MIDDLE_TIER_PX),
			wide: columnWidthPercents(demands, TABLE_WIDE_TIER_PX),
		});
	}
	alignTrailing(tables, allocations);
	for (const entry of tables) {
		const percents = allocations.get(groupKey(entry));
		if (percents === undefined) {
			continue;
		}
		entry.table.children.unshift({
			type: 'element',
			tagName: 'colgroup',
			properties: {},
			children: percents.narrow.map((percent, index) => ({
				type: 'element' as const,
				tagName: 'col',
				properties: {
					style: `--flx-col-width:${percent.toString()}%;--flx-col-width-mid:${percents.mid[index].toString()}%;--flx-col-width-wide:${percents.wide[index].toString()}%`,
				},
				children: [],
			})),
		});
	}
}

export const rehypeTableColumnWidths = (): RehypePlugin => () => (tree) => {
	sizeTables(tree);
};

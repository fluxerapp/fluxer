// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';

interface GlyphTable {
	readonly prose: Record<string, number>;
}

const glyphs = JSON.parse(
	readFileSync(fileURLToPath(new URL('../../scripts/DocsTableGlyphs.json', import.meta.url)), 'utf8'),
) as GlyphTable;

export const PX = {
	cellPad: 24,
	codeGlyph: 7.872,
	codeChrome: 8.203,
	headerBold: 1.035,
	lineHeight: 20,
	proseFallback: 8.6,
} as const;

export const TABLE_BUDGET_PX = 528;

export const TABLE_FLOOR_CAP_SHARE = 1 / 3;

export const TABLE_NARROW_TIER_PX = TABLE_BUDGET_PX;

export const TABLE_MIDDLE_TIER_PX = 768;

export const TABLE_WIDE_TIER_PX = 960;
export const TABLE_MAX_COLUMNS = 3;
export const TABLE_MAX_CELL_CHARS = 120;
export const TABLE_MAX_IDENT_CHARS = 24;

const TABLE_FIXED_REFERENCE_PX = TABLE_BUDGET_PX;

export interface Piece {
	readonly text: string;
	readonly code: boolean;
}

function pieceWidth(piece: Piece, bold: boolean): number {
	if (piece.code) {
		return piece.text.length * PX.codeGlyph;
	}
	let width = 0;
	for (const character of piece.text) {
		width += glyphs.prose[character] ?? PX.proseFallback;
	}
	return bold ? width * PX.headerBold : width;
}

function atoms(parts: ReadonlyArray<Piece>): Array<Piece> {
	const out: Array<Piece> = [];
	for (const part of parts) {
		for (const fragment of part.text.split(/(?<=\s)|(?<=-)(?!\d)/u)) {
			out.push({text: fragment, code: part.code});
		}
	}
	return out;
}

export function minContentPx(parts: ReadonlyArray<Piece>, bold = false): number {
	let best = 0;
	let runWidth = 0;
	let runHasCode = false;
	const flush = (): void => {
		const width = runWidth + (runHasCode ? PX.codeChrome : 0);
		if (width > best) {
			best = width;
		}
		runWidth = 0;
		runHasCode = false;
	};
	for (const atom of atoms(parts)) {
		runWidth += pieceWidth({text: atom.text.replace(/\s+$/u, ''), code: atom.code}, bold);
		if (atom.code) {
			runHasCode = true;
		}
		if (/[\s-]$/u.test(atom.text)) {
			flush();
		}
	}
	flush();
	return best;
}

export function maxContentPx(parts: ReadonlyArray<Piece>, bold = false): number {
	let width = 0;
	for (const part of parts) {
		width += pieceWidth(part, bold);
		if (part.code) {
			width += PX.codeChrome;
		}
	}
	return width;
}

export interface ColumnDemandPx {
	readonly minPx: number;

	readonly maxPx: number;
}

export function columnWidthPercents(
	demands: ReadonlyArray<ColumnDemandPx>,
	referencePx: number = TABLE_FIXED_REFERENCE_PX,
): Array<number> {
	const cap = referencePx * TABLE_FLOOR_CAP_SHARE;
	const floors = demands.map((demand) => Math.min(demand.minPx + PX.cellPad, cap));
	const wants = demands.map((demand, index) => Math.max(demand.maxPx + PX.cellPad, floors[index]));
	const floorTotal = floors.reduce((a, b) => a + b, 0);
	if (floorTotal >= referencePx) {
		const wantTotal = wants.reduce((a, b) => a + b, 0);
		return toPercents(floors.map((floor, index) => Math.max(floor, (wants[index] / wantTotal) * referencePx)));
	}
	const target = [...floors];
	let slack = referencePx - floorTotal;
	while (slack > 0.5) {
		const pressure = target.map((value, index) => Math.max(0, wants[index] - value));
		const total = pressure.reduce((a, b) => a + b, 0);
		if (total <= 0) {
			break;
		}
		const spend = Math.min(slack, total);
		for (const [index, value] of pressure.entries()) {
			target[index] += (spend * value) / total;
		}
		slack -= spend;
	}
	return toPercents(target);
}

function toPercents(target: ReadonlyArray<number>): Array<number> {
	const total = target.reduce((a, b) => a + b, 0);
	const percents = target.map((value) => Math.round((value / total) * 1000) / 10);
	let widest = 0;
	for (let index = 1; index < percents.length; index += 1) {
		if (percents[index] > percents[widest]) {
			widest = index;
		}
	}
	const drift = Math.round((100 - percents.reduce((a, b) => a + b, 0)) * 10) / 10;
	if (drift !== 0) {
		percents[widest] = Math.round((percents[widest] + drift) * 10) / 10;
	}
	return percents;
}

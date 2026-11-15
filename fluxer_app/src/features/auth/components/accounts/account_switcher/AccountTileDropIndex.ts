// SPDX-License-Identifier: AGPL-3.0-or-later

interface AccountSlotBox {
	readonly left: number;
	readonly top: number;
	readonly width: number;
	readonly height: number;
}

interface AccountGridPoint {
	readonly x: number;
	readonly y: number;
}

interface AccountGridRow {
	readonly first: number;
	readonly centerY: number;
	readonly centers: ReadonlyArray<number>;
}

export interface AccountGridLayout {
	readonly rows: ReadonlyArray<AccountGridRow>;
	readonly rowOf: ReadonlyArray<number>;
	readonly rtl: boolean;
}

export function buildAccountGridLayout(slots: ReadonlyArray<AccountSlotBox>, rtl: boolean): AccountGridLayout {
	const rows: Array<{first: number; top: number; centerY: number; centers: Array<number>}> = [];
	const rowOf: Array<number> = [];
	slots.forEach((slot, index) => {
		const centerX = slot.left + slot.width / 2;
		const row = rows.at(-1);
		if (row == null || Math.abs(slot.top - row.top) > slot.height / 2) {
			rows.push({
				first: index,
				top: slot.top,
				centerY: slot.top + slot.height / 2,
				centers: [rtl ? -centerX : centerX],
			});
		} else {
			row.centers.push(rtl ? -centerX : centerX);
		}
		rowOf.push(rows.length - 1);
	});
	return {rows, rowOf, rtl};
}

function positionInRow(row: AccountGridRow, u: number): number {
	const {first, centers} = row;
	const lastOffset = centers.length - 1;
	if (u < centers[0]) {
		return first - 0.5;
	}
	if (u > centers[lastOffset]) {
		return first + lastOffset + 0.5;
	}
	if (lastOffset === 0) {
		return first;
	}
	let offset = 0;
	while (offset + 1 < lastOffset && u >= centers[offset + 1]) {
		offset += 1;
	}
	return first + offset + (u - centers[offset]) / (centers[offset + 1] - centers[offset]);
}

export interface AccountDropTarget {
	readonly index: number;
	readonly row: number;
}

export function initialAccountDropTarget(layout: AccountGridLayout, index: number): AccountDropTarget {
	return {index, row: layout.rowOf[index] ?? 0};
}

function resolveIndex(position: number, row: number, indexRow: number, current: number): number {
	if (row > indexRow) {
		return Math.floor(position);
	}
	if (row < indexRow) {
		return Math.ceil(position);
	}
	if (position >= current + 1) {
		return Math.floor(position);
	}
	if (position <= current - 1) {
		return Math.ceil(position);
	}
	return current;
}

export function resolveAccountDropTarget(
	layout: AccountGridLayout,
	pointer: AccountGridPoint,
	current: AccountDropTarget,
): AccountDropTarget {
	const {rows, rowOf} = layout;
	const indexRow = rowOf[current.index];
	if (indexRow == null) {
		return current;
	}
	let row = Math.min(current.row, rows.length - 1);
	while (row + 1 < rows.length && pointer.y >= rows[row + 1].centerY) {
		row += 1;
	}
	while (row > 0 && pointer.y <= rows[row - 1].centerY) {
		row -= 1;
	}
	const position = positionInRow(rows[row], layout.rtl ? -pointer.x : pointer.x);
	return {index: resolveIndex(position, row, indexRow, current.index), row};
}

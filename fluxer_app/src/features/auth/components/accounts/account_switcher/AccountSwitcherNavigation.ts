// SPDX-License-Identifier: AGPL-3.0-or-later

const ACCOUNT_SWITCHER_TYPE_AHEAD_RESET_MS = 700;
export const ACCOUNT_SWITCHER_SEARCH_THRESHOLD = 13;

type GridKey = 'ArrowLeft' | 'ArrowRight' | 'ArrowUp' | 'ArrowDown' | 'Home' | 'End';

const GRID_KEYS: ReadonlySet<string> = new Set<GridKey>([
	'ArrowLeft',
	'ArrowRight',
	'ArrowUp',
	'ArrowDown',
	'Home',
	'End',
]);

export function isGridKey(key: string): key is GridKey {
	return GRID_KEYS.has(key);
}

interface TileRect {
	readonly left: number;
	readonly top: number;
	readonly width: number;
}

const ROW_TOLERANCE_PX = 1;

function rowTops(rects: ReadonlyArray<TileRect>): Array<number> {
	const tops: Array<number> = [];
	for (const rect of rects) {
		if (!tops.some((top) => Math.abs(top - rect.top) <= ROW_TOLERANCE_PX)) {
			tops.push(rect.top);
		}
	}
	return tops.sort((a, b) => a - b);
}

function rowOf(tops: ReadonlyArray<number>, rect: TileRect): number {
	return tops.findIndex((top) => Math.abs(top - rect.top) <= ROW_TOLERANCE_PX);
}

export interface GridMove {
	readonly rects: ReadonlyArray<TileRect>;
	readonly index: number;
	readonly key: GridKey;
	readonly rtl: boolean;
}

export function gridTargetIndex({rects, index, key, rtl}: GridMove): number {
	const count = rects.length;
	if (count === 0) {
		return -1;
	}
	const last = count - 1;
	const from = Math.min(Math.max(index, 0), last);
	switch (key) {
		case 'Home':
			return 0;
		case 'End':
			return last;
		case 'ArrowLeft':
			return Math.max(0, Math.min(last, from + (rtl ? 1 : -1)));
		case 'ArrowRight':
			return Math.max(0, Math.min(last, from + (rtl ? -1 : 1)));
		case 'ArrowUp':
		case 'ArrowDown': {
			const tops = rowTops(rects);
			const targetRow = rowOf(tops, rects[from]) + (key === 'ArrowDown' ? 1 : -1);
			if (targetRow < 0 || targetRow >= tops.length) {
				return from;
			}
			const center = rects[from].left + rects[from].width / 2;
			let best = from;
			let bestDistance = Number.POSITIVE_INFINITY;
			rects.forEach((rect, candidate) => {
				if (Math.abs(rect.top - tops[targetRow]) > ROW_TOLERANCE_PX) {
					return;
				}
				const distance = Math.abs(rect.left + rect.width / 2 - center);
				if (distance < bestDistance) {
					best = candidate;
					bestDistance = distance;
				}
			});
			return best;
		}
	}
}

export interface SlotKeyEvent {
	readonly code: string;
	readonly altKey: boolean;
	readonly ctrlKey: boolean;
	readonly metaKey: boolean;
	readonly shiftKey: boolean;
}

const SLOT_CODE_PATTERN = /^(?:Digit|Numpad)([1-9])$/;

export function slotIndexFromKeyEvent(event: SlotKeyEvent): number | null {
	if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
		return null;
	}
	const match = SLOT_CODE_PATTERN.exec(event.code);
	if (match == null) {
		return null;
	}
	return Number(match[1]) - 1;
}

export interface TypeAheadState {
	readonly buffer: string;
	readonly lastAt: number;
}

export const EMPTY_TYPE_AHEAD: TypeAheadState = Object.freeze({buffer: '', lastAt: 0});

export function advanceTypeAhead(state: TypeAheadState, character: string, now: number): TypeAheadState {
	const expired = now - state.lastAt > ACCOUNT_SWITCHER_TYPE_AHEAD_RESET_MS;
	const buffer = (expired ? '' : state.buffer) + character.toLocaleLowerCase();
	return {buffer, lastAt: now};
}

export function typeAheadIsActive(state: TypeAheadState, now: number): boolean {
	return state.buffer.length > 0 && now - state.lastAt <= ACCOUNT_SWITCHER_TYPE_AHEAD_RESET_MS;
}

function isRepeatedCharacter(buffer: string): boolean {
	return buffer.length > 1 && [...buffer].every((character) => character === buffer[0]);
}

export function typeAheadMatch(
	labels: ReadonlyArray<ReadonlyArray<string>>,
	buffer: string,
	fromIndex: number,
): number | null {
	if (buffer.length === 0 || labels.length === 0) {
		return null;
	}
	const repeated = isRepeatedCharacter(buffer);
	const query = repeated ? buffer[0] : buffer;
	const start = repeated || buffer.length === 1 ? fromIndex + 1 : fromIndex;
	for (let offset = 0; offset < labels.length; offset++) {
		const index = (((start + offset) % labels.length) + labels.length) % labels.length;
		if (labels[index].some((label) => label.toLocaleLowerCase().startsWith(query))) {
			return index;
		}
	}
	return null;
}

export function isTypeAheadCharacter(event: {
	readonly key: string;
	readonly altKey: boolean;
	readonly ctrlKey: boolean;
	readonly metaKey: boolean;
}): boolean {
	if (event.altKey || event.ctrlKey || event.metaKey) {
		return false;
	}
	return [...event.key].length === 1 && event.key.trim().length === 1;
}

export function accountMatchesSearch(labels: ReadonlyArray<string>, query: string): boolean {
	const needle = query.trim().toLocaleLowerCase();
	if (needle.length === 0) {
		return true;
	}
	return labels.some((label) => label.toLocaleLowerCase().includes(needle));
}

export const SwitcherEscapeAction = Object.freeze({
	CLEAR_SEARCH: 'clear_search',
	LEAVE_MANAGE: 'leave_manage',
	LEAVE_AUTH: 'leave_auth',
	CLOSE: 'close',
} as const);

export type SwitcherEscapeAction = (typeof SwitcherEscapeAction)[keyof typeof SwitcherEscapeAction];

export interface SwitcherEscapeState {
	readonly hasSearchText: boolean;
	readonly manage: boolean;
	readonly auth: boolean;
}

export function switcherEscapeAction(state: SwitcherEscapeState): SwitcherEscapeAction {
	if (state.auth) {
		return SwitcherEscapeAction.LEAVE_AUTH;
	}
	if (state.hasSearchText) {
		return SwitcherEscapeAction.CLEAR_SEARCH;
	}
	if (state.manage) {
		return SwitcherEscapeAction.LEAVE_MANAGE;
	}
	return SwitcherEscapeAction.CLOSE;
}

export function focusKeyAfterRemoval(
	previousKeys: ReadonlyArray<string>,
	removedKey: string,
	nextKeys: ReadonlyArray<string>,
): string | null {
	if (nextKeys.length === 0) {
		return null;
	}
	const removedIndex = previousKeys.indexOf(removedKey);
	if (removedIndex < 0) {
		return nextKeys[0];
	}
	for (let index = removedIndex + 1; index < previousKeys.length; index++) {
		if (nextKeys.includes(previousKeys[index])) {
			return previousKeys[index];
		}
	}
	for (let index = removedIndex - 1; index >= 0; index--) {
		if (nextKeys.includes(previousKeys[index])) {
			return previousKeys[index];
		}
	}
	return nextKeys[0];
}

export interface InitialFocusInput {
	readonly accountKeys: ReadonlyArray<string>;
	readonly requestedKey: string | null;
	readonly mostRecentOtherKey: string | null;
	readonly currentKey: string | null;
}

export function initialFocusKey({
	accountKeys,
	requestedKey,
	mostRecentOtherKey,
	currentKey,
}: InitialFocusInput): string | null {
	for (const candidate of [requestedKey, mostRecentOtherKey, currentKey]) {
		if (candidate != null && accountKeys.includes(candidate)) {
			return candidate;
		}
	}
	return accountKeys[0] ?? null;
}

const AVATAR_MEDIA_SIZES = [160, 240, 320, 480] as const;
export type SwitcherAvatarMediaSize = (typeof AVATAR_MEDIA_SIZES)[number];

const ROOT_FONT_SIZE_PX = 16;
const PHONE_BREAKPOINT_PX = 42 * ROOT_FONT_SIZE_PX;

function clamp(min: number, value: number, max: number): number {
	return Math.min(max, Math.max(min, value));
}

export function switcherTileSizePx(viewportWidth: number): number {
	if (viewportWidth < PHONE_BREAKPOINT_PX) {
		return clamp(5.75 * ROOT_FONT_SIZE_PX, viewportWidth * 0.26, 7.5 * ROOT_FONT_SIZE_PX);
	}
	return clamp(6.5 * ROOT_FONT_SIZE_PX, 4.75 * ROOT_FONT_SIZE_PX + viewportWidth * 0.05, 9.75 * ROOT_FONT_SIZE_PX);
}

export function switcherAvatarMediaSize(tileSizePx: number, devicePixelRatio: number): SwitcherAvatarMediaSize {
	const target = tileSizePx * clamp(1, devicePixelRatio, 3);
	for (const size of AVATAR_MEDIA_SIZES) {
		if (size >= target) {
			return size;
		}
	}
	return AVATAR_MEDIA_SIZES[AVATAR_MEDIA_SIZES.length - 1];
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {compareStrings} from '@app/lib/list/CompareStrings';
import {compare} from '@fluxer/snowflake/src/SnowflakeUtils';

interface OrderedAccount {
	readonly storageKey: string;
	readonly userId: string;
	readonly switcherPosition?: number;
}

function compareAccountAge(a: OrderedAccount, b: OrderedAccount): number {
	const ageDifference = compare(a.userId, b.userId);
	if (ageDifference !== 0) {
		return ageDifference;
	}
	return compareStrings(a.storageKey, b.storageKey);
}

export function compareSwitcherOrder(a: OrderedAccount, b: OrderedAccount): number {
	const aPosition = a.switcherPosition;
	const bPosition = b.switcherPosition;
	if (aPosition !== undefined && bPosition !== undefined) {
		return aPosition !== bPosition ? aPosition - bPosition : compareAccountAge(a, b);
	}
	if (aPosition !== undefined) {
		return -1;
	}
	if (bPosition !== undefined) {
		return 1;
	}
	return compareAccountAge(a, b);
}

export function mergeSwitcherOrder(
	allKeys: ReadonlyArray<string>,
	reorderedKeys: ReadonlyArray<string>,
): Array<string> {
	const reordered = new Set(reorderedKeys);
	const queue = reorderedKeys.filter((key) => allKeys.includes(key));
	return allKeys.map((key) => (reordered.has(key) ? (queue.shift() ?? key) : key));
}

export function moveKey(keys: ReadonlyArray<string>, key: string, targetIndex: number): Array<string> {
	const from = keys.indexOf(key);
	if (from < 0) {
		return [...keys];
	}
	const to = Math.max(0, Math.min(keys.length - 1, targetIndex));
	const next = keys.filter((candidate) => candidate !== key);
	next.splice(to, 0, key);
	return next;
}

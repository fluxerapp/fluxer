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

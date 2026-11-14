// SPDX-License-Identifier: AGPL-3.0-or-later

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

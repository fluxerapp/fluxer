// SPDX-License-Identifier: AGPL-3.0-or-later

function stripWeakness(entityTag: string): string {
	return entityTag.startsWith('W/') ? entityTag.slice(2) : entityTag;
}

export function entityTagMatches(ifNoneMatch: string, etag: string): boolean {
	const target = stripWeakness(etag.trim());
	return ifNoneMatch
		.split(',')
		.map((candidate) => candidate.trim())
		.some((candidate) => candidate === '*' || stripWeakness(candidate) === target);
}

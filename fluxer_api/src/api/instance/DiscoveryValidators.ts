// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {entityTagMatches} from '../utils/EntityTag';

export interface DiscoveryValidators {
	etag: string;
	lastModified: Date;
}

export function nextDiscoveryValidators(document: unknown, previous: DiscoveryValidators | null): DiscoveryValidators {
	const etag = `"${createHash('sha256').update(JSON.stringify(document)).digest('hex')}"`;
	if (previous !== null && previous.etag === etag) {
		return previous;
	}
	return {etag, lastModified: new Date(Math.floor(Date.now() / 1000) * 1000)};
}

export function isDiscoveryNotModified(
	validators: DiscoveryValidators,
	ifNoneMatch: string | undefined,
	ifModifiedSince: string | undefined,
): boolean {
	if (ifNoneMatch !== undefined) {
		return entityTagMatches(ifNoneMatch, validators.etag);
	}
	if (ifModifiedSince === undefined) {
		return false;
	}
	const since = Date.parse(ifModifiedSince);
	return !Number.isNaN(since) && validators.lastModified.getTime() <= since;
}

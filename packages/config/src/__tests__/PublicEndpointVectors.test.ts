// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {normalizePublicEndpoint} from '@fluxer/config/src/EndpointDerivation';
import {describe, expect, test} from 'vitest';

interface PublicEndpointVector {
	url: string;
	base_domain: string;
	public_port: number | null;
	normalized: string;
}

const VECTORS_PATH = path.resolve(
	path.dirname(fileURLToPath(import.meta.url)),
	'../../../../fluxer_common/src/testdata/public_endpoint_vectors.json',
);

function readVectors(): Array<PublicEndpointVector> {
	const parsed: unknown = JSON.parse(readFileSync(VECTORS_PATH, 'utf8'));
	if (!Array.isArray(parsed) || parsed.length === 0) {
		throw new Error(`no public endpoint vectors in ${VECTORS_PATH}`);
	}
	return parsed.map((vector, index) => {
		if (
			typeof vector?.url !== 'string' ||
			typeof vector?.base_domain !== 'string' ||
			typeof vector?.normalized !== 'string' ||
			(vector?.public_port !== null && typeof vector?.public_port !== 'number')
		) {
			throw new Error(`malformed public endpoint vector at index ${index} in ${VECTORS_PATH}`);
		}
		return vector as PublicEndpointVector;
	});
}

const vectors = readVectors();

describe('normalizePublicEndpoint shared vectors', () => {
	test('reads the vector file the Rust conformance test reads', () => {
		expect(vectors.length).toBeGreaterThan(0);
	});
	test.each(vectors)('$url @ $base_domain port $public_port', (vector) => {
		expect(normalizePublicEndpoint(vector.url, vector.base_domain, vector.public_port ?? undefined)).toBe(
			vector.normalized,
		);
	});
});

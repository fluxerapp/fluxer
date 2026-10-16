// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {Config} from '@app/api/Config';
import {profilePseudonym} from '@app/api/user/ProfileVisibility';
import {NON_SELF_HOSTED_RESERVED_DISCRIMINATORS} from '@fluxer/constants/src/DiscriminatorConstants';
import {describe, expect, it} from 'vitest';

interface PseudonymVector {
	secret: string;
	user_id: string;
	username: string;
	discriminator: string;
}

interface PseudonymFixture {
	development_secret: string;
	reserved_discriminators: Array<number>;
	vectors: Array<PseudonymVector>;
}

const VECTORS_PATH = path.resolve(
	path.dirname(fileURLToPath(import.meta.url)),
	'../../../../../fluxer_common/src/testdata/profile_pseudonym_vectors.json',
);

const fixture = JSON.parse(readFileSync(VECTORS_PATH, 'utf8')) as PseudonymFixture;

describe('profile pseudonym vectors shared with the users service', () => {
	it('has vectors', () => {
		expect(fixture.vectors.length).toBeGreaterThan(0);
	});

	it.each(fixture.vectors)('$user_id under $secret', (vector) => {
		expect(profilePseudonym(vector.user_id, vector.secret)).toEqual({
			username: vector.username,
			discriminator: vector.discriminator,
		});
	});

	it('pins the reserved discriminators and the development secret', () => {
		expect([...NON_SELF_HOSTED_RESERVED_DISCRIMINATORS]).toEqual(fixture.reserved_discriminators);
		expect(Config.auth.profilePseudonymSecret).toBe(fixture.development_secret);
	});
});

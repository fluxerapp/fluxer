// SPDX-License-Identifier: AGPL-3.0-or-later

import {setCachedProductName} from '@app/api/instance/ProductName';
import {isUsernameTaken} from '@app/api/user/UniqueUsernames';
import {afterEach, describe, expect, it} from 'vitest';

const noUsers = {findUsersByUsername: async () => []};

describe('username-only mode', () => {
	afterEach(() => setCachedProductName(null));

	it('treats the system user name as taken', async () => {
		setCachedProductName('Tavern');
		expect(await isUsernameTaken(noUsers, 'tavern')).toBe(true);
		expect(await isUsernameTaken(noUsers, 'Tavern')).toBe(true);
		expect(await isUsernameTaken(noUsers, 'taverns')).toBe(false);
	});
});

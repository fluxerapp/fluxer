/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import type {User} from '~/records/UserRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import UserStore from '~/stores/UserStore';
import {parseQuery} from '~/utils/SearchQueryParser';

const CURRENT_USER_ID = 'current-user-id';
const CURRENT_USER: User = {
	id: CURRENT_USER_ID,
	username: 'currentuser',
	discriminator: '0001',
	avatar: null,
	flags: 0,
};

describe('SearchQueryParser', () => {
	beforeEach(() => {
		AuthenticationStore.setUserId(CURRENT_USER_ID);
		UserStore.users = {};
		UserStore.cacheUsers([CURRENT_USER]);
	});

	afterEach(() => {
		AuthenticationStore.setUserId(null);
		UserStore.users = {};
	});

	it('resolves @me for from filters', () => {
		const params = parseQuery('from:@me');
		expect(params.authorId).toEqual([CURRENT_USER_ID]);
	});

	it('resolves @me for mentions filters regardless of case', () => {
		const params = parseQuery('mentions:@Me');
		expect(params.mentions).toEqual([CURRENT_USER_ID]);
	});

	it('resolves @me for exclude filters', () => {
		const params = parseQuery('-from:@me');
		expect(params.excludeAuthorId).toEqual([CURRENT_USER_ID]);
	});
});

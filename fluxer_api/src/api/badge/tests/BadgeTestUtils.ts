// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {BadgeTypes} from '@fluxer/constants/src/BadgeConstants';
import type {BadgeMutationResponse, CreateBadgeRequest} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {BadgeResponse} from '@fluxer/schema/src/domains/badge/BadgeSchemas';

export const TEST_BADGE_ICON = '<svg viewBox="0 0 16 16"><circle cx="8" cy="8" r="8" fill="#4641d9"/></svg>';

export async function createTestBadge(
	harness: ApiTestHarness,
	token: string,
	overrides: Partial<CreateBadgeRequest> = {},
): Promise<BadgeResponse> {
	const {badge} = await createBuilder<BadgeMutationResponse>(harness, token)
		.post('/admin/badges')
		.body({type: BadgeTypes.USER, name: 'Test Badge', tooltip: 'A test badge', icon: TEST_BADGE_ICON, ...overrides})
		.expect(HTTP_STATUS.OK)
		.execute();
	return badge;
}

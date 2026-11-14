// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';

export async function setLimitOverride(
	harness: ApiTestHarness,
	limits: Partial<Record<LimitKey, number>>,
): Promise<() => Promise<void>> {
	const admin = await setUserACLs(harness, await createTestAccount(harness), [
		AdminACLs.AUTHENTICATE,
		AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
		AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
	]);
	const current = await createBuilder<{limit_config: LimitConfigSnapshot}>(harness, admin.token)
		.get('/admin/limit-config')
		.execute();
	const writeConfig = async (rules: LimitConfigSnapshot['rules']) => {
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: {traitDefinitions: current.limit_config.traitDefinitions, rules}})
			.execute();
	};
	await writeConfig([...current.limit_config.rules, {id: 'test_limit_override', limits}]);
	return async () => {
		await writeConfig(current.limit_config.rules);
	};
}

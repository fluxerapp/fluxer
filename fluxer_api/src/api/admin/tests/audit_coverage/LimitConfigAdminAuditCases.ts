// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';

interface LimitConfigReadResponse {
	limit_config: LimitConfigSnapshot;
}

async function readLimitConfig(harness: ApiTestHarness, admin: TestAccount): Promise<LimitConfigReadResponse> {
	return createBuilder<LimitConfigReadResponse>(harness, admin.token)
		.get('/admin/limit-config')
		.expect(HTTP_STATUS.OK)
		.execute();
}

export const LimitConfigAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/limit-config',
		async prepare({harness, admin}) {
			const current = await readLimitConfig(harness, admin);
			return {
				request: {path: '/admin/limit-config'},
				expected: {
					action: 'get_limit_config',
					targetType: 'limit_config',
					targetId: '0',
					metadata: {rule_count: String(current.limit_config.rules.length)},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/limit-config',
		async prepare({harness, admin}) {
			const current = await readLimitConfig(harness, admin);
			const rules = current.limit_config.rules.map((rule) => ({
				id: rule.id,
				filters: rule.filters,
				limits: rule.id === 'default' ? {...rule.limits, max_guilds: (rule.limits.max_guilds ?? 0) + 1} : rule.limits,
			}));
			const traitDefinitions = [...current.limit_config.traitDefinitions, 'coverage'];
			return {
				request: {
					path: '/admin/limit-config',
					body: {
						limit_config: {
							traitDefinitions,
							rules: [...rules, {id: 'coverage', filters: {traits: ['coverage']}, limits: {max_guilds: 5}}],
						},
					},
				},
				expected: {
					action: 'update_limit_config',
					targetType: 'limit_config',
					targetId: '0',
					metadata: {
						rule_count: String(rules.length + 1),
						changed_rule_count: '2',
						trait_definition_count: String(traitDefinitions.length),
					},
				},
			};
		},
	},
];

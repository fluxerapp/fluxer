// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AdminAuditCoverageCase,
	AdminAuditCoverageContext,
} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {expect} from 'vitest';

async function suspend(context: AdminAuditCoverageContext): Promise<{userId: string; decisionId: string}> {
	const target = await createTestAccount(context.harness);
	const {user} = await createBuilder<{user: {enforcement_decision_id: string}}>(context.harness, context.admin.token)
		.put(`/admin/users/${target.userId}/ban`)
		.header('X-Audit-Log-Reason', 'Coverage suspension')
		.body({
			duration_hours: 24,
			decision: {
				reason_code: DeletionReasons.SPAM,
				source: 'own_review',
				automation: 'none',
				statement: 'Coverage decision',
			},
		})
		.execute();
	return {userId: target.userId, decisionId: user.enforcement_decision_id};
}

export const EnforcementAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/users/:user_id/network-block-candidates',
		async prepare(context) {
			const target = await createTestAccount(context.harness, {ipAddress: '45.33.10.1'});
			return {
				request: {path: `/admin/users/${target.userId}/network-block-candidates`},
				expected: {
					action: 'list_network_block_candidates',
					targetType: 'user',
					targetId: target.userId,
					metadata: {result_count: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/enforcement-decisions',
		async prepare(context) {
			const {userId} = await suspend(context);
			return {
				request: {path: `/admin/enforcement-decisions?subject_type=user&subject_id=${userId}`},
				expected: {
					action: 'list_enforcement_decisions',
					targetType: 'user',
					targetId: userId,
					metadata: {list: 'subject', result_count: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/enforcement-decisions',
		name: 'review_queue',
		async prepare() {
			return {
				request: {path: '/admin/enforcement-decisions?review_queue=true'},
				expected: {
					action: 'list_enforcement_decisions',
					targetType: 'enforcement_decision',
					targetId: '0',
					metadata: {list: 'review_queue', result_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/enforcement-decisions/:decision_id',
		async prepare(context) {
			const {decisionId} = await suspend(context);
			return {
				request: {path: `/admin/enforcement-decisions/${decisionId}`},
				expected: {
					action: 'view_enforcement_decision',
					targetType: 'enforcement_decision',
					targetId: decisionId,
					metadata: {decision_id: decisionId, kind: 'suspension'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/enforcement-decisions/:decision_id/notes',
		async prepare(context) {
			const {decisionId} = await suspend(context);
			return {
				request: {path: `/admin/enforcement-decisions/${decisionId}/notes`, body: {body: 'Coverage note'}},
				expected: {
					action: 'annotate_enforcement_decision',
					targetType: 'enforcement_decision',
					targetId: decisionId,
					metadata: {decision_id: decisionId, note_id: expect.any(String)},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/enforcement-decisions/:decision_id/end',
		async prepare(context) {
			const {userId, decisionId} = await suspend(context);
			return {
				request: {path: `/admin/enforcement-decisions/${decisionId}/end`, body: {outcome: 'lifted'}},
				expected: {
					action: 'end_enforcement_decision',
					targetType: 'enforcement_decision',
					targetId: decisionId,
					metadata: {decision_id: decisionId, kind: 'suspension', outcome: 'lifted', subject_id: userId},
				},
			};
		},
	},
];

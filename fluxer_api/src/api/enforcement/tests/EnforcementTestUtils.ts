// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, listTestEmails, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import type {
	EnforcementDecisionInput,
	EnforcementDecisionResponse,
} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';

export const INTERNAL_NOTE = 'Checked the reported messages';

export type DecisionBody = Omit<Partial<EnforcementDecisionInput>, 'report_ids' | 'evidence'> & {
	report_ids?: Array<string>;
	evidence?: Array<{channel_id: string; message_id: string}>;
};

export function decisionBody(overrides: DecisionBody = {}): DecisionBody {
	return {
		reason_code: DeletionReasons.SPAM,
		source: 'own_review',
		automation: 'none',
		statement: 'You sent the same invite link to many people.',
		...overrides,
	};
}

export async function createStaff(harness: ApiTestHarness, acls: Array<string> = [AdminACLs.WILDCARD]) {
	return setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE, ...acls]);
}

export function suspendRequest(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	body: Record<string, unknown>,
	internalNote: string | null = INTERNAL_NOTE,
) {
	const builder = createBuilder<{
		user: {enforcement_decision_id: string | null; temp_banned_until: string | null};
		skipped_ips: Array<{ip: string; reason: string}>;
	}>(harness, staff.token)
		.put(`/admin/users/${userId}/ban`)
		.body(body);
	return internalNote === null ? builder : builder.header('X-Audit-Log-Reason', internalNote);
}

export async function suspend(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	decision: DecisionBody = decisionBody(),
	extra: Record<string, unknown> = {},
): Promise<string> {
	const {user} = await suspendRequest(harness, staff, userId, {duration_hours: 24, decision, ...extra})
		.expect(HTTP_STATUS.OK)
		.execute();
	return user.enforcement_decision_id!;
}

export async function warn(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	decision: DecisionBody = decisionBody(),
): Promise<string> {
	const {decision_id} = await createBuilder<{decision_id: string}>(harness, staff.token)
		.post(`/admin/users/${userId}/warnings`)
		.header('X-Audit-Log-Reason', INTERNAL_NOTE)
		.body({decision})
		.expect(HTTP_STATUS.OK)
		.execute();
	return decision_id;
}

export async function getDecision(
	harness: ApiTestHarness,
	staff: TestAccount,
	decisionId: string,
): Promise<EnforcementDecisionResponse> {
	return createBuilder<EnforcementDecisionResponse>(harness, staff.token)
		.get(`/admin/enforcement-decisions/${decisionId}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

export async function listDecisions(harness: ApiTestHarness, staff: TestAccount, userId: string) {
	const {decisions} = await createBuilder<{decisions: Array<EnforcementDecisionResponse>}>(harness, staff.token)
		.get(`/admin/enforcement-decisions?subject_type=user&subject_id=${userId}`)
		.expect(HTTP_STATUS.OK)
		.execute();
	return decisions;
}

export async function loadUser(account: TestAccount): Promise<User> {
	return (await getUserRepository().findUnique(createUserID(BigInt(account.userId))))!;
}

export async function emailsTo(harness: ApiTestHarness, account: TestAccount, type: string) {
	return (await listTestEmails(harness, {recipient: account.email})).filter((email) => email.type === type);
}

export function closeRequest(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	body: Record<string, unknown>,
	internalNote: string | null = INTERNAL_NOTE,
) {
	const builder = createBuilder<{
		user: {enforcement_decision_id: string | null; pending_deletion_at: string | null};
	}>(harness, staff.token)
		.put(`/admin/users/${userId}/deletion`)
		.body(body);
	return internalNote === null ? builder : builder.header('X-Audit-Log-Reason', internalNote);
}

export async function close(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	decision: DecisionBody = decisionBody(),
	days = 74,
): Promise<string> {
	const {user} = await closeRequest(harness, staff, userId, {
		days_until_deletion: days,
		basis: {type: 'enforcement', decision},
	})
		.expect(HTTP_STATUS.OK)
		.execute();
	return user.enforcement_decision_id!;
}

export function limitRequest(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	body: Record<string, unknown>,
) {
	return createBuilder<{user: {flags: string; premium_flags: number}}>(harness, staff.token)
		.put(`/admin/users/${userId}/feature-limits`)
		.header('X-Audit-Log-Reason', INTERNAL_NOTE)
		.body(body);
}

export async function limitFeatures(
	harness: ApiTestHarness,
	staff: TestAccount,
	userId: string,
	limit: string,
	decision: DecisionBody = decisionBody(),
	extra: Record<string, unknown> = {},
): Promise<string> {
	await limitRequest(harness, staff, userId, {limit, decision, ...extra})
		.expect(HTTP_STATUS.OK)
		.execute();
	const [latest] = await listDecisions(harness, staff, userId);
	return latest!.decision_id;
}

export function endDecision(
	harness: ApiTestHarness,
	staff: TestAccount,
	decisionId: string,
	outcome: 'lifted' | 'reversed' = 'lifted',
) {
	return createBuilder<EnforcementDecisionResponse>(harness, staff.token)
		.post(`/admin/enforcement-decisions/${decisionId}/end`)
		.header('X-Audit-Log-Reason', 'Appeal reviewed')
		.body({outcome});
}

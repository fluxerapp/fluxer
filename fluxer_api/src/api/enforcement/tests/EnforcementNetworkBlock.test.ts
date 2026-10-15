// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {resetIpBanExemptionsForTesting} from '@app/api/ban/IpBanExemptions';
import {getConfig} from '@app/api/Config';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import {formatNoticeDate} from '@app/api/enforcement/NoticeDates';
import {
	close,
	createStaff,
	type DecisionBody,
	decisionBody,
	endDecision,
	getDecision,
	INTERNAL_NOTE,
	loadUser,
	suspend,
	suspendRequest,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {BannedIps} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

interface ValidationErrorBody {
	errors: Array<{path: string; code: string}>;
}

interface RestrictionResponse {
	user: {enforcement_decision_id: string | null};
	skipped_ips: Array<{ip: string; reason: string}>;
}

interface BlockErrorBody {
	code: string;
	message: string;
	ip_address: string;
	appeal_email: string | null;
	appeals_supported: boolean;
	ban_kind: string;
	expires_at: string | null;
	reason: {title: string; url: string | null} | null;
}

const TARGET_IP = '45.33.10.1';
const DAY = ms('1 day');

describe('Enforcement network blocks', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;
	const adminRepository = new AdminRepository();

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		ipBanCache.resetCaches();
		staff = await createStaff(harness);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function blockingSuspension(
		target: TestAccount,
		ips: Array<string>,
		{decision = decisionBody(), days}: {decision?: DecisionBody; days?: number} = {},
	) {
		return suspendRequest(harness, staff, target.userId, {
			duration_hours: 0,
			decision,
			block_networks: {ips, days},
		});
	}

	async function refusedBlock(target: TestAccount, ips: Array<string>): Promise<string> {
		const body = await createBuilder<ValidationErrorBody>(harness, staff.token)
			.put(`/admin/users/${target.userId}/ban`)
			.header('X-Audit-Log-Reason', INTERNAL_NOTE)
			.body({duration_hours: 0, decision: decisionBody(), block_networks: {ips}})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		return body.errors[0]!.code;
	}

	async function requestFrom(ip: string, path = '/users/@me') {
		return createBuilderWithoutAuth<BlockErrorBody>(harness).get(path).header('x-forwarded-for', ip).executeRaw();
	}

	async function insertPlainBlock(ip: string, expiresAt: Date): Promise<void> {
		await upsertOne(
			BannedIps.insertWithTtl(
				{
					ip,
					ban_kind: 'temporary_24h',
					reason: 'platform_admin_enforcement',
					expires_at: expiresAt,
					created_at: new Date(),
					decision_id: null,
				},
				Math.ceil((expiresAt.getTime() - Date.now()) / 1000),
			),
		);
	}

	describe('writing blocks', () => {
		test('a permanent suspension blocks recent networks for 90 days and the notice names the end', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const before = Date.now();
			const response = await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
			expect(response.skipped_ips).toEqual([]);
			const decisionId = response.user.enforcement_decision_id!;

			const [row] = await adminRepository.listDecisionIpBans(BigInt(decisionId));
			expect(row?.ip).toBe(TARGET_IP);
			expect(row?.kind).toBe('temporary_24h');
			const expiresAt = row!.expiresAt!.getTime();
			expect(expiresAt).toBeGreaterThanOrEqual(before + 90 * DAY);
			expect(expiresAt).toBeLessThanOrEqual(Date.now() + 90 * DAY);

			const decision = await getDecision(harness, staff, decisionId);
			expect(decision.network_blocks).toEqual([{ip: TARGET_IP, expires_at: row!.expiresAt!.toISOString()}]);
			const line = `We also blocked access from networks this account used until ${formatNoticeDate(row!.expiresAt!, 'en-US')}.`;
			expect(decision.notice_text).toContain(line);
			expect(decision.notice.effects).toContain(line);
			expect(decision.notice_text).not.toContain(TARGET_IP);
		});

		test('the block length can be set and is recorded on the audit row', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const response = await blockingSuspension(target, [TARGET_IP], {days: 7}).expect(HTTP_STATUS.OK).execute();
			const [row] = await adminRepository.listDecisionIpBans(BigInt(response.user.enforcement_decision_id!));
			expect(row!.expiresAt!.getTime()).toBeLessThanOrEqual(Date.now() + 7 * DAY);
			expect(row!.expiresAt!.getTime()).toBeGreaterThan(Date.now() + 6 * DAY);
		});

		test('only a permanent suspension or a closure can block networks', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const body = await createBuilder<ValidationErrorBody>(harness, staff.token)
				.put(`/admin/users/${target.userId}/ban`)
				.header('X-Audit-Log-Reason', INTERNAL_NOTE)
				.body({duration_hours: 24, decision: decisionBody(), block_networks: {ips: [TARGET_IP]}})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(body.errors[0]).toMatchObject({
				path: 'block_networks',
				code: ValidationErrorCodes.ENFORCEMENT_NETWORK_BLOCK_NOT_ALLOWED,
			});
			const holder = await createBuilder<ValidationErrorBody>(harness, staff.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.body({days_until_deletion: 14, basis: {type: 'holder_request'}, block_networks: {ips: [TARGET_IP]}})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(holder.errors[0]!.code).toBe(ValidationErrorCodes.ENFORCEMENT_NETWORK_BLOCK_NOT_ALLOWED);
			expect(await adminRepository.loadAllBannedIpEntries()).toEqual([]);
		});

		test('blocking networks needs the IP block and IP view permissions', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			staff = await createStaff(harness, [AdminACLs.USER_TEMP_BAN, AdminACLs.BAN_IP_ADD]);
			await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACL).execute();
			expect((await loadUser(target)).enforcementDecisionId).toBeNull();
		});

		function sightings(entries: Array<[string, number]>) {
			vi.spyOn(getUserRepository(), 'listAuthSessionTombstones').mockResolvedValue(
				entries.map(([ip, lastUsed]) => ({clientIp: ip, approximateLastUsedAt: new Date(lastUsed)})) as never,
			);
		}

		test('only addresses the account used in the last 30 days can be blocked', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			sightings([['45.33.20.1', Date.now() - 40 * DAY]]);
			expect(await refusedBlock(target, ['45.33.20.1'])).toBe(ValidationErrorCodes.ENFORCEMENT_NETWORK_UNKNOWN);
			expect(await refusedBlock(target, ['45.33.30.1'])).toBe(ValidationErrorCodes.ENFORCEMENT_NETWORK_UNKNOWN);
			await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
		});

		test('exempt addresses and special-purpose ranges are skipped and listed', async () => {
			const config = getConfig();
			const exempt = config.ipBanExemptIps;
			config.ipBanExemptIps = ['45.33.40.1'];
			resetIpBanExemptionsForTesting();
			try {
				const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
				sightings([
					['45.33.40.1', Date.now()],
					['100.64.1.1', Date.now()],
					['192.168.1.5', Date.now()],
				]);
				const response = await blockingSuspension(target, ['45.33.40.1', '100.64.1.1', '192.168.1.5', TARGET_IP])
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(response.skipped_ips).toEqual([
					{ip: '45.33.40.1', reason: 'exempt'},
					{ip: '100.64.1.1', reason: 'special_purpose'},
					{ip: '192.168.1.5', reason: 'special_purpose'},
				]);
				expect((await adminRepository.loadAllBannedIpEntries()).map((entry) => entry.ip)).toEqual([TARGET_IP]);
			} finally {
				config.ipBanExemptIps = exempt;
				resetIpBanExemptionsForTesting();
			}
		});

		test('an address shared with an unrestricted account is refused, IPv6 by /64', async () => {
			const ipv6 = '2a01:e0a:d10:95b0::1';
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			await createTestAccount(harness, {ipAddress: TARGET_IP});
			expect(await refusedBlock(target, [TARGET_IP])).toBe(ValidationErrorCodes.ENFORCEMENT_NETWORK_SHARED);

			const v6Target = await createTestAccount(harness, {ipAddress: ipv6});
			await createTestAccount(harness, {ipAddress: '2a01:e0a:d10:95b0:abcd::7'});
			expect(await refusedBlock(v6Target, [ipv6])).toBe(ValidationErrorCodes.ENFORCEMENT_NETWORK_SHARED);
			expect(await adminRepository.loadAllBannedIpEntries()).toEqual([]);
		});

		test('accounts that are themselves suspended or closed do not count as sharing', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const suspended = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const closed = await createTestAccount(harness, {ipAddress: TARGET_IP});
			await suspend(harness, staff, suspended.userId);
			await close(harness, staff, closed.userId);

			const candidates = await createBuilder<{
				networks: Array<{ip: string; skip_reason: string | null; other_unrestricted_accounts: number}>;
			}>(harness, staff.token)
				.get(`/admin/users/${target.userId}/network-block-candidates`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(candidates.networks).toEqual([
				{ip: TARGET_IP, last_seen_at: expect.any(String), skip_reason: null, other_unrestricted_accounts: 0},
			]);
			await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
		});

		test('a closure blocks networks too', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const {user} = await createBuilder<RestrictionResponse>(harness, staff.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.header('X-Audit-Log-Reason', INTERNAL_NOTE)
				.body({
					days_until_deletion: 74,
					basis: {type: 'enforcement', decision: decisionBody()},
					block_networks: {ips: [TARGET_IP]},
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await adminRepository.listDecisionIpBans(BigInt(user.enforcement_decision_id!))).toHaveLength(1);
		});
	});

	describe('linked rows and the blocklist', () => {
		test('a plain write never overwrites a linked row', async () => {
			await adminRepository.banIpForDecision(TARGET_IP, 42n, new Date(Date.now() + 90 * DAY));
			await adminRepository.banIp(TARGET_IP);
			await adminRepository.banIpTemp(TARGET_IP, 60);
			const [row] = await adminRepository.loadAllBannedIpEntries();
			expect(row?.decisionId).toBe(42n);
			expect(row?.kind).toBe('temporary_24h');

			await createBuilder(harness, staff.token)
				.post('/admin/blocklists/ip/entries')
				.body({ip: TARGET_IP})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.IP_BAN_LINKED_TO_DECISION)
				.execute();
		});

		test('a plain row becomes linked and keeps the later expiry, another decision keeps its row', async () => {
			const later = new Date(Date.now() + 200 * DAY);
			await insertPlainBlock(TARGET_IP, later);
			const linked = await adminRepository.banIpForDecision(TARGET_IP, 7n, new Date(Date.now() + 90 * DAY));
			expect(linked?.decisionId).toBe(7n);
			expect(linked?.expiresAt?.getTime()).toBe(later.getTime());
			expect(await adminRepository.banIpForDecision(TARGET_IP, 8n, new Date(Date.now() + DAY))).toBeNull();
			expect((await adminRepository.loadAllBannedIpEntries())[0]?.decisionId).toBe(7n);

			await adminRepository.banIp('45.33.50.1');
			expect(await adminRepository.banIpForDecision('45.33.50.1', 9n, new Date(Date.now() + DAY))).toBeNull();
		});

		test('the blocklist refuses to remove a linked row and the check names the decision', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const {user} = await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
			const decisionId = user.enforcement_decision_id!;
			const refused = await createBuilder<{decision_id: string}>(harness, staff.token)
				.delete(`/admin/blocklists/ip/entries/${encodeURIComponent(TARGET_IP)}`)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.IP_BAN_LINKED_TO_DECISION)
				.execute();
			expect(refused.decision_id).toBe(decisionId);
			const check = await createBuilder<{banned: boolean; decision_id: string | null}>(harness, staff.token)
				.get(`/admin/blocklists/ip/entries/${encodeURIComponent(TARGET_IP)}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(check).toMatchObject({banned: true, decision_id: decisionId});
		});

		test('ending the decision removes its rows and needs the IP removal permission', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const {user} = await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
			const decisionId = user.enforcement_decision_id!;
			const limited = await createStaff(harness, [AdminACLs.ENFORCEMENT_VIEW, AdminACLs.USER_TEMP_BAN]);
			await endDecision(harness, limited, decisionId)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACL)
				.execute();

			const ended = await endDecision(harness, staff, decisionId).expect(HTTP_STATUS.OK).execute();
			expect(ended.network_blocks).toEqual([]);
			expect(await adminRepository.loadAllBannedIpEntries()).toEqual([]);
			expect(ipBanCache.isBanned(TARGET_IP)).toBe(false);
		});

		test('a closure takes over the blocks of the suspension it replaces and reversing it removes them', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const {user} = await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
			const closureId = await close(harness, staff, target.userId);
			expect(await adminRepository.listDecisionIpBans(BigInt(user.enforcement_decision_id!))).toEqual([]);
			expect(await adminRepository.listDecisionIpBans(BigInt(closureId))).toHaveLength(1);
			const closure = await getDecision(harness, staff, closureId);
			expect(closure.network_blocks).toHaveLength(1);
			await endDecision(harness, staff, closureId, 'reversed').expect(HTTP_STATUS.OK).execute();
			expect(await adminRepository.loadAllBannedIpEntries()).toEqual([]);
			await endDecision(harness, staff, user.enforcement_decision_id!, 'reversed').expect(HTTP_STATUS.OK).execute();
		});

		test('a temporary suspension that replaces a blocking one removes its blocks', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
			await suspend(harness, staff, target.userId);
			expect(await adminRepository.loadAllBannedIpEntries()).toEqual([]);
		});

		test('executing the deletion removes the rows of every decision on the account', async () => {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			await blockingSuspension(target, [TARGET_IP]).expect(HTTP_STATUS.OK).execute();
			const closureId = await close(harness, staff, target.userId);
			const closed = await loadUser(target);
			const due = new Date(Date.now() - ms('1 minute'));
			await getUserRepository().updateDeletionSchedule(closed, {
				flags: closed.flags,
				pending_deletion_at: due,
				deletion_reason_code: closed.deletionReasonCode,
				deletion_scheduled_by: closed.deletionScheduledBy,
				deletion_scheduled_at: closed.deletionScheduledAt,
			});
			expect(await adminRepository.listDecisionIpBans(BigInt(closureId))).toHaveLength(1);
			await createBuilderWithoutAuth(harness)
				.post(`/test/worker/process-pending-deletion/${target.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await adminRepository.loadAllBannedIpEntries()).toEqual([]);
		});
	});

	describe('the block screen', () => {
		async function linkedBlock(reasonCode: DecisionBody['reason_code']): Promise<string> {
			const target = await createTestAccount(harness, {ipAddress: TARGET_IP});
			const {user} = await blockingSuspension(target, [TARGET_IP], {decision: decisionBody({reason_code: reasonCode})})
				.expect(HTTP_STATUS.OK)
				.execute();
			return user.enforcement_decision_id!;
		}

		test('a rule shown on the network screen gives its title and the appeals address', async () => {
			const decisionId = await linkedBlock(DeletionReasons.SPAM);
			expect(ipBanCache.getMatch(TARGET_IP)?.decision).toEqual({
				decisionId: BigInt(decisionId),
				reasonCode: DeletionReasons.SPAM,
			});
			const {response, json} = await requestFrom(TARGET_IP);
			expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(json.code).toBe(APIErrorCodes.GLOBAL_IP_TEMPORARILY_BANNED);
			expect(json.reason?.title).toBe('Spam');
			expect(json.appeal_email).toBe('appeals@fluxer.com');
			expect(json.appeals_supported).toBe(true);
			expect(json.message).toMatch(
				/^Fluxer blocked access from this network because of activity that broke its rules\. The block ends on /,
			);
			expect(json.message).not.toMatch(/platform admin|sign in/i);
			expect(JSON.stringify(json)).not.toContain(decisionId);
		});

		test('a withheld reason is byte-identical to a plain block', async () => {
			const expiresAt = new Date(Date.now() + 90 * DAY);
			await insertPlainBlock(TARGET_IP, expiresAt);
			await ipBanCache.refresh();
			const plain = await requestFrom(TARGET_IP);
			expect(plain.response.status).toBe(HTTP_STATUS.FORBIDDEN);

			const target = await createTestAccount(harness);
			const decisionId = await suspend(
				harness,
				staff,
				target.userId,
				decisionBody({reason_code: DeletionReasons.SCAM_OR_SOCIAL_ENGINEERING}),
			);
			await adminRepository.banIpForDecision(TARGET_IP, BigInt(decisionId), expiresAt);
			await ipBanCache.refresh();
			expect(ipBanCache.getMatch(TARGET_IP)?.decision?.reasonCode).toBe(DeletionReasons.SCAM_OR_SOCIAL_ENGINEERING);
			const withheld = await requestFrom(TARGET_IP);

			expect(withheld.text).toBe(plain.text);
			expect(withheld.json.reason).toBeNull();
			expect(withheld.json.appeal_email).toBe('support@fluxer.com');
		});

		test('self-hosted blocks give no contact and do not support appeals', async () => {
			const config = getConfig();
			const selfHosted = config.instance.selfHosted;
			config.instance.selfHosted = true;
			try {
				await linkedBlock(DeletionReasons.SPAM);
				const {json} = await requestFrom(TARGET_IP);
				expect(json.appeal_email).toBeNull();
				expect(json.appeals_supported).toBe(false);
				expect(json.message).not.toMatch(/@|Fluxer/);
			} finally {
				config.instance.selfHosted = selfHosted;
			}
		});

		test('instance discovery is served to a blocked network and an unknown sign-in gets the block', async () => {
			await linkedBlock(DeletionReasons.SPAM);
			const discovery = await requestFrom(TARGET_IP, '/.well-known/fluxer');
			expect(discovery.response.status).toBe(HTTP_STATUS.OK);
			const login = await createBuilderWithoutAuth<BlockErrorBody>(harness)
				.post('/auth/login')
				.header('x-forwarded-for', TARGET_IP)
				.body({email: 'nobody@example.com', password: 'a-strong-password'})
				.executeRaw();
			expect(login.response.status).toBe(HTTP_STATUS.FORBIDDEN);
			expect(login.json.code).toBe(APIErrorCodes.GLOBAL_IP_TEMPORARILY_BANNED);
		});
	});
});

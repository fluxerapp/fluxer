// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import type {IpInfoLookupResult} from '@pkgs/geoip/src/IpInfoService';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {setInjectedIpInfoService} from '../../middleware/ServiceMiddleware';
import {CassandraSuspiciousIpRepository} from '../../risk/SuspiciousIpRepository';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';

function createUniqueTestIp(): string {
	return `198.51.${randomInt(0, 256)}.${randomInt(1, 255)}`;
}

function ipInfoResult(ip: string, overrides: Partial<IpInfoLookupResult> = {}): IpInfoLookupResult {
	return {
		ip,
		available: true,
		riskNote: 'test',
		geo: {
			countryCode: 'US',
			countryName: 'United States',
			continent: 'North America',
			continentCode: 'NA',
			region: null,
			regionCode: null,
			city: null,
			postalCode: null,
			timezone: null,
			latitude: null,
			longitude: null,
			accuracyRadiusKm: null,
		},
		asn: {
			asn: 'AS64500',
			number: 64500,
			name: 'Test ISP',
			domain: null,
			type: null,
		},
		mobile: {
			name: null,
			mcc: null,
			mnc: null,
		},
		anonymous: {
			isAnonymous: false,
			providerName: null,
			isVpn: false,
			isProxy: false,
			isResidentialProxy: false,
			isTor: false,
			isRelay: false,
			percentDaysSeen: null,
		},
		flags: {
			isAnycast: false,
			isHosting: false,
			isMobile: false,
			isSatellite: false,
		},
		...overrides,
	};
}

describe('Admin Deletion Queue', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		setInjectedIpInfoService({
			async lookup(ip: string) {
				return ipInfoResult(ip);
			},
		});
	});
	afterEach(async () => {
		setInjectedIpInfoService(undefined);
		await harness?.shutdown();
	});
	test('admin scheduling queues deletion and rescheduling replaces the old Cassandra row', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		const userRepository = new UserRepository();
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const firstSchedule = await createBuilder<{
			user: {
				pending_deletion_at: string;
			};
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: 2, days_until_deletion: 60})
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		const secondSchedule = await createBuilder<{
			user: {
				pending_deletion_at: string;
			};
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: 2, days_until_deletion: 62})
			.execute();
		const firstDate = firstSchedule.user.pending_deletion_at.slice(0, 10);
		const secondDate = secondSchedule.user.pending_deletion_at.slice(0, 10);
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		expect(
			(await userRepository.findUsersPendingDeletionByDate(firstDate)).some(
				(row) => row.user_id.toString() === targetUser.userId,
			),
		).toBe(false);
		expect(
			(await userRepository.findUsersPendingDeletionByDate(secondDate)).some(
				(row) => row.user_id.toString() === targetUser.userId,
			),
		).toBe(true);
	});
	test('cancel deletion clears the queued KV entry', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const schedule = await createBuilder<{
			user: {
				pending_deletion_at: string;
			};
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: 1, days_until_deletion: 60})
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		await createBuilder<{
			user: {
				pending_deletion_at: string | null;
			};
		}>(harness, `${admin.token}`)
			.delete(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
		expect(
			(await new UserRepository().findUsersPendingDeletionByDate(schedule.user.pending_deletion_at.slice(0, 10))).some(
				(row) => row.user_id.toString() === targetUser.userId,
			),
		).toBe(false);
	});
	test('user-requested scheduled deletion does not ban user identifiers', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete', 'ban:ip:check', 'ban:email:check']);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: DeletionReasons.USER_REQUESTED, days_until_deletion: 14})
			.execute();
		const ipBan = await createBuilder<{banned: boolean}>(harness, `${admin.token}`)
			.get(`/admin/blocklists/ip/entries/${encodeURIComponent(targetIp)}`)
			.execute();
		const emailBan = await createBuilder<{banned: boolean}>(harness, `${admin.token}`)
			.get(`/admin/blocklists/email/entries/${encodeURIComponent(targetUser.email)}`)
			.execute();
		expect(ipBan.banned).toBe(false);
		expect(emailBan.banned).toBe(false);
	});
	test('moderation scheduled deletion bans email and marks IP suspicious without banning it', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete', 'ban:ip:check', 'ban:email:check']);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
			.execute();
		const ipBan = await createBuilder<{banned: boolean}>(harness, `${admin.token}`)
			.get(`/admin/blocklists/ip/entries/${encodeURIComponent(targetIp)}`)
			.execute();
		const emailBan = await createBuilder<{banned: boolean}>(harness, `${admin.token}`)
			.get(`/admin/blocklists/email/entries/${encodeURIComponent(targetUser.email)}`)
			.execute();
		const suspiciousIp = await new CassandraSuspiciousIpRepository().findActiveByIp(targetIp);
		expect(ipBan.banned).toBe(false);
		expect(emailBan.banned).toBe(true);
		expect(suspiciousIp?.source).toBe('scheduled_deletion');
		expect(suspiciousIp?.sourceUserId).toBe(targetUser.userId);
	});
	test('moderation scheduled deletion does not mark trusted paid VPN IPs suspicious', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		setInjectedIpInfoService({
			async lookup(ip: string) {
				return ipInfoResult(ip, {
					anonymous: {
						isAnonymous: true,
						providerName: 'Example Privacy Relay LLC',
						isVpn: true,
						isProxy: false,
						isResidentialProxy: false,
						isTor: false,
						isRelay: false,
						percentDaysSeen: null,
					},
				});
			},
		});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete', 'ban:ip:check', 'ban:email:check']);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
			.execute();
		const ipBan = await createBuilder<{banned: boolean}>(harness, `${admin.token}`)
			.get(`/admin/blocklists/ip/entries/${encodeURIComponent(targetIp)}`)
			.execute();
		const suspiciousIp = await new CassandraSuspiciousIpRepository().findActiveByIp(targetIp);
		expect(ipBan.banned).toBe(false);
		expect(suspiciousIp).toBeNull();
	});
	test('moderation scheduled deletion does not mark mobile carrier IPs suspicious', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		setInjectedIpInfoService({
			async lookup(ip: string) {
				return ipInfoResult(ip, {
					asn: {
						asn: 'AS64501',
						number: 64501,
						name: 'Test Mobile',
						domain: null,
						type: 'mobile',
					},
					mobile: {
						name: 'Test Mobile',
						mcc: '001',
						mnc: '01',
					},
					flags: {
						isAnycast: false,
						isHosting: false,
						isMobile: true,
						isSatellite: false,
					},
				});
			},
		});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete', 'ban:ip:check', 'ban:email:check']);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
			.execute();
		const ipBan = await createBuilder<{banned: boolean}>(harness, `${admin.token}`)
			.get(`/admin/blocklists/ip/entries/${encodeURIComponent(targetIp)}`)
			.execute();
		const suspiciousIp = await new CassandraSuspiciousIpRepository().findActiveByIp(targetIp);
		expect(ipBan.banned).toBe(false);
		expect(suspiciousIp).toBeNull();
	});
	test('rejects a scheduled deletion reason code outside the registry', async () => {
		const admin = await createTestAccount(harness);
		const targetUser = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const {json} = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
			}>;
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.body({reason_code: 999, days_until_deletion: 60})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors.some((entry) => entry.path === 'reason_code')).toBe(true);
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
	});
	test('accepts the highest scheduled deletion reason code of the registry', async () => {
		const admin = await createTestAccount(harness);
		const targetUser = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.body({reason_code: DeletionReasons.IMPERSONATION_OR_FAKE_IDENTITY, days_until_deletion: 60})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
	});
	test('rejects a bulk schedule_user_deletion job with a reason code outside the registry', async () => {
		const admin = await createTestAccount(harness);
		const targetUser = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const {json} = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
			}>;
		}>(harness, `${admin.token}`)
			.post('/admin/bulk-jobs')
			.body({task: 'schedule_user_deletion', user_ids: [targetUser.userId], reason_code: 0})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors.some((entry) => entry.path === 'reason_code')).toBe(true);
	});
});

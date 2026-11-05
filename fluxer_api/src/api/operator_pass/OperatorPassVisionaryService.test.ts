// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getCacheService, getOperatorPassWallService, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCertificateService} from '@app/api/operator_pass/OperatorPassCertificateService';
import {
	endOperatorPassesForDeletion,
	OperatorPassHoldingService,
} from '@app/api/operator_pass/OperatorPassHoldingService';
import {OperatorPassLabsRoleService} from '@app/api/operator_pass/OperatorPassLabsRole';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {
	describeOperatorPassVisionary,
	getOperatorPassVisionaryPassId,
	OperatorPassVisionaryService,
} from '@app/api/operator_pass/OperatorPassVisionaryService';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {OPERATOR_PASS_VISIONARY_BOX_VERSION} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {OPERATOR_PASS_TERMS_VERSION} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import {afterAll, beforeAll, expect, test, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;
const VISIONARY_SINCE = new Date('2025-03-14T09:30:00.000Z');

let harness: ApiTestHarness;
let priceRisesAt: Date | null;

beforeAll(async () => {
	harness = await createApiTestHarness();
	priceRisesAt = Config.operatorPass.priceRisesAt;
	Config.operatorPass.priceRisesAt = new Date(Date.now() + 30 * DAY_MS);
});

afterAll(async () => {
	Config.operatorPass.priceRisesAt = priceRisesAt;
	await harness.shutdown();
});

async function createVisionary(): Promise<UserID> {
	const account = await createTestAccount(harness);
	await createBuilder(harness, account.token)
		.post(`/test/users/${account.userId}/security-flags`)
		.body({email_verified: true})
		.execute();
	await createBuilder(harness, account.token)
		.post(`/test/users/${account.userId}/premium`)
		.body({
			premium_type: UserPremiumTypes.LIFETIME,
			premium_since: VISIONARY_SINCE.toISOString(),
			premium_lifetime_sequence: 42,
		})
		.execute();
	return createUserID(BigInt(account.userId));
}

function createServices() {
	const repository = new OperatorPassRepository();
	const emails = new TestEmailService();
	const userRepository = getUserRepository();
	const roleService = new OperatorPassLabsRoleService(
		{getMember: vi.fn(async () => null)} as unknown as ConstructorParameters<typeof OperatorPassLabsRoleService>[0],
		{members: {}} as unknown as ConstructorParameters<typeof OperatorPassLabsRoleService>[1],
	);
	const deliverToLabs = vi.spyOn(roleService, 'deliver');
	const wallService = getOperatorPassWallService();
	const dispatchPresence = vi.fn();
	const holdingService = new OperatorPassHoldingService({
		repository,
		userRepository,
		roleService,
		gatewayService: {dispatchPresence} as unknown as ConstructorParameters<
			typeof OperatorPassHoldingService
		>[0]['gatewayService'],
		emailService: emails,
		wallService,
		findPurchase: (purchaseId) => repository.findPurchase(purchaseId),
	});
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const visionary = new OperatorPassVisionaryService({
		repository,
		userRepository,
		holdingService,
		wallService,
		roleService,
		certificates,
		emailService: emails,
		cacheService: getCacheService(),
	});
	return {repository, emails, holdingService, visionary, certificates, deliverToLabs, dispatchPresence};
}

function lastVisionaryState(dispatchPresence: ReturnType<typeof vi.fn>, userId: UserID): string | undefined {
	const updates = dispatchPresence.mock.calls
		.map(([payload]) => payload as {userId: UserID; event: string; data: {visionary?: {state: string}}})
		.filter((payload) => payload.userId === userId && payload.event === 'OPERATOR_PASS_UPDATE');
	return updates.at(-1)?.data.visionary?.state;
}

async function turnOn(visionary: OperatorPassVisionaryService, userId: UserID) {
	const user = await getUserRepository().findUniqueAssert(userId);
	await visionary.turnOn({
		user,
		request: {
			certificate_name: 'Ada Lovelace',
			show_badge: false,
			terms_accepted: true,
			age_confirmed: true,
			terms_version: OPERATOR_PASS_TERMS_VERSION,
			box_text_version: OPERATOR_PASS_VISIONARY_BOX_VERSION,
		},
		boxText: 'I accept the Operator Pass terms.',
		clientIp: '203.0.113.9',
		userAgent: 'vitest',
	});
}

test('turning the pass on twice gives one pass, one certificate and one email, with the badge and wall off', async () => {
	const userId = await createVisionary();
	const {repository, emails, visionary, deliverToLabs} = createServices();
	await turnOn(visionary, userId);
	await turnOn(visionary, userId).catch(() => turnOn(visionary, userId));

	const passId = getOperatorPassVisionaryPassId(userId);
	const unit = await repository.findUnit(passId);
	expect(unit).toMatchObject({source: 'visionary', transferable: false, buyer_id: null, purchase_id: null});
	expect(unit?.delivered_at.toISOString()).toBe(VISIONARY_SINCE.toISOString());
	const pages = await repository.listCertificateVersions(passId);
	expect(pages).toHaveLength(1);
	expect(pages[0]).toMatchObject({source: 'visionary', phase: 'visionary'});
	expect(pages[0]?.paid_at.toISOString()).toBe(VISIONARY_SINCE.toISOString());
	expect((await repository.findVisionaryActivation(userId))?.certificate_number).toBe(pages[0]?.number);
	expect(emails.listSentEmails().filter((email) => email.type === 'operator_pass_visionary_turned_on')).toHaveLength(1);
	const user = await getUserRepository().findUniqueAssert(userId);
	expect(user.hiddenProfileBadges.has('operator_pass')).toBe(true);
	expect((await repository.findWallEntry(userId))?.opted_out).toBe(true);
	expect(deliverToLabs).toHaveBeenCalledWith(expect.objectContaining({id: userId}), null);
	expect((await repository.findOperatorPass(userId))?.granted_at.toISOString()).toBe(VISIONARY_SINCE.toISOString());
});

test('a turn-on that fails partway stays available and a retry finishes it with one email', async () => {
	const userId = await createVisionary();
	const {repository, emails, visionary, certificates} = createServices();
	vi.spyOn(certificates, 'issueVisionaryPage').mockRejectedValueOnce(new Error('render failed'));
	await expect(turnOn(visionary, userId)).rejects.toThrow('render failed');

	const user = await getUserRepository().findUniqueAssert(userId);
	const half = await repository.findVisionaryActivation(userId);
	expect(half?.activated_at).not.toBeNull();
	expect(
		describeOperatorPassVisionary({
			user,
			visionarySince: VISIONARY_SINCE,
			activation: half,
			removal: null,
			labsMember: false,
			certificateUrl: null,
		})?.state,
	).toBe('available');

	await turnOn(visionary, userId);
	const passId = getOperatorPassVisionaryPassId(userId);
	const pages = await repository.listCertificateVersions(passId);
	expect(pages).toHaveLength(1);
	const done = await repository.findVisionaryActivation(userId);
	expect(done?.completed_at).not.toBeNull();
	expect(done?.certificate_number).toBe(pages[0]?.number);
	expect(emails.listSentEmails().filter((email) => email.type === 'operator_pass_visionary_turned_on')).toHaveLength(1);
});

test('the pass shows the date of the Visionary purchase, not an earlier Plutonium start', async () => {
	const userId = await createVisionary();
	const paidAt = new Date('2025-09-02T12:00:00.000Z');
	const {repository, visionary} = createServices();
	vi.spyOn(getUserRepository(), 'findGiftCodesByRedeemer').mockResolvedValueOnce([
		{redeemedAt: paidAt, revokedAt: null, visionarySequenceNumber: 42, durationQuantity: 0},
	] as unknown as Awaited<ReturnType<ReturnType<typeof getUserRepository>['findGiftCodesByRedeemer']>>);
	await turnOn(visionary, userId);

	const passId = getOperatorPassVisionaryPassId(userId);
	const pages = await repository.listCertificateVersions(passId);
	expect(pages[0]?.paid_at.toISOString()).toBe(paidAt.toISOString());
	expect((await repository.findUnit(passId))?.delivered_at.toISOString()).toBe(paidAt.toISOString());
	expect((await repository.findVisionaryActivation(userId))?.visionary_since?.toISOString()).toBe(paidAt.toISOString());
});

test('the last pass update after turning on and after ending carries the final Visionary state', async () => {
	const userId = await createVisionary();
	const {visionary, dispatchPresence} = createServices();
	await turnOn(visionary, userId);
	expect(lastVisionaryState(dispatchPresence, userId)).toBe('on');

	await getUserRepository().patchUpsert(
		userId,
		{premium_type: null},
		(await getUserRepository().findUniqueAssert(userId)).toRow(),
	);
	expect(await visionary.endIfVisionaryEnded(userId)).toBe(true);
	expect(lastVisionaryState(dispatchPresence, userId)).toBe('ended');
});

test('an ended Visionary ends the pass and cancels its certificate page', async () => {
	const userId = await createVisionary();
	const {repository, emails, visionary} = createServices();
	await turnOn(visionary, userId);
	await getUserRepository().patchUpsert(
		userId,
		{premium_type: null},
		(await getUserRepository().findUniqueAssert(userId)).toRow(),
	);

	expect(await visionary.endIfVisionaryEnded(userId)).toBe(true);
	const passId = getOperatorPassVisionaryPassId(userId);
	expect(await repository.findUnit(passId)).toMatchObject({status: 'revoked', end_reason: 'visionary_ended'});
	expect((await repository.findLatestCertificate(passId))?.cancelled_at).not.toBeNull();
	expect((await repository.findVisionaryActivation(userId))?.end_reason).toBe('visionary_ended');
	expect(await repository.findOperatorPass(userId)).toBeNull();
	expect(emails.listSentEmails().some((email) => email.type === 'operator_pass_revoked')).toBe(true);
});

test('ending again after the end was not recorded sends one revoked email', async () => {
	const userId = await createVisionary();
	const {repository, emails, visionary} = createServices();
	await turnOn(visionary, userId);
	await getUserRepository().patchUpsert(
		userId,
		{premium_type: null},
		(await getUserRepository().findUniqueAssert(userId)).toRow(),
	);
	vi.spyOn(repository, 'updateVisionaryActivation').mockRejectedValueOnce(new Error('write lost'));
	await expect(visionary.endIfVisionaryEnded(userId)).rejects.toThrow('write lost');

	expect(await visionary.endIfVisionaryEnded(userId)).toBe(true);
	expect(await visionary.endIfVisionaryEnded(userId)).toBe(false);
	expect((await repository.findVisionaryActivation(userId))?.end_reason).toBe('visionary_ended');
	expect(emails.listSentEmails().filter((email) => email.type === 'operator_pass_revoked')).toHaveLength(1);
});

test('deleting the account clears the name and PDF of the Visionary certificate page', async () => {
	const userId = await createVisionary();
	const {repository, visionary} = createServices();
	await turnOn(visionary, userId);
	await endOperatorPassesForDeletion(repository, getCacheService(), userId);

	const page = await repository.findLatestCertificate(getOperatorPassVisionaryPassId(userId));
	expect(page?.holder_name).toBe('');
	expect(page?.pdf.length).toBe(0);
	const activation = await repository.findVisionaryActivation(userId);
	expect(activation).toMatchObject({consent_ip: null, consent_user_agent: null, end_reason: 'deletion'});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {clearTestEmails, createUniqueEmail, findLastTestEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {getChannel, sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ErrorResponse {
	code: string;
	retry_after?: number;
}

class RecordingExecutor implements CassandraQueryExecutorForTesting {
	readonly statements: Array<string> = [];
	private readonly inner = new InMemoryCassandraQueryExecutor();

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		this.statements.push(query.cql);
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
		this.statements.push(...queries.map((entry) => entry.query));
		await this.inner.executeBatch(queries, atomic);
	}

	reset(): void {
		this.inner.reset();
	}
}

async function requestCode(harness: ApiTestHarness): Promise<{email: string; code: string}> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-single-use');
	await createBuilderWithoutAuth(harness)
		.post('/reports/dsa/email/send')
		.body({email})
		.expect(HTTP_STATUS.OK)
		.execute();
	const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
	if (typeof code !== 'string') {
		throw new Error('No DSA verification code was sent');
	}
	return {email, code};
}

function send(harness: ApiTestHarness, email: string) {
	return createBuilderWithoutAuth<ErrorResponse>(harness).post('/reports/dsa/email/send').body({email});
}

async function lastSentCode(harness: ApiTestHarness): Promise<string | undefined> {
	return findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
}

async function countCodeEmails(harness: ApiTestHarness): Promise<number> {
	return (await listTestEmails(harness)).filter((sent) => sent.type === 'dsa_report_verification').length;
}

async function moveLastSend(email: string, sentAt: Date): Promise<void> {
	const repository = new ReportRepository();
	const row = await repository.getDsaEmailVerification(email.toLowerCase());
	if (!row) {
		throw new Error('No DSA verification row to move');
	}
	await repository.upsertDsaEmailVerification({...row, last_sent_at: sentAt});
}

function verify(harness: ApiTestHarness, email: string, code: string) {
	return createBuilderWithoutAuth<{ticket: string}>(harness).post('/reports/dsa/email/verify').body({email, code});
}

async function issueTicket(harness: ApiTestHarness): Promise<string> {
	const {email, code} = await requestCode(harness);
	const {ticket} = await verify(harness, email, code).expect(HTTP_STATUS.OK).execute();
	return ticket;
}

async function createMessageLinks(harness: ApiTestHarness, count: number): Promise<Array<string>> {
	const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
	const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
	const links: Array<string> = [];
	for (let index = 0; index < count; index++) {
		const message = await sendChannelMessage(harness, members[0].token, channel.id, `DSA notice target ${index}`);
		links.push(`https://web.fluxer.app/channels/${guild.id}/${channel.id}/${message.id}`);
	}
	return links;
}

function submitNotice(harness: ApiTestHarness, ticket: string, link: string) {
	return createBuilderWithoutAuth<{report_id: string}>(harness).post('/reports/dsa').body({
		ticket,
		report_type: 'message',
		category: 'hate_speech',
		message_link: link,
		additional_info: 'This message calls for violence against a group of people.',
		reporter_full_legal_name: 'Jane Doe',
		reporter_country_of_residence: 'DE',
	});
}

async function countReports(): Promise<number> {
	return (await new ReportRepository().listAllReportsPaginated(100)).length;
}

function hashCode(code: string): string {
	return createHash('sha256').update(code).digest('hex');
}

describe('DSA verification codes and tickets', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('a code verifies once', async () => {
		const {email, code} = await requestCode(harness);
		const {ticket} = await verify(harness, email, code).expect(HTTP_STATUS.OK).execute();
		expect(ticket).toMatch(/^[0-9a-f]{64}$/);
		expect(await new ReportRepository().getDsaEmailVerification(email.toLowerCase())).toBeNull();
		await verify(harness, email, code)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_VERIFICATION_CODE)
			.execute();
	});

	test('two verifications of one code at the same time issue one ticket', async () => {
		const {email, code} = await requestCode(harness);
		const results = await Promise.all([
			verify(harness, email, code).executeRaw(),
			verify(harness, email, code).executeRaw(),
		]);
		const statuses = results.map((result) => result.response.status).sort();
		expect(statuses).toEqual([HTTP_STATUS.OK, HTTP_STATUS.BAD_REQUEST]);
		const rejected = results.find((result) => result.response.status === HTTP_STATUS.BAD_REQUEST);
		expect((rejected?.json as ErrorResponse | undefined)?.code).toBe(APIErrorCodes.INVALID_DSA_VERIFICATION_CODE);
	});

	test('a wrong code keeps the sent code usable', async () => {
		const {email, code} = await requestCode(harness);
		await verify(harness, email, 'AAAA-AAAA')
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_VERIFICATION_CODE)
			.execute();
		await verify(harness, email, code).expect(HTTP_STATUS.OK).execute();
	});

	test('a second send within a minute is refused and the first code still verifies', async () => {
		const {email, code} = await requestCode(harness);
		const {response, json} = await send(harness, email.toUpperCase()).executeRaw();
		expect(response.status).toBe(429);
		const refusal = json as ErrorResponse;
		expect(refusal.code).toBe(APIErrorCodes.RATE_LIMITED);
		expect(refusal.retry_after).toBeGreaterThan(0);
		expect(refusal.retry_after).toBeLessThanOrEqual(60);
		const retryAfter = Number(response.headers.get('Retry-After'));
		expect(retryAfter).toBeGreaterThan(0);
		expect(retryAfter).toBeLessThanOrEqual(60);
		expect(await countCodeEmails(harness)).toBe(1);
		await verify(harness, email, code).expect(HTTP_STATUS.OK).execute();
	});

	test('a send refused by the resend wait does not count toward the hourly limit', async () => {
		const {email} = await requestCode(harness);
		for (let attempt = 0; attempt < 4; attempt++) {
			await send(harness, email).expect(429, APIErrorCodes.RATE_LIMITED).execute();
		}
		for (let resend = 0; resend < 2; resend++) {
			await moveLastSend(email, new Date(Date.now() - 61_000));
			await send(harness, email).expect(HTTP_STATUS.OK).execute();
		}
		expect(await countCodeEmails(harness)).toBe(3);
	});

	test('a send after a minute replaces the code', async () => {
		const {email, code} = await requestCode(harness);
		await moveLastSend(email, new Date(Date.now() - 61_000));
		await send(harness, email).expect(HTTP_STATUS.OK).execute();
		const resent = await lastSentCode(harness);
		expect(resent).toBeTruthy();
		expect(resent).not.toBe(code);
		expect(await countCodeEmails(harness)).toBe(2);
		await verify(harness, email, code)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_VERIFICATION_CODE)
			.execute();
		await verify(harness, email, resent!).expect(HTTP_STATUS.OK).execute();
	});

	test('an expired code does not hold back a new send', async () => {
		const email = createUniqueEmail('dsa-single-use');
		await clearTestEmails(harness);
		await new ReportRepository().upsertDsaEmailVerification({
			email_lower: email.toLowerCase(),
			code_hash: hashCode('ABCD-EFGH'),
			expires_at: new Date(Date.now() - 1000),
			last_sent_at: new Date(Date.now() - 5000),
		});
		await send(harness, email).expect(HTTP_STATUS.OK).execute();
		const code = await lastSentCode(harness);
		await verify(harness, email, code!).expect(HTTP_STATUS.OK).execute();
	});

	test('an expired code is rejected', async () => {
		const email = createUniqueEmail('dsa-single-use');
		const code = 'ABCD-EFGH';
		await new ReportRepository().upsertDsaEmailVerification({
			email_lower: email.toLowerCase(),
			code_hash: hashCode(code),
			expires_at: new Date(Date.now() - 1000),
			last_sent_at: new Date(Date.now() - 600_000),
		});
		await verify(harness, email, code)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_VERIFICATION_CODE)
			.execute();
	});

	test('a ticket files one report', async () => {
		const ticket = await issueTicket(harness);
		const [first, second] = await createMessageLinks(harness, 2);
		const before = await countReports();
		await submitNotice(harness, ticket, first).expect(HTTP_STATUS.OK).execute();
		expect(await new ReportRepository().getDsaTicket(ticket)).toBeNull();
		await submitNotice(harness, ticket, second)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_TICKET)
			.execute();
		expect(await countReports()).toBe(before + 1);
	});

	test('two notices sent at the same time with one ticket file one report', async () => {
		const ticket = await issueTicket(harness);
		const [first, second] = await createMessageLinks(harness, 2);
		const before = await countReports();
		const results = await Promise.all([
			submitNotice(harness, ticket, first).executeRaw(),
			submitNotice(harness, ticket, second).executeRaw(),
		]);
		const statuses = results.map((result) => result.response.status).sort();
		expect(statuses).toEqual([HTTP_STATUS.OK, HTTP_STATUS.BAD_REQUEST]);
		const rejected = results.find((result) => result.response.status === HTTP_STATUS.BAD_REQUEST);
		expect((rejected?.json as ErrorResponse | undefined)?.code).toBe(APIErrorCodes.INVALID_DSA_TICKET);
		expect(await countReports()).toBe(before + 1);
	});

	test('an expired ticket is rejected', async () => {
		const ticket = 'e'.repeat(64);
		await new ReportRepository().createDsaTicket({
			ticket,
			email_lower: createUniqueEmail('dsa-single-use'),
			expires_at: new Date(Date.now() - 1000),
			created_at: new Date(Date.now() - 3_600_000),
		});
		const [link] = await createMessageLinks(harness, 1);
		const before = await countReports();
		await submitNotice(harness, ticket, link)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_TICKET)
			.execute();
		expect(await countReports()).toBe(before);
	});

	test('a rejected notice keeps the ticket', async () => {
		const ticket = await issueTicket(harness);
		const [link] = await createMessageLinks(harness, 1);
		await submitNotice(harness, ticket, 'https://web.fluxer.app/channels/1/2/3')
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		await submitNotice(harness, ticket, link).expect(HTTP_STATUS.OK).execute();
	});
});

describe('DSA ticket and code consumption in the repository', () => {
	let executor: RecordingExecutor;

	beforeEach(() => {
		executor = new RecordingExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	test('ticket and code writes are conditional', async () => {
		const repository = new ReportRepository();
		const email = createUniqueEmail('dsa-single-use');
		await repository.createDsaTicket({
			ticket: 'c'.repeat(64),
			email_lower: email,
			expires_at: new Date(Date.now() + 3_600_000),
			created_at: new Date(),
		});
		for (const code of ['FRST-CODE', 'SCND-CODE']) {
			await repository.upsertDsaEmailVerification({
				email_lower: email,
				code_hash: hashCode(code),
				expires_at: new Date(Date.now() + 600_000),
				last_sent_at: new Date(),
			});
		}
		await repository.consumeDsaEmailVerification(email, hashCode('SCND-CODE'));
		await repository.consumeDsaTicket('c'.repeat(64), email);
		const writes = executor.statements.filter(
			(cql) => /dsa_report_(tickets|email_verifications)/.test(cql) && !/^\s*SELECT/i.test(cql),
		);
		expect(writes.map((cql) => cql.trim().split(/\s+/)[0])).toEqual(['INSERT', 'INSERT', 'UPDATE', 'DELETE', 'DELETE']);
		for (const cql of writes) {
			expect(cql).toMatch(/\sIF\s/);
		}
	});

	test('creating a ticket that already exists fails and keeps the first row', async () => {
		const repository = new ReportRepository();
		const email = createUniqueEmail('dsa-single-use');
		const row = {
			ticket: 'd'.repeat(64),
			email_lower: email,
			expires_at: new Date(Date.now() + 3_600_000),
			created_at: new Date(),
		};
		await repository.createDsaTicket(row);
		await expect(repository.createDsaTicket({...row, email_lower: 'other@example.com'})).rejects.toThrow();
		expect((await repository.getDsaTicket(row.ticket))?.email_lower).toBe(email);
	});

	test('a resent code replaces the stored code', async () => {
		const repository = new ReportRepository();
		const email = createUniqueEmail('dsa-single-use');
		await repository.upsertDsaEmailVerification({
			email_lower: email,
			code_hash: hashCode('OLDC-ODE2'),
			expires_at: new Date(Date.now() + 600_000),
			last_sent_at: new Date(Date.now() - 60_000),
		});
		const resent = {
			email_lower: email,
			code_hash: hashCode('NEWC-ODE2'),
			expires_at: new Date(Date.now() + 600_000),
			last_sent_at: new Date(),
		};
		await repository.upsertDsaEmailVerification(resent);
		expect(await repository.getDsaEmailVerification(email)).toEqual(resent);
		expect(await repository.consumeDsaEmailVerification(email, hashCode('OLDC-ODE2'))).toBe(false);
		expect(await repository.consumeDsaEmailVerification(email, hashCode('NEWC-ODE2'))).toBe(true);
	});

	test('codes sent at the same time leave one usable code', async () => {
		const repository = new ReportRepository();
		const email = createUniqueEmail('dsa-single-use');
		const hashes = ['AAAA-0001', 'AAAA-0002', 'AAAA-0003'].map(hashCode);
		await Promise.all(
			hashes.map((codeHash) =>
				repository.upsertDsaEmailVerification({
					email_lower: email,
					code_hash: codeHash,
					expires_at: new Date(Date.now() + 600_000),
					last_sent_at: new Date(),
				}),
			),
		);
		const stored = await repository.getDsaEmailVerification(email);
		expect(hashes).toContain(stored?.code_hash);
		const consumed: Array<boolean> = [];
		for (const codeHash of hashes) {
			consumed.push(await repository.consumeDsaEmailVerification(email, codeHash));
		}
		expect(consumed.filter(Boolean)).toHaveLength(1);
		expect(await repository.getDsaEmailVerification(email)).toBeNull();
	});

	test('a ticket is consumed once, also under concurrent claims', async () => {
		const repository = new ReportRepository();
		const email = createUniqueEmail('dsa-single-use');
		const row = {
			ticket: 'a'.repeat(64),
			email_lower: email,
			expires_at: new Date(Date.now() + 3_600_000),
			created_at: new Date(),
		};
		await repository.createDsaTicket(row);
		expect(await repository.consumeDsaTicket(row.ticket, email)).toBe(true);
		expect(await repository.consumeDsaTicket(row.ticket, email)).toBe(false);
		await repository.createDsaTicket({...row, ticket: 'b'.repeat(64)});
		const claims = await Promise.all([
			repository.consumeDsaTicket('b'.repeat(64), email),
			repository.consumeDsaTicket('b'.repeat(64), email),
		]);
		expect(claims.filter(Boolean)).toHaveLength(1);
		expect(await repository.getDsaTicket('b'.repeat(64))).toBeNull();
	});

	test('a code is consumed only with its current hash', async () => {
		const repository = new ReportRepository();
		const email = createUniqueEmail('dsa-single-use');
		await repository.upsertDsaEmailVerification({
			email_lower: email,
			code_hash: hashCode('NEWC-ODE1'),
			expires_at: new Date(Date.now() + 600_000),
			last_sent_at: new Date(),
		});
		expect(await repository.consumeDsaEmailVerification(email, hashCode('OLDC-ODE1'))).toBe(false);
		expect(await repository.getDsaEmailVerification(email)).not.toBeNull();
		expect(await repository.consumeDsaEmailVerification(email, hashCode('NEWC-ODE1'))).toBe(true);
		expect(await repository.consumeDsaEmailVerification(email, hashCode('NEWC-ODE1'))).toBe(false);
		expect(await repository.getDsaEmailVerification(email)).toBeNull();
	});
});

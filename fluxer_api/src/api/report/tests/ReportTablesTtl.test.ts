// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {clearTestEmails, createUniqueEmail, findLastTestEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {DSAReportEmailVerifications, DSAReportTickets, IARSubmissions} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(THIS_DIR, '../../../../..');

const SCHEMA = JSON.parse(fs.readFileSync(path.join(REPO_ROOT, 'tools/dev/cassandra_target_schema.json'), 'utf8')) as {
	tables: Array<{name: string; options: string}>;
};

function schemaOptions(name: string): string {
	const table = SCHEMA.tables.find((entry) => entry.name === name);
	expect(table).toBeDefined();
	return table!.options;
}

describe('report table TTLs', () => {
	it('keeps reports without a table TTL', () => {
		expect(IARSubmissions.defaultTtlSeconds).toBeUndefined();
		expect(schemaOptions(IARSubmissions.name)).not.toContain('default_time_to_live');
	});
});

describe('report table TTLs cover the service windows', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	it('a verification code and its ticket expire in the service before their rows do', async () => {
		const repository = new ReportRepository();
		await clearTestEmails(harness);
		const email = createUniqueEmail('dsa-ttl');
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email})
			.expect(HTTP_STATUS.OK)
			.execute();
		const verification = await repository.getDsaEmailVerification(email.toLowerCase());
		expect(verification).not.toBeNull();
		const codeWindow = verification!.expires_at.getTime() - verification!.last_sent_at.getTime();
		expect(codeWindow).toBeLessThanOrEqual(DSAReportEmailVerifications.defaultTtlSeconds! * 1000);
		expect(codeWindow).toBeGreaterThan(DSAReportEmailVerifications.defaultTtlSeconds! * 1000 - 60_000);

		const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
		expect(typeof code).toBe('string');
		const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
			.post('/reports/dsa/email/verify')
			.body({email, code})
			.expect(HTTP_STATUS.OK)
			.execute();
		const ticketRow = await repository.getDsaTicket(ticket);
		expect(ticketRow).not.toBeNull();
		const ticketWindow = ticketRow!.expires_at.getTime() - ticketRow!.created_at.getTime();
		expect(ticketWindow).toBeLessThanOrEqual(DSAReportTickets.defaultTtlSeconds! * 1000);
		expect(ticketWindow).toBeGreaterThan(DSAReportTickets.defaultTtlSeconds! * 1000 - 60_000);
	});
});

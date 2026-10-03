// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ValidationErrorResponse {
	code: string;
	errors?: Array<{path: string; message: string}>;
}

interface EntryPage {
	items: Array<{value: string; match_subdomains: boolean | null}>;
}

describe('Admin url-domain blocklist patterns', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'ban:url_domain:add',
			'ban:url_domain:check',
			'ban:url_domain:remove',
		]);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function add(domain: string, matchSubdomains?: boolean): Promise<void> {
		await createBuilder(harness, admin.token)
			.post('/admin/blocklists/url-domain/entries')
			.body(matchSubdomains === undefined ? {domain} : {domain, match_subdomains: matchSubdomains})
			.expect(204)
			.execute();
	}

	async function check(value: string): Promise<boolean> {
		const json = await createBuilder<{banned: boolean}>(harness, admin.token)
			.get(`/admin/blocklists/url-domain/entries/${encodeURIComponent(value)}`)
			.expect(200)
			.execute();
		return json.banned;
	}

	async function list(): Promise<EntryPage['items']> {
		const json = await createBuilder<EntryPage>(harness, admin.token)
			.get('/admin/blocklists/url-domain/entries?limit=200')
			.expect(200)
			.execute();
		return json.items;
	}

	it('stores a canonical pattern and reports the hosts it covers', async () => {
		await add('**Shop**.OnRender.com.');
		expect(await list()).toMatchObject([{value: '*shop*.onrender.com', match_subdomains: true}]);
		expect(await check('shop-2.onrender.com')).toBe(true);
		expect(await check('https://www.myshop.onrender.com/checkout')).toBe(true);
		expect(await check('onrender.com')).toBe(false);
		expect(await check('docs.onrender.com')).toBe(false);
	});

	it('records whether the entry is a pattern in the audit log', async () => {
		await add('*shop*.onrender.com', false);
		await add('shop.example.com');
		const logs = (await getAdminRepository().listAllAuditLogsPaginated(1000)).filter(
			(log) => log.action === 'ban_url_domain',
		);
		const metadata = logs.map((log) => Object.fromEntries(log.metadata));
		expect(metadata).toEqual(
			expect.arrayContaining([
				{domain: '*shop*.onrender.com', match_subdomains: 'false', pattern: 'true'},
				{domain: 'shop.example.com', match_subdomains: 'true', pattern: 'false'},
			]),
		);
	});

	it('rejects patterns that are too broad or malformed', async () => {
		for (const domain of ['*', '*.com', '*shop*.co.uk', '*.onrender.com', '*ab*.onrender.com', 'shop.*.example.com']) {
			const json = await createBuilder<ValidationErrorResponse>(harness, admin.token)
				.post('/admin/blocklists/url-domain/entries')
				.body({domain})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
			expect(json.errors?.[0]?.path, domain).toBe('domain');
		}
		expect(await list()).toEqual([]);
	});

	it('validates the value on update', async () => {
		const json = await createBuilder<ValidationErrorResponse>(harness, admin.token)
			.patch(`/admin/blocklists/url-domain/entries/${encodeURIComponent('*.com')}`)
			.body({})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(json.errors?.[0]?.path).toBe('domain');
	});

	it('stores internationalized domains in ASCII form', async () => {
		await add('Bücher.Example.');
		expect((await list()).map((entry) => entry.value)).toEqual(['xn--bcher-kva.example']);
		expect(await check('www.bücher.example')).toBe(true);
	});

	it('accepts an add for a domain that is already blocked', async () => {
		await add('shop.example.com');
		await add('shop.example.com', false);
		expect(await list()).toMatchObject([{value: 'shop.example.com', match_subdomains: false}]);
	});

	it('removes a pattern through any spelling that canonicalizes to it', async () => {
		await add('*shop*.onrender.com');
		await createBuilder(harness, admin.token)
			.delete(`/admin/blocklists/url-domain/entries/${encodeURIComponent('*SHOP**.onrender.com')}`)
			.expect(204)
			.execute();
		expect(await list()).toEqual([]);
		expect(await check('shop.onrender.com')).toBe(false);
	});

	it('blocks messages whose masked links or autolinks point at a covered host', async () => {
		await add('*shop*.onrender.com');
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, member.token, 'Links');
		const channel = await createChannel(harness, member.token, guild.id, 'general');
		await ensureSessionStarted(harness, member.token);
		for (const content of [
			'[open the store](https://shop-2.onrender.com)',
			'<https://login@www.shop.onrender.com:8443/x>',
			'https://SHOP.onrender.com./',
		]) {
			await createBuilder(harness, member.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content})
				.expect(403, APIErrorCodes.CONTENT_BLOCKED)
				.execute();
		}
		await createBuilder(harness, member.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: '[docs](https://docs.onrender.com) and https://onrender.com'})
			.expect(200)
			.execute();
	});

	it('blocks rich embeds that link to a covered host', async () => {
		await add('*shop*.onrender.com');
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, member.token, 'Embeds');
		const channel = await createChannel(harness, member.token, guild.id, 'general');
		await ensureSessionStarted(harness, member.token);
		await createBuilder(harness, member.token)
			.post(`/channels/${channel.id}/messages`)
			.body({embeds: [{title: 'Store', url: 'https://shop.onrender.com/'}]})
			.expect(403, APIErrorCodes.CONTENT_BLOCKED)
			.execute();
	});
});

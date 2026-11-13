// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, randomBytes} from 'node:crypto';
import {clearAttachmentMediaDecisionsForTesting} from '@app/api/attachment/AttachmentMediaAccess';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {createDmChannel, createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {acceptInvite, createChannelInvite} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {canonicalInput} from '@pkgs/media_proxy_utils/src/AttachmentUrlSignature';
import {HttpResponse, http} from 'msw';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const FILE_BYTES = randomBytes(64);
const IGNORED_HEADERS = new Set(['date', 'x-request-id', 'x-fluxer-version']);

interface UpstreamRequest {
	method: string;
	url: URL;
	headers: Headers;
}

interface SentMessage {
	channelId: string;
	messageId: string;
	url: string;
}

function upstreamBase(): string {
	return `http://${Config.mediaProxy.host}:${Config.mediaProxy.port}`;
}

function stubMediaProxy(requests: Array<UpstreamRequest>) {
	return http.all(`${upstreamBase()}/attachments/*`, ({request}) => {
		requests.push({method: request.method, url: new URL(request.url), headers: request.headers});
		const range = request.headers.get('range');
		const headers = {
			'Content-Type': 'image/png',
			'Cache-Control': 'public, max-age=31536000',
			'CDN-Cache-Control': 'public, max-age=31536000',
			'Access-Control-Allow-Origin': '*',
			'Accept-Ranges': 'bytes',
			'Set-Cookie': 'tracking=1',
		};
		if (range === 'bytes=0-9') {
			return new HttpResponse(FILE_BYTES.subarray(0, 10), {
				status: 206,
				headers: {...headers, 'Content-Range': `bytes 0-9/${FILE_BYTES.length}`, 'Content-Length': '10'},
			});
		}
		if (range !== null) {
			return new HttpResponse(null, {status: 416, headers: {'Content-Range': `bytes */${FILE_BYTES.length}`}});
		}
		const disposition = request.url.includes('download=1') ? {'Content-Disposition': 'attachment'} : {};
		return new HttpResponse(request.method === 'HEAD' ? null : FILE_BYTES, {
			status: 200,
			headers: {...headers, ...disposition, 'Content-Length': String(FILE_BYTES.length)},
		});
	});
}

function comparableHeaders(response: Response): Record<string, string> {
	return Object.fromEntries([...response.headers.entries()].filter(([name]) => !IGNORED_HEADERS.has(name)));
}

async function snapshot(response: Response) {
	return {status: response.status, headers: comparableHeaders(response), body: await response.text()};
}

function routePath(url: string): string {
	const parsed = new URL(url);
	return `${parsed.pathname.replace(/^\/api/u, '')}${parsed.search}`;
}

describe('attachments served through the API', () => {
	let harness: ApiTestHarness;
	let upstream: Array<UpstreamRequest>;
	let admin: TestAccount;
	let owner: TestAccount;
	let member: TestAccount;
	let outsider: TestAccount;
	let channelId: string;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		upstream = [];
		server.use(stubMediaProxy(upstream));
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		owner = await createTestAccount(harness);
		member = await createTestAccount(harness);
		outsider = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Proxy Guild');
		channelId = (await createChannel(harness, owner.token, guild.id, 'media')).id;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
		const otherGuild = await createGuild(harness, outsider.token, 'Other Guild');
		await createChannel(harness, outsider.token, otherGuild.id, 'elsewhere');
	});

	afterEach(async () => {
		clearAttachmentMediaDecisionsForTesting();
		await harness?.shutdown();
	});

	async function setProxied(enabled: boolean): Promise<void> {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({media: {proxy_attachments_through_api: enabled}})
			.execute();
	}

	async function sendImage(token: string, targetChannelId: string): Promise<SentMessage> {
		const {response, json} = await sendMessageWithAttachments(
			harness,
			token,
			targetChannelId,
			{content: 'image', attachments: [{id: 0, filename: 'yeah.png'}]},
			[{index: 0, filename: 'yeah.png', data: loadFixture('yeah.png')}],
		);
		expect(response.status).toBe(200);
		return {channelId: targetChannelId, messageId: json.id, url: json.attachments![0]!.url!};
	}

	async function get(path: string, token?: string, headers: Record<string, string> = {}): Promise<Response> {
		return await harness.app.request(path, {
			headers: {'x-forwarded-for': '127.0.0.1', ...(token ? {Authorization: token} : {}), ...headers},
		});
	}

	async function mintToken(token: string, body: Record<string, unknown> = {}): Promise<Response> {
		return harness.app.request('/users/@me/attachment-token', {
			method: 'POST',
			headers: {'x-forwarded-for': '127.0.0.1', Authorization: token, 'Content-Type': 'application/json'},
			body: JSON.stringify(body),
		});
	}

	test('with the setting off links, the route and the token endpoint stay as before', async () => {
		const sent = await sendImage(owner.token, channelId);
		expect(sent.url.startsWith(`${Config.endpoints.media}/attachments/${channelId}/`)).toBe(true);
		expect(new URL(sent.url).searchParams.get('hm')).not.toBeNull();
		const apiPath = `/channels/${channelId}/attachments/${new URL(sent.url).pathname.split('/').slice(-2).join('/')}`;
		const unknownPath = apiPath.replace('/attachments/', '/attachments-unknown/');
		for (const token of [member.token, undefined]) {
			expect(await snapshot(await get(`${apiPath}?mid=${sent.messageId}`, token))).toEqual(
				await snapshot(await get(`${unknownPath}?mid=${sent.messageId}`, token)),
			);
			expect(await snapshot(await get(`${apiPath}?mt=abc.def`, token))).toEqual(
				await snapshot(await get(`${unknownPath}?mt=abc.def`, token)),
			);
		}
		const minted = await snapshot(await mintToken(member.token));
		const unknownPost = await snapshot(
			await harness.app.request('/users/@me/attachment-token-unknown', {
				method: 'POST',
				headers: {'x-forwarded-for': '127.0.0.1', Authorization: member.token, 'Content-Type': 'application/json'},
				body: '{}',
			}),
		);
		expect(minted).toEqual(unknownPost);
		const refreshed = await createBuilder<{refreshed_urls: Array<{refreshed: string}>}>(harness, member.token)
			.post('/attachments/refresh-urls')
			.body({attachment_urls: [sent.url]})
			.execute();
		expect(refreshed.refreshed_urls[0]!.refreshed.startsWith(`${Config.endpoints.media}/attachments/`)).toBe(true);
		const discovery = await createBuilder<{features: Record<string, unknown>}>(harness, '')
			.get('/.well-known/fluxer')
			.execute();
		expect('proxy_attachments_through_api' in discovery.features).toBe(false);
		expect(upstream).toEqual([]);
	});

	test('with the setting on a member gets the file and the media proxy gets a signed request', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		expect(sent.url.startsWith(`${Config.endpoints.apiClient}/channels/${channelId}/attachments/`)).toBe(true);
		expect(sent.url.endsWith(`?mid=${sent.messageId}`)).toBe(true);
		const response = await get(`${routePath(sent.url)}&ex=1&is=2&hm=3&uc=dp&mt=x&other=1&width=64`, member.token, {
			Cookie: 'session=1',
			'X-Extra': '1',
		});
		expect(response.status).toBe(200);
		expect(Buffer.from(await response.arrayBuffer())).toEqual(FILE_BYTES);
		expect(response.headers.get('cache-control')).toBe('private, max-age=3600');
		expect(response.headers.get('cdn-cache-control')).toBe('no-store');
		expect(response.headers.get('content-security-policy')).toBe("default-src 'none'; sandbox");
		expect(response.headers.get('x-content-type-options')).toBe('nosniff');
		expect(response.headers.get('referrer-policy')).toBe('no-referrer');
		expect(response.headers.get('set-cookie')).toBeNull();
		expect(response.headers.get('access-control-allow-credentials')).toBeNull();
		expect(upstream).toHaveLength(1);
		const request = upstream[0]!;
		const key = `attachments/${channelId}/${new URL(sent.url).pathname.split('/').slice(-2).join('/')}`;
		expect(request.url.pathname).toBe(`/${key}`);
		expect([...request.url.searchParams.keys()].sort()).toEqual(['ex', 'hm', 'is', 'width']);
		const secret = Buffer.from(Config.mediaProxy.attachmentUrls.secretsBase64[0]!, 'base64');
		const expected = createHmac('sha256', secret)
			.update(canonicalInput(key, request.url.searchParams.get('ex')!, request.url.searchParams.get('is')!, ''))
			.digest('hex');
		expect(request.url.searchParams.get('hm')).toBe(expected);
		expect(request.headers.get('cookie')).toBeNull();
		expect(request.headers.get('authorization')).toBeNull();
		expect(request.headers.get('x-extra')).toBeNull();
	});

	test('with the setting on everyone who cannot read the message gets the same 404', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const path = routePath(sent.url);
		const missing = await get(path, outsider.token);
		expect(missing.status).toBe(404);
		const notFound = await snapshot(missing);
		const filename = new URL(sent.url).pathname.split('/').pop()!;
		for (const attempt of [
			path.replace(filename, `other-${filename}`),
			path.replace(`mid=${sent.messageId}`, `mid=${BigInt(sent.messageId) + 1n}`),
			path.replace(filename, `..%2F${filename}`),
		]) {
			expect(await snapshot(await get(attempt, member.token))).toEqual(notFound);
		}
		await createFriendship(harness, owner, member);
		const dm = await createDmChannel(harness, owner.token, member.userId);
		const dmSent = await sendImage(owner.token, dm.id);
		expect((await get(routePath(dmSent.url), member.token)).status).toBe(200);
		expect(await snapshot(await get(routePath(dmSent.url), outsider.token))).toEqual(notFound);
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channelId}/messages/${sent.messageId}`)
			.expect(204)
			.execute();
		clearAttachmentMediaDecisionsForTesting();
		expect(await snapshot(await get(path, member.token))).toEqual(notFound);
		expect(upstream.every((request) => request.url.pathname.includes(dm.id))).toBe(true);
	});

	test('with the setting on ranges, heads and transforms pass through', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const path = routePath(sent.url);
		const partial = await get(path, member.token, {Range: 'bytes=0-9'});
		expect(partial.status).toBe(206);
		expect(partial.headers.get('content-range')).toBe(`bytes 0-9/${FILE_BYTES.length}`);
		expect(Buffer.from(await partial.arrayBuffer())).toEqual(FILE_BYTES.subarray(0, 10));
		expect(upstream.at(-1)!.headers.get('range')).toBe('bytes=0-9');
		expect((await get(path, member.token, {Range: 'bytes=999-1000'})).status).toBe(416);
		const head = await harness.app.request(path, {
			method: 'HEAD',
			headers: {'x-forwarded-for': '127.0.0.1', Authorization: member.token},
		});
		expect(head.status).toBe(200);
		expect(await head.text()).toBe('');
		expect(upstream.at(-1)!.method).toBe('HEAD');
		for (let attempt = 0; attempt < 20; attempt++) {
			const repeated = await harness.app.request(path, {
				method: attempt % 2 === 0 ? 'HEAD' : 'GET',
				headers: {'x-forwarded-for': '127.0.0.1', Authorization: member.token},
			});
			await repeated.arrayBuffer();
			expect(repeated.status).toBe(200);
		}
		const download = await get(`${path}&download=1&format=webp&quality=low&height=32`, member.token);
		expect(download.headers.get('content-disposition')).toBe('attachment');
		const forwarded = upstream.at(-1)!.url.searchParams;
		expect([
			forwarded.get('download'),
			forwarded.get('format'),
			forwarded.get('quality'),
			forwarded.get('height'),
		]).toEqual(['1', 'webp', 'low', '32']);
		const before = upstream.length;
		for (const bad of ['width=0', 'width=abc', 'format=exe', 'quality=max', 'width=1&width=2']) {
			expect((await get(`${path}&${bad}`, member.token)).status).toBe(400);
		}
		expect(upstream.length).toBe(before);
	});

	test('with the setting on a media token works only where it was meant to', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const other = await sendImage(owner.token, channelId);
		const minted = await mintToken(member.token);
		expect(minted.status).toBe(200);
		const {token} = (await minted.json()) as {token: string};
		const path = routePath(sent.url);
		expect((await get(`${path}&mt=${encodeURIComponent(token)}`)).status).toBe(200);
		const tampered = `${token.slice(0, -2)}${token.endsWith('AA') ? 'BB' : 'AA'}`;
		const rejected = await get(`${path}&mt=${encodeURIComponent(tampered)}`);
		expect(rejected.status).toBe(401);
		expect(rejected.headers.get('cache-control')).toBe('no-store');
		expect((await get(`/users/@me?mt=${encodeURIComponent(token)}`)).status).toBe(401);
		const outsiderToken = ((await (await mintToken(outsider.token)).json()) as {token: string}).token;
		expect((await get(`${path}&mt=${encodeURIComponent(outsiderToken)}`)).status).toBe(404);
		const scoped = await mintToken(member.token, {url: sent.url});
		const scopedToken = ((await scoped.json()) as {token: string}).token;
		expect((await get(`${path}&mt=${encodeURIComponent(scopedToken)}`)).status).toBe(200);
		expect((await get(`${routePath(other.url)}&mt=${encodeURIComponent(scopedToken)}`)).status).toBe(401);
		expect((await mintToken(outsider.token, {url: sent.url})).status).toBe(404);
		await setProxied(false);
		expect((await get(`${path}&mt=${encodeURIComponent(token)}`)).status).toBe(404);
	});

	test('with the setting on refreshed links point at the API and never at the media proxy', async () => {
		await setProxied(true);
		const mediaUrl = `${Config.endpoints.media}/attachments/${channelId}/1544971349200470016/cat.png?ex=1&is=2&hm=3`;
		const refreshed = await createBuilder<{refreshed_urls: Array<{refreshed: string}>}>(harness, member.token)
			.post('/attachments/refresh-urls')
			.body({attachment_urls: [mediaUrl]})
			.execute();
		expect(refreshed.refreshed_urls[0]!.refreshed).toBe(
			`${Config.endpoints.apiClient}/channels/${channelId}/attachments/1544971349200470016/cat.png`,
		);
		const discovery = await createBuilder<{features: Record<string, unknown>}>(harness, '')
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.features.proxy_attachments_through_api).toBe(true);
	});
});

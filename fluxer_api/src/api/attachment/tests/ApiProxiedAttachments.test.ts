// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, randomBytes} from 'node:crypto';
import {clearAttachmentMediaDecisionsForTesting} from '@app/api/attachment/AttachmentMediaAccess';
import {
	type AttachmentMediaTokenPayload,
	openAttachmentMediaToken,
	sealAttachmentMediaToken,
} from '@app/api/attachment/AttachmentMediaToken';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {
	createChannel,
	createGuild,
	loadFixture,
	sendMessageWithAttachments,
} from '@app/api/channel/tests/AttachmentTestUtils';
import {createDmChannel, createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {acceptInvite, createChannelInvite, leaveGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {TestMediaService} from '@app/api/test/TestMediaService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {createSnowflakeGenerator} from '@fluxer/snowflake/src/Snowflake';
import {canonicalInput} from '@pkgs/media_proxy_utils/src/AttachmentUrlSignature';
import {HttpResponse, http} from 'msw';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

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
	let guildId: string;

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
		guildId = guild.id;
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

	test('with the setting on a grid of images loading at once is not refused', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const path = routePath(sent.url);
		const responses = await Promise.all(Array.from({length: 24}, () => get(path, member.token)));
		expect(responses.map((response) => response.status)).toEqual(responses.map(() => 200));
		const bodies = await Promise.all(responses.map(async (response) => Buffer.from(await response.arrayBuffer())));
		expect(bodies.every((body) => body.equals(FILE_BYTES))).toBe(true);
	});

	async function mintFor(token: string, url: string): Promise<string> {
		const minted = await mintToken(token, {url});
		expect(minted.status).toBe(200);
		return ((await minted.json()) as {token: string}).token;
	}

	function withToken(path: string, token: string): string {
		return `${path}&mt=${encodeURIComponent(token)}`;
	}

	test('with the setting on a file token opens that one file for five minutes', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const path = routePath(sent.url);
		const before = Date.now();
		const minted = await mintToken(member.token, {url: sent.url});
		expect(minted.status).toBe(200);
		const body = (await minted.json()) as {token: string; expires_at: string};
		const lifetimeMs = Date.parse(body.expires_at) - before;
		expect(lifetimeMs).toBeGreaterThan(4 * 60 * 1000);
		expect(lifetimeMs).toBeLessThanOrEqual(5 * 60 * 1000 + 1000);
		expect(openAttachmentMediaToken(body.token)).toMatchObject({
			c: channelId,
			a: new URL(sent.url).pathname.split('/').at(-2),
		});
		const readable = Buffer.from(body.token, 'base64url').toString('latin1');
		for (const secret of [member.userId, channelId, sent.messageId]) {
			expect(readable.includes(secret)).toBe(false);
		}
		const opened = await get(withToken(path, body.token));
		expect(opened.status).toBe(200);
		expect(opened.headers.get('cache-control')).toBe('private, no-store');
		expect(Buffer.from(await opened.arrayBuffer())).toEqual(FILE_BYTES);
		expect((await get(withToken(path, body.token), undefined, {Range: 'bytes=0-9'})).status).toBe(206);
		const tampered = `${body.token.slice(0, -2)}${body.token.endsWith('AA') ? 'BB' : 'AA'}`;
		const rejected = await get(withToken(path, tampered));
		expect(rejected.status).toBe(401);
		expect(rejected.headers.get('cache-control')).toBe('no-store');
		expect((await get(`/users/@me?mt=${encodeURIComponent(body.token)}`)).status).toBe(401);
		const reissue = await harness.app.request('/users/@me/attachment-token', {
			method: 'POST',
			headers: {'x-forwarded-for': '127.0.0.1', 'Content-Type': 'application/json'},
			body: JSON.stringify({url: `${sent.url}&mt=${encodeURIComponent(body.token)}`}),
		});
		expect(reissue.status).toBe(401);
	});

	test('with the setting on a file token does not open another file', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const other = await sendImage(owner.token, channelId);
		const token = await mintFor(member.token, sent.url);
		expect((await get(withToken(routePath(other.url), token))).status).toBe(401);
		const otherPath = routePath(other.url).replace(`mid=${other.messageId}`, `mid=${sent.messageId}`);
		expect((await get(withToken(otherPath, token))).status).toBe(401);
		expect((await get(withToken(routePath(sent.url), token))).status).toBe(200);
	});

	test('with the setting on a file token is only issued to someone who can read the file', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		expect((await mintToken(outsider.token, {url: sent.url})).status).toBe(404);
		const ownGuild = await createGuild(harness, outsider.token, 'Own Guild');
		const ownChannelId = (await createChannel(harness, outsider.token, ownGuild.id, 'own')).id;
		const own = await sendImage(outsider.token, ownChannelId);
		const outsiderToken = await mintFor(outsider.token, own.url);
		expect((await get(withToken(routePath(own.url), outsiderToken))).status).toBe(200);
		expect((await get(withToken(routePath(sent.url), outsiderToken))).status).toBe(401);
		const swapped = routePath(own.url).replace(`/channels/${ownChannelId}/`, `/channels/${channelId}/`);
		expect((await get(withToken(swapped, outsiderToken))).status).toBe(401);
		const memberToken = await mintFor(member.token, sent.url);
		const payload = openAttachmentMediaToken(memberToken)!;
		expect(payload.u).toBe(member.userId);
		const forged = sealAttachmentMediaToken({...payload, u: outsider.userId});
		expect((await get(withToken(routePath(sent.url), forged))).status).toBe(401);
	});

	test('with the setting on an expired file token is refused', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const payload = openAttachmentMediaToken(await mintFor(member.token, sent.url))!;
		const expired = sealAttachmentMediaToken({...payload, expiresAt: Date.now() - 1});
		expect((await get(withToken(routePath(sent.url), expired))).status).toBe(401);
	});

	test('with the setting on a file token stops working once access is gone', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const path = routePath(sent.url);
		const token = await mintFor(member.token, sent.url);
		expect((await get(withToken(path, token))).status).toBe(200);
		await leaveGuild(harness, member.token, guildId);
		clearAttachmentMediaDecisionsForTesting();
		expect((await get(withToken(path, token))).status).toBe(404);
		const ownerToken = await mintFor(owner.token, sent.url);
		expect((await get(withToken(path, ownerToken))).status).toBe(200);
		await createBuilder(harness, owner.token).post('/auth/logout').expect(204).execute();
		clearAttachmentMediaDecisionsForTesting();
		expect((await get(withToken(path, ownerToken))).status).toBe(401);
		await setProxied(false);
		expect((await get(withToken(path, token))).status).toBe(404);
	});

	test('with the setting on no token covers the whole account', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		expect((await mintToken(member.token)).status).toBe(400);
		expect((await mintToken(member.token, {url: `${Config.endpoints.apiClient}/users/@me`})).status).toBe(404);
		const payload = openAttachmentMediaToken(await mintFor(member.token, sent.url))!;
		const {c: _channel, a: _attachment, ...accountWide} = payload;
		const accountToken = sealAttachmentMediaToken(accountWide as unknown as AttachmentMediaTokenPayload);
		expect(openAttachmentMediaToken(accountToken)).toBeNull();
		expect((await get(withToken(routePath(sent.url), accountToken))).status).toBe(401);
		const viaHeader = await get(routePath(sent.url), member.token);
		expect(viaHeader.status).toBe(200);
		expect(Buffer.from(await viaHeader.arrayBuffer())).toEqual(FILE_BYTES);
	});

	test('with the setting on refreshed links point at the API and never at the media proxy', async () => {
		await setProxied(true);
		const attachmentId = createSnowflakeGenerator().generate();
		const mediaUrl = `${Config.endpoints.media}/attachments/${channelId}/${attachmentId}/cat.png?ex=1&is=2&hm=3`;
		const refreshed = await createBuilder<{refreshed_urls: Array<{refreshed: string}>}>(harness, member.token)
			.post('/attachments/refresh-urls')
			.body({attachment_urls: [mediaUrl]})
			.execute();
		expect(refreshed.refreshed_urls[0]!.refreshed).toBe(
			`${Config.endpoints.apiClient}/channels/${channelId}/attachments/${attachmentId}/cat.png`,
		);
		const discovery = await createBuilder<{features: Record<string, unknown>}>(harness, '')
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.features.proxy_attachments_through_api).toBe(true);
	});

	function watchPrivateFetches(sent: SentMessage): () => Array<string> {
		const spy = vi.spyOn(TestMediaService.prototype, 'getMetadata');
		const key = `/attachments/${sent.channelId}/`;
		return () => {
			const urls = spy.mock.calls.flatMap(([request]) =>
				request.type === 'external' && request.url.includes(key) ? [request.url] : [],
			);
			spy.mockRestore();
			return urls;
		};
	}

	function mediaFormOf(sent: SentMessage): string {
		const [attachmentId, filename] = new URL(sent.url).pathname.split('/').slice(-2);
		return `${Config.endpoints.media}/attachments/${sent.channelId}/${attachmentId}/${filename}`;
	}

	test('with the setting on a copied link cannot be turned into a meme, a GIF or an avatar by an outsider', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const before = upstream.length;
		const privateFetches = watchPrivateFetches(sent);
		for (const url of [mediaFormOf(sent), `${mediaFormOf(sent)}?ex=1&is=2&hm=3`, sent.url]) {
			const meme = await harness.app.request('/users/@me/memes', {
				method: 'POST',
				headers: {'x-forwarded-for': '127.0.0.1', Authorization: outsider.token, 'Content-Type': 'application/json'},
				body: JSON.stringify({url, name: 'copy'}),
			});
			expect(meme.status).toBe(400);
			const resolved = await createBuilder<{
				entries: Array<{content_type: string; width: number; placeholder: unknown}>;
			}>(harness, outsider.token)
				.post('/users/@me/favorite-gifs/resolve')
				.body({urls: [url]})
				.execute();
			expect(resolved.entries[0]).toMatchObject({content_type: '', width: 0, placeholder: null});
		}
		const memes = await createBuilder<Array<unknown>>(harness, outsider.token).get('/users/@me/memes').execute();
		expect(memes).toEqual([]);
		expect(upstream.length).toBe(before);
		expect(privateFetches()).toEqual([]);
	});

	test('with the setting on a linked file never borrows the access of the message that links it', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const edited = await createBuilder<{attachments: Array<unknown> | null}>(harness, owner.token)
			.patch(`/channels/${channelId}/messages/${sent.messageId}`)
			.body({content: 'edited', attachments: []})
			.execute();
		expect(edited.attachments ?? []).toEqual([]);
		clearAttachmentMediaDecisionsForTesting();
		expect((await get(routePath(sent.url), owner.token)).status).toBe(404);
		const linking = await createBuilder<{embeds: Array<{image?: {url: string; proxy_url: string}}>}>(
			harness,
			owner.token,
		)
			.post(`/channels/${channelId}/messages`)
			.body({embeds: [{title: 'linked', image: {url: mediaFormOf(sent)}}]})
			.execute();
		const image = linking.embeds[0]!.image!;
		for (const url of [image.url, image.proxy_url]) {
			expect(new URL(url).searchParams.has('mid')).toBe(false);
			expect(url.startsWith(`${Config.endpoints.apiClient}/channels/${channelId}/attachments/`)).toBe(true);
			expect((await get(routePath(url), owner.token)).status).toBe(404);
			expect((await get(routePath(url), member.token)).status).toBe(404);
		}
	});

	test('with the setting on a linked file cannot be saved or previewed by someone who cannot read it', async () => {
		await setProxied(true);
		const sent = await sendImage(owner.token, channelId);
		const ownGuild = await createGuild(harness, outsider.token, 'Linking Guild');
		const ownChannelId = (await createChannel(harness, outsider.token, ownGuild.id, 'links')).id;
		const privateFetches = watchPrivateFetches(sent);
		const linking = await createBuilder<{id: string}>(harness, outsider.token)
			.post(`/channels/${ownChannelId}/messages`)
			.body({embeds: [{title: 'linked', image: {url: mediaFormOf(sent)}}]})
			.execute();
		const meme = await harness.app.request(`/channels/${ownChannelId}/messages/${linking.id}/memes`, {
			method: 'POST',
			headers: {'x-forwarded-for': '127.0.0.1', Authorization: outsider.token, 'Content-Type': 'application/json'},
			body: JSON.stringify({embed_index: 0, name: 'copy'}),
		});
		expect(meme.status).not.toBe(201);
		const memes = await createBuilder<Array<unknown>>(harness, outsider.token).get('/users/@me/memes').execute();
		expect(memes).toEqual([]);
		const webhook = await createWebhook(harness, ownChannelId, outsider.token, 'linker');
		for (const url of [mediaFormOf(sent), sent.url]) {
			const posted = await createBuilderWithoutAuth<{author: {avatar: string | null}}>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({content: 'hello', avatar_url: url})
				.execute();
			expect(posted.author.avatar).toBeNull();
		}
		for (const url of [mediaFormOf(sent), sent.url]) {
			const unfurled = await createBuilder<Array<unknown>>(harness, outsider.token)
				.post('/unfurl')
				.body({url})
				.execute();
			expect(unfurled).toEqual([]);
		}
		expect(privateFetches()).toEqual([]);
	});
});

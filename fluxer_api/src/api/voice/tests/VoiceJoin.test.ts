// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, createHmac} from 'node:crypto';
import {createAdminWithAcls} from '@app/api/admin/tests/AdminTestUtils';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {VoiceJoinTokenRepository} from '@app/api/voice/VoiceJoinTokenRepository';
import {
	hashJoinSecret,
	setVoiceNodeEndpointVerifierForTesting,
	VOICE_NODE_AUDIENCE,
	VOICE_NODE_PATH,
	voiceJoinCommand,
} from '@app/api/voice/VoiceNodeService';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {setVoiceEndpointResolverForTesting, VoiceEndpointError} from '@app/api/voice/VoiceServerEndpoints';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	CreateVoiceJoinTokenResponse,
	ListVoiceJoinTokensResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import type {
	VoiceJoinPreviewResponse,
	VoiceJoinResponse,
	VoiceNodeResponse,
} from '@fluxer/schema/src/domains/voice/VoiceNodeSchemas';
import {AccessToken} from 'livekit-server-sdk';
import {HttpResponse, http} from 'msw';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const SECRET = 'a'.repeat(64);

function joinBody(overrides: Record<string, unknown> = {}) {
	return {
		endpoint: 'wss://203.0.113.7/livekit-fra-server-1',
		api_key: 'fluxer-fra-server-1',
		api_secret: SECRET,
		...overrides,
	};
}

function nodeJwt(claims: Record<string, unknown>, secret = SECRET): string {
	const encode = (value: unknown) => Buffer.from(JSON.stringify(value)).toString('base64url');
	const input = `${encode({alg: 'HS256', typ: 'JWT'})}.${encode(claims)}`;
	return `Bearer ${input}.${createHmac('sha256', secret).update(input).digest('base64url')}`;
}

function nodeClaims(method: string, body: string, overrides: Record<string, unknown> = {}) {
	return {
		iss: 'fluxer-fra-server-1',
		aud: VOICE_NODE_AUDIENCE,
		method,
		path: VOICE_NODE_PATH,
		exp: Math.floor(Date.now() / 1000) + 60,
		sha256: body === '' ? '' : createHash('sha256').update(body).digest('base64'),
		...overrides,
	};
}

describe('voice join tokens and node routes', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let verified: Array<string>;

	async function createRegion(id: string) {
		await createBuilder(harness, admin.token)
			.post('/admin/voice/regions')
			.body({id, name: `Region ${id}`, emoji: ':earth_africa:', latitude: 50, longitude: 8})
			.execute();
	}

	async function createToken(body: Record<string, unknown> = {}) {
		return createBuilder<CreateVoiceJoinTokenResponse>(harness, admin.token)
			.post('/admin/voice/join-tokens')
			.body(body)
			.execute();
	}

	async function join(token: string, body = joinBody(), status: number = HTTP_STATUS.OK, code?: string) {
		return createBuilder<VoiceJoinResponse>(harness, `VoiceJoin ${token}`)
			.post('/voice/join')
			.body(body)
			.expect(status, code)
			.execute();
	}

	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await createAdminWithAcls(harness, [
			AdminACLs.VOICE_REGION_CREATE,
			AdminACLs.VOICE_REGION_LIST,
			AdminACLs.VOICE_SERVER_CREATE,
			AdminACLs.VOICE_SERVER_LIST,
			AdminACLs.VOICE_SERVER_UPDATE,
			AdminACLs.VOICE_SERVER_DELETE,
			AdminACLs.VOICE_JOIN_TOKEN_CREATE,
			AdminACLs.VOICE_JOIN_TOKEN_LIST,
			AdminACLs.VOICE_JOIN_TOKEN_REVOKE,
		]);
		verified = [];
		setVoiceNodeEndpointVerifierForTesting(async (endpoint) => {
			verified.push(endpoint);
		});
		await createRegion('fra');
	});

	afterEach(async () => {
		setVoiceNodeEndpointVerifierForTesting(null);
		await harness?.shutdown();
	});

	test('creates a token that previews without being used and joins once', async () => {
		const created = await createToken({region_id: 'fra'});
		expect(created.token).toMatch(/^fvj_\d+_[A-Za-z0-9_-]{43}$/);
		expect(created.command).toContain(`voice `);
		expect(created.command).toContain(created.token);
		const preview = await createBuilder<VoiceJoinPreviewResponse>(harness, `VoiceJoin ${created.token}`)
			.post('/voice/join/preview')
			.body({public_ipv4: '203.0.113.7'})
			.execute();
		expect(preview.region.id).toBe('fra');
		expect(preview.region.source).toBe('token');
		expect(preview.server_id).toBe('fra-server-1');
		expect(preview.server_id_pinned).toBe(false);
		expect(preview.webhook_url).toMatch(/\/webhooks\/livekit$/);
		const joined = await join(created.token);
		expect(joined).toMatchObject({server_id: 'fra-server-1', region_id: 'fra'});
		expect(verified).toEqual(['wss://203.0.113.7/livekit-fra-server-1']);
		const server = await new VoiceRepository().getServer('fra', 'fra-server-1');
		expect(server).toMatchObject({apiKey: 'fluxer-fra-server-1', isActive: true, draining: false});
		await join(
			created.token,
			joinBody({api_key: 'other'}),
			HTTP_STATUS.UNAUTHORIZED,
			APIErrorCodes.VOICE_JOIN_TOKEN_INVALID,
		);
		const list = await createBuilder<ListVoiceJoinTokensResponse>(harness, admin.token)
			.get('/admin/voice/join-tokens')
			.execute();
		expect(list.tokens[0]).toMatchObject({token_id: created.token_id, status: 'used', used_server_id: 'fra-server-1'});
		expect(JSON.stringify(list)).not.toContain(created.token);
	});

	test('keeps the bundled server ID for the bundled LiveKit even after it is deleted', async () => {
		const before = {defaultRegion: Config.voice.defaultRegion, bundled: Config.voice.bundled};
		Config.voice.defaultRegion = {id: 'fra', name: 'Region fra', emoji: ':earth_africa:', latitude: 50, longitude: 8};
		Config.voice.bundled = true;
		try {
			const created = await createToken({region_id: 'fra'});
			const preview = await createBuilder<VoiceJoinPreviewResponse>(harness, `VoiceJoin ${created.token}`)
				.post('/voice/join/preview')
				.body({public_ipv4: '203.0.113.7'})
				.execute();
			expect(preview.server_id).toBe('fra-server-2');
			await join(
				created.token,
				joinBody({server_id: 'fra-server-1'}),
				HTTP_STATUS.CONFLICT,
				APIErrorCodes.VOICE_SERVER_ID_IN_USE,
			);
			const joined = await join(created.token);
			expect(joined).toMatchObject({server_id: 'fra-server-2', region_id: 'fra'});
			expect(await new VoiceRepository().getServer('fra', 'fra-server-1')).toBeNull();
			await createBuilder(harness, admin.token)
				.post('/admin/voice/join-tokens')
				.body({region_id: 'fra', server_id: 'fra-server-1'})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.VOICE_SERVER_ID_IN_USE)
				.execute();
			Config.voice.bundled = false;
			const unbundled = await createToken({region_id: 'fra', server_id: 'fra-server-1'});
			expect(await join(unbundled.token, joinBody({api_key: 'joined-fra-1'}))).toMatchObject({
				server_id: 'fra-server-1',
			});
		} finally {
			Config.voice.defaultRegion = before.defaultRegion;
			Config.voice.bundled = before.bundled;
		}
	});

	test('rejects revoked, expired, malformed and misplaced tokens', async () => {
		const revoked = await createToken();
		await createBuilder(harness, admin.token).delete(`/admin/voice/join-tokens/${revoked.token_id}`).execute();
		await join(revoked.token, joinBody(), HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_JOIN_TOKEN_INVALID);
		const secret = 'b'.repeat(43);
		await new VoiceJoinTokenRepository().create({
			token_id: 42n,
			secret_hash: hashJoinSecret(secret),
			created_by: 1n,
			created_at: new Date(Date.now() - 7200_000),
			expires_at: new Date(Date.now() - 3600_000),
			claim_id: null,
			claimed_at: null,
			used_at: null,
			used_by_ip: null,
			used_server_id: null,
			revoked_at: null,
			region_id: null,
			region_name: null,
			region_emoji: null,
			region_latitude: null,
			region_longitude: null,
			server_id: null,
			hostname: null,
			internal_endpoint: null,
			soft_connection_limit: null,
			notes: null,
			vip_only: false,
			required_guild_features: null,
			allowed_guild_ids: null,
			allowed_user_ids: null,
		});
		await join(`fvj_42_${secret}`, joinBody(), HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_JOIN_TOKEN_INVALID);
		await join('fvj_1_short', joinBody(), HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_JOIN_TOKEN_INVALID);
		const pending = await createToken();
		await createBuilder(harness, `VoiceJoin ${pending.token}`)
			.get('/admin/voice/join-tokens')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
		await createBuilder(harness, `Bearer ${pending.token}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});

	test('releases the claim when the endpoint check fails so the token works again', async () => {
		const created = await createToken();
		setVoiceNodeEndpointVerifierForTesting(async () => {
			throw new VoiceEndpointError('unreachable', 'HTTP 502');
		});
		await join(created.token, joinBody(), HTTP_STATUS.BAD_REQUEST, APIErrorCodes.VOICE_JOIN_ENDPOINT_UNREACHABLE);
		setVoiceNodeEndpointVerifierForTesting(async () => {
			throw new VoiceEndpointError('key_mismatch', 'HTTP 401');
		});
		await join(created.token, joinBody(), HTTP_STATUS.BAD_REQUEST, APIErrorCodes.VOICE_JOIN_KEY_MISMATCH);
		setVoiceNodeEndpointVerifierForTesting(async () => {});
		await join(created.token);
	});

	test('refuses an endpoint that is not wss, an IPv6 literal or another hostname than the preset', async () => {
		const created = await createToken({hostname: 'voice.example.com'});
		for (const endpoint of [
			'https://voice.example.com/livekit-x',
			'wss://[2001:db8::1]/livekit-x',
			'wss://other.example.com/livekit-x',
		]) {
			await createBuilder(harness, `VoiceJoin ${created.token}`)
				.post('/voice/join')
				.body(joinBody({endpoint}))
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		}
		await join(created.token, joinBody({endpoint: 'wss://voice.example.com/livekit-fra-server-1'}));
	});

	test('rejects a duplicate server id or api key and two joins with one token', async () => {
		const first = await createToken({server_id: 'fra-server-1'});
		const pinned = await createBuilder<VoiceJoinPreviewResponse>(harness, `VoiceJoin ${first.token}`)
			.post('/voice/join/preview')
			.body({})
			.execute();
		expect(pinned).toMatchObject({
			server_id: 'fra-server-1',
			server_id_pinned: true,
			region: {id: 'fra', source: 'default'},
		});
		const requested = await createBuilder<VoiceJoinPreviewResponse>(harness, `VoiceJoin ${first.token}`)
			.post('/voice/join/preview')
			.body({region_id: 'fra'})
			.execute();
		expect(requested.region.source).toBe('requested');
		await join(first.token);
		const second = await createToken({server_id: 'fra-server-1'});
		await join(second.token, joinBody({api_key: 'fresh'}), HTTP_STATUS.CONFLICT, APIErrorCodes.VOICE_SERVER_ID_IN_USE);
		await join(second.token, joinBody({server_id: 'x'}), HTTP_STATUS.CONFLICT, APIErrorCodes.VOICE_SERVER_ID_IN_USE);
		const third = await createToken();
		await join(third.token, joinBody(), HTTP_STATUS.CONFLICT, APIErrorCodes.VOICE_SERVER_API_KEY_IN_USE);
		const racing = await createToken();
		let release: () => void = () => {};
		setVoiceNodeEndpointVerifierForTesting(() => new Promise<void>((resolve) => (release = resolve)));
		const winner = join(racing.token, joinBody({api_key: 'race-a', server_id: 'fra-race'}));
		await new Promise((resolve) => setTimeout(resolve, 50));
		await join(racing.token, joinBody({api_key: 'race-b'}), HTTP_STATUS.CONFLICT, APIErrorCodes.VOICE_JOIN_TOKEN_BUSY);
		release();
		await winner;
		expect(await new VoiceRepository().getServer('fra', 'fra-race')).toMatchObject({apiKey: 'race-a'});
	});

	test('creates a new region only from a token preset', async () => {
		const created = await createToken({
			region_id: 'sgp',
			region_name: 'Singapore',
			region_emoji: '🇸🇬',
			region_latitude: 1.35,
			region_longitude: 103.8,
		});
		const joined = await join(created.token);
		expect(joined.region_id).toBe('sgp');
		expect(await new VoiceRepository().getRegion('sgp')).toMatchObject({
			name: 'Singapore',
			latitude: 1.35,
			longitude: 103.8,
		});
		await createBuilder(harness, admin.token)
			.post('/admin/voice/join-tokens')
			.body({region_id: 'ams', region_name: 'Amsterdam', region_latitude: 52.4})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await createBuilder(harness, admin.token)
			.post('/admin/voice/join-tokens')
			.body({region_id: 'fra', region_latitude: 50, region_longitude: 8})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const other = await createToken();
		await join(other.token, joinBody({api_key: 'k2', region_id: 'nowhere'}), HTTP_STATUS.NOT_FOUND);
	});

	describe('node routes', () => {
		beforeEach(async () => {
			const created = await createToken({server_id: 'fra-server-1'});
			await join(created.token);
		});

		async function nodeRequest(method: 'GET' | 'PATCH' | 'DELETE', auth: string, body?: unknown) {
			const builder = createBuilder<VoiceNodeResponse>(harness, auth);
			const routed = {
				GET: () => builder.get(VOICE_NODE_PATH),
				PATCH: () => builder.patch(VOICE_NODE_PATH),
				DELETE: () => builder.delete(VOICE_NODE_PATH),
			}[method]();
			return body === undefined ? routed : routed.body(body);
		}

		test('reads and updates its own row', async () => {
			const node = await (await nodeRequest('GET', nodeJwt(nodeClaims('GET', '')))).execute();
			expect(node).toMatchObject({server_id: 'fra-server-1', draining: false, is_active: true});
			const body = {draining: true};
			const updated = await (
				await nodeRequest('PATCH', nodeJwt(nodeClaims('PATCH', JSON.stringify(body))), body)
			).execute();
			expect(updated.draining).toBe(true);
		});

		test('allows a minute of clock skew and reports a larger one', async () => {
			const now = Math.floor(Date.now() / 1000);
			const ok = [
				{iat: now - 170, exp: now - 50},
				{iat: now + 50, exp: now + 170},
			];
			for (const times of ok) {
				await (await nodeRequest('GET', nodeJwt(nodeClaims('GET', '', times)))).execute();
			}
			const skewed = [{iat: now - 300, exp: now - 180}, {iat: now + 180, exp: now + 300}, {exp: now + 3600}];
			for (const times of skewed) {
				await (await nodeRequest('GET', nodeJwt(nodeClaims('GET', '', times))))
					.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_NODE_TOKEN_EXPIRED)
					.execute();
			}
			await (await nodeRequest('GET', nodeJwt(nodeClaims('GET', '', {exp: now - 300}), 'b'.repeat(64))))
				.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID)
				.execute();
		});

		test('rejects tokens that are not node tokens', async () => {
			const cases: Array<[string, string]> = [
				[nodeJwt(nodeClaims('GET', ''), 'b'.repeat(64)), APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID],
				[nodeJwt(nodeClaims('DELETE', '')), APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID],
				[nodeJwt(nodeClaims('GET', '', {path: '/voice/other'})), APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID],
				[nodeJwt(nodeClaims('GET', '', {aud: undefined})), APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID],
				[nodeJwt(nodeClaims('GET', '', {video: {roomJoin: true}})), APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID],
				[nodeJwt(nodeClaims('GET', '', {iss: 'nobody'})), APIErrorCodes.VOICE_NODE_UNKNOWN],
			];
			for (const [auth, code] of cases) {
				await (await nodeRequest('GET', auth)).expect(HTTP_STATUS.UNAUTHORIZED, code).execute();
			}
			const participant = new AccessToken('fluxer-fra-server-1', SECRET, {identity: 'user_1_x'});
			participant.addGrant({roomJoin: true, room: 'guild_1_channel_2'});
			await (await nodeRequest('GET', `Bearer ${await participant.toJwt()}`))
				.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID)
				.execute();
			const body = {draining: true};
			await (await nodeRequest('PATCH', nodeJwt(nodeClaims('PATCH', '{"draining":false}')), body))
				.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID)
				.execute();
		});

		test('cannot turn itself on or change its region', async () => {
			for (const body of [{is_active: true}, {region_id: 'other'}, {internal_endpoint: 'http://10.0.0.1:7880'}]) {
				await (await nodeRequest('PATCH', nodeJwt(nodeClaims('PATCH', JSON.stringify(body))), body))
					.expect(HTTP_STATUS.BAD_REQUEST)
					.execute();
			}
		});

		test('tells a deleted server from a changed secret', async () => {
			await createBuilder(harness, admin.token)
				.patch('/admin/voice/regions/fra/servers/fra-server-1')
				.body({api_secret: 'c'.repeat(64)})
				.execute();
			await (await nodeRequest('GET', nodeJwt(nodeClaims('GET', ''))))
				.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_NODE_SIGNATURE_INVALID)
				.execute();
			await (await nodeRequest('DELETE', nodeJwt(nodeClaims('DELETE', ''), 'c'.repeat(64))))
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await (await nodeRequest('GET', nodeJwt(nodeClaims('GET', ''), 'c'.repeat(64))))
				.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_NODE_UNKNOWN)
				.execute();
		});
	});

	describe('node routes on a server that joined with a hostname and soft limit', () => {
		const pinnedClaims = (method: string, body: string) => nodeClaims(method, body, {iss: 'fluxer-pinned'});

		beforeEach(async () => {
			const created = await createToken({
				server_id: 'fra-pinned',
				hostname: 'voice.example.com',
				soft_connection_limit: 100,
				internal_endpoint: 'http://10.0.0.5:7880',
			});
			await join(
				created.token,
				joinBody({api_key: 'fluxer-pinned', endpoint: 'wss://voice.example.com/livekit-fra-pinned'}),
			);
		});

		async function patch(body: Record<string, unknown>) {
			return createBuilder<VoiceNodeResponse>(harness, nodeJwt(pinnedClaims('PATCH', JSON.stringify(body))))
				.patch(VOICE_NODE_PATH)
				.body(body);
		}

		test('keeps the hostname of the token', async () => {
			await (await patch({endpoint: 'wss://other.example.com/livekit-fra-pinned'}))
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.VOICE_JOIN_ENDPOINT_INVALID)
				.execute();
			const moved = await (await patch({endpoint: 'wss://voice.example.com/livekit-moved'})).execute();
			expect(moved.endpoint).toBe('wss://voice.example.com/livekit-moved');
		});

		test('keeps the soft limit of the token', async () => {
			await (await patch({soft_connection_limit: 5000})).expect(HTTP_STATUS.BAD_REQUEST).execute();
			const same = await (await patch({soft_connection_limit: 100})).execute();
			expect(same.soft_connection_limit).toBe(100);
		});

		test('checks the new public endpoint even with an internal endpoint', async () => {
			const probes: Array<[string, boolean]> = [];
			setVoiceNodeEndpointVerifierForTesting(async (endpoint, _keys, internal, probePublic) => {
				probes.push([endpoint, probePublic && internal !== null]);
			});
			await (await patch({endpoint: 'wss://voice.example.com/livekit-new'})).execute();
			expect(probes).toEqual([['wss://voice.example.com/livekit-new', true]]);
		});

		test('moves when the public hostname resolves to a private address for the API', async () => {
			setVoiceNodeEndpointVerifierForTesting(null);
			setVoiceEndpointResolverForTesting(async () => ['10.0.0.9']);
			const seen: Array<string> = [];
			server.use(
				http.get('http://10.0.0.5:7880/', () => {
					seen.push('probe');
					return HttpResponse.text('OK');
				}),
				http.post('http://10.0.0.5:7880/twirp/livekit.RoomService/ListRooms', () => {
					seen.push('keys');
					return HttpResponse.json({rooms: []});
				}),
			);
			try {
				const moved = await (await patch({endpoint: 'wss://voice.example.com/livekit-split'})).execute();
				expect(moved.endpoint).toBe('wss://voice.example.com/livekit-split');
				expect(seen).toEqual(['probe', 'keys']);
			} finally {
				setVoiceEndpointResolverForTesting(null);
			}
		});
	});

	test('puts the public port of the instance in the join command', () => {
		expect(voiceJoinCommand('https://chat.example.com:8443', 'chat.example.com', 'fvj_1_x')).toBe(
			'curl -fsSL https://fluxer.dev/install.sh | sh -s -- voice chat.example.com:8443 fvj_1_x',
		);
		expect(voiceJoinCommand('https://chat.example.com', 'chat.example.com', 'fvj_1_x')).toBe(
			'curl -fsSL https://fluxer.dev/install.sh | sh -s -- voice chat.example.com fvj_1_x',
		);
		expect(voiceJoinCommand('https://chat.example.com:443/', 'chat.example.com', 'fvj_1_x')).toContain(
			' voice chat.example.com fvj_1_x',
		);
	});

	test('needs a token for preview', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/voice/join/preview')
			.body({})
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.VOICE_JOIN_TOKEN_INVALID)
			.execute();
	});
});

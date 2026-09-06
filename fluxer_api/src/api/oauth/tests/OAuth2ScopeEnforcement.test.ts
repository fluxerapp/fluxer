// SPDX-License-Identifier: AGPL-3.0-or-later

import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createAdminApiKey} from '../../admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {createGuild, createRole, getRoles} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	authorizeOAuth2,
	createOAuth2Application,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
} from './OAuthTestUtils';

interface OAuth2TokenResponse {
	token: string;
	user_id: string;
	scopes: Array<string>;
	application_id: string;
}

interface UserMeResponse {
	id: string;
	username: string;
	email?: string | null;
}

interface UserGuildResponse {
	id: string;
	name: string;
	permissions?: string;
}

interface UserGuildRoleResponse {
	id: string;
	name: string;
	permissions: string;
}

async function createOAuth2Token(
	harness: ApiTestHarness,
	userId: string,
	scopes: Array<string>,
	applicationId?: string,
): Promise<OAuth2TokenResponse> {
	return createBuilder<OAuth2TokenResponse>(harness, '')
		.post('/test/oauth2/access-token')
		.body({
			user_id: userId,
			scopes,
			...(applicationId ? {application_id: applicationId} : {}),
		})
		.execute();
}

describe('OAuth2 Scope Enforcement', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(() => {
		Config.dev.validateResponses = true;
	});
	describe('Scope enforcement for user endpoints (/users/@me)', () => {
		test('GET /users/@me with bearer token succeeds when user is authenticated', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			const json = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(account.userId);
			expect(json.username).toBe(account.username);
		});
		test('GET /users/@me with bearer token returns user without email when only identify scope', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			const json = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(account.userId);
			expect(json.email).toBeNull();
		});
		test('GET /users/@me with bearer token returns email when email scope is present', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'email']);
			const json = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(account.userId);
			expect(json.email).toBe(account.email);
		});
		test('GET /users/@me with session token (no scope check) succeeds', async () => {
			const account = await createTestAccount(harness);
			const json = await createBuilder<UserMeResponse>(harness, account.token)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(account.userId);
			expect(json.email).toBe(account.email);
		});
		test('GET /users/@me with bot token (no scope check) succeeds', async () => {
			const appOwner = await createTestAccount(harness);
			const application = await createOAuth2Application(harness, appOwner, {
				name: 'Bot Token Test',
				redirect_uris: [],
			});
			const json = await createBuilder<UserMeResponse>(harness, `Bot ${application.bot.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(application.bot.id);
		});
		test('GET /users/@me with bearer token fails when identify scope is missing', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['guilds']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('GET /users/@me/settings rejects OAuth2 bearer tokens on unsupported endpoints', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'email']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/settings')
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
		});
	});
	describe('Scope enforcement for guild endpoints (/users/@me/guilds)', () => {
		test('GET /users/@me/guilds with guilds scope succeeds', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'guilds']);
			const json = await createBuilder<Array<UserGuildResponse>>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(json)).toBe(true);
			expect(json.length).toBeGreaterThan(0);
			expect(json[0]?.name).toBe('Test Guild');
		});
		test('GET /guilds/:guild_id with guilds scope succeeds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['guilds']);
			const json = await createBuilder<UserGuildResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get(`/guilds/${guild.id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(guild.id);
			expect(json.name).toBe('Test Guild');
		});
		test('GET /guilds/:guild_id without guilds scope fails with MISSING_OAUTH_SCOPE', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/guilds/${guild.id}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('GET /guilds/:guild_id/roles with guilds scope succeeds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const role = await createRole(harness, account.token, guild.id, {name: 'OAuth Visible Role'});
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['guilds']);
			const json = await getRoles(harness, `Bearer ${oauth2Token.token}`, guild.id);
			const roles = json as Array<UserGuildRoleResponse>;
			expect(roles.some((item) => item.id === role.id && item.name === 'OAuth Visible Role')).toBe(true);
		});
		test('GET /guilds/:guild_id/roles without guilds scope fails with MISSING_OAUTH_SCOPE', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/guilds/${guild.id}/roles`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('GET /users/@me/guilds without guilds scope fails with MISSING_OAUTH_SCOPE', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('GET /users/@me/guilds with session token (no scope check) succeeds', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const json = await createBuilder<Array<UserGuildResponse>>(harness, account.token)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(json)).toBe(true);
			expect(json.length).toBeGreaterThan(0);
		});
		test('GET /users/@me/guilds with bot token (no scope check) succeeds', async () => {
			const appOwner = await createTestAccount(harness);
			const application = await createOAuth2Application(harness, appOwner, {
				name: 'Bot Guilds Test',
				redirect_uris: [],
			});
			const json = await createBuilder<Array<UserGuildResponse>>(harness, `Bot ${application.bot.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(json)).toBe(true);
		});
	});
	describe('Scope enforcement for connections endpoint (/users/@me/connections)', () => {
		test('GET /users/@me/connections with connections scope succeeds', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'connections']);
			const json = await createBuilder<Array<unknown>>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/connections')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(json)).toBe(true);
		});
		test('GET /users/@me/connections without connections scope fails with MISSING_OAUTH_SCOPE', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/connections')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('GET /users/@me/connections with session token succeeds', async () => {
			const account = await createTestAccount(harness);
			const json = await createBuilder<Array<unknown>>(harness, account.token)
				.get('/users/@me/connections')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(json)).toBe(true);
		});
	});
	describe('Scope enforcement for userinfo endpoint (/oauth2/userinfo)', () => {
		test('GET /oauth2/userinfo with identify scope succeeds', async () => {
			const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
			const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
				client_id: application.id,
				redirect_uri: redirectURI,
				scope: 'identify',
			});
			const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code: authCodeResponse.code,
				redirect_uri: redirectURI,
			});
			await createBuilder(harness, `Bearer ${tokenResponse.access_token}`)
				.get('/oauth2/userinfo')
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('GET /oauth2/userinfo without identify scope fails with MISSING_OAUTH_SCOPE', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['guilds']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/oauth2/userinfo')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('GET /oauth2/userinfo omits user ACLs when response validation is disabled', async () => {
			const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
			const staffUser = await setUserACLs(harness, endUser, ['admin:authenticate']);
			const authCodeResponse = await authorizeOAuth2(harness, staffUser.token, {
				client_id: application.id,
				redirect_uri: redirectURI,
				scope: 'identify',
			});
			const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code: authCodeResponse.code,
				redirect_uri: redirectURI,
			});
			Config.dev.validateResponses = false;
			const json = await createBuilder<Record<string, unknown>>(harness, `Bearer ${tokenResponse.access_token}`)
				.get('/oauth2/userinfo')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json).not.toHaveProperty('acls');
		});
		test('GET /oauth2/userinfo with session token is unauthorized', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.get('/oauth2/userinfo')
				.expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED')
				.execute();
		});
	});
	describe('Multiple scope scenarios', () => {
		test('Token with identify but not email cannot access email in response', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			const json = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.email).toBeNull();
		});
		test('Token with identify and email can access both user info and email', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'email']);
			const json = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(account.userId);
			expect(json.email).toBe(account.email);
		});
		test('Token with guilds but not identify cannot access /users/@me/guilds without auth context', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['guilds']);
			const json = await createBuilder<Array<UserGuildResponse>>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(json)).toBe(true);
			expect(json.length).toBeGreaterThan(0);
		});
		test('Token with all common scopes can access all corresponding endpoints', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'email', 'guilds']);
			const userJson = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson.id).toBe(account.userId);
			expect(userJson.email).toBe(account.email);
			const guildsJson = await createBuilder<Array<UserGuildResponse>>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(guildsJson)).toBe(true);
			expect(guildsJson.length).toBeGreaterThan(0);
		});
	});
	describe('Edge cases', () => {
		test('Empty scopes set cannot access /users/@me', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, []);
			await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('Token with empty scopes cannot access guilds endpoint', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, []);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('Scope checking does not affect session tokens', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const userJson = await createBuilder<UserMeResponse>(harness, account.token)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson.id).toBe(account.userId);
			expect(userJson.email).toBe(account.email);
			const guildsJson = await createBuilder<Array<UserGuildResponse>>(harness, account.token)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(guildsJson)).toBe(true);
		});
		test('Scope checking does not affect bot tokens', async () => {
			const appOwner = await createTestAccount(harness);
			const application = await createOAuth2Application(harness, appOwner, {
				name: 'Bot Scope Check Test',
				redirect_uris: [],
			});
			const userJson = await createBuilder<UserMeResponse>(harness, `Bot ${application.bot.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson.id).toBe(application.bot.id);
			const guildsJson = await createBuilder<Array<UserGuildResponse>>(harness, `Bot ${application.bot.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(guildsJson)).toBe(true);
		});
	});
	describe('Admin OAuth2 application integration', () => {
		test('built-in admin OAuth2 token can access admin endpoints when user has proper ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const oauth2Token = await createOAuth2Token(
				harness,
				admin.userId,
				['identify', 'email'],
				ADMIN_OAUTH2_APPLICATION_ID.toString(),
			);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('third-party OAuth2 token cannot access admin endpoints even with proper user ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const oauth2Token = await createOAuth2Token(harness, admin.userId, ['identify', 'email']);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
		});
		test('Session token can access admin endpoints with proper ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('Admin API key can access admin endpoints with granted ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
			const apiKey = await createAdminApiKey(harness, admin, 'Test Key', ['user:lookup'], null);
			await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
		});
		test('built-in admin OAuth2 token still requires proper user ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			const oauth2Token = await createOAuth2Token(
				harness,
				admin.userId,
				['identify', 'email'],
				ADMIN_OAUTH2_APPLICATION_ID.toString(),
			);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
		});
		test('built-in admin OAuth2 token without admin:authenticate ACL cannot access admin endpoints', async () => {
			const user = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(
				harness,
				user.userId,
				['identify', 'email'],
				ADMIN_OAUTH2_APPLICATION_ID.toString(),
			);
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${user.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
	});
	describe('Negative tests - Error response verification', () => {
		test('Endpoint without proper guilds scope returns 403 with MISSING_OAUTH_SCOPE code', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
			const json = await createBuilder<{
				code: string;
				message: string;
			}>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
			expect(json.code).toBe('MISSING_OAUTH_SCOPE');
		});
		test('Admin endpoint with third-party OAuth2 token returns ACCESS_DENIED', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const oauth2Token = await createOAuth2Token(harness, admin.userId, ['identify', 'email']);
			const json = await createBuilder<{
				code: string;
				message: string;
			}>(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
			expect(json.code).toBe('ACCESS_DENIED');
		});
		test('Admin endpoint with built-in admin OAuth2 token but missing ACL returns MISSING_ACL code', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			const oauth2Token = await createOAuth2Token(
				harness,
				admin.userId,
				['identify', 'email'],
				ADMIN_OAUTH2_APPLICATION_ID.toString(),
			);
			const json = await createBuilder<{
				code: string;
				message: string;
			}>(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
			expect(json.code).toBe('MISSING_ACL');
		});
		test('Unauthenticated request returns 401 UNAUTHORIZED', async () => {
			await createBuilder(harness, '').get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED').execute();
		});
		test('Invalid bearer token returns 401', async () => {
			await createBuilder(harness, 'Bearer invalid_token_12345')
				.get('/users/@me')
				.expect(HTTP_STATUS.UNAUTHORIZED, 'UNAUTHORIZED')
				.execute();
		});
	});
	describe('Scope combinations and boundaries', () => {
		test('Token with only email scope (no identify) cannot access user info', async () => {
			const account = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account.userId, ['email']);
			await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('Multiple OAuth2 tokens with different scopes work independently', async () => {
			const account = await createTestAccount(harness);
			await createGuild(harness, account.token, 'Test Guild');
			const identifyToken = await createOAuth2Token(harness, account.userId, ['identify']);
			const guildsToken = await createOAuth2Token(harness, account.userId, ['identify', 'guilds']);
			const fullToken = await createOAuth2Token(harness, account.userId, ['identify', 'email', 'guilds']);
			const userJson1 = await createBuilder<UserMeResponse>(harness, `Bearer ${identifyToken.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson1.email).toBeNull();
			await createBuilder(harness, `Bearer ${identifyToken.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
			const guildsJson = await createBuilder<Array<UserGuildResponse>>(harness, `Bearer ${guildsToken.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(guildsJson.length).toBeGreaterThan(0);
			const userJson2 = await createBuilder<UserMeResponse>(harness, `Bearer ${guildsToken.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson2.email).toBeNull();
			const userJson3 = await createBuilder<UserMeResponse>(harness, `Bearer ${fullToken.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson3.email).toBe(account.email);
			const guildsJson2 = await createBuilder<Array<UserGuildResponse>>(harness, `Bearer ${fullToken.token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(guildsJson2.length).toBeGreaterThan(0);
		});
		test('Token created for one user cannot access another users data', async () => {
			const account1 = await createTestAccount(harness);
			const account2 = await createTestAccount(harness);
			const oauth2Token = await createOAuth2Token(harness, account1.userId, ['identify', 'email']);
			const json = await createBuilder<UserMeResponse>(harness, `Bearer ${oauth2Token.token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.id).toBe(account1.userId);
			expect(json.id).not.toBe(account2.userId);
		});
	});
	describe('Real OAuth2 flow scope enforcement', () => {
		test('OAuth2 token from real authorization flow respects scope restrictions', async () => {
			const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
			await createGuild(harness, endUser.token, 'Test Guild');
			const {authorizeOAuth2, exchangeOAuth2AuthorizationCode} = await import('./OAuthTestUtils');
			const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
				client_id: application.id,
				redirect_uri: redirectURI,
				scope: 'identify',
			});
			const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code: authCodeResponse.code,
				redirect_uri: redirectURI,
			});
			const userJson = await createBuilder<UserMeResponse>(harness, `Bearer ${tokenResponse.access_token}`)
				.get('/users/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(userJson.id).toBe(endUser.userId);
			expect(userJson.email).toBeNull();
			await createBuilder(harness, `Bearer ${tokenResponse.access_token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
				.execute();
		});
		test('OAuth2 token with guilds scope from real flow can access guilds', async () => {
			const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
			await createGuild(harness, endUser.token, 'Real Flow Guild');
			const {authorizeOAuth2, exchangeOAuth2AuthorizationCode} = await import('./OAuthTestUtils');
			const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
				client_id: application.id,
				redirect_uri: redirectURI,
				scope: 'identify guilds',
			});
			const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code: authCodeResponse.code,
				redirect_uri: redirectURI,
			});
			const guildsJson = await createBuilder<Array<UserGuildResponse>>(harness, `Bearer ${tokenResponse.access_token}`)
				.get('/users/@me/guilds')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(guildsJson)).toBe(true);
			expect(guildsJson.length).toBeGreaterThan(0);
			expect(guildsJson.some((g) => g.name === 'Real Flow Guild')).toBe(true);
		});
	});
});

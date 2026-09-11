// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_APPLICATION_REDIRECT_URIS} from '@fluxer/constants/src/LimitConstants';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {
	ApplicationCreateRequest,
	ApplicationResponse,
	ApplicationUpdateRequest,
	AuthorizeConsentRequest,
	AuthorizeRequest,
	IntrospectRequestForm,
	RevokeRequestForm,
} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import {describe, expect, it} from 'vitest';

const buildRedirectURIs = (count: number) =>
	Array.from({length: count}, (_, index) => `https://example.com/callback/${index}`);

const buildApplicationResponse = (redirectURIs: Array<string>) => ({
	id: '1234567890123456789',
	name: 'Test Application',
	redirect_uris: redirectURIs,
	bot_public: true,
	bot_require_code_grant: false,
});

describe('application redirect_uris bounds', () => {
	it('accepts the maximum redirect URI count on both the request and the response', () => {
		const uris = buildRedirectURIs(MAX_APPLICATION_REDIRECT_URIS);
		expect(ApplicationCreateRequest.safeParse({name: 'Test Application', redirect_uris: uris}).success).toBe(true);
		expect(ApplicationResponse.safeParse(buildApplicationResponse(uris)).success).toBe(true);
	});
	it('rejects one redirect URI beyond the maximum on both the request and the response', () => {
		const uris = buildRedirectURIs(MAX_APPLICATION_REDIRECT_URIS + 1);
		expect(ApplicationCreateRequest.safeParse({name: 'Test Application', redirect_uris: uris}).success).toBe(false);
		expect(ApplicationResponse.safeParse(buildApplicationResponse(uris)).success).toBe(false);
	});
});

describe.each([
	{name: 'authorize query', schema: AuthorizeRequest},
	{name: 'consent request', schema: AuthorizeConsentRequest},
])('$name', ({schema}) => {
	const required = {client_id: '123', scope: 'identify'};
	it('normalizes shared fields without adding optional defaults', () => {
		expect(schema.parse({client_id: ' 123 ', scope: ' identify '})).toEqual({client_id: 123n, scope: 'identify'});
	});
	it.each(['S256', 'plain'])('preserves the PKCE method %s and shared fields', (method) => {
		expect(
			schema.parse({
				...required,
				redirect_uri: ' https://example.com/callback ',
				state: ' nonce ',
				permissions: '0',
				guild_id: '456',
				channel_id: '789',
				code_challenge: ' challenge ',
				code_challenge_method: method,
			}),
		).toEqual({
			client_id: 123n,
			scope: 'identify',
			redirect_uri: 'https://example.com/callback',
			state: 'nonce',
			permissions: '0',
			guild_id: 456n,
			channel_id: 789n,
			code_challenge: 'challenge',
			code_challenge_method: method,
		});
	});
	it.each([
		{client_id: undefined},
		{client_id: '1.5'},
		{scope: undefined},
		{scope: ' '},
		{state: ''},
		{redirect_uri: '/relative'},
		{code_challenge: ''},
		{code_challenge_method: 's256'},
	])('rejects malformed shared fields %j', (fields) => {
		expect(schema.safeParse({...required, ...fields}).success).toBe(false);
	});
	it('leaves registered redirect policy to the authorization service', () => {
		expect(schema.parse({...required, redirect_uri: 'http://example.com/callback'}).redirect_uri).toBe(
			'http://example.com/callback',
		);
	});
});

describe('authorization endpoint differences', () => {
	it('keeps response_type validation strict only for the authorize query', () => {
		const input = {client_id: '123', scope: 'identify', response_type: 'token'};
		expect(AuthorizeRequest.safeParse(input).success).toBe(false);
		expect(AuthorizeConsentRequest.parse(input).response_type).toBe('token');
	});
	it('does not leak query-only controls into the consent request', () => {
		const input = {client_id: '123', scope: 'identify', prompt: 'none', disable_guild_select: 'true'};
		expect(AuthorizeRequest.parse(input)).toEqual({...input, client_id: 123n});
		expect(AuthorizeConsentRequest.parse(input)).toEqual({client_id: 123n, scope: 'identify'});
	});
});

describe.each([
	{name: 'introspection', schema: IntrospectRequestForm},
	{name: 'revocation', schema: RevokeRequestForm},
])('$name credentials', ({schema}) => {
	it('keeps body credentials optional for authorization-header authentication', () => {
		expect(schema.parse({token: ' token '})).toEqual({token: 'token'});
	});
	it('normalizes explicit client credentials', () => {
		expect(schema.parse({token: 'token', client_id: '123', client_secret: ' secret '})).toEqual({
			token: 'token',
			client_id: 123n,
			client_secret: 'secret',
		});
	});
	it.each([{token: ''}, {client_id: 'not-an-id'}, {client_secret: ''}])('rejects invalid credentials %j', (fields) => {
		expect(schema.safeParse({token: 'token', ...fields}).success).toBe(false);
	});
});

it.each(['access_token', 'refresh_token'])('preserves %s hint only on revocation', (hint) => {
	const input = {token: 'token', token_type_hint: hint};
	expect(RevokeRequestForm.parse(input)).toEqual(input);
	expect(IntrospectRequestForm.parse(input)).toEqual({token: 'token'});
});

describe('application redirect policies', () => {
	it.each([
		'https://example.com/callback',
		'http://localhost/callback',
		'http://app.localhost/callback',
		'http://127.0.0.1:3000/callback',
		'http://[::1]:3000/callback',
		'http://192.0.2.1/callback',
	])('accepts registered URI %s', (uri) => {
		expect(ApplicationCreateRequest.parse({name: 'Application', redirect_uris: [uri]}).redirect_uris).toEqual([uri]);
	});
	it.each([
		'http://example.com/callback',
		'https://',
		'/relative',
		'ftp://example.com/callback',
	])('rejects registered URI %s', (uri) => {
		expect(ApplicationCreateRequest.safeParse({name: 'Application', redirect_uris: [uri]}).success).toBe(false);
	});
	it('defaults missing create URIs but does not clear missing update URIs', () => {
		expect(ApplicationCreateRequest.parse({name: 'Application'})).toEqual({name: 'Application', redirect_uris: []});
		expect(ApplicationUpdateRequest.parse({})).toEqual({});
	});
	it.each([null, []])('clears explicitly supplied URIs %j', (uris) => {
		expect(ApplicationUpdateRequest.parse({redirect_uris: uris})).toEqual({redirect_uris: []});
	});
});

describe('application bot authenticators', () => {
	const response = {
		...buildApplicationResponse([]),
		bot: {id: '123', username: 'bot', discriminator: '0001', bio: null, flags: 0},
	};
	it('accepts every canonical authenticator type', () => {
		const types = Object.values(UserAuthenticatorTypes);
		expect(ApplicationResponse.parse({...response, bot: {...response.bot, authenticator_types: types}}).bot).toEqual({
			...response.bot,
			authenticator_types: types,
		});
	});
	it.each([-1, 0.5, 999])('rejects unknown authenticator %j', (type) => {
		expect(
			ApplicationResponse.safeParse({...response, bot: {...response.bot, authenticator_types: [type]}}).success,
		).toBe(false);
	});
});

import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	WebAuthnAuthenticateRequest,
	WebAuthnMfaRequest,
	WebAuthnRegisterRequest,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	UserSettingsUpdateRequest,
	UserUpdateWithVerificationRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';
import type {ZodType} from 'zod';
import {Validator} from '../../Validator';

const credential = {
	id: 'credential-id',
	rawId: 'credential-id',
	type: 'public-key',
	clientExtensionResults: {},
};
const registration = {
	...credential,
	response: {clientDataJSON: 'client-data', attestationObject: 'attestation'},
};
const authentication = {
	...credential,
	response: {clientDataJSON: '', authenticatorData: '', signature: '', userHandle: ''},
};

async function validateRequest(schema: ZodType, body: unknown): Promise<Response> {
	const app = new Hono();
	app.onError((error, ctx) => {
		if (error instanceof InputValidationError) {
			return ctx.json({errors: error.getLocalizedErrors()}, 400);
		}
		throw error;
	});
	app.post('/validate', Validator('json', schema), (ctx) => Response.json(ctx.req.valid('json')));
	return app.request('/validate', {
		method: 'POST',
		headers: {'Content-Type': 'application/json'},
		body: JSON.stringify(body),
	});
}

describe.each([
	{
		name: 'registration',
		schema: WebAuthnRegisterRequest,
		field: 'response',
		response: registration,
		body: {challenge: 'challenge', name: 'Passkey'},
		challenge: 'challenge',
	},
	{
		name: 'authentication',
		schema: WebAuthnAuthenticateRequest,
		field: 'response',
		response: authentication,
		body: {challenge: 'challenge'},
		challenge: 'challenge',
	},
	{
		name: 'MFA login',
		schema: WebAuthnMfaRequest,
		field: 'response',
		response: authentication,
		body: {challenge: 'challenge', ticket: 'ticket'},
		challenge: 'challenge',
	},
	{
		name: 'sudo account update',
		schema: UserUpdateWithVerificationRequest,
		field: 'webauthn_response',
		response: authentication,
		body: {mfa_method: 'webauthn', webauthn_challenge: 'challenge'},
		challenge: 'webauthn_challenge',
	},
])('$name validation', ({schema, field, response, body, challenge}) => {
	it.each([
		{name: 'empty results', value: {}},
		{
			name: 'nested empty values and unknown extensions',
			value: {credProps: {}, appid: false, futureExtension: {empty: {}, text: '', entries: [{}, '', null, 0]}},
		},
	])('preserves $name', async ({value: clientExtensionResults}) => {
		const input = {...body, [field]: {...response, clientExtensionResults}};
		const result = await validateRequest(schema, input);
		expect(result.status).toBe(200);
		expect(await result.json()).toEqual(input);
	});

	it.each([
		{name: 'missing results', value: undefined, path: ''},
		{name: 'null results', value: null, path: ''},
		{name: 'array results', value: [], path: ''},
		{name: 'non-boolean appid', value: {appid: 'false'}, path: '.appid'},
		{name: 'null credential properties', value: {credProps: null}, path: '.credProps'},
	])('rejects $name', async ({value, path}) => {
		const result = await validateRequest(schema, {...body, [field]: {...response, clientExtensionResults: value}});
		expect(result.status).toBe(400);
		expect(await result.json()).toMatchObject({errors: [{path: `${field}.clientExtensionResults${path}`}]});
	});

	it.each([
		{key: 'id', value: undefined},
		{key: 'rawId', value: undefined},
		{key: 'type', value: 'invalid'},
		{key: 'response', value: null},
	])('rejects malformed credential $key', async ({key, value}) => {
		const result = await validateRequest(schema, {...body, [field]: {...response, [key]: value}});
		expect(result.status).toBe(400);
		expect(await result.json()).toMatchObject({errors: [{path: `${field}.${key}`}]});
	});

	it('still validates the challenge outside the credential', async () => {
		const result = await validateRequest(schema, {...body, [field]: response, [challenge]: ''});
		expect(result.status).toBe(400);
		expect(await result.json()).toMatchObject({errors: [{path: challenge}]});
	});
});

it('retains profile clearing beside a WebAuthn sudo response', async () => {
	const result = await validateRequest(UserUpdateWithVerificationRequest, {
		webauthn_response: authentication,
		avatar: '',
		banner: '',
		bio: '',
	});
	expect(result.status).toBe(200);
	expect(await result.json()).toEqual({webauthn_response: authentication, avatar: null, banner: null, bio: null});
});

it('retains empty-object clearing outside WebAuthn credentials', async () => {
	const result = await validateRequest(UserSettingsUpdateRequest, {custom_status: {}});
	expect(result.status).toBe(200);
	expect(await result.json()).toEqual({custom_status: null});
});

it.each(['__proto__', 'constructor', 'toString'])('does not treat unknown key %s as a schema field', async (field) => {
	const result = await validateRequest(UserUpdateWithVerificationRequest, {[field]: {}, avatar: ''});
	expect(result.status).toBe(200);
	expect(await result.json()).toEqual({avatar: null});
});

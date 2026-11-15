// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {registerControllers} from '@app/api/app/ControllerRegistry';
import {Config} from '@app/api/Config';
import ContentFilterMiddleware, {
	extractStringValues,
	shouldSkipContentFilterPath,
} from '@app/api/middleware/ContentFilterMiddleware';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {requestBodySchemas} from '@app/api/middleware/RequestBodyBinaryFields';
import {MultipartUploadRoute} from '@app/api/middleware/RequestBodyLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {requireRequestJsonBody} from '@app/api/utils/RequestJsonBody';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {MessageRequestSchema} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {collectBodyFieldPaths} from '@fluxer/schema/src/primitives/Base64BinaryFields';
import {Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const BANNED = 'zebracrossingbanned';

function buildApp() {
	const app = new Hono<HonoEnv>({strict: true});
	app.use(ContentFilterMiddleware);
	app.patch('/profile', async (ctx) => ctx.json(await requireRequestJsonBody(ctx.req)));
	app.post(
		'/messages',
		OpenAPI({
			operationId: 'send_test_message',
			summary: 'Send a test message',
			description: 'Echoes the message body',
			requestSchema: MessageRequestSchema,
			responseSchema: null,
			tags: 'Test',
		}),
		async (ctx) => ctx.json(await requireRequestJsonBody(ctx.req)),
	);
	app.post('/upload', MultipartUploadRoute(), async (ctx) => ctx.json({ok: true}));
	app.post('/form', async (ctx) => ctx.json(Object.fromEntries(await ctx.req.formData())));
	app.onError(AppErrorHandler);
	return app;
}

describe('extractStringValues', () => {
	test('omits fields with identifier and secret suffixes', () => {
		const body = {
			label: 'visible label',
			actor_user_id: '111111111111111111',
			channel_ids: ['222222222222222222'],
			content_hash: 'f'.repeat(64),
			encryption_key: 'p256dh-value',
			original_proof: 'email-proof-token',
			state_token: 'state-token',
		};
		const result = extractStringValues(body);
		expect(result).toEqual(['visible label']);
	});
});

describe('binary request body fields', () => {
	async function schemaNames(): Promise<Map<unknown, string>> {
		const root = path.resolve(import.meta.dirname, '../../../../../packages/schema/src/domains');
		const names = new Map<unknown, string>();
		for (const file of fs.readdirSync(root, {recursive: true, encoding: 'utf8'})) {
			if (!file.endsWith('.ts') || file.endsWith('.test.ts')) continue;
			const exports: Record<string, unknown> = await import(path.join(root, file));
			for (const [exportName, value] of Object.entries(exports)) {
				names.set(value, `${path.basename(file, '.ts')}.${exportName}`);
			}
		}
		return names;
	}

	test('no request body field is base64 in one schema and text in another', async () => {
		registerControllers(new Hono<HonoEnv>(), {
			...Config,
			dev: {...Config.dev, testModeEnabled: true},
			instance: {...Config.instance, selfHosted: false},
		});
		const names = await schemaNames();
		const binarySchemas = new Map<string, Array<string>>();
		const otherSchemas = new Map<string, Array<string>>();
		for (const schema of requestBodySchemas) {
			const name = names.get(schema) ?? 'inline schema';
			const paths = collectBodyFieldPaths(schema);
			for (const path of paths.binary) binarySchemas.set(path, [...(binarySchemas.get(path) ?? []), name]);
			for (const path of paths.other) otherSchemas.set(path, [...(otherSchemas.get(path) ?? []), name]);
		}
		const collisions = [...binarySchemas.keys()]
			.filter((path) => otherSchemas.has(path))
			.map(
				(path) =>
					`${path}: base64 in ${binarySchemas.get(path)!.join(', ')}, text in ${otherSchemas.get(path)!.join(', ')}`,
			);
		expect(binarySchemas.size).toBeGreaterThan(0);
		expect(collisions).toEqual([]);
	});
});

describe('shouldSkipContentFilterPath', () => {
	test('skips credential-only request bodies', () => {
		const paths = [
			'/users/@me/email-change/verify-original',
			'/users/@me/mfa/totp/enable',
			'/users/@me/password-change/complete',
			'/reports/dsa/email/verify',
		];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([true, true, true, true]);
	});
	test('does not skip in-app report submissions', () => {
		const paths = ['/reports/message', '/reports/user', '/reports/flows/message/submissions'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([false, false, false]);
	});
	test('does not skip public content update request bodies', () => {
		const paths = ['/guilds/123/vanity-url', '/channels/123/messages', '/users/@me'];
		const result = paths.map((path) => shouldSkipContentFilterPath(path));
		expect(result).toEqual([false, false, false]);
	});
});

describe('content filter content type handling', () => {
	beforeEach(() => phraseBlocklistCache.add(BANNED));
	afterEach(() => phraseBlocklistCache.remove(BANNED));

	test.each([
		'application/json',
		'application/json; x=multipart/form-data',
		'application/json; x=application/x-www-form-urlencoded',
		'multipart/form-data; boundary=x',
		'application/x-www-form-urlencoded',
	])('blocks a banned phrase in a JSON body sent as %s', async (contentType) => {
		const response = await buildApp().request('/profile', {
			method: 'PATCH',
			headers: {'Content-Type': contentType},
			body: JSON.stringify({bio: `hello ${BANNED}`}),
		});
		expect(response.status).toBe(403);
	});

	test('blocks a banned phrase in an embed image url', async () => {
		const response = await buildApp().request('/profile', {
			method: 'PATCH',
			headers: {'Content-Type': 'application/json'},
			body: JSON.stringify({embeds: [{image: {url: `https://example.com/${BANNED}.png`}}]}),
		});
		expect(response.status).toBe(403);
	});

	test('skips a voice message waveform but still screens the message text', async () => {
		const send = (content: string) =>
			buildApp().request('/messages', {
				method: 'POST',
				headers: {'Content-Type': 'application/json'},
				body: JSON.stringify({content, attachments: [{id: 0, filename: 'voice.ogg', waveform: `AAAA${BANNED}A`}]}),
			});
		expect((await send('hello')).status).toBe(200);
		expect((await send(`hello ${BANNED}`)).status).toBe(403);
	});

	test('leaves multipart bodies on upload routes unread', async () => {
		const form = new FormData();
		form.set('payload_json', JSON.stringify({content: `hello ${BANNED}`}));
		form.set('files[0]', new File([new Uint8Array([1, 2, 3])], 'a.bin'));
		const response = await buildApp().request('/upload', {method: 'POST', body: form});
		expect(response.status).toBe(200);
	});

	test('keeps form bodies readable on form routes', async () => {
		const response = await buildApp().request('/form', {
			method: 'POST',
			headers: {'Content-Type': 'application/x-www-form-urlencoded'},
			body: 'grant_type=client_credentials&scope=bot',
		});
		expect(response.status).toBe(200);
		expect(await response.json()).toEqual({grant_type: 'client_credentials', scope: 'bot'});
	});
});

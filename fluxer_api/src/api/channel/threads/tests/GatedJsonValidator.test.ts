// SPDX-License-Identifier: AGPL-3.0-or-later

import {GatedJsonValidator} from '@app/api/channel/threads/GatedJsonValidator';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';
import {z} from 'zod';

const Control = z.object({name: z.string().max(10)});
const Gated = Control.extend({default_auto_archive_duration: z.number().int()});

function touchesGate(body: unknown): boolean {
	return typeof body === 'object' && body !== null && 'default_auto_archive_duration' in body;
}

function createApp(active: boolean): Hono<HonoEnv> {
	const app = new Hono<HonoEnv>();
	app.onError(AppErrorHandler);
	app.post('/control', Validator('json', Control), (ctx) => ctx.json(ctx.req.valid('json')));
	app.post('/gated', GatedJsonValidator(Control, Gated, {touchesGate, active: () => active}), (ctx) =>
		ctx.json(ctx.req.valid('json')),
	);
	return app;
}

async function post(app: Hono<HonoEnv>, path: string, body: string): Promise<{status: number; text: string}> {
	const response = await app.request(path, {method: 'POST', headers: {'content-type': 'application/json'}, body});
	return {status: response.status, text: await response.text()};
}

describe('GatedJsonValidator', () => {
	const bodies = [
		'{"name":"general","default_auto_archive_duration":60}',
		'{"name":"general"}',
		'{"name":"far too long for the control schema","default_auto_archive_duration":60}',
		'{"name":',
		'{"default_auto_archive_duration":"soon"}',
	];

	it('behaves exactly like the control schema while inactive', async () => {
		const app = createApp(false);
		for (const body of bodies) {
			expect(await post(app, '/gated', body)).toEqual(await post(app, '/control', body));
		}
	});

	it('keeps gated keys when the body touches the gate and the caller is active', async () => {
		const app = createApp(true);
		expect(await post(app, '/gated', bodies[0]!)).toEqual({
			status: 200,
			text: '{"name":"general","default_auto_archive_duration":60}',
		});
		expect((await post(app, '/gated', bodies[4]!)).status).toBe(400);
		expect(await post(app, '/gated', bodies[1]!)).toEqual(await post(app, '/control', bodies[1]!));
	});
});

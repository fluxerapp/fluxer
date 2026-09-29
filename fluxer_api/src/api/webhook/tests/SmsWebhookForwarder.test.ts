// SPDX-License-Identifier: AGPL-3.0-or-later

import {PHONE_INBOUND_SUBJECT} from '@app/api/infrastructure/activity/Contract.generated';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {
	installSmsWebhookForwarder,
	SMS_INBOUND_TTL,
	SMS_WEBHOOK_MAX_BODY_BYTES,
} from '@app/api/webhook/SmsWebhookForwarder';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';

const SIGNATURE = `${'a'.repeat(27)}=`;
const SID = `SM${'0'.repeat(32)}`;

function rig() {
	const published: Array<{subject: string; options: Record<string, unknown>}> = [];
	const app = new Hono();
	installSmsWebhookForwarder(app as unknown as HonoApp, () => ({
		publish: async (subject: string, _data: unknown, options?: Record<string, unknown>) => {
			published.push({subject, options: options ?? {}});
			return {} as never;
		},
	}));
	const post = (body: string, signature = SIGNATURE) =>
		app.request('/webhooks/twilio/sms', {
			method: 'POST',
			headers: {'content-type': 'application/x-www-form-urlencoded', 'x-twilio-signature': signature},
			body,
		});
	return {published, post};
}

describe('sms webhook forwarder', () => {
	it('forwards a well-formed webhook with its MessageSid and a short ttl', async () => {
		const {published, post} = rig();
		const res = await post(`MessageSid=${SID}&From=%2B15555550123&Body=123456`);
		expect(res.status).toBe(200);
		expect(published).toEqual([
			{subject: PHONE_INBOUND_SUBJECT, options: expect.objectContaining({msgID: SID, ttl: SMS_INBOUND_TTL})},
		]);
	});

	it('refuses unsigned, malformed and oversized posts before touching the stream', async () => {
		const {published, post} = rig();
		expect((await post(`MessageSid=${SID}&Body=1`, '')).status).toBe(403);
		expect((await post('MessageSid=random&Body=1')).status).toBe(403);
		expect((await post(`MessageSid=${SID}&Body=${'x'.repeat(SMS_WEBHOOK_MAX_BODY_BYTES)}`)).status).toBe(413);
		expect(published).toEqual([]);
	});
});

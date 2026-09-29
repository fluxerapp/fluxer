// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	HEADER,
	type InboundSms,
	PHONE_INBOUND_SUBJECT,
	PHONE_RPC_VERSION,
} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import type {JetStreamClient} from '@nats-io/jetstream';

const PUBLISH_TIMEOUT_MS = 2000;
const EMPTY_TWIML = '<?xml version="1.0" encoding="UTF-8"?><Response/>';
export const SMS_WEBHOOK_MAX_BODY_BYTES = 8192;
export const SMS_INBOUND_TTL = String(24 * 60 * 60);
const TWILIO_SIGNATURE = /^[A-Za-z0-9+/]{27}=$/;
const MESSAGE_SID = /^(SM|MM)[0-9a-f]{32}$/;

export function installSmsWebhookForwarder(
	routes: HonoApp,
	jetStream: () => Pick<JetStreamClient, 'publish'> | null,
): void {
	routes.post('/webhooks/twilio/sms', RateLimitMiddleware(RateLimitConfigs.WEBHOOK_TWILIO_SMS), async (ctx) => {
		const declared = Number(ctx.req.header('content-length') ?? '0');
		if (declared > SMS_WEBHOOK_MAX_BODY_BYTES) return ctx.text('too large', 413);
		const rawBody = await ctx.req.text();
		if (Buffer.byteLength(rawBody) > SMS_WEBHOOK_MAX_BODY_BYTES) return ctx.text('too large', 413);
		const signature = ctx.req.header(HEADER.twilioSignature) ?? '';
		const msgID = new URLSearchParams(rawBody).get('MessageSid') ?? '';
		if (!TWILIO_SIGNATURE.test(signature) || !MESSAGE_SID.test(msgID)) return ctx.text('forbidden', 403);
		const js = jetStream();
		if (!js) {
			Logger.warn('Inbound SMS webhook could not be forwarded, JetStream is not connected');
			return ctx.text('unavailable', 500);
		}
		const message: InboundSms = {
			v: PHONE_RPC_VERSION,
			received_at_ms: Date.now(),
			signature,
			body: rawBody,
		};
		try {
			await js.publish(PHONE_INBOUND_SUBJECT, JSON.stringify(message), {
				msgID,
				ttl: SMS_INBOUND_TTL,
				timeout: PUBLISH_TIMEOUT_MS,
			});
		} catch (error) {
			Logger.warn({err: error}, 'Inbound SMS webhook publish failed');
			return ctx.text('unavailable', 500);
		}
		return ctx.text(EMPTY_TWIML, 200, {'Content-Type': 'text/xml'});
	});
}

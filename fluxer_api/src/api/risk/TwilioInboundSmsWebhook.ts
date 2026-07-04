// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import {PHONE_ADD_CLEARABLE_FLAGS} from '@fluxer/constants/src/UserConstants';
import {PHONE_E164_REGEX} from '@fluxer/schema/src/primitives/UserValidators';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {phonePrefixBanCache} from '../auth/PhonePrefixBanCache';
import {PhoneVerificationReuseStore} from '../auth/PhoneVerificationReuseStore';
import type {InboundSmsChallengeService} from '../auth/services/InboundSmsChallengeService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import {Logger} from '../Logger';
import type {HonoApp} from '../types/HonoEnv';
import type {IUserRepository} from '../user/IUserRepository';
import {mapUserToPrivateResponse} from '../user/UserMappers';
import {resolveRequestClientIp} from '../utils/IpUtils';

interface TwilioInboundSmsWebhookContext {
	authToken: string;
	publicWebhookUrl: string;
	inboundSmsChallengeService: InboundSmsChallengeService;
	userRepository: IUserRepository;
	gatewayService: IGatewayService;
	cacheService: ICacheService;
}

export function installTwilioInboundSmsWebhook(app: HonoApp, ctx: TwilioInboundSmsWebhookContext): void {
	const phoneReuseStore = new PhoneVerificationReuseStore(ctx.cacheService);
	app.post('/webhooks/twilio/sms', async (c) => {
		const rawBody = await c.req.text();
		const params = parseFormUrlEncoded(rawBody);
		const signature = c.req.header('x-twilio-signature') ?? '';
		if (!verifyTwilioSignature(ctx.authToken, ctx.publicWebhookUrl, params, signature)) {
			Logger.warn({ip: resolveRequestClientIp(c.req.raw) ?? '?'}, 'Twilio webhook signature failed; rejecting');
			return c.text('forbidden', 403);
		}
		const fromPhone = params.get('From') ?? '';
		const body = (params.get('Body') ?? '').trim();
		const messageSid = params.get('MessageSid') ?? '';
		if (!fromPhone || !body) {
			return c.text(twimlAck(), 200, twiml());
		}
		if (!PHONE_E164_REGEX.test(fromPhone) || phonePrefixBanCache.isBlocked(fromPhone)) {
			Logger.info({fromPhone, messageSid}, 'Twilio inbound SMS: sender phone blocked');
			return c.text(twimlAck(), 200, twiml());
		}
		try {
			const consumed = await ctx.inboundSmsChallengeService.consumeChallenge({code: body, fromPhone});
			if (!consumed) {
				Logger.info({fromPhone, messageSid}, 'Twilio inbound SMS: no matching challenge');
				return c.text(twimlAck(), 200, twiml());
			}
			const user = await ctx.userRepository.findUnique(consumed.userId);
			if (!user) {
				Logger.warn({userId: String(consumed.userId)}, 'Twilio inbound SMS: user vanished');
				return c.text(twimlAck(), 200, twiml());
			}
			if (!(await phoneReuseStore.claimVerificationSlot(fromPhone))) {
				Logger.info({fromPhone, userId: String(consumed.userId)}, 'Twilio inbound SMS: phone recently used');
				return c.text(twimlAck(), 200, twiml());
			}
			const newFlags = (user.suspiciousActivityFlags ?? 0) & ~PHONE_ADD_CLEARABLE_FLAGS;
			await ctx.userRepository.patchUpsert(
				user.id,
				{has_verified_phone: true, suspicious_activity_flags: newFlags},
				user.toRow(),
			);
			const updatedUser = await ctx.userRepository.findUnique(user.id);
			if (updatedUser) {
				await ctx.gatewayService.dispatchPresence({
					userId: user.id,
					event: 'USER_UPDATE',
					data: mapUserToPrivateResponse(updatedUser),
				});
			}
			Logger.info({userId: String(consumed.userId), fromPhone}, 'Twilio inbound SMS: challenge consumed');
			return c.text(twimlAck(), 200, twiml());
		} catch (err) {
			Logger.error({err: String(err)}, 'Twilio inbound SMS handler crashed');
			return c.text(twimlAck(), 200, twiml());
		}
	});
}

function verifyTwilioSignature(
	authToken: string,
	url: string,
	params: URLSearchParams,
	expectedSignature: string,
): boolean {
	if (!expectedSignature) return false;
	const sortedKeys = [...params.keys()].sort();
	let payload = url;
	for (const key of sortedKeys) {
		payload += key + (params.get(key) ?? '');
	}
	const computed = createHmac('sha1', authToken).update(payload).digest('base64');
	const a = Buffer.from(computed);
	const b = Buffer.from(expectedSignature);
	if (a.length !== b.length) return false;
	return timingSafeEqual(a, b);
}

function parseFormUrlEncoded(body: string): URLSearchParams {
	return new URLSearchParams(body);
}

function twiml(): {
	[key: string]: string;
} {
	return {'Content-Type': 'text/xml; charset=utf-8'};
}

function twimlAck(): string {
	return '<?xml version="1.0" encoding="UTF-8"?><Response/>';
}

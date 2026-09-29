// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {
	errorForPhoneReply,
	getPhoneVerificationClient,
	type PhoneRpcRequestInfo,
} from '@app/api/auth/PhoneVerificationClient';
import type {UserID} from '@app/api/BrandedTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {Challenge, PhoneChannel} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {accountStateDepsFromContext, applyPhoneVerified, outcomeOf} from '@app/api/user/services/AccountStateApplier';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {BotUserAuthEndpointAccessDeniedError} from '@fluxer/errors/src/domains/auth/BotUserAuthEndpointAccessDeniedError';
import {InvalidPhoneNumberError} from '@fluxer/errors/src/domains/auth/InvalidPhoneNumberError';
import {PhoneVerificationRequiredError} from '@fluxer/errors/src/domains/auth/PhoneVerificationRequiredError';
import {SmsVerificationUnavailableError} from '@fluxer/errors/src/domains/auth/SmsVerificationUnavailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {PHONE_E164_REGEX} from '@fluxer/schema/src/primitives/UserValidators';

type PhoneVerificationStartResult =
	| {channel: 'sms'}
	| {
			channel: 'inbound_challenge';
			challengeCode: string;
			ourNumber: string;
			expiresAt: Date;
	  };

interface IssuedChallenge {
	challengeCode: string;
	ourNumber: string;
	expiresAt: Date;
}

interface SendPhoneVerificationOptions {
	clientIp: string;
	channel?: PhoneChannel;
	hasCaptchaToken: boolean;
	verifyCaptcha: () => Promise<boolean>;
}

function assertPhoneFormat(phone: string): void {
	if (!PHONE_E164_REGEX.test(phone)) {
		throw new InvalidPhoneNumberError();
	}
}

async function loadRequestingUser(ctx: ApiContext, userId: UserID): Promise<User> {
	const user = await ctx.services.users.findUnique(userId);
	if (!user) throw new UnknownUserError();
	if (user.isBot) throw new BotUserAuthEndpointAccessDeniedError();
	return user;
}

function requestInfo(ctx: ApiContext, userId: UserID, phone: string): PhoneRpcRequestInfo {
	return {userId: userId.toString(), phone, requestId: ctx.request.requestId};
}

function toIssuedChallenge(challenge: Challenge): IssuedChallenge {
	return {
		challengeCode: challenge.challenge_code,
		ourNumber: challenge.our_number,
		expiresAt: new Date(challenge.expires_at_ms),
	};
}

export async function sendPhoneVerificationCode(
	ctx: ApiContext,
	phone: string,
	userId: UserID,
	options: SendPhoneVerificationOptions,
): Promise<PhoneVerificationStartResult> {
	assertPhoneFormat(phone);
	const user = await loadRequestingUser(ctx, userId);
	const start = (captchaPassed: boolean) =>
		getPhoneVerificationClient().start(
			{
				user_id: user.id.toString(),
				user_flags: user.flags.toString(),
				has_verified_phone: user.hasVerifiedPhone,
				phone,
				requested_channel: options.channel ?? null,
				client_ip: options.clientIp,
				captcha_passed: captchaPassed,
			},
			requestInfo(ctx, userId, phone),
		);
	const captchaPassed = options.hasCaptchaToken && (await options.verifyCaptcha());
	const reply = await start(captchaPassed);
	if (reply.result === 'error' && reply.code === 'captcha_required' && !captchaPassed) {
		await options.verifyCaptcha();
		Logger.warn({userId: userId.toString()}, 'Phone verification asked for a captcha without a captcha check');
	}
	switch (reply.result) {
		case 'sms_sent':
			return {channel: 'sms'};
		case 'inbound_challenge':
			return {channel: 'inbound_challenge', ...toIssuedChallenge(reply)};
		case 'error':
			throw errorForPhoneReply(reply);
	}
}

export async function startInboundPhoneChallenge(ctx: ApiContext, userId: UserID): Promise<IssuedChallenge> {
	const user = await loadRequestingUser(ctx, userId);
	const reply = await getPhoneVerificationClient().challenge(
		{user_id: user.id.toString()},
		requestInfo(ctx, userId, ''),
	);
	if (reply.result === 'error') throw errorForPhoneReply(reply);
	return toIssuedChallenge(reply);
}

export async function verifyPhoneCode(ctx: ApiContext, phone: string, code: string, userId: UserID): Promise<void> {
	assertPhoneFormat(phone);
	const user = await ctx.services.users.findUnique(userId);
	if (!user || (user.flags & UserFlags.DELETED) !== 0n) throw new PhoneVerificationRequiredError();
	if (user.isBot) throw new BotUserAuthEndpointAccessDeniedError();
	const reply = await getPhoneVerificationClient().check(
		{
			user_id: user.id.toString(),
			user_flags: user.flags.toString(),
			phone,
			code,
			client_ip: ctx.request.clientIp ?? '',
		},
		requestInfo(ctx, userId, phone),
	);
	if (reply.result === 'error') throw errorForPhoneReply(reply);
	const action = reply.action;
	if (action.type !== 'phone_verified' || action.user_id !== user.id.toString()) {
		Logger.error({actionType: action.type}, 'Phone verification returned an unexpected action');
		throw new SmsVerificationUnavailableError();
	}
	if (action.expires_at_ms <= Date.now()) {
		const expired = outcomeOf(action, 'expired');
		await emitActivity('action_outcome', action.key, expired, null, expired.action_id);
		throw new SmsVerificationUnavailableError();
	}
	const outcome = await applyPhoneVerified(accountStateDepsFromContext(ctx, getAdminRepository()), action);
	await emitActivity('action_outcome', action.key, outcome, null, outcome.action_id);
	if (outcome.status === 'ineligible') throw new PhoneVerificationRequiredError();
}

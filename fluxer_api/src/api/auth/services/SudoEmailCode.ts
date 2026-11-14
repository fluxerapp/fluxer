// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {
	assertEmailSendAllowed,
	budgetEnvFor,
	recordAccountCodeFailure,
	recordEmailSend,
} from '@app/api/auth/AccountBudgets';
import {generateCode, generateOpaqueToken, hashCode, normalizeEmailKey, sha256Hex} from '@app/api/auth/AccountCodes';
import {checkEmailCode, storeEmailCode} from '@app/api/auth/EmailCodeStore';
import {getCachedAccountIdentityMode} from '@app/api/instance/AccountIdentityModeCache';
import {getEmailDeliveryState} from '@app/api/instance/AccountsConfigCache';
import type {User} from '@app/api/models/User';
import {EmailCodeAvailability} from '@fluxer/constants/src/AccountConfigConstants';
import {resolveEmailCodeAvailability} from '@fluxer/constants/src/AccountTierRules';
import {InvalidSignInCodeError} from '@fluxer/errors/src/domains/auth/InvalidSignInCodeError';
import {SignInCodeExpiredError} from '@fluxer/errors/src/domains/auth/SignInCodeExpiredError';
import {UnknownSignInTicketError} from '@fluxer/errors/src/domains/auth/UnknownSignInTicketError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {ms, seconds} from 'itty-time';

const SUDO_CODE_TTL_SECONDS = seconds('10 minutes');
const CODE_SCOPE = 'sudo';

interface SudoCodeRecord {
	user_id: string;
	code_hash: string;
}

function recordKey(ticket: string): string {
	return `sudo:email_code:${sha256Hex(ticket)}`;
}

type EmailCodeUser = Pick<User, 'email' | 'emailVerified'>;

export function getEmailCodeAvailability(user: EmailCodeUser): EmailCodeAvailability {
	return resolveEmailCodeAvailability({
		identityMode: getCachedAccountIdentityMode(),
		deliveryState: getEmailDeliveryState(),
		hasEmail: user.email !== null,
		emailVerified: user.emailVerified,
	});
}

export function emailCodesCanBeSent(availability: EmailCodeAvailability): boolean {
	return availability === EmailCodeAvailability.AVAILABLE || availability === EmailCodeAvailability.NO_OWNED_EMAIL;
}

export function hasOwnedEmail(user: EmailCodeUser): boolean {
	return getEmailCodeAvailability(user) === EmailCodeAvailability.AVAILABLE;
}

export async function sendSudoEmailCode(
	ctx: ApiContext,
	params: {user: User; request: Request},
): Promise<{ticket: string; code_expires_at: string; resend_available_at: string}> {
	const {user, request} = params;
	const email = user.email!;
	const env = budgetEnvFor(ctx.services, request);
	const clientIp = requireClientIp(request, {
		trustClientIpHeader: ctx.services.config.proxy.trust_client_ip_header,
		clientIpHeaderName: ctx.services.config.proxy.client_ip_header,
	});
	const emailKey = normalizeEmailKey(email);
	await assertEmailSendAllowed(env, {emailKey, requester: 'known', clientIp, userId: user.id.toString()});
	const ticket = generateOpaqueToken();
	const code = generateCode();
	const now = Date.now();
	const expiresAt = new Date(now + SUDO_CODE_TTL_SECONDS * 1000);
	await storeEmailCode<SudoCodeRecord>(ctx, {
		key: recordKey(ticket),
		record: {user_id: user.id.toString(), code_hash: hashCode(code, CODE_SCOPE)},
		ttlSeconds: SUDO_CODE_TTL_SECONDS,
	});
	await recordEmailSend(env, {emailKey, requester: 'known'});
	await ctx.services.email.send('reauth_code', email, {code, expiresAt}, user.locale);
	return {
		ticket,
		code_expires_at: expiresAt.toISOString(),
		resend_available_at: new Date(now + ms('30 seconds')).toISOString(),
	};
}

export async function verifySudoEmailCode(
	ctx: ApiContext,
	params: {user: User; ticket: string | undefined; code: string | undefined; request: Request},
): Promise<void> {
	const {user, ticket, code} = params;
	if (!ticket) throw new UnknownSignInTicketError();
	const check = await checkEmailCode<SudoCodeRecord>(ctx, {
		key: recordKey(ticket),
		code,
		scope: CODE_SCOPE,
		ttlSeconds: SUDO_CODE_TTL_SECONDS,
		accepts: (record) => record.user_id === user.id.toString(),
	});
	if (check.kind === 'missing') throw new UnknownSignInTicketError();
	if (check.kind === 'exhausted') throw new SignInCodeExpiredError();
	if (check.kind === 'wrong') {
		await recordAccountCodeFailure(budgetEnvFor(ctx.services, params.request), user.id.toString());
		throw new InvalidSignInCodeError();
	}
}

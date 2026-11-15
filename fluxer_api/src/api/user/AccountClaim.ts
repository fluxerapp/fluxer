// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {
	assertEmailSendAllowed,
	budgetEnvFor,
	chargeAddressLookup,
	recordAddressCodeFailure,
	recordEmailSend,
	requesterNetworkKey,
} from '@app/api/auth/AccountBudgets';
import {generateCode, hashCode, normalizeEmailKey} from '@app/api/auth/AccountCodes';
import {emitCodeSent} from '@app/api/auth/AuthIdentify';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import {assertInstanceAcceptsEmailDomain, assertRegistrationEmailDeliverable} from '@app/api/auth/AuthRegistration';
import {checkEmailCode, storeEmailCode} from '@app/api/auth/EmailCodeStore';
import {confirmEmailOwnership} from '@app/api/auth/EmailOwnership';
import {emailCodesCanBeSent, getEmailCodeAvailability} from '@app/api/auth/services/SudoEmailCode';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {getCachedAccountIdentityMode} from '@app/api/instance/AccountIdentityModeCache';
import {getEffectiveVerificationBasis, getEmailDeliveryState} from '@app/api/instance/AccountsConfigCache';
import type {User} from '@app/api/models/User';
import {getAccountTier, getLocalCredential} from '@app/api/user/AccountTier';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {hashPassword} from '@app/api/utils/PasswordUtils';
import {
	ClaimAccountNextSteps,
	type EmailDeliveryState,
	EmailDeliveryStates,
	VerificationBases,
} from '@fluxer/constants/src/AccountConfigConstants';
import {type AccountIdentityMode, AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AccountTiers, LocalCredentialKinds} from '@fluxer/constants/src/AccountTierConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccountAlreadyClaimedError} from '@fluxer/errors/src/domains/auth/AccountAlreadyClaimedError';
import {EmailCodesUnavailableError} from '@fluxer/errors/src/domains/auth/EmailCodesUnavailableError';
import {InvalidSignInCodeError} from '@fluxer/errors/src/domains/auth/InvalidSignInCodeError';
import {
	PasswordRequiredToClaimError,
	type PasswordRequiredToClaimReason,
	PasswordRequiredToClaimReasons,
} from '@fluxer/errors/src/domains/auth/PasswordRequiredToClaimError';
import {SignInCodeExpiredError} from '@fluxer/errors/src/domains/auth/SignInCodeExpiredError';
import {UseClaimRouteError} from '@fluxer/errors/src/domains/auth/UseClaimRouteError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ClaimAccountResponse, EmailVerificationCodeResponse} from '@fluxer/schema/src/domains/user/AccountSchemas';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {seconds} from 'itty-time';

const CODE_TTL_SECONDS = seconds('10 minutes');
const RESEND_COOLDOWN_MS = 30_000;
const CODE_SCOPE = 'email_verification';

interface VerificationCodeRecord {
	email: string;
	code_hash: string;
	network: string;
}

function verificationKey(user: User): string {
	return `user:email_verification:${user.id}`;
}

async function dispatchUserUpdate(ctx: ApiContext, user: User): Promise<void> {
	await ctx.services.gateway.dispatchPresence({
		userId: user.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(user),
	});
}

async function sendCode(
	ctx: ApiContext,
	params: {user: User; email: string; request: Request; clientIp: string},
): Promise<EmailVerificationCodeResponse> {
	const {user, email, request} = params;
	const network = requesterNetworkKey(params.clientIp);
	const env = budgetEnvFor(ctx.services, request);
	const emailKey = normalizeEmailKey(email);
	const now = Date.now();
	const expiresAt = new Date(now + CODE_TTL_SECONDS * 1000);
	const code = generateCode();
	await storeEmailCode<VerificationCodeRecord>(ctx, {
		key: verificationKey(user),
		record: {email: email.toLowerCase(), code_hash: hashCode(code, CODE_SCOPE), network},
		ttlSeconds: CODE_TTL_SECONDS,
	});
	await ctx.services.email.send('email_verification_code', email, {code, expiresAt}, user.locale);
	await recordEmailSend(env, {emailKey, requester: 'unknown'});
	return codeTimes(now);
}

function codeTimes(now: number): EmailVerificationCodeResponse {
	return {
		code_expires_at: new Date(now + CODE_TTL_SECONDS * 1000).toISOString(),
		resend_available_at: new Date(now + RESEND_COOLDOWN_MS).toISOString(),
	};
}

async function assertCodeSendAllowed(
	ctx: ApiContext,
	params: {email: string; request: Request; clientIp: string},
): Promise<void> {
	await assertEmailSendAllowed(budgetEnvFor(ctx.services, params.request), {
		emailKey: normalizeEmailKey(params.email),
		requester: 'unknown',
		clientIp: params.clientIp,
	});
}

function claimCredentialReason(
	identityMode: AccountIdentityMode,
	deliveryState: EmailDeliveryState,
): PasswordRequiredToClaimReason | null {
	if (identityMode === AccountIdentityModes.USERNAME) return PasswordRequiredToClaimReasons.USERNAME_IDENTITY;
	if (deliveryState !== EmailDeliveryStates.ON) return PasswordRequiredToClaimReasons.EMAIL_DELIVERY_OFF;
	return getEffectiveVerificationBasis() === VerificationBases.LOCAL_CREDENTIAL
		? PasswordRequiredToClaimReasons.LOCAL_CREDENTIAL_BASIS
		: null;
}

export async function claimAccount(
	ctx: ApiContext,
	params: {user: User; email: string | undefined; password: string | undefined; request: Request; clientIp: string},
): Promise<ClaimAccountResponse> {
	const {user, password, request, clientIp} = params;
	if (getAccountTier(user) !== AccountTiers.UNCLAIMED) throw new AccountAlreadyClaimedError();
	const identityMode = getCachedAccountIdentityMode();
	const deliveryState = getEmailDeliveryState();
	if (!password && getLocalCredential(user) === LocalCredentialKinds.NONE) {
		const reason = claimCredentialReason(identityMode, deliveryState);
		if (reason !== null) throw new PasswordRequiredToClaimError(reason);
	}
	if (password && (await AuthPassword.isPasswordPwned(ctx, password))) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const passwordPatch = password
		? {password_hash: await hashPassword(password), password_last_changed_at: new Date()}
		: {};
	if (identityMode === AccountIdentityModes.USERNAME) {
		return await completeClaim(ctx, {user, patch: passwordPatch, withPassword: password !== undefined});
	}
	const email = params.email;
	if (!email) throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_IS_REQUIRED);
	await assertInstanceAcceptsEmailDomain(email, 'email');
	await assertRegistrationEmailDeliverable(email, ctx.services.emailDnsValidation);
	await assertAddressFree(ctx, {user, email, clientIp, request});
	if (deliveryState !== EmailDeliveryStates.ON) {
		return await completeClaim(ctx, {
			user,
			patch: {email, email_verified: true, email_bounced: false, ...passwordPatch},
			withPassword: password !== undefined,
		});
	}
	await assertCodeSendAllowed(ctx, {email, request, clientIp});
	const updated = await ctx.services.users.patchUpsert(
		user.id,
		{email, email_verified: false, email_bounced: false, ...passwordPatch},
		user.toRow(),
	);
	const delivery = await sendCode(ctx, {user: updated, email, request, clientIp});
	await dispatchUserUpdate(ctx, updated);
	emitClaimed(updated, password !== undefined);
	emitCodeSent(updated.id.toString(), 'claim', email);
	return {next_step: ClaimAccountNextSteps.CONFIRM_EMAIL, user: mapUserToPrivateResponse(updated), ...delivery};
}

async function assertAddressFree(
	ctx: ApiContext,
	params: {user: User; email: string; clientIp: string; request: Request},
): Promise<void> {
	const owner = await ctx.services.users.findByEmail(params.email);
	if (owner === null || owner.id === params.user.id) return;
	await chargeAddressLookup(budgetEnvFor(ctx.services, params.request), {
		clientIp: params.clientIp,
		emailKey: normalizeEmailKey(params.email),
		requester: 'unknown',
	});
	throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
}

async function completeClaim(
	ctx: ApiContext,
	params: {user: User; patch: Partial<UserRow>; withPassword: boolean},
): Promise<ClaimAccountResponse> {
	const updated = await ctx.services.users.patchUpsert(params.user.id, params.patch, params.user.toRow());
	await dispatchUserUpdate(ctx, updated);
	emitClaimed(updated, params.withPassword);
	return {next_step: ClaimAccountNextSteps.NONE, user: mapUserToPrivateResponse(updated)};
}

function emitClaimed(user: User, withPassword: boolean): void {
	void emitActivity('account_claimed', user.id.toString(), {user_id: user.id.toString(), with_password: withPassword});
}

export async function sendEmailVerificationCode(
	ctx: ApiContext,
	params: {user: User; request: Request; clientIp: string},
): Promise<EmailVerificationCodeResponse> {
	const {user} = params;
	const tier = getAccountTier(user);
	if (tier === AccountTiers.UNCLAIMED || user.email === null) throw new UseClaimRouteError();
	if (!emailCodesCanBeSent(getEmailCodeAvailability(user))) throw new EmailCodesUnavailableError();
	if (tier === AccountTiers.VERIFIED) {
		throw user.emailVerified ? new AccountAlreadyClaimedError() : new EmailCodesUnavailableError();
	}
	await assertCodeSendAllowed(ctx, {email: user.email, request: params.request, clientIp: params.clientIp});
	const result = await sendCode(ctx, {
		user,
		email: user.email,
		request: params.request,
		clientIp: params.clientIp,
	});
	emitCodeSent(user.id.toString(), 'verify', user.email);
	return result;
}

export async function confirmEmailVerification(
	ctx: ApiContext,
	params: {user: User; code: string; request: Request},
): Promise<UserPrivateResponse> {
	const {user, code, request} = params;
	const email = user.email?.toLowerCase() ?? null;
	const check = await checkEmailCode<VerificationCodeRecord>(ctx, {
		key: verificationKey(user),
		code,
		scope: CODE_SCOPE,
		ttlSeconds: CODE_TTL_SECONDS,
		accepts: (record) => email !== null && record.email === email,
	});
	if (check.kind === 'wrong') {
		await recordAddressCodeFailure(budgetEnvFor(ctx.services, request), {
			emailKey: normalizeEmailKey(email!),
			network: check.record.network,
		});
		throw new InvalidSignInCodeError();
	}
	if (check.kind !== 'ok') throw new SignInCodeExpiredError();
	return mapUserToPrivateResponse(await confirmEmailOwnership(ctx, user));
}

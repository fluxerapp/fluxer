// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {
	addressLookupCaptchaSteps,
	assertEmailSendAllowed,
	budgetEnvFor,
	chargeAddressLookup,
	codeVerifyCaptchaSteps,
	emailSendCaptchaSteps,
	enforceLookupHardLimits,
	type RequesterClass,
	recordAccountCodeFailure,
	recordAddressCodeFailure,
	recordEmailSend,
	requesterNetworkKey,
} from '@app/api/auth/AccountBudgets';
import {
	deviceProofMatches,
	generateCode,
	generateOpaqueToken,
	hashCode,
	issueDeviceProof,
	normalizeEmailKey,
	sha256Hex,
} from '@app/api/auth/AccountCodes';
import {createMfaTicketResponse, emitLogin, type LoginMfaResult} from '@app/api/auth/AuthLogin';
import {
	type AccountCreationResult,
	assertAccountCreationPastSetup,
	assertRegistrationConsent,
	assertRegistrationEmailDeliverable,
	assertRequestedProfileAllowed,
	createAccount,
	enforceRegistrationRateLimits,
	type RegistrationDependencies,
	resolveInstantAccountAvailabilityFor,
	resolveRegistrationAccess,
} from '@app/api/auth/AuthRegistration';
import * as AuthSession from '@app/api/auth/AuthSession';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {checkEmailCode, storeEmailCode} from '@app/api/auth/EmailCodeStore';
import {confirmEmailOwnership} from '@app/api/auth/EmailOwnership';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {createUserID} from '@app/api/BrandedTypes';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {EmailCodePurpose} from '@app/api/infrastructure/activity/Contract.generated';
import {isBlockedEmailDomain} from '@app/api/infrastructure/activity/SharedLists';
import {
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository, getOAuth2TokenRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {TierContext} from '@app/api/user/AccountTier';
import {BELOW_MINIMUM_AGE_TRAIT} from '@app/api/user/UserTraits';
import {extractEmailDomain} from '@app/api/utils/EmailDomainUtils';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {AccountBelowMinimumAgeError} from '@fluxer/errors/src/domains/auth/AccountBelowMinimumAgeError';
import {EmailCodeResendCooldownError} from '@fluxer/errors/src/domains/auth/EmailCodeResendCooldownError';
import {EmailCodesUnavailableError} from '@fluxer/errors/src/domains/auth/EmailCodesUnavailableError';
import {InvalidSignInCodeError} from '@fluxer/errors/src/domains/auth/InvalidSignInCodeError';
import {PasswordSignInRequiredError} from '@fluxer/errors/src/domains/auth/PasswordSignInRequiredError';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {RegistrationRejectedError} from '@fluxer/errors/src/domains/auth/RegistrationRejectedError';
import {SignInCodeExpiredError} from '@fluxer/errors/src/domains/auth/SignInCodeExpiredError';
import {UnknownSignInTicketError} from '@fluxer/errors/src/domains/auth/UnknownSignInTicketError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {parseAcceptLanguage} from '@fluxer/i18n/src/locale/resolution/AcceptLanguageNegotiation';
import {getSubnet} from '@fluxer/ip_utils/src/IpAddress';
import {isCaptchaActive} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import type {IdentifyResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {formatGeoipLocation} from '@pkgs/geoip/src/GeoipLookup';
import {seconds} from 'itty-time';

const TICKET_TTL_SECONDS = seconds('15 minutes');
const CODE_TTL_SECONDS = seconds('10 minutes');
const RESEND_COOLDOWN_MS = 30_000;
const IDENTIFY_FLOOR_MS = 300;
const CODE_SCOPE = 'sign_in';
const UNKNOWN_PASSKEY_OWNER = createUserID(0n);

export type CaptchaGate = (escalationSteps: number) => Promise<void>;

interface IdentifyTicket {
	identifier: string;
	user_id: string | null;
	known_requester: boolean;
	created_at: number;
}

interface EmailCodeRecord {
	code_hash: string;
	purpose: 'sign_in' | 'sign_up';
	sent_at: number;
	network: string;
}

function ticketKey(ticket: string): string {
	return `auth:identify:${sha256Hex(ticket)}`;
}

function codeKey(ticket: string): string {
	return `auth:email_code:${sha256Hex(ticket)}`;
}

async function waitForFloor(startedAt: number): Promise<void> {
	const remaining = IDENTIFY_FLOOR_MS - (Date.now() - startedAt);
	if (remaining > 0) await new Promise((resolve) => setTimeout(resolve, remaining));
}

async function emailCodesAvailable(): Promise<boolean> {
	return (await getInstanceConfigRepository().getSignInFeatures()).emailCodes;
}

async function assertIdentifierFirst(): Promise<void> {
	if (!(await getInstanceConfigRepository().getSignInFeatures()).identifierFirst) {
		throw new FeatureTemporarilyDisabledError();
	}
}

async function isKnownRequester(
	ctx: ApiContext,
	owner: User | null,
	params: {clientIp: string; deviceProof: string | undefined},
): Promise<boolean> {
	if (!owner) return false;
	if (deviceProofMatches(params.deviceProof, owner.id)) return true;
	return await ctx.services.users.checkIpAuthorized(owner.id, params.clientIp);
}

export async function identify(
	ctx: ApiContext,
	params: {
		identifier: string;
		deviceProof: string | undefined;
		registrationUrlCode: string | null | undefined;
		request: Request;
		clientIp: string;
		captcha: CaptchaGate;
	},
): Promise<IdentifyResponse> {
	const startedAt = Date.now();
	await assertIdentifierFirst();
	const identifier = params.identifier.trim().toLowerCase();
	const owner = await ctx.services.users.findByEmail(identifier);
	const ownerUsable = owner !== null && !owner.isBot;
	await ctx.services.users.listWebAuthnCredentials(ownerUsable ? owner.id : UNKNOWN_PASSKEY_OWNER);
	isBlockedEmailDomain(extractEmailDomain(identifier));
	const knownRequester = await isKnownRequester(ctx, ownerUsable ? owner : null, params);
	const requester: RequesterClass = knownRequester ? 'known' : 'unknown';
	const env = budgetEnvFor(ctx.services, params.request);
	if (!isCaptchaActive(await getInstanceConfigRepository().getCaptchaConfig())) {
		await enforceLookupHardLimits(env, params.clientIp);
	}
	const lookup = {clientIp: params.clientIp, emailKey: normalizeEmailKey(identifier), requester};
	const steps = await addressLookupCaptchaSteps(env, lookup);
	if (steps !== null) await params.captcha(steps);
	await chargeAddressLookup(env, lookup);
	const ticket = generateOpaqueToken();
	await ctx.services.cache.set<IdentifyTicket>(
		ticketKey(ticket),
		{
			identifier,
			user_id: ownerUsable ? owner.id.toString() : null,
			known_requester: knownRequester,
			created_at: startedAt,
		},
		TICKET_TTL_SECONDS,
	);
	const registration = ownerUsable ? null : await describeRegistration(ctx, params.registrationUrlCode);
	void emitActivity('identifier_lookup', identifier, {
		result: ownerUsable ? 'existing' : 'new',
		ip: params.clientIp,
		subnet: getSubnet(params.clientIp),
	});
	await waitForFloor(startedAt);
	let signInMethod: IdentifyResponse['sign_in_method'] = null;
	if (ownerUsable) {
		signInMethod = owner.passwordHash !== null ? 'password' : 'code';
	}
	return {
		ticket,
		account: ownerUsable ? 'existing' : 'new',
		sign_in_method: signInMethod,
		registration,
	};
}

async function describeRegistration(
	ctx: ApiContext,
	registrationUrlCode: string | null | undefined,
): Promise<IdentifyResponse['registration']> {
	const repository = getInstanceConfigRepository();
	const [registration, appPublic, codes, instantAccountAvailability] = await Promise.all([
		repository.getRegistrationConfig(),
		repository.getAppPublicConfig(),
		emailCodesAvailable(),
		resolveInstantAccountAvailabilityFor(repository, registrationUrlCode),
	]);
	return {
		mode: registration.mode,
		consent_required:
			!ctx.services.config.instance.selfHosted ||
			appPublic.legal.terms_url !== null ||
			appPublic.legal.privacy_url !== null,
		requires_password: !codes,
		instant_account_availability: instantAccountAvailability,
	};
}

async function readTicket(ctx: ApiContext, ticket: string): Promise<IdentifyTicket> {
	const record = await ctx.services.cache.get<IdentifyTicket>(ticketKey(ticket));
	if (!record) throw new UnknownSignInTicketError();
	return record;
}

function describeRequester(request: Request, geoipLabel: string | null, locale: string | null) {
	const info = resolveSessionClientInfo({
		userAgent: request.headers.get('user-agent'),
		reportedOs: null,
		productName: getInstanceProductName(),
	});
	return {
		location: geoipLabel ?? getContentMessage('auth.unknown_location', locale),
		device: [info.browser ?? info.platform, info.os].filter(Boolean).join(', ') || '-',
	};
}

export async function sendEmailCode(
	ctx: ApiContext,
	params: {
		ticket: string;
		registrationUrlCode: string | null | undefined;
		request: Request;
		clientIp: string;
		captcha: CaptchaGate;
	},
): Promise<{purpose: 'sign_in' | 'sign_up'; code_expires_at: string; resend_available_at: string}> {
	const record = await readTicket(ctx, params.ticket);
	if (!(await emailCodesAvailable())) throw new EmailCodesUnavailableError();
	const purpose = record.user_id === null ? 'sign_up' : 'sign_in';
	await assertCodeSignInAllowed(ctx, record);
	const env = budgetEnvFor(ctx.services, params.request);
	const emailKey = normalizeEmailKey(record.identifier);
	const requester: RequesterClass = record.known_requester ? 'known' : 'unknown';
	const existing = await ctx.services.cache.get<EmailCodeRecord>(codeKey(params.ticket));
	const now = Date.now();
	if (existing && now - existing.sent_at < RESEND_COOLDOWN_MS) {
		throw new EmailCodeResendCooldownError(Math.ceil((RESEND_COOLDOWN_MS - (now - existing.sent_at)) / 1000));
	}
	const steps = await emailSendCaptchaSteps(env, {emailKey, requester, clientIp: params.clientIp});
	if (purpose === 'sign_up') {
		await assertAccountCreationPastSetup(ctx, getInstanceConfigRepository(), params.clientIp);
		await params.captcha(steps ?? 0);
		await resolveRegistrationAccess(getInstanceConfigRepository(), params.registrationUrlCode, record.identifier);
		await assertRegistrationEmailDeliverable(record.identifier, ctx.services.emailDnsValidation);
		if (!existing) {
			await enforceRegistrationRateLimits(ctx, {
				enforceRateLimits: !ctx.services.config.dev.relaxRegistrationRateLimits,
				clientIp: params.clientIp,
				emailKey: record.identifier,
			});
		}
	} else if (steps !== null) {
		await params.captcha(steps);
	}
	await assertEmailSendAllowed(env, {
		emailKey,
		requester,
		clientIp: params.clientIp,
		userId: record.user_id ?? undefined,
	});
	const expiresAt = new Date(now + CODE_TTL_SECONDS * 1000);
	const owner = record.user_id ? await ctx.services.users.findUnique(createUserID(BigInt(record.user_id))) : null;
	if (!owner?.emailBounced) {
		const code = generateCode();
		await storeEmailCode<EmailCodeRecord>(ctx, {
			key: codeKey(params.ticket),
			record: {
				code_hash: hashCode(code, CODE_SCOPE),
				purpose,
				sent_at: now,
				network: requesterNetworkKey(params.clientIp),
			},
			ttlSeconds: CODE_TTL_SECONDS,
		});
		const locale = owner?.locale ?? parseAcceptLanguage(params.request.headers.get('accept-language'));
		const geoip = await lookupGeoip(params.clientIp);
		const requesterInfo = describeRequester(params.request, formatGeoipLocation(geoip, locale), locale);
		if (purpose === 'sign_in') {
			await ctx.services.email.send(
				'sign_in_code',
				record.identifier,
				{code, expiresAt, requester: requesterInfo},
				locale,
			);
		} else {
			await ctx.services.email.send(
				'sign_up_code',
				record.identifier,
				{code, expiresAt, requester: requesterInfo},
				locale,
			);
		}
		await recordEmailSend(env, {emailKey, requester});
		emitCodeSent(owner?.id.toString() ?? null, purpose, record.identifier);
	}
	return {
		purpose,
		code_expires_at: expiresAt.toISOString(),
		resend_available_at: new Date(now + RESEND_COOLDOWN_MS).toISOString(),
	};
}

async function assertCodeSignInAllowed(ctx: ApiContext, ticket: IdentifyTicket): Promise<void> {
	if (ticket.user_id === null) return;
	const owner = await ctx.services.users.findUnique(createUserID(BigInt(ticket.user_id)));
	if (owner?.passwordHash != null) throw new PasswordSignInRequiredError();
}

export function emitCodeSent(userId: string | null, purpose: EmailCodePurpose, email: string): void {
	void emitActivity('email_code_sent', userId ?? email, {
		user_id: userId,
		purpose,
		email_domain: extractEmailDomain(email) ?? '',
	});
}

async function recordWrongCode(
	ctx: ApiContext,
	params: {ticket: IdentifyTicket; record: EmailCodeRecord; request: Request},
): Promise<void> {
	const env = budgetEnvFor(ctx.services, params.request);
	if (params.ticket.known_requester && params.ticket.user_id) {
		await recordAccountCodeFailure(env, params.ticket.user_id);
		return;
	}
	await recordAddressCodeFailure(env, {
		emailKey: normalizeEmailKey(params.ticket.identifier),
		network: params.record.network,
	});
}

async function consumeCode(
	ctx: ApiContext,
	params: {ticketId: string; ticket: IdentifyTicket; code: string; request: Request},
) {
	const check = await checkEmailCode<EmailCodeRecord>(ctx, {
		key: codeKey(params.ticketId),
		code: params.code,
		scope: CODE_SCOPE,
		ttlSeconds: CODE_TTL_SECONDS,
	});
	if (check.kind === 'wrong') {
		await recordWrongCode(ctx, {ticket: params.ticket, record: check.record, request: params.request});
		throw new InvalidSignInCodeError();
	}
	if (check.kind !== 'ok') throw new SignInCodeExpiredError();
	await ctx.services.cache.delete(ticketKey(params.ticketId));
	return check.record;
}

export type EmailCodeVerifyResult =
	| {kind: 'sign_in'; user: User; token: string; deviceProof: string}
	| {kind: 'mfa'; mfa: LoginMfaResult}
	| {kind: 'sign_up'; result: AccountCreationResult};

export async function verifyEmailCode(
	ctx: ApiContext,
	deps: RegistrationDependencies,
	params: {
		ticket: string;
		code: string;
		consent: boolean | undefined;
		registrationUrlCode: string | null | undefined;
		inviteCode: string | null | undefined;
		username?: string;
		globalName?: string;
		request: Request;
		clientIp: string;
		requestCache: RequestCache;
		tierContext: TierContext;
		captcha: CaptchaGate;
	},
): Promise<EmailCodeVerifyResult> {
	const ticket = await readTicket(ctx, params.ticket);
	const steps = await codeVerifyCaptchaSteps(budgetEnvFor(ctx.services, params.request), {
		emailKey: normalizeEmailKey(ticket.identifier),
		requester: ticket.known_requester ? 'known' : 'unknown',
	});
	if (steps !== null) await params.captcha(steps);
	await assertCodeSignInAllowed(ctx, ticket);
	if (ticket.user_id === null) {
		await assertAccountCreationPastSetup(ctx, deps.instanceConfigRepository, params.clientIp);
		assertRegistrationConsent(
			ctx.services.config,
			await deps.instanceConfigRepository.getAppPublicConfig(),
			params.consent,
		);
		await assertRequestedProfileAllowed(ctx, deps, {username: params.username, globalName: params.globalName});
	}
	const record = await consumeCode(ctx, {
		ticketId: params.ticket,
		ticket,
		code: params.code,
		request: params.request,
	});
	if (record.purpose === 'sign_up') {
		const result = await createAccount(ctx, deps, {
			input: {
				email: ticket.identifier,
				emailProven: true,
				username: params.username,
				globalName: params.globalName,
				consent: params.consent,
				inviteCode: params.inviteCode,
				registrationUrlCode: params.registrationUrlCode,
			},
			request: params.request,
			clientIp: params.clientIp,
			requestCache: params.requestCache,
			source: 'app',
			tierContext: params.tierContext,
		});
		return {kind: 'sign_up', result};
	}
	return await completeCodeSignIn(ctx, {ticket, request: params.request, clientIp: params.clientIp});
}

async function takeOverUnconfirmedAccount(ctx: ApiContext, user: User): Promise<User> {
	const {users} = ctx.services;
	const updated = await confirmEmailOwnership(ctx, user);
	await users.deleteAllAuthorizedIps(user.id);
	const oauth2Tokens = getOAuth2TokenRepository();
	await oauth2Tokens.deleteAllAccessTokensForUser(user.id);
	await oauth2Tokens.deleteAllRefreshTokensForUser(user.id);
	await revokeAllAuthSessions(ctx.services, user.id);
	return updated;
}

async function completeCodeSignIn(
	ctx: ApiContext,
	params: {ticket: IdentifyTicket; request: Request; clientIp: string},
): Promise<EmailCodeVerifyResult> {
	const {users} = ctx.services;
	const owner = params.ticket.user_id ? await users.findUnique(createUserID(BigInt(params.ticket.user_id))) : null;
	const currentOwner = await users.findByEmail(params.ticket.identifier);
	if (!owner || !currentOwner || currentOwner.id !== owner.id) throw new UnknownSignInTicketError();
	AuthUtility.assertNonBotUser(ctx, owner);
	if (owner.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT)) throw new RegistrationPendingApprovalError();
	if (owner.traits.has(REGISTRATION_REJECTED_TRAIT)) throw new RegistrationRejectedError();
	if (owner.traits.has(BELOW_MINIMUM_AGE_TRAIT)) throw new AccountBelowMinimumAgeError();
	const allowed = await AuthUtility.assertSignInAllowed(ctx, owner);
	const hasMfa =
		allowed.authenticatorTypes.has(UserAuthenticatorTypes.TOTP) ||
		allowed.authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN);
	if (hasMfa) {
		AuthUtility.assertMfaTicketAllowed(ctx, allowed);
		const mfa = await createMfaTicketResponse(ctx, allowed, await resolveWebAuthnSecondFactor(ctx, allowed), {
			authorizeNetwork: true,
		});
		return {kind: 'mfa', mfa};
	}
	const user = allowed.emailVerified ? allowed : await takeOverUnconfirmedAccount(ctx, allowed);
	await users.createAuthorizedIp(user.id, params.clientIp);
	const [token] = await AuthSession.createAuthSession(ctx, {
		user,
		origin: AuthSession.resolveSessionOrigin(ctx, params.request),
	});
	emitLogin(user, true, {method: 'email_code'});
	return {kind: 'sign_in', user, token, deviceProof: issueDeviceProof(user.id)};
}

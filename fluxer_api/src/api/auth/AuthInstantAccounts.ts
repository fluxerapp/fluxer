// SPDX-License-Identifier: AGPL-3.0-or-later

import {timingSafeEqual} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import {budgetEnvFor, instantAccountCaptchaSteps} from '@app/api/auth/AccountBudgets';
import {generateOpaqueToken, sha256Hex} from '@app/api/auth/AccountCodes';
import {
	type AccountCreationResult,
	assertAccountCreationPastSetup,
	assertInstantAccountAvailable,
	assertRegistrationConsent,
	createAccount,
	type RegistrationDependencies,
	resolveInstantAccountAvailabilityFor,
} from '@app/api/auth/AuthRegistration';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {TierContext} from '@app/api/user/AccountTier';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {InvalidAccountHandoffNonceError} from '@fluxer/errors/src/domains/auth/InvalidAccountHandoffNonceError';
import {RegistrationClosedError} from '@fluxer/errors/src/domains/auth/RegistrationClosedError';
import {UnknownAccountHandoffError} from '@fluxer/errors/src/domains/auth/UnknownAccountHandoffError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';
import {seconds} from 'itty-time';

const HANDOFF_TTL_SECONDS = seconds('15 minutes');

interface AccountHandoffRecord {
	global_name: string;
	nonce_hash: string;
	client_ip: string;
	locale: string | null;
	registration_url_code?: string | null;
	created_at: number;
}

function isHandoffRecord(value: unknown): value is AccountHandoffRecord {
	return (
		isJsonRecord(value) &&
		typeof value.global_name === 'string' &&
		typeof value.nonce_hash === 'string' &&
		typeof value.client_ip === 'string' &&
		(value.locale === null || typeof value.locale === 'string') &&
		(value.registration_url_code == null || typeof value.registration_url_code === 'string') &&
		typeof value.created_at === 'number'
	);
}

function assertDisplayNameAllowed(globalName: string): void {
	if (profileSubstringBlocklistCache.containsBannedSubstring('global_name', globalName)) {
		throw new ContentBlockedError();
	}
}

function handoffKey(code: string): string {
	return `auth:account_handoff:${sha256Hex(code)}`;
}

function isRetryableBeforeCreation(error: unknown): boolean {
	if (error instanceof InputValidationError || error instanceof ContentBlockedError) return true;
	return error instanceof FluxerError && error.status === 429;
}

async function restoreHandoff(ctx: ApiContext, handoffCode: string, record: AccountHandoffRecord): Promise<void> {
	const remainingSeconds = HANDOFF_TTL_SECONDS - Math.ceil((Date.now() - record.created_at) / 1000);
	if (remainingSeconds <= 0) return;
	await ctx.services.kv.setex(handoffKey(handoffCode), remainingSeconds, JSON.stringify(record));
}

export async function instantAccountChallengeSteps(
	ctx: ApiContext,
	request: Request,
	clientIp: string,
	registrationUrlCode: string | null = null,
) {
	const repository = getInstanceConfigRepository();
	assertInstantAccountAvailable(await resolveInstantAccountAvailabilityFor(repository, registrationUrlCode));
	const auth = await repository.getAuthConfig();
	return await instantAccountCaptchaSteps(budgetEnvFor(ctx.services, request), {
		clientIp,
		escalationPerHour: auth.instant_accounts.pow_escalation_per_hour,
	});
}

export async function createAccountHandoff(
	ctx: ApiContext,
	params: {
		globalName: string;
		nonceHash: string;
		locale: string | undefined;
		registrationUrlCode: string | null | undefined;
		clientIp: string;
	},
): Promise<{handoff_code: string}> {
	const repository = getInstanceConfigRepository();
	await assertAccountCreationPastSetup(ctx, repository, params.clientIp);
	assertInstantAccountAvailable(await resolveInstantAccountAvailabilityFor(repository, params.registrationUrlCode));
	assertDisplayNameAllowed(params.globalName);
	const handoffCode = generateOpaqueToken();
	const record: AccountHandoffRecord = {
		global_name: params.globalName,
		nonce_hash: params.nonceHash,
		client_ip: params.clientIp,
		locale: params.locale ?? null,
		registration_url_code: params.registrationUrlCode?.trim() || null,
		created_at: Date.now(),
	};
	await ctx.services.kv.setex(handoffKey(handoffCode), HANDOFF_TTL_SECONDS, JSON.stringify(record));
	return {handoff_code: handoffCode};
}

export async function redeemHandoff(
	ctx: ApiContext,
	deps: RegistrationDependencies,
	params: {
		handoffCode: string;
		nonce: string;
		consent: boolean | undefined;
		globalName: string | undefined;
		request: Request;
		requestCache: RequestCache;
		tierContext: TierContext;
	},
): Promise<{user: User; token: string; deviceProof: string; entry: GuildEntryResult | null}> {
	await assertAccountCreationPastSetup(
		ctx,
		deps.instanceConfigRepository,
		requireConfiguredClientIp(params.request, ctx.services.config.proxy),
	);
	assertRegistrationConsent(
		ctx.services.config,
		await deps.instanceConfigRepository.getAppPublicConfig(),
		params.consent,
	);
	if (params.globalName !== undefined) assertDisplayNameAllowed(params.globalName);
	const raw = await ctx.services.kv.getdel(handoffKey(params.handoffCode));
	const record = raw === null ? null : parseJsonWithGuard(raw, isHandoffRecord);
	if (!record) throw new UnknownAccountHandoffError();
	const expected = Buffer.from(record.nonce_hash, 'hex');
	const actual = Buffer.from(sha256Hex(params.nonce), 'hex');
	if (expected.length !== actual.length || !timingSafeEqual(expected, actual)) {
		throw new InvalidAccountHandoffNonceError();
	}
	let result: AccountCreationResult;
	try {
		result = await createAccount(ctx, deps, {
			input: {
				email: null,
				emailProven: false,
				globalName: params.globalName ?? record.global_name,
				consent: params.consent,
				locale: record.locale,
				registrationUrlCode: record.registration_url_code ?? null,
			},
			request: params.request,
			clientIp: record.client_ip,
			requestCache: params.requestCache,
			source: 'marketing',
			tierContext: params.tierContext,
		});
	} catch (error) {
		if (isRetryableBeforeCreation(error)) await restoreHandoff(ctx, params.handoffCode, record);
		throw error;
	}
	if (result.pendingApproval) throw new RegistrationClosedError();
	return {user: result.user, token: result.token, deviceProof: result.deviceProof, entry: result.entry};
}

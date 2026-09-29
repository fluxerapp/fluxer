// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {Config} from '@app/api/Config';
import {ANONYMOUS_EXPERIMENT_TARGETING, resolveExperimentTargeting} from '@app/api/experiment/ExperimentTargeting';
import {sharedListHas} from '@app/api/infrastructure/activity/SharedLists';
import type {InstanceCaptchaEffectiveConfig} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {extractEmailDomain} from '@app/api/utils/EmailDomainUtils';
import {Headers} from '@fluxer/constants/src/Headers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {CaptchaRequiredError, InvalidCaptchaError} from '@fluxer/errors/src/CaptchaErrors';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {type AltchaCaptchaConfig, altchaCaptchaAppliesTo} from '@fluxer/schema/src/domains/admin/AltchaCaptchaSchemas';
import type {InstanceCaptchaProvider} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {createCaptchaProvider} from '@pkgs/captcha/src/CaptchaProviderFactory';
import type {ICaptchaProvider} from '@pkgs/captcha/src/ICaptchaProvider';
import {AltchaProvider} from '@pkgs/captcha/src/providers/AltchaProvider';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';

const ALTCHA_SPENT_CHALLENGE_KEY_PREFIX = 'captcha:altcha:spent:';

function deriveAltchaSecret(label: string): string {
	return createHmac('sha256', Config.auth.sudoModeSecret).update(label).digest('hex');
}

function createAltchaProvider(config: AltchaCaptchaConfig): AltchaProvider {
	return new AltchaProvider({
		hmacSignatureSecret: deriveAltchaSecret('fluxer-altcha-challenge-signature-v1'),
		hmacKeySignatureSecret: deriveAltchaSecret('fluxer-altcha-key-signature-v1'),
		cost: config.cost,
		maxCounter: config.max_counter,
		claimChallenge: (signature, ttlSeconds) =>
			getKVClient().setnx(`${ALTCHA_SPENT_CHALLENGE_KEY_PREFIX}${signature}`, '1', ttlSeconds),
		logger: Logger,
	});
}

async function altchaChallengeData(altcha: AltchaProvider | null): Promise<Record<string, unknown> | undefined> {
	if (!altcha) return undefined;
	return {captcha_provider: 'altcha', altcha_challenge: await altcha.createChallenge()};
}

async function resolveAltchaProvider(ctx: Context<HonoEnv>, user: User | undefined): Promise<AltchaProvider | null> {
	const config = await ctx.get('instanceConfigRepository').getAltchaCaptchaConfig();
	const targeting = user ? await resolveExperimentTargeting(user, [config]) : ANONYMOUS_EXPERIMENT_TARGETING;
	if (!altchaCaptchaAppliesTo(config, user ? user.id.toString() : null, targeting)) return null;
	return createAltchaProvider(config);
}

function resolveProviderSecret(
	config: InstanceCaptchaEffectiveConfig,
	provider: InstanceCaptchaProvider,
): string | null {
	if (provider === 'hcaptcha') {
		return config.hcaptcha_secret_key;
	}
	if (provider === 'turnstile') {
		return config.turnstile_secret_key;
	}
	return null;
}

function resolveCaptchaProvider(
	config: InstanceCaptchaEffectiveConfig,
	requestedType: string | undefined,
): ICaptchaProvider {
	if (Config.dev.testModeEnabled) {
		return createCaptchaProvider({mode: 'test'});
	}
	const requestedProvider =
		requestedType === 'hcaptcha' || requestedType === 'turnstile'
			? requestedType
			: config.provider === 'hcaptcha' || config.provider === 'turnstile'
				? config.provider
				: null;
	if (!requestedProvider) {
		throw new Error('Captcha is enabled but no provider is configured');
	}
	const secretKey = resolveProviderSecret(config, requestedProvider);
	if (!secretKey) {
		throw new InvalidCaptchaError();
	}
	return createCaptchaProvider({mode: requestedProvider, secretKey});
}

export async function verifyCaptchaToken(ctx: Context<HonoEnv>): Promise<void> {
	const captchaConfig = await ctx.get('instanceConfigRepository').getEffectiveCaptchaConfig();
	if (!captchaConfig.enabled && !(Config.dev.testModeEnabled && Config.captcha.enabled)) return;
	const user = ctx.get('user') as User | undefined;
	if (sharedListHas('email_domain_exempt', extractEmailDomain(user?.email))) return;
	if (userHasCaptchaExemptFlag(user)) return;
	if (await requestUserHasCaptchaExemptFlag(ctx)) return;
	const altcha = await resolveAltchaProvider(ctx, user);
	const token = ctx.req.header(Headers.X_CAPTCHA_TOKEN);
	if (!token) {
		throw new CaptchaRequiredError(await altchaChallengeData(altcha));
	}
	const requestedType = ctx.req.header(Headers.X_CAPTCHA_TYPE);
	if (requestedType === 'altcha') {
		if (!altcha || !(await altcha.verify({token}))) {
			throw new InvalidCaptchaError(await altchaChallengeData(altcha));
		}
		return;
	}
	const provider = resolveCaptchaProvider(captchaConfig, requestedType);
	const isValid = await provider.verify({
		token,
		remoteIp:
			extractClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			}) ?? undefined,
	});
	if (!isValid) {
		throw new InvalidCaptchaError(await altchaChallengeData(altcha));
	}
}

function userHasCaptchaExemptFlag(user: User | null | undefined): boolean {
	return user != null && (user.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;
}

async function requestUserHasCaptchaExemptFlag(ctx: Context<HonoEnv>): Promise<boolean> {
	try {
		const body = (await ctx.req.raw.clone().json()) as unknown;
		if (!body || typeof body !== 'object' || Array.isArray(body)) return false;
		const email = (body as Record<string, unknown>).email;
		if (typeof email !== 'string') return false;
		const user = await ctx.get('userRepository').findByEmail(email);
		return userHasCaptchaExemptFlag(user);
	} catch {
		return false;
	}
}

export const CaptchaMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	await verifyCaptchaToken(ctx);
	await next();
});

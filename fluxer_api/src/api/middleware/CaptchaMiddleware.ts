// SPDX-License-Identifier: AGPL-3.0-or-later

import {Headers} from '@fluxer/constants/src/Headers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {CaptchaRequiredError, InvalidCaptchaError} from '@fluxer/errors/src/CaptchaErrors';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {createCaptchaProvider} from '@pkgs/captcha/src/CaptchaProviderFactory';
import type {ICaptchaProvider} from '@pkgs/captcha/src/ICaptchaProvider';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import {Config} from '../Config';
import type {InstanceCaptchaEffectiveConfig, InstanceCaptchaProvider} from '../instance/InstanceConfigRepository';
import type {User} from '../models/User';
import {accountPolicyContactHasCapability} from '../risk/AccountPolicyService';
import type {HonoEnv} from '../types/HonoEnv';

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
	if (accountPolicyContactHasCapability(user?.email, 'captcha_exempt')) return;
	if (userHasCaptchaExemptFlag(user)) return;
	if (await requestUserHasCaptchaExemptFlag(ctx)) return;
	const token = ctx.req.header(Headers.X_CAPTCHA_TOKEN);
	if (!token) {
		throw new CaptchaRequiredError();
	}
	const provider = resolveCaptchaProvider(captchaConfig, ctx.req.header(Headers.X_CAPTCHA_TYPE));
	const isValid = await provider.verify({
		token,
		remoteIp:
			extractClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			}) ?? undefined,
	});
	if (!isValid) {
		throw new InvalidCaptchaError();
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

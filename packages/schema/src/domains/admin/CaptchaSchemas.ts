// SPDX-License-Identifier: AGPL-3.0-or-later

import {CaptchaProviders} from '@fluxer/constants/src/CaptchaConstants';
import {CaptchaProviderSchema} from '@fluxer/schema/src/domains/account/AccountTierSchemas';
import {z} from 'zod';

const CAPTCHA_MIN_COST = 1000;
const CAPTCHA_MAX_COST = 20000;
const CAPTCHA_MIN_MAX_COUNTER = 100;
const CAPTCHA_MAX_MAX_COUNTER = 20000;

const captchaConfigFields = {
	provider: CaptchaProviderSchema,
	cost: z.number().int().min(CAPTCHA_MIN_COST).max(CAPTCHA_MAX_COST),
	max_counter: z.number().int().min(CAPTCHA_MIN_MAX_COUNTER).max(CAPTCHA_MAX_MAX_COUNTER),
};

export const CaptchaConfigSchema = z.object({
	provider: captchaConfigFields.provider.default(CaptchaProviders.ALTCHA),
	cost: captchaConfigFields.cost.default(5000),
	max_counter: captchaConfigFields.max_counter.default(1000),
});

export type CaptchaConfig = z.infer<typeof CaptchaConfigSchema>;

export const DEFAULT_CAPTCHA_CONFIG: CaptchaConfig = CaptchaConfigSchema.parse({});

export const CaptchaConfigUpdateRequest = z
	.object({
		...captchaConfigFields,
		enabled: z.boolean().describe('Deprecated, use provider. false means none and true means altcha'),
	})
	.partial();

export type CaptchaConfigUpdateRequest = z.infer<typeof CaptchaConfigUpdateRequest>;

export const CaptchaConfigResponse = CaptchaConfigSchema.extend({
	enabled: z.boolean().describe('Deprecated, use provider. Whether any captcha is asked for'),
});

export type CaptchaConfigResponse = z.infer<typeof CaptchaConfigResponse>;

export function isCaptchaActive(config: Pick<CaptchaConfig, 'provider'>): boolean {
	return config.provider !== CaptchaProviders.NONE;
}

export function captchaProviderFromLegacyEnabled(enabled: boolean): CaptchaConfig['provider'] {
	return enabled ? CaptchaProviders.ALTCHA : CaptchaProviders.NONE;
}

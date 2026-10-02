// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const CAPTCHA_MIN_COST = 1000;
export const CAPTCHA_MAX_COST = 20000;
export const CAPTCHA_MIN_MAX_COUNTER = 100;
export const CAPTCHA_MAX_MAX_COUNTER = 20000;

const captchaConfigFields = {
	enabled: z.boolean(),
	cost: z.number().int().min(CAPTCHA_MIN_COST).max(CAPTCHA_MAX_COST),
	max_counter: z.number().int().min(CAPTCHA_MIN_MAX_COUNTER).max(CAPTCHA_MAX_MAX_COUNTER),
};

export const CaptchaConfigSchema = z.object({
	enabled: captchaConfigFields.enabled.default(true),
	cost: captchaConfigFields.cost.default(5000),
	max_counter: captchaConfigFields.max_counter.default(1000),
});

export type CaptchaConfig = z.infer<typeof CaptchaConfigSchema>;

export const DEFAULT_CAPTCHA_CONFIG: CaptchaConfig = CaptchaConfigSchema.parse({});

export const CaptchaConfigUpdateRequest = z.object(captchaConfigFields).partial();

export type CaptchaConfigUpdateRequest = z.infer<typeof CaptchaConfigUpdateRequest>;

export const CaptchaConfigResponse = CaptchaConfigSchema;

export type CaptchaConfigResponse = z.infer<typeof CaptchaConfigResponse>;

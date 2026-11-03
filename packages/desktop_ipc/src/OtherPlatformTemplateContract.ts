// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS = Object.freeze({
	fetch: 'desktop-other-platform-template:fetch',
} as const);

const OTHER_PLATFORM_TEMPLATE_CODE_PATTERN = /^[A-Za-z0-9]{1,64}$/;

export function isOtherPlatformTemplateCode(value: unknown): value is string {
	return typeof value === 'string' && OTHER_PLATFORM_TEMPLATE_CODE_PATTERN.test(value);
}

export const OtherPlatformTemplateFetchFailure = Object.freeze({
	INVALID_CODE: 'invalid_code',
	NOT_FOUND: 'not_found',
	RATE_LIMITED: 'rate_limited',
	NETWORK: 'network',
	INVALID_RESPONSE: 'invalid_response',
} as const);

export type OtherPlatformTemplateFetchFailure =
	(typeof OtherPlatformTemplateFetchFailure)[keyof typeof OtherPlatformTemplateFetchFailure];

interface OtherPlatformTemplateFetchSuccess {
	readonly ok: true;
	readonly template: Readonly<Record<string, unknown>>;
}

export interface OtherPlatformTemplateFetchError {
	readonly ok: false;
	readonly failure: OtherPlatformTemplateFetchFailure;
}

export type OtherPlatformTemplateFetchResult = OtherPlatformTemplateFetchSuccess | OtherPlatformTemplateFetchError;

export interface OtherPlatformTemplateAPI {
	fetch: (code: string) => Promise<OtherPlatformTemplateFetchResult>;
}

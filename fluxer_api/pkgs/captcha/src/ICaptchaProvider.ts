// SPDX-License-Identifier: AGPL-3.0-or-later

export interface VerifyCaptchaParams {
	token: string;
	remoteIp?: string;
}

export type CaptchaProviderType = 'hcaptcha' | 'recaptcha' | 'turnstile' | 'altcha' | 'test' | 'unavailable';

export interface ICaptchaProvider {
	readonly type: CaptchaProviderType;
	verify(params: VerifyCaptchaParams): Promise<boolean>;
}

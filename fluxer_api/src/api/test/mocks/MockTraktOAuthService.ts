// SPDX-License-Identifier: AGPL-3.0-or-later

import {vi} from 'vitest';
import type {UserID} from '../../BrandedTypes';
import type {ITraktOAuthService, TraktAuthorizeResult, TraktCallbackResult} from '../../trakt/ITraktOAuthService';

interface MockTraktOAuthServiceOptions {
	authorizeResult?: TraktAuthorizeResult;
	callbackResult?: TraktCallbackResult;
	restoreAndVerifyResult?: {
		username: string;
	} | null;
	shouldFailAuthorize?: boolean;
	shouldFailCallback?: boolean;
}

export class MockTraktOAuthService implements ITraktOAuthService {
	readonly authorizeSpy = vi.fn();
	readonly callbackSpy = vi.fn();
	readonly restoreAndVerifySpy = vi.fn();
	readonly storeRefreshTokenSpy = vi.fn();
	readonly revokeSpy = vi.fn();
	private options: MockTraktOAuthServiceOptions;

	constructor(options: MockTraktOAuthServiceOptions = {}) {
		this.options = options;
		this.setupDefaults();
	}

	private setupDefaults(): void {
		this.authorizeSpy.mockImplementation(async () => {
			if (this.options.shouldFailAuthorize) {
				throw new Error('Mock authorise failure');
			}
			return this.options.authorizeResult ?? {authorizeUrl: 'https://trakt.tv/oauth/authorize?mock=true'};
		});
		this.callbackSpy.mockImplementation(async () => {
			if (this.options.shouldFailCallback) {
				throw new Error('Mock callback failure');
			}
			if (!this.options.callbackResult) {
				throw new Error('No callbackResult configured in mock');
			}
			return this.options.callbackResult;
		});
		this.restoreAndVerifySpy.mockImplementation(async () => {
			return this.options.restoreAndVerifyResult ?? null;
		});
		this.storeRefreshTokenSpy.mockResolvedValue(undefined);
		this.revokeSpy.mockResolvedValue(undefined);
	}

	async authorize(userId: UserID): Promise<TraktAuthorizeResult> {
		return this.authorizeSpy(userId);
	}

	async callback(params: URLSearchParams): Promise<TraktCallbackResult> {
		return this.callbackSpy(params);
	}

	async restoreAndVerify(traktUserId: string): Promise<{
		username: string;
	} | null> {
		return this.restoreAndVerifySpy(traktUserId);
	}

	async storeRefreshToken(traktUserId: string, refreshToken: string): Promise<void> {
		return this.storeRefreshTokenSpy(traktUserId, refreshToken);
	}

	async revoke(traktUserId: string): Promise<void> {
		return this.revokeSpy(traktUserId);
	}

	configure(options: Partial<MockTraktOAuthServiceOptions>): void {
		this.options = {...this.options, ...options};
		this.setupDefaults();
	}

	reset(): void {
		this.authorizeSpy.mockReset();
		this.callbackSpy.mockReset();
		this.restoreAndVerifySpy.mockReset();
		this.storeRefreshTokenSpy.mockReset();
		this.revokeSpy.mockReset();
		this.options = {};
		this.setupDefaults();
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {
	BlueskyAuthorizeResult,
	BlueskyCallbackResult,
	IBlueskyOAuthService,
} from '@app/api/bluesky/IBlueskyOAuthService';
import {vi} from 'vitest';

interface MockBlueskyOAuthServiceOptions {
	authorizeResult?: BlueskyAuthorizeResult;
	callbackResult?: BlueskyCallbackResult;
	shouldFailAuthorize?: boolean;
	shouldFailCallback?: boolean;
}

export class MockBlueskyOAuthService implements IBlueskyOAuthService {
	readonly authorizeSpy = vi.fn();
	readonly callbackSpy = vi.fn();
	readonly clientMetadata: Record<string, unknown> = {client_id: 'https://test/metadata.json'};
	readonly jwks: Record<string, unknown> = {keys: []};
	private options: MockBlueskyOAuthServiceOptions;

	constructor(options: MockBlueskyOAuthServiceOptions = {}) {
		this.options = options;
		this.setupDefaults();
	}

	private setupDefaults(): void {
		this.authorizeSpy.mockImplementation(async () => {
			if (this.options.shouldFailAuthorize) {
				throw new Error('Mock authorise failure');
			}
			return this.options.authorizeResult ?? {authorizeUrl: 'https://bsky.social/oauth/authorize?mock=true'};
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
	}

	async authorize(handle: string, userId: UserID): Promise<BlueskyAuthorizeResult> {
		return this.authorizeSpy(handle, userId);
	}

	async callback(params: URLSearchParams): Promise<BlueskyCallbackResult> {
		return this.callbackSpy(params);
	}

	configure(options: Partial<MockBlueskyOAuthServiceOptions>): void {
		this.options = {...this.options, ...options};
		this.setupDefaults();
	}

	reset(): void {
		this.authorizeSpy.mockReset();
		this.callbackSpy.mockReset();
		this.options = {};
		this.setupDefaults();
	}
}

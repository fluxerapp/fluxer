// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {createUserID, type UserID} from '../BrandedTypes';
import type {TraktOAuthConfig} from '../config/APIConfig';
import {Logger} from '../Logger';
import {isJsonRecord} from '../utils/JsonBoundaryUtils';
import type {ITraktOAuthService, TraktAuthorizeResult, TraktCallbackResult} from './ITraktOAuthService';
import {createTraktOAuthStateStore, createTraktRefreshTokenStore} from './TraktOAuthStores';

const TRAKT_AUTHORIZE_URL = 'https://trakt.tv/oauth/authorize';
const TRAKT_TOKEN_URL = 'https://api.trakt.tv/oauth/token';
const TRAKT_SETTINGS_URL = 'https://api.trakt.tv/users/settings';
const TRAKT_USER_AGENT = 'Fluxer/1.0';
const OAUTH_STATE_TTL_SECONDS = 3600;
const REFRESH_TOKEN_TTL_SECONDS = 31_536_000;

interface TraktTokenResponse {
	access_token?: string;
	refresh_token?: string;
	token_type?: string;
	expires_in?: number;
	scope?: string;
	created_at?: number;
}

interface TraktUserSettingsResponse {
	user?: {
		username?: string;
		ids?: {
			slug?: string;
			uuid?: string;
		};
	};
	uuid?: string;
}

function extractTraktUserIdentity(settings: TraktUserSettingsResponse): {traktUserId: string; username: string} | null {
	const username = settings.user?.username ?? settings.user?.ids?.slug;
	const traktUserId = settings.user?.ids?.uuid ?? settings.uuid;
	if (!username || !traktUserId) {
		return null;
	}
	return {traktUserId, username};
}

async function readTraktJsonResponse(response: Response, label: string): Promise<Record<string, unknown>> {
	if (!response.ok) {
		const responseBody = await response.text();
		throw new Error(`Trakt ${label} request failed with status ${response.status}: ${responseBody}`);
	}
	const json: unknown = await response.json();
	if (!isJsonRecord(json)) {
		throw new Error(`Trakt ${label} response is not a JSON object`);
	}
	return json;
}

export class TraktOAuthService implements ITraktOAuthService {
	private constructor(
		private readonly config: TraktOAuthConfig,
		private readonly redirectUri: string,
		private readonly stateStore: ReturnType<typeof createTraktOAuthStateStore>,
		private readonly refreshTokenStore: ReturnType<typeof createTraktRefreshTokenStore>,
	) {}

	static create(config: TraktOAuthConfig, kvClient: IKVProvider, apiPublicEndpoint: string): TraktOAuthService {
		const baseUrl = apiPublicEndpoint.replace(/\/$/, '');
		return new TraktOAuthService(
			config,
			`${baseUrl}/connections/trakt/callback`,
			createTraktOAuthStateStore(kvClient, OAUTH_STATE_TTL_SECONDS),
			createTraktRefreshTokenStore(kvClient, REFRESH_TOKEN_TTL_SECONDS),
		);
	}

	async authorize(userId: UserID): Promise<TraktAuthorizeResult> {
		const state = randomUUID();
		await this.stateStore.set(state, {userId: String(userId)});
		const params = new URLSearchParams({
			response_type: 'code',
			client_id: this.config.client_id,
			redirect_uri: this.redirectUri,
			state,
		});
		return {authorizeUrl: `${TRAKT_AUTHORIZE_URL}?${params.toString()}`};
	}

	async callback(params: URLSearchParams): Promise<TraktCallbackResult> {
		const code = params.get('code');
		const state = params.get('state');
		if (!code || !state) {
			throw new Error('Trakt OAuth callback is missing code or state');
		}
		const storedState = await this.stateStore.get(state);
		if (!storedState) {
			throw new Error('Trakt OAuth state is invalid or expired');
		}
		const userId = createUserID(BigInt(storedState.userId));
		const tokenResponse = await this.exchangeCode(code);
		const accessToken = tokenResponse.access_token;
		const refreshToken = tokenResponse.refresh_token;
		if (!accessToken || !refreshToken) {
			throw new Error('Trakt OAuth token response is missing tokens');
		}
		const settings = await this.fetchUserSettings(accessToken);
		const identity = extractTraktUserIdentity(settings);
		if (!identity) {
			throw new Error('Trakt user settings response is missing user identity');
		}
		await this.storeRefreshToken(identity.traktUserId, refreshToken);
		return {
			userId,
			traktUserId: identity.traktUserId,
			username: identity.username,
		};
	}

	async restoreAndVerify(traktUserId: string): Promise<{
		username: string;
	} | null> {
		try {
			const refreshToken = await this.refreshTokenStore.get(traktUserId);
			if (!refreshToken) {
				return null;
			}
			const tokenResponse = await this.refreshAccessToken(refreshToken);
			const accessToken = tokenResponse.access_token;
			const nextRefreshToken = tokenResponse.refresh_token;
			if (!accessToken) {
				return null;
			}
			if (nextRefreshToken) {
				await this.storeRefreshToken(traktUserId, nextRefreshToken);
			}
			const settings = await this.fetchUserSettings(accessToken);
			const identity = extractTraktUserIdentity(settings);
			if (!identity) {
				return null;
			}
			return {username: identity.username};
		} catch (error) {
			Logger.error(
				{
					traktUserId,
					error: error instanceof Error ? error.message : String(error),
				},
				'Failed to restore and verify Trakt session',
			);
			return null;
		}
	}

	async storeRefreshToken(traktUserId: string, refreshToken: string): Promise<void> {
		await this.refreshTokenStore.set(traktUserId, refreshToken);
	}

	async revoke(traktUserId: string): Promise<void> {
		await this.refreshTokenStore.del(traktUserId);
	}

	private async exchangeCode(code: string): Promise<TraktTokenResponse> {
		return this.requestToken({
			code,
			client_id: this.config.client_id,
			client_secret: this.config.client_secret,
			redirect_uri: this.redirectUri,
			grant_type: 'authorization_code',
		});
	}

	private async refreshAccessToken(refreshToken: string): Promise<TraktTokenResponse> {
		return this.requestToken({
			refresh_token: refreshToken,
			client_id: this.config.client_id,
			client_secret: this.config.client_secret,
			redirect_uri: this.redirectUri,
			grant_type: 'refresh_token',
		});
	}

	private getTraktApiHeaders(accessToken?: string): Record<string, string> {
		const headers: Record<string, string> = {
			'Content-Type': 'application/json',
			'User-Agent': TRAKT_USER_AGENT,
			'trakt-api-version': '2',
			'trakt-api-key': this.config.client_id,
		};
		if (accessToken) {
			headers.Authorization = `Bearer ${accessToken}`;
		}
		return headers;
	}

	private async requestToken(body: Record<string, string>): Promise<TraktTokenResponse> {
		const response = await fetch(TRAKT_TOKEN_URL, {
			method: 'POST',
			headers: this.getTraktApiHeaders(),
			body: JSON.stringify(body),
		});
		return readTraktJsonResponse(response, 'token') as TraktTokenResponse;
	}

	private async fetchUserSettings(accessToken: string): Promise<TraktUserSettingsResponse> {
		const response = await fetch(TRAKT_SETTINGS_URL, {
			method: 'GET',
			headers: this.getTraktApiHeaders(accessToken),
		});
		return readTraktJsonResponse(response, 'settings') as TraktUserSettingsResponse;
	}
}

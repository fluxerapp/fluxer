// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {readFile} from 'node:fs/promises';
import {Config} from '@app/api/Config';
import {
	classifyGooglePlayStatus,
	GooglePlayApiError,
	parseRetryAfterMs,
} from '@app/api/store_billing/google_play/GooglePlayApiError';
import {ms, seconds} from 'itty-time';
import {type CryptoKey, importPKCS8, SignJWT} from 'jose';
import {z} from 'zod';

export const GOOGLE_PLAY_ANDROIDPUBLISHER_SCOPE = 'https://www.googleapis.com/auth/androidpublisher';
export const GOOGLE_DEFAULT_TOKEN_URI = 'https://oauth2.googleapis.com/token';
const JWT_BEARER_GRANT_TYPE = 'urn:ietf:params:oauth:grant-type:jwt-bearer';
const ASSERTION_TTL_SECONDS = seconds('1 hour');
const DEFAULT_EXPIRES_IN_SECONDS = seconds('1 hour');
const REFRESH_MARGIN_MS = ms('5 minutes');
const TOKEN_REQUEST_TIMEOUT_MS = ms('10 seconds');

const ServiceAccountJsonSchema = z.object({
	client_email: z.string().optional(),
	private_key: z.string().optional(),
	private_key_id: z.string().optional(),
	token_uri: z.string().optional(),
});

const TokenResponseSchema = z.object({
	access_token: z.string().min(1),
	expires_in: z.union([z.number(), z.string()]).optional(),
	token_type: z.string().optional(),
});

const TokenErrorSchema = z.object({
	error: z.string().optional(),
	error_description: z.string().optional(),
});

export interface GooglePlayServiceAccountCredentials {
	clientEmail: string;
	privateKey: string;
	privateKeyId: string | null;
	tokenUri: string;
}

interface CachedAccessToken {
	fingerprint: string;
	token: string;
	expiresAtMs: number;
}

interface GooglePlayAccessTokenProviderOptions {
	now?: () => number;
	timeoutMs?: number;
}

function hasText(value: string | undefined): value is string {
	return value !== undefined && value.trim().length > 0;
}

function normalizePem(value: string): string {
	return value.replaceAll('\\n', '\n');
}

function parseJsonOrNull(raw: string): unknown {
	try {
		return JSON.parse(raw);
	} catch {
		return null;
	}
}

function configFingerprint(): string {
	const cfg = Config.googlePlay;
	return createHash('sha256')
		.update(
			JSON.stringify([
				cfg.clientEmail ?? null,
				cfg.privateKey ?? null,
				cfg.privateKeyPath ?? null,
				cfg.serviceAccountJsonPath ?? null,
				cfg.tokenUri,
			]),
		)
		.digest('hex');
}

export async function resolveGooglePlayCredentials(): Promise<GooglePlayServiceAccountCredentials> {
	const cfg = Config.googlePlay;
	let serviceAccount: z.infer<typeof ServiceAccountJsonSchema> = {};
	if (hasText(cfg.serviceAccountJsonPath)) {
		const raw = await readFile(cfg.serviceAccountJsonPath, 'utf8');
		const parsed = ServiceAccountJsonSchema.safeParse(parseJsonOrNull(raw));
		if (!parsed.success) {
			throw new GooglePlayApiError('Google Play service account file is invalid', 'auth', null, 'invalid_credentials');
		}
		serviceAccount = parsed.data;
	}
	const clientEmail = hasText(cfg.clientEmail) ? cfg.clientEmail : serviceAccount.client_email;
	let privateKey: string | undefined;
	if (hasText(cfg.privateKey)) {
		privateKey = cfg.privateKey;
	} else if (hasText(cfg.privateKeyPath)) {
		privateKey = await readFile(cfg.privateKeyPath, 'utf8');
	} else {
		privateKey = serviceAccount.private_key;
	}
	if (!hasText(clientEmail) || !hasText(privateKey)) {
		throw new GooglePlayApiError('Google Play credentials are not configured', 'auth', null, 'missing_credentials');
	}
	const tokenUri =
		cfg.tokenUri !== GOOGLE_DEFAULT_TOKEN_URI || !hasText(serviceAccount.token_uri)
			? cfg.tokenUri
			: serviceAccount.token_uri;
	return {
		clientEmail: clientEmail.trim(),
		privateKey: normalizePem(privateKey),
		privateKeyId: hasText(serviceAccount.private_key_id) ? serviceAccount.private_key_id : null,
		tokenUri,
	};
}

function parseExpiresInSeconds(value: number | string | undefined): number {
	const parsed = typeof value === 'string' ? Number.parseInt(value.trim(), 10) : value;
	if (parsed === undefined || !Number.isFinite(parsed) || parsed <= 0) {
		return DEFAULT_EXPIRES_IN_SECONDS;
	}
	return parsed;
}

export class GooglePlayAccessTokenProvider {
	private cached: CachedAccessToken | null = null;
	private inFlight: {fingerprint: string; promise: Promise<string>} | null = null;
	private readonly signingKeys = new Map<string, Promise<CryptoKey>>();
	private readonly now: () => number;
	private readonly timeoutMs: number;

	constructor(options: GooglePlayAccessTokenProviderOptions = {}) {
		this.now = options.now ?? Date.now;
		this.timeoutMs = options.timeoutMs ?? TOKEN_REQUEST_TIMEOUT_MS;
	}

	async getAccessToken(): Promise<string> {
		const fingerprint = configFingerprint();
		const cached = this.cached;
		if (cached && cached.fingerprint === fingerprint && this.now() < cached.expiresAtMs - REFRESH_MARGIN_MS) {
			return cached.token;
		}
		if (this.inFlight && this.inFlight.fingerprint === fingerprint) {
			return await this.inFlight.promise;
		}
		const promise = this.fetchAccessToken(fingerprint);
		const entry = {fingerprint, promise};
		this.inFlight = entry;
		try {
			return await promise;
		} finally {
			if (this.inFlight === entry) {
				this.inFlight = null;
			}
		}
	}

	invalidate(token?: string): void {
		if (token === undefined || this.cached?.token === token) {
			this.cached = null;
		}
	}

	private async fetchAccessToken(fingerprint: string): Promise<string> {
		const credentials = await resolveGooglePlayCredentials();
		const assertion = await this.signAssertion(credentials);
		let response: Response;
		try {
			response = await fetch(credentials.tokenUri, {
				method: 'POST',
				headers: {
					'Content-Type': 'application/x-www-form-urlencoded',
					Accept: 'application/json',
				},
				body: new URLSearchParams({grant_type: JWT_BEARER_GRANT_TYPE, assertion}).toString(),
				signal: AbortSignal.timeout(this.timeoutMs),
			});
		} catch (error) {
			throw new GooglePlayApiError(
				`Google OAuth token request failed: ${error instanceof Error ? error.message : String(error)}`,
				'retryable',
				null,
				'network_error',
			);
		}
		const body: unknown = await response.json().catch(() => null);
		if (!response.ok) {
			const kind = classifyGooglePlayStatus(response.status);
			const parsedError = TokenErrorSchema.safeParse(body);
			const reason = parsedError.success ? (parsedError.data.error ?? null) : null;
			throw new GooglePlayApiError(
				`Google OAuth token request failed with ${response.status}`,
				kind === 'retryable' ? 'retryable' : 'auth',
				response.status,
				reason,
				parseRetryAfterMs(response.headers.get('retry-after'), this.now()),
			);
		}
		const parsed = TokenResponseSchema.safeParse(body);
		if (!parsed.success) {
			throw new GooglePlayApiError(
				'Google OAuth token response is malformed',
				'retryable',
				response.status,
				'malformed_response',
			);
		}
		const token = parsed.data.access_token;
		if (configFingerprint() === fingerprint) {
			this.cached = {
				fingerprint,
				token,
				expiresAtMs: this.now() + parseExpiresInSeconds(parsed.data.expires_in) * 1000,
			};
		}
		return token;
	}

	private async signAssertion(credentials: GooglePlayServiceAccountCredentials): Promise<string> {
		const key = await this.signingKey(credentials.privateKey);
		const nowSeconds = Math.floor(this.now() / 1000);
		return await new SignJWT({scope: GOOGLE_PLAY_ANDROIDPUBLISHER_SCOPE})
			.setProtectedHeader({
				alg: 'RS256',
				typ: 'JWT',
				...(credentials.privateKeyId ? {kid: credentials.privateKeyId} : {}),
			})
			.setIssuer(credentials.clientEmail)
			.setAudience(credentials.tokenUri)
			.setIssuedAt(nowSeconds)
			.setExpirationTime(nowSeconds + ASSERTION_TTL_SECONDS)
			.sign(key);
	}

	private async signingKey(privateKey: string): Promise<CryptoKey> {
		const cacheKey = createHash('sha256').update(privateKey).digest('hex');
		const cached = this.signingKeys.get(cacheKey);
		if (cached) {
			return await cached;
		}
		const pending = importPKCS8(privateKey, 'RS256');
		this.signingKeys.set(cacheKey, pending);
		try {
			return await pending;
		} catch {
			this.signingKeys.delete(cacheKey);
			throw new GooglePlayApiError('Google Play private key is invalid', 'auth', null, 'invalid_credentials');
		}
	}
}

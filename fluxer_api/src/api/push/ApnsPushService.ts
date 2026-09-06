// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {readFile} from 'node:fs/promises';
import {type ClientHttp2Session, connect, constants} from 'node:http2';
import {importPKCS8, SignJWT} from 'jose';
import {Config} from '../Config';
import type {PushProviderEnvironment} from '../config/APIConfig';
import {Logger} from '../Logger';

const APNS_PROVIDER_TOKEN_TTL_SECONDS = 50 * 60;
const APNS_REQUEST_TIMEOUT_MS = 5000;
const APNS_MAX_RESPONSE_BYTES = 8192;
const APNS_PUSH_TYPE_ALERT = 'alert';
const APNS_PUSH_TYPE_BACKGROUND = 'background';
const APNS_CATEGORY_MESSAGE = 'FLUXER_MESSAGE';
const APNS_DEFAULT_SOUND = 'default';
const HTTP2_OK_MIN = 200;
const HTTP2_OK_MAX = 299;

interface SendApnsPushParams {
	userId: string;
	subscriptionId: string;
	deviceToken: string;
	appId: string;
	providerEnvironment: PushProviderEnvironment;
	payload: Record<string, unknown>;
}

interface SendApnsPushResult {
	success: boolean;
	shouldDelete: boolean;
	reason?: string;
	statusCode?: number;
}

interface CachedProviderToken {
	token: string;
	expiresAtSeconds: number;
}

interface Http2Response {
	statusCode: number;
	body: string;
}

const providerTokenCache = new Map<string, CachedProviderToken>();
const apnsSigningKeys = new Map<string, ReturnType<typeof importPKCS8>>();
const apnsSessions = new Map<string, ClientHttp2Session>();

export async function sendApnsPush(params: SendApnsPushParams): Promise<SendApnsPushResult> {
	const cfg = Config.push.apns;
	if (!cfg.enabled) {
		return {success: false, shouldDelete: false, reason: 'apns_disabled'};
	}
	const topic = resolveApnsTopic(params.appId, params.providerEnvironment);
	if (!topic) {
		return {success: false, shouldDelete: false, reason: 'apns_topic_missing'};
	}
	const providerToken = await getProviderToken();
	if (!providerToken) {
		return {success: false, shouldDelete: false, reason: 'apns_auth_missing'};
	}
	const requestBody = JSON.stringify(buildApnsPayload(params.payload));
	const host = params.providerEnvironment === 'development' ? 'api.sandbox.push.apple.com' : 'api.push.apple.com';
	const headers = buildApnsHeaders({providerToken, topic, payload: params.payload});
	try {
		const response = await sendApnsHttp2Request({
			origin: `https://${host}`,
			path: `/3/device/${params.deviceToken}`,
			headers,
			body: requestBody,
		});
		if (response.statusCode >= HTTP2_OK_MIN && response.statusCode <= HTTP2_OK_MAX) {
			return {success: true, shouldDelete: false, statusCode: response.statusCode};
		}
		const reason = parseApnsReason(response.body) ?? `http_${response.statusCode}`;
		return {
			success: false,
			shouldDelete: isPermanentApnsFailure(response.statusCode, reason),
			reason,
			statusCode: response.statusCode,
		};
	} catch (error) {
		Logger.warn({error, userId: params.userId, subscriptionId: params.subscriptionId}, 'APNs push request failed');
		return {success: false, shouldDelete: false, reason: 'apns_network_error'};
	}
}

function resolveApnsTopic(appId: string, environment: PushProviderEnvironment): string | null {
	const apps = Config.push.apns.apps;
	const exact = apps.find((app) => app.appId === appId && app.environment === environment && app.topic);
	if (exact?.topic) return exact.topic;
	const fallback = apps.find((app) => app.appId === appId && app.topic);
	return fallback?.topic ?? null;
}

async function getProviderToken(): Promise<string | null> {
	const cfg = Config.push.apns;
	if (!cfg.teamId || !cfg.keyId) return null;
	const privateKey = await resolveApnsPrivateKey();
	if (!privateKey) return null;
	const cacheKey = createHash('sha256').update(`${cfg.teamId}:${cfg.keyId}:${privateKey}`).digest('hex');
	const nowSeconds = Math.floor(Date.now() / 1000);
	const cached = providerTokenCache.get(cacheKey);
	if (cached && cached.expiresAtSeconds > nowSeconds) {
		return cached.token;
	}
	const key = await apnsSigningKey(privateKey);
	const token = await new SignJWT({})
		.setProtectedHeader({alg: 'ES256', kid: cfg.keyId})
		.setIssuer(cfg.teamId)
		.setIssuedAt(nowSeconds)
		.sign(key);
	providerTokenCache.set(cacheKey, {token, expiresAtSeconds: nowSeconds + APNS_PROVIDER_TOKEN_TTL_SECONDS});
	return token;
}

async function resolveApnsPrivateKey(): Promise<string | null> {
	const cfg = Config.push.apns;
	if (cfg.privateKey && cfg.privateKey.trim().length > 0) {
		return normalizePem(cfg.privateKey);
	}
	if (!cfg.privateKeyPath) return null;
	const pem = await readFile(cfg.privateKeyPath, 'utf8');
	return normalizePem(pem);
}

export async function ensureApnsSigningKey(): Promise<void> {
	if (!Config.push.apns.enabled) return;
	const privateKey = await resolveApnsPrivateKey();
	if (!privateKey) return;
	await apnsSigningKey(privateKey);
	Logger.info('APNs signing key loaded');
}

async function apnsSigningKey(privateKey: string): Promise<Awaited<ReturnType<typeof importPKCS8>>> {
	const cached = apnsSigningKeys.get(privateKey);
	if (cached) return await cached;
	const pending = importPKCS8(privateKey, 'ES256');
	apnsSigningKeys.set(privateKey, pending);
	try {
		return await pending;
	} catch (error) {
		apnsSigningKeys.delete(privateKey);
		throw error;
	}
}

function normalizePem(value: string): string {
	return value.replaceAll('\\n', '\n');
}

function buildApnsHeaders(params: {
	providerToken: string;
	topic: string;
	payload: Record<string, unknown>;
}): Record<string, string> {
	const data = isRecord(params.payload.data) ? params.payload.data : {};
	const collapseId = optionalString(params.payload.tag) ?? optionalString(data.message_id);
	const isClear = isClearNotificationPayload(params.payload);
	const headers: Record<string, string> = {
		[constants.HTTP2_HEADER_METHOD]: constants.HTTP2_METHOD_POST,
		authorization: `bearer ${params.providerToken}`,
		'apns-topic': params.topic,
		'apns-push-type': isClear ? APNS_PUSH_TYPE_BACKGROUND : APNS_PUSH_TYPE_ALERT,
		'apns-priority': isClear ? '5' : '10',
		'apns-expiration': String(Math.floor(Date.now() / 1000) + (isClear ? 3600 : 86400)),
		'content-type': 'application/json',
	};
	if (collapseId && Buffer.byteLength(collapseId) <= 64) {
		headers['apns-collapse-id'] = collapseId;
	}
	return headers;
}

function buildApnsPayload(payload: Record<string, unknown>): Record<string, unknown> {
	if (isClearNotificationPayload(payload)) {
		const data = isRecord(payload.data) ? payload.data : {};
		return {
			...data,
			type: 'notification_clear',
			action: 'clear_channel',
			aps: {'content-available': 1},
		};
	}
	const data = isRecord(payload.data) ? payload.data : {};
	const notification = isRecord(payload.notification) ? payload.notification : {};
	const title = optionalString(notification.title) ?? optionalString(payload.title) ?? 'Fluxer';
	const body = optionalString(notification.body) ?? optionalString(payload.body) ?? '';
	const badge = normalizeBadgeCount(data.badge_count);
	const channelId = optionalString(data.channel_id);
	const threadId =
		optionalString(data.notification_tag) ?? (channelId ? `channel:${channelId}` : undefined) ?? 'fluxer-message';
	const imageUrl = firstString([payload.image_url, notification.image]);
	const aps: Record<string, unknown> = {
		alert: {title, body},
		sound: APNS_DEFAULT_SOUND,
		'thread-id': threadId,
		category: APNS_CATEGORY_MESSAGE,
		'interruption-level': 'active',
		'relevance-score': 0.5,
	};
	if (badge !== undefined) {
		aps.badge = badge;
	}
	if (imageUrl) {
		aps['mutable-content'] = 1;
	}
	return {
		...data,
		title,
		body,
		url: optionalString(data.url) ?? optionalString(notification.navigate),
		image_url: imageUrl,
		aps,
	};
}

function isClearNotificationPayload(payload: Record<string, unknown>): boolean {
	return payload.type === 'notification_clear' || payload.action === 'clear_channel';
}

async function sendApnsHttp2Request(params: {
	origin: string;
	path: string;
	headers: Record<string, string>;
	body: string;
}): Promise<Http2Response> {
	const session = getApnsSession(params.origin);
	return new Promise<Http2Response>((resolve, reject) => {
		const req = session.request({
			...params.headers,
			[constants.HTTP2_HEADER_PATH]: params.path,
		});
		const chunks: Array<Buffer> = [];
		let bodyBytes = 0;
		let statusCode = 0;
		const timeout = setTimeout(() => {
			req.close(constants.NGHTTP2_CANCEL);
			reject(new Error('APNs request timed out'));
		}, APNS_REQUEST_TIMEOUT_MS);
		req.on('response', (headers) => {
			const rawStatus = headers[constants.HTTP2_HEADER_STATUS];
			statusCode = typeof rawStatus === 'number' ? rawStatus : Number(rawStatus ?? 0);
		});
		req.on('data', (chunk) => {
			const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
			bodyBytes += buffer.length;
			if (bodyBytes > APNS_MAX_RESPONSE_BYTES) {
				clearTimeout(timeout);
				req.close(constants.NGHTTP2_CANCEL);
				reject(new Error('APNs response body too large'));
				return;
			}
			chunks.push(buffer);
		});
		req.on('end', () => {
			clearTimeout(timeout);
			resolve({statusCode, body: Buffer.concat(chunks).toString('utf8')});
		});
		req.on('error', (error) => {
			clearTimeout(timeout);
			dropApnsSession(params.origin);
			reject(error);
		});
		req.end(params.body);
	});
}

function getApnsSession(origin: string): ClientHttp2Session {
	const existing = apnsSessions.get(origin);
	if (existing && !existing.closed && !existing.destroyed) {
		return existing;
	}
	const session = connect(origin);
	apnsSessions.set(origin, session);
	session.on('goaway', () => dropApnsSession(origin));
	session.on('error', (error) => {
		Logger.warn({error, origin}, 'APNs HTTP/2 session error');
		dropApnsSession(origin);
	});
	session.on('close', () => apnsSessions.delete(origin));
	return session;
}

function dropApnsSession(origin: string): void {
	const session = apnsSessions.get(origin);
	apnsSessions.delete(origin);
	if (session && !session.destroyed) {
		session.destroy();
	}
}

function parseApnsReason(body: string): string | null {
	if (!body) return null;
	try {
		const parsed: unknown = JSON.parse(body);
		if (isRecord(parsed) && typeof parsed.reason === 'string') return parsed.reason;
		return null;
	} catch {
		return null;
	}
}

function isPermanentApnsFailure(statusCode: number, reason: string): boolean {
	if (statusCode === 410) return true;
	if (statusCode === 400 && (reason === 'BadDeviceToken' || reason === 'DeviceTokenNotForTopic')) return true;
	return reason === 'Unregistered';
}

function normalizeBadgeCount(value: unknown): number | undefined {
	if (typeof value === 'number' && Number.isFinite(value)) return Math.max(0, Math.floor(value));
	if (typeof value === 'string') {
		const parsed = Number.parseInt(value, 10);
		return Number.isFinite(parsed) ? Math.max(0, parsed) : undefined;
	}
	return undefined;
}

function firstString(values: Array<unknown>): string | undefined {
	for (const value of values) {
		const stringValue = optionalString(value);
		if (stringValue) return stringValue;
	}
	return undefined;
}

function optionalString(value: unknown): string | undefined {
	return typeof value === 'string' && value.length > 0 ? value : undefined;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export const ApnsPushServiceTestHooks = {
	buildApnsPayload,
	buildApnsHeaders,
	isPermanentApnsFailure,
	apnsSigningKey,
};

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {Logger} from '@fluxer/logger/src/Logger';
import {Config} from '../../Config';
import {lookupGeoip} from '../../utils/IpUtils';

const logger = new Logger('UploadRelay');
const RELAY_PATH_PREFIX = '/v1/relay';

interface RelayTokenPayload {
	b: string;
	k: string;
	m: 'put';
	u?: string;
	p?: number;
	ct?: string;
	mb: number;
	e: number;
}

interface UploadRelayConfig {
	endpoint: string;
	relaySecretBase64: string;
	tokenTtlSecs: number;
	maxBodyBytes: number;
	keepDirectCountries: Array<string>;
}

type UploadRelayDecision = UploadRelayConfig | null;

function base64UrlEncode(buf: Buffer): string {
	return buf.toString('base64url');
}

function decodeRelaySecret(relaySecretBase64: string): Buffer {
	const decoded = Buffer.from(relaySecretBase64, 'base64');
	if (decoded.length < 32) {
		throw new Error('relay secret must be >= 32 bytes after base64 decode');
	}
	return decoded;
}

function signRelayToken(payload: RelayTokenPayload, relaySecretBase64: string): string {
	const secret = decodeRelaySecret(relaySecretBase64);
	const json = Buffer.from(JSON.stringify(payload), 'utf-8');
	const payloadB64 = base64UrlEncode(json);
	const sig = createHmac('sha256', secret).update(payloadB64).digest();
	return `${payloadB64}.${base64UrlEncode(sig)}`;
}

interface BuildRelayUrlInput {
	bucket: string;
	key: string;
	uploadId?: string;
	partNumber?: number;
	contentType?: string;
	maxBytes?: number;
	config: UploadRelayConfig;
}

function encodeKeyPath(key: string): string {
	return key
		.split('/')
		.map((segment) => encodeURIComponent(segment))
		.join('/');
}

function relayEndpointBase(endpoint: string): string {
	return endpoint.replace(/\/+$/u, '').replace(/\/v1\/relay$/u, '');
}

function buildRelayUrl({bucket, key, uploadId, partNumber, contentType, maxBytes, config}: BuildRelayUrlInput): string {
	const relayEndpoint = relayEndpointBase(config.endpoint);
	const expiresAt = Math.floor(Date.now() / 1000) + config.tokenTtlSecs;
	const payload: RelayTokenPayload = {
		b: bucket,
		k: key,
		m: 'put',
		mb: maxBytes ?? config.maxBodyBytes,
		e: expiresAt,
		...(uploadId ? {u: uploadId} : {}),
		...(partNumber ? {p: partNumber} : {}),
		...(contentType ? {ct: contentType} : {}),
	};
	const token = signRelayToken(payload, config.relaySecretBase64);
	const params = new URLSearchParams();
	params.set('t', token);
	if (uploadId) params.set('uploadId', uploadId);
	if (partNumber) params.set('partNumber', String(partNumber));
	return `${relayEndpoint}${RELAY_PATH_PREFIX}/${encodeKeyPath(key)}?${params.toString()}`;
}

export async function resolveUploadRelayDecision(clientIp: string | undefined | null): Promise<UploadRelayDecision> {
	const relayConfig = Config.mediaProxy.uploadRelay;
	if (!clientIp) {
		return relayConfig;
	}
	let countryCode: string | null = null;
	try {
		const geo = await lookupGeoip(clientIp);
		countryCode = geo.countryCode;
	} catch (error) {
		logger.warn({clientIp, error}, 'geoip lookup failed for upload relay decision; using upload relay');
		return relayConfig;
	}
	const keepDirectCountries = new Set(relayConfig.keepDirectCountries.map((code) => code.toUpperCase()));
	if (countryCode && keepDirectCountries.has(countryCode.toUpperCase())) {
		return null;
	}
	return relayConfig;
}

export function applyUploadRelayDecision(params: {
	presignedUrl: string;
	bucket: string;
	key: string;
	relayDecision: UploadRelayDecision;
	uploadId?: string;
	partNumber?: number;
	contentType?: string;
	maxBytes?: number;
}): string {
	const {presignedUrl, relayDecision, bucket, key, uploadId, partNumber, contentType, maxBytes} = params;
	if (!relayDecision) return presignedUrl;
	return buildRelayUrl({bucket, key, uploadId, partNumber, contentType, maxBytes, config: relayDecision});
}

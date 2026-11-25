// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyObject} from 'node:crypto';
import {importFederationPrivateKey, importFederationPublicKey} from '@app/api/federation/FederationCrypto';
import {isValidFederationOrigin} from '@app/api/federation/FederationEvent';
import {readEnvValue} from '@fluxer/config/src/config_loader/EnvironmentOverrides';

const DEFAULT_TIMEOUT_MS = 10_000;
const DEFAULT_MAX_ATTEMPTS = 10;
const DEFAULT_RETRY_BASE_MS = 1_000;
const DEFAULT_RETRY_MAX_MS = 3_600_000;
const KEY_ID_PATTERN = /^[A-Za-z0-9._-]{1,64}$/;
const GUILD_ID_PATTERN = /^\d{1,20}$/;

export interface FederationPeerConfig {
	instanceId: string;
	baseUrl: string;
	publicKey: KeyObject;
}

export interface FederationConfig {
	enabled: boolean;
	instanceId: string;
	keyId: string;
	privateKey: KeyObject | null;
	peers: ReadonlyArray<FederationPeerConfig>;
	guildIds: ReadonlySet<string>;
	testChannelId: string | null;
	timeoutMs: number;
	maxAttempts: number;
	retryBaseMs: number;
	retryMaxMs: number;
}

export interface FederationPeerConfigInput {
	instance_id: string;
	base_url: string;
	public_key: string;
}

function readTrimmed(env: NodeJS.ProcessEnv, name: string): string | undefined {
	return readEnvValue(env, name)?.trim();
}

function readBoolean(env: NodeJS.ProcessEnv, name: string): boolean | undefined {
	const raw = readTrimmed(env, name)?.toLowerCase();
	if (raw === undefined) return undefined;
	if (raw === 'true' || raw === '1') return true;
	if (raw === 'false' || raw === '0') return false;
	throw new Error(`${name} must be true or false`);
}

function readInteger(env: NodeJS.ProcessEnv, name: string, fallback: number): number {
	const raw = readTrimmed(env, name);
	if (raw === undefined) return fallback;
	if (!/^\d+$/.test(raw)) {
		throw new Error(`${name} must be a non-negative integer`);
	}
	const value = Number(raw);
	if (!Number.isSafeInteger(value)) {
		throw new Error(`${name} must be a safe integer`);
	}
	return value;
}

function normalizeBaseUrl(value: string, peerInstanceId: string): string {
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		throw new Error(`Invalid federation peer base_url for ${peerInstanceId}`);
	}
	if (
		(url.protocol !== 'https:' && url.protocol !== 'http:') ||
		url.username ||
		url.password ||
		url.search ||
		url.hash
	) {
		throw new Error(`Invalid federation peer base_url for ${peerInstanceId}`);
	}
	return url.toString().replace(/\/+$/, '');
}

function parsePeers(raw: string | undefined): Array<FederationPeerConfig> {
	if (raw === undefined) return [];
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		throw new Error('FLUXER_FEDERATION_PEERS must be valid JSON');
	}
	if (!Array.isArray(parsed)) {
		throw new Error('FLUXER_FEDERATION_PEERS must be a JSON array');
	}
	const seen = new Set<string>();
	const peers: Array<FederationPeerConfig> = [];
	for (const entry of parsed) {
		if (typeof entry !== 'object' || entry === null || Array.isArray(entry)) {
			throw new Error('FLUXER_FEDERATION_PEERS entries must be objects');
		}
		const input = entry as Partial<FederationPeerConfigInput>;
		if (typeof input.instance_id !== 'string' || !isValidFederationOrigin(input.instance_id)) {
			throw new Error('FLUXER_FEDERATION_PEERS contains an invalid instance_id');
		}
		if (typeof input.base_url !== 'string' || input.base_url.trim().length === 0) {
			throw new Error(`FLUXER_FEDERATION_PEERS is missing base_url for ${input.instance_id}`);
		}
		if (typeof input.public_key !== 'string' || input.public_key.trim().length === 0) {
			throw new Error(`FLUXER_FEDERATION_PEERS is missing public_key for ${input.instance_id}`);
		}
		if (seen.has(input.instance_id)) {
			throw new Error(`FLUXER_FEDERATION_PEERS contains duplicate instance_id ${input.instance_id}`);
		}
		let publicKey: KeyObject;
		try {
			publicKey = importFederationPublicKey(input.public_key.trim());
		} catch {
			throw new Error(`FLUXER_FEDERATION_PEERS contains an invalid public_key for ${input.instance_id}`);
		}
		seen.add(input.instance_id);
		peers.push({
			instanceId: input.instance_id,
			baseUrl: normalizeBaseUrl(input.base_url.trim(), input.instance_id),
			publicKey,
		});
	}
	return peers;
}

function parseGuildIds(raw: string | undefined): Set<string> {
	const guildIds = new Set<string>();
	if (raw === undefined) return guildIds;
	for (const entry of raw.split(',')) {
		const guildId = entry.trim();
		if (guildId.length === 0) continue;
		if (!GUILD_ID_PATTERN.test(guildId)) {
			throw new Error(`FLUXER_FEDERATION_GUILD_IDS contains an invalid guild id: ${guildId}`);
		}
		guildIds.add(guildId);
	}
	return guildIds;
}

function disabledConfig(env: NodeJS.ProcessEnv): FederationConfig {
	return {
		enabled: false,
		instanceId: readTrimmed(env, 'FLUXER_FEDERATION_INSTANCE_ID') ?? '',
		keyId: readTrimmed(env, 'FLUXER_FEDERATION_KEY_ID') ?? '',
		privateKey: null,
		peers: [],
		guildIds: new Set(),
		testChannelId: null,
		timeoutMs: DEFAULT_TIMEOUT_MS,
		maxAttempts: DEFAULT_MAX_ATTEMPTS,
		retryBaseMs: DEFAULT_RETRY_BASE_MS,
		retryMaxMs: DEFAULT_RETRY_MAX_MS,
	};
}

export function loadFederationConfig(env: NodeJS.ProcessEnv = process.env): FederationConfig {
	if (readBoolean(env, 'FLUXER_FEDERATION_ENABLED') !== true) {
		return disabledConfig(env);
	}
	const instanceId = readTrimmed(env, 'FLUXER_FEDERATION_INSTANCE_ID');
	if (instanceId === undefined || !isValidFederationOrigin(instanceId)) {
		throw new Error('FLUXER_FEDERATION_INSTANCE_ID must be a lowercase federation instance identifier');
	}
	const keyId = readTrimmed(env, 'FLUXER_FEDERATION_KEY_ID');
	if (keyId === undefined || !KEY_ID_PATTERN.test(keyId)) {
		throw new Error('FLUXER_FEDERATION_KEY_ID must be 1 to 64 characters of letters, digits, dot, underscore or dash');
	}
	const privateKeyValue = readEnvValue(env, 'FLUXER_FEDERATION_PRIVATE_KEY');
	if (privateKeyValue === undefined) {
		throw new Error('FLUXER_FEDERATION_PRIVATE_KEY is required when federation is enabled');
	}
	const privateKey = importFederationPrivateKey(privateKeyValue);
	const peers = parsePeers(readEnvValue(env, 'FLUXER_FEDERATION_PEERS'));
	if (peers.length === 0) {
		throw new Error('FLUXER_FEDERATION_PEERS must contain at least one peer when federation is enabled');
	}
	const guildIds = parseGuildIds(readEnvValue(env, 'FLUXER_FEDERATION_GUILD_IDS'));
	if (guildIds.size === 0) {
		throw new Error('FLUXER_FEDERATION_GUILD_IDS must contain at least one guild id when federation is enabled');
	}
	const testChannelIdRaw = readTrimmed(env, 'FLUXER_FEDERATION_TEST_CHANNEL_ID');
	if (testChannelIdRaw !== undefined && !GUILD_ID_PATTERN.test(testChannelIdRaw)) {
		throw new Error('FLUXER_FEDERATION_TEST_CHANNEL_ID must be a numeric snowflake');
	}
	const timeoutMs = readInteger(env, 'FLUXER_FEDERATION_TIMEOUT_MS', DEFAULT_TIMEOUT_MS);
	if (timeoutMs < 100 || timeoutMs > 60_000) {
		throw new Error('FLUXER_FEDERATION_TIMEOUT_MS must be between 100 and 60000');
	}
	const maxAttempts = readInteger(env, 'FLUXER_FEDERATION_MAX_ATTEMPTS', DEFAULT_MAX_ATTEMPTS);
	if (maxAttempts < 1 || maxAttempts > 100) {
		throw new Error('FLUXER_FEDERATION_MAX_ATTEMPTS must be between 1 and 100');
	}
	const retryBaseMs = readInteger(env, 'FLUXER_FEDERATION_RETRY_BASE_MS', DEFAULT_RETRY_BASE_MS);
	if (retryBaseMs < 100) {
		throw new Error('FLUXER_FEDERATION_RETRY_BASE_MS must be at least 100');
	}
	const retryMaxMs = readInteger(env, 'FLUXER_FEDERATION_RETRY_MAX_MS', DEFAULT_RETRY_MAX_MS);
	if (retryMaxMs < retryBaseMs) {
		throw new Error('FLUXER_FEDERATION_RETRY_MAX_MS must be at least FLUXER_FEDERATION_RETRY_BASE_MS');
	}
	return {
		enabled: true,
		instanceId,
		keyId,
		privateKey,
		peers,
		guildIds,
		testChannelId: testChannelIdRaw ?? null,
		timeoutMs,
		maxAttempts,
		retryBaseMs,
		retryMaxMs,
	};
}

let cachedConfig: FederationConfig | null = null;

export function getFederationConfig(): FederationConfig {
	cachedConfig ??= loadFederationConfig();
	return cachedConfig;
}

export function resetFederationConfigForTesting(): void {
	cachedConfig = null;
}

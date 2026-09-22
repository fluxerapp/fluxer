// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {IpInfoCache, IpInfoRequestAuditEvent, IpInfoRequestAuditLogger} from '@pkgs/geoip/src/IpInfoService';
import {type IPostgresClient, quoteIdentifier} from '@pkgs/postgres/src/Client';

interface PostgresIpInfoOptions {
	client?: IPostgresClient;
	getClient?: () => IPostgresClient;
	onError?: (error: unknown, operation: string) => void;
}

const VALUE_SEPARATOR = '\u001f';
export const IPINFO_CACHE_TTL_SECONDS = 14 * 24 * 60 * 60;
export const IPINFO_REQUEST_AUDIT_TTL_SECONDS = 90 * 24 * 60 * 60;

function getClient(options: PostgresIpInfoOptions): IPostgresClient | null {
	return options.client ?? options.getClient?.() ?? null;
}

function valueKey(value: unknown): string {
	return JSON.stringify(value);
}

function rowKey(values: ReadonlyArray<unknown>): string {
	return values.map(valueKey).join(VALUE_SEPARATOR);
}

function table(client: IPostgresClient): string {
	return quoteIdentifier(client.kvTable());
}

async function upsertKvRow(
	client: IPostgresClient,
	tableName: string,
	partitionKey: string,
	key: string,
	row: Record<string, unknown>,
	ttlSeconds: number,
): Promise<void> {
	const expiresAt = new Date(Date.now() + ttlSeconds * 1000);
	await client.query(
		`INSERT INTO ${table(client)} (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = EXCLUDED.row_data, expires_at = EXCLUDED.expires_at, updated_at = now()`,
		[tableName, partitionKey, key, JSON.stringify(row), expiresAt],
	);
}

export function createPostgresIpInfoCache(options: PostgresIpInfoOptions): IpInfoCache {
	return {
		async get<T>(key: string): Promise<T | null> {
			try {
				const client = getClient(options);
				if (!client) return null;
				const result = await client.query<{row_data: {payload?: string}}>(
					`SELECT row_data FROM ${table(client)} WHERE table_name = $1 AND row_key = $2 AND (expires_at IS NULL OR expires_at > now()) LIMIT 1`,
					['ipinfo_cache', rowKey([key])],
				);
				const payload = result.rows[0]?.row_data?.payload;
				return typeof payload === 'string' ? (JSON.parse(payload) as T) : null;
			} catch (error) {
				options.onError?.(error, 'ipinfo_cache_get');
				return null;
			}
		},
		async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
			let payload: string;
			try {
				payload = JSON.stringify(value);
			} catch (error) {
				options.onError?.(error, 'ipinfo_cache_serialize');
				return;
			}
			try {
				const client = getClient(options);
				if (!client) return;
				await upsertKvRow(
					client,
					'ipinfo_cache',
					rowKey([key]),
					rowKey([key]),
					{cache_key: key, payload},
					ttlSeconds != null && Number.isFinite(ttlSeconds) && ttlSeconds > 0 ? ttlSeconds : IPINFO_CACHE_TTL_SECONDS,
				);
			} catch (error) {
				options.onError?.(error, 'ipinfo_cache_set');
			}
		},
	};
}

export function createPostgresIpInfoRequestAuditLogger(options: PostgresIpInfoOptions): IpInfoRequestAuditLogger {
	return {
		async record(event: IpInfoRequestAuditEvent): Promise<void> {
			try {
				const client = getClient(options);
				if (!client) return;
				const bucketDate = formatUtcDate(event.requestedAt);
				const bucketHour = event.requestedAt.getUTCHours();
				const eventId = randomUUID();
				await upsertKvRow(
					client,
					'ipinfo_requests_by_hour',
					rowKey([bucketDate, bucketHour]),
					rowKey([bucketDate, bucketHour, event.requestedAt.toISOString(), eventId]),
					{
						bucket_date: bucketDate,
						bucket_hour: bucketHour,
						requested_at: event.requestedAt.toISOString(),
						event_id: eventId,
						source: event.source,
						reason: event.reason,
						ip: event.ip,
						cache_key: event.cacheKey,
						request_url: event.requestUrl,
						http_status: event.httpStatus,
						outcome: event.outcome,
						available: event.available,
						risk_note: event.riskNote,
						latency_ms: event.latencyMs,
						response_ip: event.responseIp,
						country_code: event.countryCode,
						asn: event.asnNumber,
						is_anonymous: event.isAnonymous,
						is_tor: event.isTor,
						is_vpn: event.isVpn,
						is_proxy: event.isProxy,
						is_residential_proxy: event.isResidentialProxy,
						metadata_json: serializeMetadata(event.metadata),
					},
					IPINFO_REQUEST_AUDIT_TTL_SECONDS,
				);
			} catch (error) {
				options.onError?.(error, 'ipinfo_request_audit_record');
			}
		},
	};
}

function formatUtcDate(value: Date): string {
	return value.toISOString().slice(0, 10);
}

function serializeMetadata(metadata: IpInfoRequestAuditEvent['metadata']): string | null {
	if (!metadata || Object.keys(metadata).length === 0) return null;
	try {
		return JSON.stringify(metadata);
	} catch {
		return null;
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {ICassandraClient} from '@pkgs/cassandra/src/Client';
import type {IpInfoRequestAuditEvent, IpInfoRequestAuditLogger} from '@pkgs/geoip/src/IpInfoService';

const TABLE = 'ipinfo_requests_by_hour';
const INSERT_CQL = `INSERT INTO ${TABLE} (
	bucket_date,
	bucket_hour,
	requested_at,
	event_id,
	source,
	reason,
	ip,
	cache_key,
	request_url,
	http_status,
	outcome,
	available,
	risk_note,
	latency_ms,
	response_ip,
	country_code,
	asn,
	is_anonymous,
	is_tor,
	is_vpn,
	is_proxy,
	is_residential_proxy,
	metadata_json
) VALUES (
	:bucket_date,
	:bucket_hour,
	:requested_at,
	:event_id,
	:source,
	:reason,
	:ip,
	:cache_key,
	:request_url,
	:http_status,
	:outcome,
	:available,
	:risk_note,
	:latency_ms,
	:response_ip,
	:country_code,
	:asn,
	:is_anonymous,
	:is_tor,
	:is_vpn,
	:is_proxy,
	:is_residential_proxy,
	:metadata_json
);`;

interface CassandraIpInfoRequestAuditOptions {
	client?: ICassandraClient;
	getClient?: () => ICassandraClient;
}

export function createCassandraIpInfoRequestAuditLogger(
	options: CassandraIpInfoRequestAuditOptions,
): IpInfoRequestAuditLogger {
	return {
		async record(event: IpInfoRequestAuditEvent): Promise<void> {
			try {
				const client = options.client ?? options.getClient?.();
				if (!client) {
					return;
				}
				await client.execute({
					cql: INSERT_CQL,
					params: {
						bucket_date: formatUtcDate(event.requestedAt),
						bucket_hour: event.requestedAt.getUTCHours(),
						requested_at: event.requestedAt,
						event_id: randomUUID(),
						source: event.source,
						reason: event.reason,
						ip: event.ip,
						cache_key: event.cacheKey,
						request_url: event.requestUrl,
						http_status: event.httpStatus,
						outcome: event.outcome,
						available: event.available,
						risk_note: event.note,
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
				});
			} catch {}
		},
	};
}

function formatUtcDate(value: Date): string {
	return value.toISOString().slice(0, 10);
}

function serializeMetadata(metadata: IpInfoRequestAuditEvent['metadata']): string | null {
	if (!metadata || Object.keys(metadata).length === 0) {
		return null;
	}
	try {
		return JSON.stringify(metadata);
	} catch {
		return null;
	}
}

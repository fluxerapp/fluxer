// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {SuspiciousIpRow} from '@app/api/database/types/RiskTypes';
import type {SuspiciousIpResult} from '@app/api/risk/RiskTypes';
import {SuspiciousIps} from '@app/api/Tables';
import {getSameIpDecisionKey, normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';

const SUSPICIOUS_IP_TTL_SECONDS = 180 * 24 * 60 * 60;
const SELECT_SUSPICIOUS_IP_CQL = SuspiciousIps.selectCql({
	where: SuspiciousIps.where.eq('ip'),
	limit: 1,
});

export interface SuspiciousIpMark {
	ip: string;
	source: string;
	reason: string;
	sourceUserId: UserID | null;
	deletionReasonCode: number | null;
	providerName: string | null;
	asn: number | null;
	asnName: string | null;
	asnType: string | null;
	riskNote: string | null;
	createdAt?: Date;
	expiresAt?: Date;
}

export interface ISuspiciousIpRepository {
	markSuspiciousIp(mark: SuspiciousIpMark): Promise<void>;
	findActiveByIp(ip: string, now?: Date): Promise<SuspiciousIpResult | null>;
}

function suspiciousIpKey(ip: string): string {
	return getSameIpDecisionKey(ip) ?? normalizeIpString(ip);
}

function defaultExpiresAt(createdAt: Date): Date {
	return new Date(createdAt.getTime() + SUSPICIOUS_IP_TTL_SECONDS * 1000);
}

export class CassandraSuspiciousIpRepository implements ISuspiciousIpRepository {
	async markSuspiciousIp(mark: SuspiciousIpMark): Promise<void> {
		const createdAt = mark.createdAt ?? new Date();
		const expiresAt = mark.expiresAt ?? defaultExpiresAt(createdAt);
		await upsertOne(
			SuspiciousIps.insertWithTtl(
				{
					ip: suspiciousIpKey(mark.ip),
					created_at: createdAt,
					updated_at: createdAt,
					expires_at: expiresAt,
					source: mark.source,
					reason: mark.reason,
					source_user_id: mark.sourceUserId,
					deletion_reason_code: mark.deletionReasonCode,
					provider_name: mark.providerName,
					asn: mark.asn,
					asn_name: mark.asnName,
					asn_type: mark.asnType,
					risk_note: mark.riskNote,
				},
				SUSPICIOUS_IP_TTL_SECONDS,
			),
		);
	}

	async findActiveByIp(ip: string, now: Date = new Date()): Promise<SuspiciousIpResult | null> {
		const row = await fetchOne<SuspiciousIpRow>(SELECT_SUSPICIOUS_IP_CQL, {ip: suspiciousIpKey(ip)});
		if (!row) return null;
		if (row.expires_at && row.expires_at <= now) return null;
		return {
			ip: row.ip,
			source: row.source,
			reason: row.reason,
			sourceUserId: row.source_user_id ? row.source_user_id.toString() : null,
			deletionReasonCode: row.deletion_reason_code ?? null,
			createdAt: row.created_at.toISOString(),
			expiresAt: row.expires_at?.toISOString() ?? null,
			riskNote: row.risk_note ?? 'suspicious IP marker present',
		};
	}
}

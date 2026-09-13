// SPDX-License-Identifier: AGPL-3.0-or-later

import {executeQuery, fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {PhoneLookupCacheRow, PhoneVerificationAttemptRow} from '@app/api/database/types/RiskTypes';
import {PhoneLookupCache, PhoneVerificationAttempts} from '@app/api/Tables';
import {createLogger} from '@fluxer/logger/src/Logger';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {PhoneLookupResult} from '@pkgs/sms/src/PhoneLookupTypes';

const CACHE_TTL_SECONDS = 7 * 24 * 60 * 60;
const ATTEMPT_TTL_SECONDS = 90 * 24 * 60 * 60;

export type PhoneAttemptRejectReason =
	| 'invalid_format'
	| 'banned_prefix'
	| 'lookup_unavailable'
	| 'invalid_number'
	| 'line_type_not_mobile'
	| 'line_type_hard_rejected'
	| 'sms_pumping_risk_high'
	| 'behavioural_risk_blocked';
export type PhoneAttemptInboundReason =
	| 'voip'
	| 'canadian'
	| 'unknown_line_type'
	| 'expensive_destination'
	| 'account_forced'
	| 'behavioural_risk';
type PhoneAttemptVerdict = 'accept' | 'reject' | 'require_inbound';

export interface RecordAttemptParams {
	phone: string;
	lookup: PhoneLookupResult | null;
	verdict: PhoneAttemptVerdict;
	rejectReason: PhoneAttemptRejectReason | null;
	inboundReason: PhoneAttemptInboundReason | null;
	lookupCacheHit: boolean;
}

export interface IPhoneLookupRepository {
	getCachedLookup(phone: string): Promise<PhoneLookupResult | null>;
	setCachedLookup(phone: string, lookup: PhoneLookupResult): Promise<void>;
	recordAttempt(params: RecordAttemptParams): Promise<void>;
}

export class CassandraPhoneLookupRepository implements IPhoneLookupRepository {
	private readonly logger: LoggerInterface;

	constructor(logger?: LoggerInterface) {
		this.logger = logger ?? createLogger('./PhoneLookupRepository');
	}

	async getCachedLookup(phone: string): Promise<PhoneLookupResult | null> {
		try {
			const row = await fetchOne<PhoneLookupCacheRow>(
				PhoneLookupCache.selectCql({where: PhoneLookupCache.where.eq('phone'), limit: 1}),
				{phone},
			);
			if (!row) return null;
			return rowToLookupResult(row);
		} catch (error) {
			this.logger.warn(
				{error: error instanceof Error ? error.message : String(error)},
				'[PhoneLookupRepository] cache read failed (treating as miss)',
			);
			return null;
		}
	}

	async setCachedLookup(phone: string, lookup: PhoneLookupResult): Promise<void> {
		try {
			const row: PhoneLookupCacheRow = {
				phone,
				looked_up_at: new Date(),
				valid: lookup.valid,
				line_type: lookup.lineType,
				country_code: lookup.countryCode,
				carrier_name: lookup.carrierName,
				sms_pumping_risk_score: lookup.smsPumpingRiskScore,
			};
			await executeQuery(PhoneLookupCache.insertWithTtl(row, CACHE_TTL_SECONDS));
		} catch (error) {
			this.logger.warn(
				{error: error instanceof Error ? error.message : String(error)},
				'[PhoneLookupRepository] cache write failed',
			);
		}
	}

	async recordAttempt(params: RecordAttemptParams): Promise<void> {
		try {
			const row: PhoneVerificationAttemptRow = {
				attempt_id: crypto.randomUUID(),
				created_at: new Date(),
				phone: params.phone,
				country_code: params.lookup?.countryCode ?? null,
				line_type: params.lookup?.lineType ?? null,
				carrier_name: params.lookup?.carrierName ?? null,
				sms_pumping_risk_score: params.lookup?.smsPumpingRiskScore ?? null,
				verdict: params.verdict,
				reject_reason: params.rejectReason,
				inbound_reason: params.inboundReason,
				lookup_cache_hit: params.lookupCacheHit,
			};
			await executeQuery(PhoneVerificationAttempts.insertWithTtl(row, ATTEMPT_TTL_SECONDS));
		} catch (error) {
			this.logger.warn(
				{error: error instanceof Error ? error.message : String(error)},
				'[PhoneLookupRepository] attempts log write failed',
			);
		}
	}
}

function rowToLookupResult(row: PhoneLookupCacheRow): PhoneLookupResult {
	return {
		valid: row.valid,
		lineType: row.line_type as PhoneLookupResult['lineType'],
		countryCode: row.country_code,
		carrierName: row.carrier_name,
		smsPumpingRiskScore: row.sms_pumping_risk_score,
	};
}

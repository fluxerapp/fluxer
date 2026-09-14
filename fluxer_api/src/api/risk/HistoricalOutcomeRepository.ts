// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {
	LatestRiskContextByUserRow,
	RiskOutcomeByAsnRow,
	RiskOutcomeByEmailDomainRow,
	RiskOutcomeByIpRow,
	RiskOutcomeBySubnetRow,
} from '@app/api/database/types/RiskTypes';
import type {
	HistoricalOutcomeCode,
	HistoricalOutcomeRecord,
	LatestRiskContextRecord,
} from '@app/api/risk/RiskHistoryTypes';
import {
	LatestRiskContextByUser,
	RiskOutcomesByAsn,
	RiskOutcomesByEmailDomain,
	RiskOutcomesByIp,
	RiskOutcomesBySubnet,
} from '@app/api/Tables';

export interface IRiskHistoryRepository {
	upsertLatestContext(context: LatestRiskContextRecord): Promise<void>;
	recordOutcomeForUser(args: {
		userId: string;
		occurredAt: Date;
		source: string;
		outcomeCodes: ReadonlyArray<HistoricalOutcomeCode>;
	}): Promise<void>;
	listByIp(ip: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<HistoricalOutcomeRecord>>;
	listBySubnet(subnet: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<HistoricalOutcomeRecord>>;
	listByEmailDomain(
		emailDomain: string,
		sinceTime: Date,
		limit: number,
	): Promise<ReadonlyArray<HistoricalOutcomeRecord>>;
	listByAsn(asn: number, sinceTime: Date, limit: number): Promise<ReadonlyArray<HistoricalOutcomeRecord>>;
}

const SELECT_LATEST_CONTEXT_BY_USER_CQL = LatestRiskContextByUser.selectCql({
	where: [LatestRiskContextByUser.where.eq('user_id')],
	limit: 1,
});
const SELECT_OUTCOMES_BY_IP_CQL = RiskOutcomesByIp.selectCql({
	where: [RiskOutcomesByIp.where.eq('ip'), RiskOutcomesByIp.where.gte('created_at')],
	limit: 200,
});
const SELECT_OUTCOMES_BY_SUBNET_CQL = RiskOutcomesBySubnet.selectCql({
	where: [RiskOutcomesBySubnet.where.eq('subnet'), RiskOutcomesBySubnet.where.gte('created_at')],
	limit: 200,
});
const SELECT_OUTCOMES_BY_EMAIL_DOMAIN_CQL = RiskOutcomesByEmailDomain.selectCql({
	where: [RiskOutcomesByEmailDomain.where.eq('email_domain'), RiskOutcomesByEmailDomain.where.gte('created_at')],
	limit: 200,
});
const SELECT_OUTCOMES_BY_ASN_CQL = RiskOutcomesByAsn.selectCql({
	where: [RiskOutcomesByAsn.where.eq('asn'), RiskOutcomesByAsn.where.gte('created_at')],
	limit: 200,
});

export class CassandraHistoricalOutcomeRepository implements IRiskHistoryRepository {
	async upsertLatestContext(context: LatestRiskContextRecord): Promise<void> {
		await upsertOne(
			LatestRiskContextByUser.insert({
				user_id: createUserID(BigInt(context.userId)),
				updated_at: context.updatedAt,
				ip: context.ip,
				subnet: context.subnet,
				email_domain: context.emailDomain,
				asn: context.asn,
			}),
		);
	}

	async recordOutcomeForUser(args: {
		userId: string;
		occurredAt: Date;
		source: string;
		outcomeCodes: ReadonlyArray<HistoricalOutcomeCode>;
	}): Promise<void> {
		const latestContext = await this.getLatestContextByUserId(args.userId);
		if (!latestContext) {
			return;
		}
		const normalizedOutcomeCodes = Array.from(
			new Set(args.outcomeCodes.filter((outcomeCode) => outcomeCode.length > 0)),
		);
		if (normalizedOutcomeCodes.length === 0) {
			return;
		}
		const userId = createUserID(BigInt(args.userId));
		const batch = new BatchBuilder();
		for (const outcomeCode of normalizedOutcomeCodes) {
			batch.addPrepared(
				RiskOutcomesByIp.insert({
					ip: latestContext.ip,
					created_at: args.occurredAt,
					user_id: userId,
					outcome_code: outcomeCode,
					source: args.source,
				}),
			);
			if (latestContext.subnet) {
				batch.addPrepared(
					RiskOutcomesBySubnet.insert({
						subnet: latestContext.subnet,
						created_at: args.occurredAt,
						user_id: userId,
						outcome_code: outcomeCode,
						source: args.source,
					}),
				);
			}
			if (latestContext.emailDomain) {
				batch.addPrepared(
					RiskOutcomesByEmailDomain.insert({
						email_domain: latestContext.emailDomain,
						created_at: args.occurredAt,
						user_id: userId,
						outcome_code: outcomeCode,
						source: args.source,
					}),
				);
			}
			if (latestContext.asn !== null) {
				batch.addPrepared(
					RiskOutcomesByAsn.insert({
						asn: latestContext.asn,
						created_at: args.occurredAt,
						user_id: userId,
						outcome_code: outcomeCode,
						source: args.source,
					}),
				);
			}
		}
		await batch.execute(false);
	}

	async listByIp(ip: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<HistoricalOutcomeRecord>> {
		const rows = await fetchMany<RiskOutcomeByIpRow>(SELECT_OUTCOMES_BY_IP_CQL, {ip, created_at: sinceTime});
		return rows.slice(0, limit).map((row) => this.mapOutcomeRow(row));
	}

	async listBySubnet(subnet: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<HistoricalOutcomeRecord>> {
		const rows = await fetchMany<RiskOutcomeBySubnetRow>(SELECT_OUTCOMES_BY_SUBNET_CQL, {
			subnet,
			created_at: sinceTime,
		});
		return rows.slice(0, limit).map((row) => this.mapOutcomeRow(row));
	}

	async listByEmailDomain(
		emailDomain: string,
		sinceTime: Date,
		limit: number,
	): Promise<ReadonlyArray<HistoricalOutcomeRecord>> {
		if (!emailDomain) {
			return [];
		}
		const rows = await fetchMany<RiskOutcomeByEmailDomainRow>(SELECT_OUTCOMES_BY_EMAIL_DOMAIN_CQL, {
			email_domain: emailDomain,
			created_at: sinceTime,
		});
		return rows.slice(0, limit).map((row) => this.mapOutcomeRow(row));
	}

	async listByAsn(asn: number, sinceTime: Date, limit: number): Promise<ReadonlyArray<HistoricalOutcomeRecord>> {
		const rows = await fetchMany<RiskOutcomeByAsnRow>(SELECT_OUTCOMES_BY_ASN_CQL, {
			asn,
			created_at: sinceTime,
		});
		return rows.slice(0, limit).map((row) => this.mapOutcomeRow(row));
	}

	private async getLatestContextByUserId(userId: string): Promise<LatestRiskContextRecord | null> {
		const row = await fetchOne<LatestRiskContextByUserRow>(SELECT_LATEST_CONTEXT_BY_USER_CQL, {
			user_id: createUserID(BigInt(userId)),
		});
		if (!row) {
			return null;
		}
		return {
			userId: row.user_id.toString(),
			ip: row.ip,
			subnet: row.subnet,
			emailDomain: row.email_domain,
			asn: row.asn,
			updatedAt: row.updated_at,
		};
	}

	private mapOutcomeRow(row: {
		user_id: UserID;
		created_at: Date;
		outcome_code: string;
		source: string;
	}): HistoricalOutcomeRecord {
		return {
			userId: row.user_id.toString(),
			createdAt: row.created_at,
			outcomeCode: row.outcome_code as HistoricalOutcomeCode,
			source: row.source,
		};
	}
}

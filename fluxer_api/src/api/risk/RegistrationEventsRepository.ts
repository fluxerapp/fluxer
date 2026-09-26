// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {
	RegistrationEventByEmailDomainRow,
	RegistrationEventByIpRow,
	RegistrationEventByPlusAddressBaseRow,
	RegistrationEventBySubnetRow,
} from '@app/api/database/types/RiskTypes';
import type {IRegistrationEventsRepository, RegistrationEventRecord} from '@app/api/risk/adapters/VelocityAdapter';
import {derivePlusAddressBase} from '@app/api/risk/PlusAddressUtils';
import {
	RegistrationEventsByEmailDomain,
	RegistrationEventsByIp,
	RegistrationEventsByPlusAddressBase,
	RegistrationEventsBySubnet,
} from '@app/api/Tables';
import {getSubnet} from '@fluxer/ip_utils/src/IpAddress';

const SELECT_BY_IP_CQL = RegistrationEventsByIp.selectCql({
	where: [RegistrationEventsByIp.where.eq('ip'), RegistrationEventsByIp.where.gte('created_at')],
	limit: 100,
});
const SELECT_BY_SUBNET_CQL = RegistrationEventsBySubnet.selectCql({
	where: [RegistrationEventsBySubnet.where.eq('subnet'), RegistrationEventsBySubnet.where.gte('created_at')],
	limit: 100,
});
const SELECT_BY_EMAIL_DOMAIN_CQL = RegistrationEventsByEmailDomain.selectCql({
	where: [
		RegistrationEventsByEmailDomain.where.eq('email_domain'),
		RegistrationEventsByEmailDomain.where.gte('created_at'),
	],
	limit: 100,
});
const SELECT_BY_PLUS_ADDRESS_BASE_CQL = RegistrationEventsByPlusAddressBase.selectCql({
	where: [
		RegistrationEventsByPlusAddressBase.where.eq('plus_address_base'),
		RegistrationEventsByPlusAddressBase.where.gte('created_at'),
	],
	limit: 100,
});

export class CassandraRegistrationEventsRepository implements IRegistrationEventsRepository {
	async recordEvent(event: RegistrationEventRecord): Promise<void> {
		const subnet = getSubnet(event.ip);
		const userId = createUserID(BigInt(event.userId));
		const email = event.email ?? null;
		const emailDomain = event.emailDomain ?? null;
		const plusAddressBase = derivePlusAddressBase(email);
		const locale = event.locale ?? null;
		const batch = new BatchBuilder();
		batch.addPrepared(
			RegistrationEventsByIp.insert({
				ip: event.ip,
				created_at: event.createdAt,
				user_id: userId,
				email,
				email_domain: emailDomain,
				locale,
			}),
		);
		if (emailDomain) {
			batch.addPrepared(
				RegistrationEventsByEmailDomain.insert({
					email_domain: emailDomain,
					created_at: event.createdAt,
					user_id: userId,
					ip: event.ip,
					email,
					locale,
				}),
			);
		}
		if (plusAddressBase) {
			batch.addPrepared(
				RegistrationEventsByPlusAddressBase.insert({
					plus_address_base: plusAddressBase,
					created_at: event.createdAt,
					user_id: userId,
					ip: event.ip,
					email,
					locale,
				}),
			);
		}
		if (subnet) {
			batch.addPrepared(
				RegistrationEventsBySubnet.insert({
					subnet,
					created_at: event.createdAt,
					user_id: userId,
					ip: event.ip,
					email,
					email_domain: emailDomain,
					locale,
				}),
			);
		}
		await batch.execute(false);
	}

	async listByIp(ip: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<RegistrationEventRecord>> {
		const rows = await fetchMany<RegistrationEventByIpRow>(SELECT_BY_IP_CQL, {ip, created_at: sinceTime});
		return rows.slice(0, limit).map(this.rowToRecord);
	}

	async listBySubnet(subnet: string, sinceTime: Date, limit: number): Promise<ReadonlyArray<RegistrationEventRecord>> {
		const rows = await fetchMany<RegistrationEventBySubnetRow>(SELECT_BY_SUBNET_CQL, {
			subnet,
			created_at: sinceTime,
		});
		return rows.slice(0, limit).map((row) => ({
			userId: row.user_id.toString(),
			email: row.email,
			emailDomain: row.email_domain,
			ip: row.ip,
			locale: row.locale,
			createdAt: row.created_at,
		}));
	}

	async listByEmailDomain(
		emailDomain: string,
		sinceTime: Date,
		limit: number,
	): Promise<ReadonlyArray<RegistrationEventRecord>> {
		if (!emailDomain) return [];
		const rows = await fetchMany<RegistrationEventByEmailDomainRow>(SELECT_BY_EMAIL_DOMAIN_CQL, {
			email_domain: emailDomain,
			created_at: sinceTime,
		});
		return rows.slice(0, limit).map((row) => ({
			userId: row.user_id.toString(),
			email: row.email,
			emailDomain: row.email_domain,
			ip: row.ip,
			locale: row.locale,
			createdAt: row.created_at,
		}));
	}

	async listByPlusAddressBase(
		plusAddressBase: string,
		sinceTime: Date,
		limit: number,
	): Promise<ReadonlyArray<RegistrationEventRecord>> {
		if (!plusAddressBase) return [];
		const rows = await fetchMany<RegistrationEventByPlusAddressBaseRow>(SELECT_BY_PLUS_ADDRESS_BASE_CQL, {
			plus_address_base: plusAddressBase,
			created_at: sinceTime,
		});
		return rows.slice(0, limit).map((row) => ({
			userId: row.user_id.toString(),
			email: row.email,
			emailDomain: row.email?.split('@')[1]?.toLowerCase() ?? null,
			ip: row.ip,
			locale: row.locale,
			createdAt: row.created_at,
		}));
	}

	private rowToRecord(row: RegistrationEventByIpRow): RegistrationEventRecord {
		return {
			userId: row.user_id.toString(),
			email: row.email,
			emailDomain: row.email_domain,
			ip: row.ip,
			locale: row.locale,
			createdAt: row.created_at,
		};
	}
}

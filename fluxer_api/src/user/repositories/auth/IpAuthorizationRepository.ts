/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createIpAuthorizationToken, type UserID} from '~/BrandedTypes';
import {UserFlags} from '~/Constants';
import {Db, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {AuthorizedIpRow, IpAuthorizationTokenRow} from '~/database/CassandraTypes';
import {AuthorizedIps, IpAuthorizationTokens, Users} from '~/Tables';
import type {IUserAccountRepository} from '../IUserAccountRepository';

export {IpAuthorizationTokens};

const AUTHORIZE_IP_BY_TOKEN_CQL = IpAuthorizationTokens.selectCql({
	where: IpAuthorizationTokens.where.eq('token_'),
	limit: 1,
});

const CHECK_IP_AUTHORIZED_CQL = AuthorizedIps.selectCql({
	where: [AuthorizedIps.where.eq('user_id'), AuthorizedIps.where.eq('ip')],
	limit: 1,
});

const GET_AUTHORIZED_IPS_CQL = AuthorizedIps.selectCql({
	where: AuthorizedIps.where.eq('user_id'),
});

export class IpAuthorizationRepository {
	constructor(private userAccountRepository: IUserAccountRepository) {}

	async checkIpAuthorized(userId: UserID, ip: string): Promise<boolean> {
		const result = await fetchOne<AuthorizedIpRow>(CHECK_IP_AUTHORIZED_CQL, {
			user_id: userId,
			ip,
		});
		return !!result;
	}

	async createAuthorizedIp(userId: UserID, ip: string): Promise<void> {
		await upsertOne(AuthorizedIps.insert({user_id: userId, ip}));
	}

	async createIpAuthorizationToken(userId: UserID, token: string): Promise<void> {
		const user = await this.userAccountRepository.findUnique(userId);
		await upsertOne(
			IpAuthorizationTokens.insert({
				token_: createIpAuthorizationToken(token),
				user_id: userId,
				email: user!.email!,
			}),
		);
	}

	async authorizeIpByToken(token: string): Promise<{userId: UserID; email: string} | null> {
		const result = await fetchOne<IpAuthorizationTokenRow>(AUTHORIZE_IP_BY_TOKEN_CQL, {token_: token});
		if (!result) {
			return null;
		}

		await deleteOneOrMany(
			IpAuthorizationTokens.deleteByPk({
				token_: createIpAuthorizationToken(token),
				user_id: result.user_id,
			}),
		);

		const user = await this.userAccountRepository.findUnique(result.user_id);
		if (!user || user.flags & UserFlags.DELETED) {
			return null;
		}

		return {userId: result.user_id, email: result.email};
	}

	async updateUserActivity(userId: UserID, clientIp: string): Promise<void> {
		const now = new Date();
		await upsertOne(
			Users.patchByPk(
				{user_id: userId},
				{
					last_active_at: Db.set(now),
					last_active_ip: Db.set(clientIp),
				},
			),
		);
	}

	async getAuthorizedIps(userId: UserID): Promise<Array<{ip: string}>> {
		const ips = await fetchMany<AuthorizedIpRow>(GET_AUTHORIZED_IPS_CQL, {user_id: userId});
		return ips.map((row) => ({ip: row.ip}));
	}

	async deleteAllAuthorizedIps(userId: UserID): Promise<void> {
		const ips = await fetchMany<AuthorizedIpRow>(GET_AUTHORIZED_IPS_CQL, {user_id: userId});

		for (const row of ips) {
			await deleteOneOrMany(
				AuthorizedIps.deleteByPk({
					user_id: userId,
					ip: row.ip,
				}),
			);
		}
	}
}

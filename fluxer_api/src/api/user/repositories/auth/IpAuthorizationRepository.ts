// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {getSameIpDecisionKey, normalizeIpString, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {createIpAuthorizationToken, type UserID} from '../../../BrandedTypes';
import {deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../../../database/CassandraQueryExecution';
import type {
	AuthorizedIpRow,
	AuthorizedIpTrustKeyRow,
	IpAuthorizationTokenRow,
} from '../../../database/types/AuthTypes';
import {AuthorizedIps, AuthorizedIpTrustKeys, IpAuthorizationTokens} from '../../../Tables';
import type {IUserAccountRepository} from '../IUserAccountRepository';

const AUTHORIZE_IP_BY_TOKEN_CQL = IpAuthorizationTokens.selectCql({
	where: IpAuthorizationTokens.where.eq('token_'),
	limit: 1,
});
const CHECK_IP_AUTHORIZED_CQL = AuthorizedIps.selectCql({
	where: [AuthorizedIps.where.eq('user_id'), AuthorizedIps.where.eq('ip')],
	limit: 1,
});
const CHECK_IP_TRUST_KEY_CQL = AuthorizedIpTrustKeys.selectCql({
	where: [AuthorizedIpTrustKeys.where.eq('user_id'), AuthorizedIpTrustKeys.where.eq('trust_key')],
	limit: 1,
});
const GET_AUTHORIZED_IPS_CQL = AuthorizedIps.selectCql({
	where: AuthorizedIps.where.eq('user_id'),
});
const GET_AUTHORIZED_IP_TRUST_KEYS_CQL = AuthorizedIpTrustKeys.selectCql({
	where: AuthorizedIpTrustKeys.where.eq('user_id'),
});

function normalizeAuthorizedIp(ip: string): string {
	return parseIpAddress(ip)?.normalized ?? normalizeIpString(ip);
}

function getAuthorizedIpTrustKey(ip: string): string | null {
	return getSameIpDecisionKey(ip);
}

export class IpAuthorizationRepository {
	constructor(private userAccountRepository: IUserAccountRepository) {}

	async checkIpAuthorized(userId: UserID, ip: string): Promise<boolean> {
		const normalizedIp = normalizeAuthorizedIp(ip);
		const trustKey = getAuthorizedIpTrustKey(normalizedIp);
		if (trustKey) {
			const trustKeyResult = await fetchOne<AuthorizedIpTrustKeyRow>(CHECK_IP_TRUST_KEY_CQL, {
				user_id: userId,
				trust_key: trustKey,
			});
			if (trustKeyResult) {
				return true;
			}
		}
		const result = await fetchOne<AuthorizedIpRow>(CHECK_IP_AUTHORIZED_CQL, {
			user_id: userId,
			ip: normalizedIp,
		});
		if (result) {
			await this.createAuthorizedIpTrustKeyFromIp(userId, normalizedIp);
			return true;
		}
		if (!trustKey || trustKey === normalizedIp) {
			return false;
		}
		const authorizedIps = await fetchMany<AuthorizedIpRow>(GET_AUTHORIZED_IPS_CQL, {user_id: userId});
		const matchedLegacyRow = authorizedIps.find((row) => getAuthorizedIpTrustKey(row.ip) === trustKey);
		if (!matchedLegacyRow) {
			return false;
		}
		await this.upsertAuthorizedIpTrustKey(userId, trustKey);
		return true;
	}

	async createAuthorizedIp(userId: UserID, ip: string): Promise<void> {
		const normalizedIp = normalizeAuthorizedIp(ip);
		await upsertOne(AuthorizedIps.insert({user_id: userId, ip: normalizedIp}));
		await this.createAuthorizedIpTrustKeyFromIp(userId, normalizedIp);
	}

	async createIpAuthorizationToken(userId: UserID, token: string, email: string): Promise<void> {
		await upsertOne(
			IpAuthorizationTokens.insert({
				token_: createIpAuthorizationToken(token),
				user_id: userId,
				email,
			}),
		);
	}

	async authorizeIpByToken(token: string): Promise<{
		userId: UserID;
		email: string;
	} | null> {
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

	async getAuthorizedIps(userId: UserID): Promise<
		Array<{
			ip: string;
		}>
	> {
		const ips = await fetchMany<AuthorizedIpRow>(GET_AUTHORIZED_IPS_CQL, {user_id: userId});
		return ips.map((row) => ({ip: row.ip}));
	}

	async deleteAllAuthorizedIps(userId: UserID): Promise<void> {
		const [ips, trustKeys] = await Promise.all([
			fetchMany<AuthorizedIpRow>(GET_AUTHORIZED_IPS_CQL, {user_id: userId}),
			fetchMany<AuthorizedIpTrustKeyRow>(GET_AUTHORIZED_IP_TRUST_KEYS_CQL, {user_id: userId}),
		]);
		for (const row of ips) {
			await deleteOneOrMany(
				AuthorizedIps.deleteByPk({
					user_id: userId,
					ip: row.ip,
				}),
			);
		}
		for (const row of trustKeys) {
			await deleteOneOrMany(
				AuthorizedIpTrustKeys.deleteByPk({
					user_id: userId,
					trust_key: row.trust_key,
				}),
			);
		}
	}

	private async createAuthorizedIpTrustKeyFromIp(userId: UserID, ip: string): Promise<void> {
		const trustKey = getAuthorizedIpTrustKey(ip);
		if (!trustKey) {
			return;
		}
		await this.upsertAuthorizedIpTrustKey(userId, trustKey);
	}

	private async upsertAuthorizedIpTrustKey(userId: UserID, trustKey: string): Promise<void> {
		await upsertOne(AuthorizedIpTrustKeys.insert({user_id: userId, trust_key: trustKey}));
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchManyInChunks, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import type {AdminApiKeyRow} from '../../database/types/AdminAuthTypes';
import {AdminApiKey} from '../../models/AdminApiKey';
import {AdminApiKeys, AdminApiKeysByCreator} from '../../Tables';
import {hashPassword} from '../../utils/PasswordUtils';
import type {CreateAdminApiKeyData, IAdminApiKeyRepository, UpdateAdminApiKeyData} from './IAdminApiKeyRepository';

function computeTtlSeconds(expiresAt: Date): number {
	const diffSeconds = Math.floor((expiresAt.getTime() - Date.now()) / 1000);
	return Math.max(diffSeconds, 1);
}

export class AdminApiKeyRepository implements IAdminApiKeyRepository {
	async create(data: CreateAdminApiKeyData, createdBy: UserID, keyId: bigint, rawKey: string): Promise<AdminApiKey> {
		const keyHash = await hashPassword(rawKey);
		const createdAt = new Date();
		const row: AdminApiKeyRow = {
			key_id: keyId,
			key_hash: keyHash,
			name: data.name,
			created_by_user_id: createdBy,
			created_at: createdAt,
			last_used_at: null,
			expires_at: data.expiresAt,
			version: 1,
			acls: data.acls,
		};
		const batch = new BatchBuilder();
		if (data.expiresAt) {
			const ttlSeconds = computeTtlSeconds(data.expiresAt);
			batch.addPrepared(AdminApiKeys.insertWithTtl(row, ttlSeconds));
			batch.addPrepared(
				AdminApiKeysByCreator.insertWithTtl(
					{
						created_by_user_id: row.created_by_user_id,
						key_id: row.key_id,
						created_at: row.created_at,
						name: row.name,
						expires_at: row.expires_at,
						last_used_at: row.last_used_at,
						version: row.version,
						acls: row.acls,
					},
					ttlSeconds,
				),
			);
		} else {
			batch.addPrepared(AdminApiKeys.upsertAll(row));
			batch.addPrepared(
				AdminApiKeysByCreator.upsertAll({
					created_by_user_id: row.created_by_user_id,
					key_id: row.key_id,
					created_at: row.created_at,
					name: row.name,
					expires_at: row.expires_at,
					last_used_at: row.last_used_at,
					version: row.version,
					acls: row.acls,
				}),
			);
		}
		await batch.execute();
		return new AdminApiKey(row);
	}

	async findById(keyId: bigint): Promise<AdminApiKey | null> {
		const query = AdminApiKeys.select({
			where: AdminApiKeys.where.eq('key_id'),
			limit: 1,
		});
		const row = await fetchOne<AdminApiKeyRow>(query.bind({key_id: keyId}));
		if (!row) {
			return null;
		}
		return new AdminApiKey(row);
	}

	async update(apiKey: AdminApiKey, data: UpdateAdminApiKeyData): Promise<AdminApiKey> {
		const row = apiKey.toRow();
		const updatedRow: AdminApiKeyRow = {
			...row,
			name: data.name ?? row.name,
			acls: data.acls ?? row.acls,
		};
		const patch = {
			name: Db.set(updatedRow.name),
			acls: Db.set(updatedRow.acls ?? new Set<string>()),
		};
		const batch = new BatchBuilder();
		if (apiKey.expiresAt) {
			const ttlSeconds = computeTtlSeconds(apiKey.expiresAt);
			batch.addPrepared(AdminApiKeys.patchByPkWithTtl({key_id: apiKey.keyId}, patch, ttlSeconds));
			batch.addPrepared(
				AdminApiKeysByCreator.patchByPkWithTtl(
					{created_by_user_id: apiKey.createdById, key_id: apiKey.keyId},
					patch,
					ttlSeconds,
				),
			);
		} else {
			batch.addPrepared(AdminApiKeys.patchByPk({key_id: apiKey.keyId}, patch));
			batch.addPrepared(
				AdminApiKeysByCreator.patchByPk({created_by_user_id: apiKey.createdById, key_id: apiKey.keyId}, patch),
			);
		}
		await batch.execute();
		return new AdminApiKey(updatedRow);
	}

	async listByCreator(createdBy: UserID): Promise<Array<AdminApiKey>> {
		const query = AdminApiKeysByCreator.select({
			where: AdminApiKeysByCreator.where.eq('created_by_user_id'),
		});
		const indexRows = await fetchMany<{
			created_by_user_id: UserID;
			key_id: bigint;
		}>(query.bind({created_by_user_id: createdBy}));
		if (indexRows.length === 0) {
			return [];
		}
		const keyIds = indexRows.map((row) => row.key_id);
		const rows = await fetchManyInChunks<AdminApiKeyRow>(
			AdminApiKeys.selectCql({where: AdminApiKeys.where.in('key_id', 'key_ids')}),
			keyIds,
			(chunk) => ({key_ids: chunk}),
		);
		return rows.map((row) => new AdminApiKey(row));
	}

	async updateLastUsed(keyId: bigint, expiresAt: Date | null): Promise<void> {
		if (expiresAt) {
			const remainingTtl = Math.max(Math.floor((expiresAt.getTime() - Date.now()) / 1000), 1);
			await upsertOne(AdminApiKeys.patchByPkWithTtl({key_id: keyId}, {last_used_at: Db.set(new Date())}, remainingTtl));
		} else {
			await upsertOne(AdminApiKeys.patchByPk({key_id: keyId}, {last_used_at: Db.set(new Date())}));
		}
	}

	async revoke(keyId: bigint, createdBy: UserID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(AdminApiKeys.deleteByPk({key_id: keyId}));
		batch.addPrepared(AdminApiKeysByCreator.deleteByPk({created_by_user_id: createdBy, key_id: keyId}));
		await batch.execute();
	}
}

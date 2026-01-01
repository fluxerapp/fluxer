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

import type {Index, MeiliSearch} from 'meilisearch';
import type {UserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import {SEARCH_MAX_TOTAL_HITS} from '~/search/constants';
import {extractTimestamp} from '~/utils/SnowflakeUtils';

const USER_INDEX_NAME = 'users';

interface SearchableUser {
	id: string;
	username: string;
	discriminator: number;
	email: string | null;
	phone: string | null;
	isBot: boolean;
	isSystem: boolean;
	flags: string;
	premiumType: number | null;
	emailVerified: boolean;
	emailBounced: boolean;
	suspiciousActivityFlags: number;
	acls: Array<string>;
	createdAt: number;
	lastActiveAt: number | null;
	tempBannedUntil: number | null;
	pendingDeletionAt: number | null;
	stripeSubscriptionId: string | null;
	stripeCustomerId: string | null;
}

interface UserSearchFilters {
	isBot?: boolean;
	isSystem?: boolean;
	emailVerified?: boolean;
	emailBounced?: boolean;
	hasPremium?: boolean;
	isTempBanned?: boolean;
	isPendingDeletion?: boolean;
	hasAcl?: Array<string>;
	minSuspiciousActivityFlags?: number;
	sortBy?: 'createdAt' | 'lastActiveAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export class UserSearchService {
	private meilisearch: MeiliSearch;
	private index: Index<SearchableUser> | null = null;

	constructor(meilisearch: MeiliSearch) {
		this.meilisearch = meilisearch;
	}

	async initialize(): Promise<void> {
		try {
			this.index = this.meilisearch.index<SearchableUser>(USER_INDEX_NAME);

			await this.index.updateSettings({
				searchableAttributes: ['username', 'id', 'email', 'phone', 'stripeSubscriptionId', 'stripeCustomerId'],
				filterableAttributes: [
					'discriminator',
					'isBot',
					'isSystem',
					'emailVerified',
					'emailBounced',
					'premiumType',
					'suspiciousActivityFlags',
					'acls',
					'createdAt',
					'lastActiveAt',
					'tempBannedUntil',
					'pendingDeletionAt',
				],
				sortableAttributes: ['createdAt', 'lastActiveAt', 'discriminator'],
				rankingRules: ['words', 'typo', 'proximity', 'attribute', 'sort', 'exactness'],
				pagination: {
					maxTotalHits: SEARCH_MAX_TOTAL_HITS,
				},
			});

			Logger.debug('User search index initialized successfully');
		} catch (error) {
			Logger.error({error}, 'Failed to initialize user search index');
			throw error;
		}
	}

	async indexUser(user: User): Promise<void> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		const searchableUser = this.convertToSearchableUser(user);

		try {
			await this.index.addDocuments([searchableUser], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({userId: user.id, error}, 'Failed to index user');
			throw error;
		}
	}

	async indexUsers(users: Array<User>): Promise<void> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		if (users.length === 0) return;

		const searchableUsers = users.map((user) => this.convertToSearchableUser(user));

		try {
			await this.index.addDocuments(searchableUsers, {primaryKey: 'id'});
		} catch (error) {
			Logger.error({count: users.length, error}, 'Failed to index users');
			throw error;
		}
	}

	async updateUser(user: User): Promise<void> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		const searchableUser = this.convertToSearchableUser(user);

		try {
			await this.index.updateDocuments([searchableUser], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({userId: user.id, error}, 'Failed to update user in search index');
			throw error;
		}
	}

	async deleteUser(userId: UserID): Promise<void> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		try {
			await this.index.deleteDocument(userId.toString());
		} catch (error) {
			Logger.error({userId, error}, 'Failed to delete user from search index');
			throw error;
		}
	}

	async deleteUsers(userIds: Array<UserID>): Promise<void> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		if (userIds.length === 0) return;

		try {
			await this.index.deleteDocuments(userIds.map((id) => id.toString()));
		} catch (error) {
			Logger.error({count: userIds.length, error}, 'Failed to delete users from search index');
			throw error;
		}
	}

	async searchUsers(
		query: string,
		filters: UserSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<{hits: Array<SearchableUser>; total: number}> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		const filterStrings = this.buildFilterStrings(filters);
		const sortField = this.buildSortField(filters);

		try {
			const result = await this.index.search(query, {
				filter: filterStrings.length > 0 ? filterStrings : undefined,
				limit: options?.limit ?? 50,
				offset: options?.offset ?? 0,
				sort: sortField,
			});

			return {
				hits: result.hits,
				total: result.estimatedTotalHits ?? 0,
			};
		} catch (error) {
			Logger.error({query, filters, error}, 'Failed to search users');
			throw error;
		}
	}

	async deleteAllDocuments(): Promise<void> {
		if (!this.index) {
			throw new Error('User search index not initialized');
		}

		try {
			await this.index.deleteAllDocuments();
			Logger.debug('All user documents deleted from search index');
		} catch (error) {
			Logger.error({error}, 'Failed to delete all user documents');
			throw error;
		}
	}

	private buildFilterStrings(filters: UserSearchFilters): Array<string> {
		const filterStrings: Array<string> = [];

		if (filters.isBot !== undefined) {
			filterStrings.push(`isBot = ${filters.isBot}`);
		}

		if (filters.isSystem !== undefined) {
			filterStrings.push(`isSystem = ${filters.isSystem}`);
		}

		if (filters.emailVerified !== undefined) {
			filterStrings.push(`emailVerified = ${filters.emailVerified}`);
		}

		if (filters.emailBounced !== undefined) {
			filterStrings.push(`emailBounced = ${filters.emailBounced}`);
		}

		if (filters.hasPremium !== undefined) {
			if (filters.hasPremium) {
				filterStrings.push(`premiumType IS NOT NULL`);
			} else {
				filterStrings.push(`premiumType IS NULL`);
			}
		}

		if (filters.isTempBanned !== undefined) {
			if (filters.isTempBanned) {
				filterStrings.push(`tempBannedUntil IS NOT NULL`);
			} else {
				filterStrings.push(`tempBannedUntil IS NULL`);
			}
		}

		if (filters.isPendingDeletion !== undefined) {
			if (filters.isPendingDeletion) {
				filterStrings.push(`pendingDeletionAt IS NOT NULL`);
			} else {
				filterStrings.push(`pendingDeletionAt IS NULL`);
			}
		}

		if (filters.hasAcl && filters.hasAcl.length > 0) {
			const aclFilters = filters.hasAcl.map((acl) => `acls = "${acl}"`).join(' OR ');
			filterStrings.push(`(${aclFilters})`);
		}

		if (filters.minSuspiciousActivityFlags !== undefined) {
			filterStrings.push(`suspiciousActivityFlags >= ${filters.minSuspiciousActivityFlags}`);
		}

		return filterStrings;
	}

	private buildSortField(filters: UserSearchFilters): Array<string> {
		const sortBy = filters.sortBy ?? 'createdAt';
		const sortOrder = filters.sortOrder ?? 'desc';

		if (sortBy === 'relevance') {
			return [];
		}

		return [`${sortBy}:${sortOrder}`];
	}

	private convertToSearchableUser(user: User): SearchableUser {
		const createdAt = Math.floor(extractTimestamp(BigInt(user.id)) / 1000);
		const lastActiveAt = user.lastActiveAt ? Math.floor(user.lastActiveAt.getTime() / 1000) : null;
		const tempBannedUntil = user.tempBannedUntil ? Math.floor(user.tempBannedUntil.getTime() / 1000) : null;
		const pendingDeletionAt = user.pendingDeletionAt ? Math.floor(user.pendingDeletionAt.getTime() / 1000) : null;

		return {
			id: user.id.toString(),
			username: user.username,
			discriminator: user.discriminator,
			email: user.email,
			phone: user.phone,
			isBot: user.isBot,
			isSystem: user.isSystem,
			flags: user.flags.toString(),
			premiumType: user.premiumType,
			emailVerified: user.emailVerified,
			emailBounced: user.emailBounced,
			suspiciousActivityFlags: user.suspiciousActivityFlags,
			acls: Array.from(user.acls),
			createdAt,
			lastActiveAt,
			tempBannedUntil,
			pendingDeletionAt,
			stripeSubscriptionId: user.stripeSubscriptionId,
			stripeCustomerId: user.stripeCustomerId,
		};
	}
}

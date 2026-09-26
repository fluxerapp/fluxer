// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AdminAuditCoverageCase,
	AdminAuditCoverageContext,
} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {
	DiscoveryApplicationStatus,
	DiscoveryCategories,
	type DiscoveryCategory,
} from '@fluxer/constants/src/DiscoveryConstants';

async function seedDiscoveryGuild(
	{harness}: AdminAuditCoverageContext,
	status: string,
	categoryType: DiscoveryCategory = DiscoveryCategories.GAMING,
): Promise<string> {
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, 'Audit Discovery Guild');
	const now = new Date();
	await new GuildDiscoveryRepository().upsert({
		guild_id: createGuildID(BigInt(guild.id)),
		status,
		category_type: categoryType,
		description: 'Audit coverage discovery listing',
		primary_language: null,
		custom_tags: [],
		applied_at: now,
		reviewed_at: status === DiscoveryApplicationStatus.APPROVED ? now : null,
		reviewed_by: null,
		review_reason: null,
		removed_at: null,
		removed_by: null,
		removal_reason: null,
	});
	return guild.id;
}

export const DiscoveryAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/discovery/applications',
		async prepare(context) {
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.PENDING);
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED);
			return {
				request: {path: '/admin/discovery/applications'},
				expected: {
					action: 'list_discovery_applications',
					targetType: 'guild',
					targetId: '0',
					metadata: {result_count: '1'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/discovery/applications/:guild_id',
		name: 'approve',
		async prepare(context) {
			const guildId = await seedDiscoveryGuild(context, DiscoveryApplicationStatus.PENDING);
			return {
				request: {
					path: `/admin/discovery/applications/${guildId}`,
					body: {status: 'approved', reason: 'Meets every requirement'},
				},
				expected: {
					action: 'approve_discovery_application',
					targetType: 'guild',
					targetId: guildId,
					metadata: {status: 'approved'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/discovery/applications/:guild_id',
		name: 'reject',
		async prepare(context) {
			const guildId = await seedDiscoveryGuild(context, DiscoveryApplicationStatus.PENDING);
			return {
				request: {
					path: `/admin/discovery/applications/${guildId}`,
					body: {status: 'rejected', reason: 'Description is too vague'},
				},
				expected: {
					action: 'reject_discovery_application',
					targetType: 'guild',
					targetId: guildId,
					metadata: {status: 'rejected'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/discovery/categories',
		async prepare() {
			return {
				request: {path: '/admin/discovery/categories'},
				expected: {
					action: 'list_discovery_categories',
					targetType: 'discovery_category',
					targetId: '0',
					metadata: {result_count: '9'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/discovery/categories/:category_id/listings',
		async prepare(context) {
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED, DiscoveryCategories.MUSIC);
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED, DiscoveryCategories.MUSIC);
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED, DiscoveryCategories.GAMING);
			return {
				request: {path: `/admin/discovery/categories/${DiscoveryCategories.MUSIC}/listings?limit=1&offset=1`},
				expected: {
					action: 'list_discovery_category_listings',
					targetType: 'discovery_category',
					targetId: '0',
					metadata: {
						category_id: DiscoveryCategories.MUSIC.toString(),
						limit: '1',
						offset: '1',
						result_count: '1',
						total: '2',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/discovery/listings',
		async prepare(context) {
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED);
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED);
			await seedDiscoveryGuild(context, DiscoveryApplicationStatus.PENDING);
			return {
				request: {path: '/admin/discovery/listings'},
				expected: {
					action: 'list_discovery_listings',
					targetType: 'guild',
					targetId: '0',
					metadata: {result_count: '2'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/discovery/listings',
		async prepare(context) {
			const guildId = await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED);
			return {
				request: {
					path: '/admin/discovery/listings',
					body: {guild_ids: [guildId], category_type: DiscoveryCategories.EDUCATION},
				},
				expected: {
					action: 'update_discovery_categories',
					targetType: 'guild',
					targetId: '0',
					metadata: {
						category_type: DiscoveryCategories.EDUCATION.toString(),
						guild_count: '1',
						updated: '1',
						failed: '0',
					},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/discovery/listings/:guild_id',
		async prepare(context) {
			const guildId = await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED);
			return {
				request: {
					path: `/admin/discovery/listings/${guildId}`,
					body: {
						description: 'A corrected discovery description',
						category_type: DiscoveryCategories.MUSIC,
						primary_language: 'fr',
						custom_tags: ['makers'],
					},
				},
				expected: {
					action: 'update_discovery_listing',
					targetType: 'guild',
					targetId: guildId,
					metadata: {
						fields: 'description,category_type,primary_language,custom_tags',
						category_type: DiscoveryCategories.MUSIC.toString(),
						primary_language: 'fr',
						status: 'approved',
					},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/discovery/listings/:guild_id',
		async prepare(context) {
			const guildId = await seedDiscoveryGuild(context, DiscoveryApplicationStatus.APPROVED);
			return {
				request: {
					path: `/admin/discovery/listings/${guildId}`,
					body: {reason: 'No longer meets the guidelines'},
				},
				expected: {
					action: 'remove_discovery_listing',
					targetType: 'guild',
					targetId: guildId,
					metadata: {},
				},
			};
		},
	},
];

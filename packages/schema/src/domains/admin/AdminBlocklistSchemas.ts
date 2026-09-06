// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BanAvatarHashRequest,
	BanEmailRequest,
	BanFileShaRequest,
	BanIpRequest,
	BanPhraseRequest,
	BanProfileSubstringRequest,
	BanUrlDomainRequest,
	BanUrlRequest,
	CheckAvatarHashRequest,
	SuspiciousEmailDomainRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
import {createStringType, Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const AdminBlocklistListType = z
	.enum([
		'ip',
		'email',
		'email-domain-suspicious',
		'phrase',
		'url',
		'url-domain',
		'file-sha',
		'avatar-hash',
		'profile-substring',
	])
	.describe('The blocklist an entry belongs to');

export type AdminBlocklistListType = z.infer<typeof AdminBlocklistListType>;

const BlocklistScopeType = BanProfileSubstringRequest.shape.scope;

export const BlocklistTypeParam = z.object({
	list_type: AdminBlocklistListType.describe('The blocklist to operate on'),
});

export type BlocklistTypeParam = z.infer<typeof BlocklistTypeParam>;

export const BlocklistEntryParam = z.object({
	list_type: AdminBlocklistListType.describe('The blocklist the entry belongs to'),
	entry_value: createStringType(1, 2048).describe('The percent-encoded value of the blocklist entry'),
});

export type BlocklistEntryParam = z.infer<typeof BlocklistEntryParam>;

export const AdminBlocklistScopeQuery = z.object({
	scope: BlocklistScopeType.optional().describe(
		'Profile field the entry is scoped to. Required on the profile-substring blocklist and rejected on every other blocklist.',
	),
});

export type AdminBlocklistScopeQuery = z.infer<typeof AdminBlocklistScopeQuery>;

export const AdminBlocklistEntryListQuery = z.object({
	limit: createQueryIntegerType({minValue: 1, maxValue: 200, defaultValue: 50}).describe(
		'Maximum number of entries to return',
	),
	after: createStringType(1, 2048)
		.optional()
		.describe('Return entries ordered after this value, taken from the next_after cursor of the previous page'),
	scope: BlocklistScopeType.optional().describe(
		'Profile field to list. Required on the profile-substring blocklist and rejected on every other blocklist.',
	),
});

export type AdminBlocklistEntryListQuery = z.infer<typeof AdminBlocklistEntryListQuery>;

const AdminBlocklistTypeResponse = z.object({
	list_type: AdminBlocklistListType,
	description: z.string().describe('What the blocklist matches and how matching is performed'),
	value_field: z.string().describe('The request body field that carries the entry value when adding to this blocklist'),
	fields: z.array(z.string()).max(8).describe('Fields entries of this blocklist accept beyond the value itself'),
	scoped: z.boolean().describe('Whether entries are scoped to a profile field and a scope must be supplied'),
	supports_bulk_create: z.boolean().describe('Whether PUT on the entry collection is accepted'),
	supports_bulk_delete: z.boolean().describe('Whether DELETE on the entry collection is accepted'),
	supports_update: z.boolean().describe('Whether PATCH on a single entry is accepted'),
});

export const AdminBlocklistTypeListResponse = z.object({
	items: z.array(AdminBlocklistTypeResponse).max(50).describe('Every blocklist exposed by this instance'),
});

const AdminBlocklistEntryResponse = z.object({
	list_type: AdminBlocklistListType,
	value: createStringType(1, 2048).describe('The canonical stored value of the entry'),
	scope: z
		.string()
		.nullable()
		.describe('The profile field the entry is scoped to, or null when the blocklist is unscoped'),
	category: z.string().nullable().describe('The category slug stored alongside the entry, or null'),
	severity: Int32Type.nullable().describe('The stored severity, or null when the blocklist has no severity'),
	source_url: z.string().nullable().describe('The upstream source the entry was imported from, or null'),
	notes: z.string().nullable().describe('The internal notes stored alongside the entry, or null'),
	content_type: z.string().nullable().describe('The MIME type hint stored alongside the entry, or null'),
	match_subdomains: z
		.boolean()
		.nullable()
		.describe('Whether subdomains are covered by the entry, or null when the blocklist has no such flag'),
	reason: z.string().nullable().describe('The stored reason for the entry, or null'),
	expires_at: z.string().nullable().describe('ISO 8601 timestamp when the entry expires, or null'),
	created_at: z.string().nullable().describe('ISO 8601 timestamp when the entry was added, or null'),
	created_by_user_id: SnowflakeStringType.nullable().describe('The admin who added the entry, or null when unknown'),
});

export const AdminBlocklistEntryListResponse = z.object({
	items: z.array(AdminBlocklistEntryResponse).max(200).describe('The blocklist entries in this page, ordered by value'),
	has_more: z.boolean().describe('Whether another page can be fetched with the next_after cursor'),
	next_after: z
		.string()
		.nullable()
		.describe('Cursor to send as after on the next request, or null when this is the last page'),
});

export const AdminBlocklistEntryCreateRequest = z
	.union([
		BanIpRequest,
		BanEmailRequest,
		SuspiciousEmailDomainRequest,
		BanPhraseRequest,
		BanUrlRequest,
		BanUrlDomainRequest,
		BanFileShaRequest,
		BanAvatarHashRequest,
		BanProfileSubstringRequest,
	])
	.describe('The entry to add, in the shape the blocklist named by list_type accepts');

export const AdminBlocklistBulkDeleteRequest = z
	.union([CheckAvatarHashRequest, BanProfileSubstringRequest])
	.describe('The entries to remove, in the shape the blocklist named by list_type accepts');

export const AdminBlocklistUrlUpdateRequest = BanUrlRequest.omit({url: true});

export const AdminBlocklistUrlDomainUpdateRequest = BanUrlDomainRequest.omit({domain: true});

export const AdminBlocklistFileShaUpdateRequest = BanFileShaRequest.omit({sha256_hex: true});

export const AdminBlocklistAvatarHashUpdateRequest = BanAvatarHashRequest.omit({hashes: true});

export const AdminBlocklistProfileSubstringUpdateRequest = BanProfileSubstringRequest.omit({substrings: true});

export const AdminBlocklistEntryUpdateRequest = z
	.union([
		AdminBlocklistUrlUpdateRequest,
		AdminBlocklistUrlDomainUpdateRequest,
		AdminBlocklistFileShaUpdateRequest,
		AdminBlocklistAvatarHashUpdateRequest,
		AdminBlocklistProfileSubstringUpdateRequest,
	])
	.describe('The stored fields to write, in the shape the blocklist named by list_type accepts');

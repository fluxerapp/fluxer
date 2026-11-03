// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const AdminSsoIdentityResponse = z.object({
	provider_id: z
		.string()
		.describe('The provider the identity belongs to: the issuer URL, or the authorization URL when no issuer is set'),
	subject: z
		.string()
		.describe('The provider subject (`sub` claim) bound to the account, exactly as the provider sent it'),
	user_id: SnowflakeStringType.describe('The account the subject is bound to'),
	claimed_at: z.iso.datetime().describe('ISO 8601 timestamp when the subject was bound to the account'),
});

export type AdminSsoIdentityResponse = z.infer<typeof AdminSsoIdentityResponse>;

export const AdminUserSsoIdentityListResponse = z.object({
	sso_identities: z
		.array(AdminSsoIdentityResponse)
		.max(100)
		.describe('Every provider subject bound to the account, across every provider it has signed in with'),
});

export type AdminUserSsoIdentityListResponse = z.infer<typeof AdminUserSsoIdentityListResponse>;

export const AdminSsoIdentityLookupQuery = z.object({
	provider_id: z.string().min(1).max(2048).describe('The provider ID, as returned in `provider_id`'),
	subject: z.string().min(1).max(1024).describe('The provider subject (`sub` claim), matched exactly'),
});

export type AdminSsoIdentityLookupQuery = z.infer<typeof AdminSsoIdentityLookupQuery>;

export const AdminSsoIdentityLookupResponse = z.object({
	sso_identity: AdminSsoIdentityResponse.nullable().describe('The bound identity, or null when no account holds it'),
});

export type AdminSsoIdentityLookupResponse = z.infer<typeof AdminSsoIdentityLookupResponse>;

export const AdminSsoIdentityLinkRequest = z.object({
	subject: z
		.string()
		.min(1)
		.max(1024)
		.describe('The provider subject (`sub` claim) to bind under the provider in use, matched exactly'),
});

export type AdminSsoIdentityLinkRequest = z.infer<typeof AdminSsoIdentityLinkRequest>;

export const AdminSsoIdentityMigrateRequest = z.object({
	from_provider_id: z
		.string()
		.min(1)
		.max(2048)
		.describe('The provider ID the subjects are bound under now, such as the issuer URL used before a change'),
});

export type AdminSsoIdentityMigrateRequest = z.infer<typeof AdminSsoIdentityMigrateRequest>;

export const AdminSsoIdentityMigrateResponse = z.object({
	to_provider_id: z.string().describe('The provider ID in use, which the subjects were moved to'),
	moved_count: z.number().int().min(0).describe('Subjects now bound under the provider in use'),
	skipped_count: z
		.number()
		.int()
		.min(0)
		.describe(
			'Subjects left where they were, because another account already holds them under the provider in use or their account no longer exists',
		),
});

export type AdminSsoIdentityMigrateResponse = z.infer<typeof AdminSsoIdentityMigrateResponse>;

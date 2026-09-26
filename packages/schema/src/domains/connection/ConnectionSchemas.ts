// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ConnectionTypes,
	ConnectionVisibilityFlags,
	ConnectionVisibilityFlagsDescriptions,
} from '@fluxer/constants/src/ConnectionConstants';
import {
	createBitflagInt32Type,
	createNamedStringLiteralUnion,
	Int32Type,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {HostnameType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

const ConnectionTypeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[ConnectionTypes.BLUESKY, 'BLUESKY', 'Bluesky social account connection'],
			[ConnectionTypes.DOMAIN, 'DOMAIN', 'Custom domain ownership connection'],
		] as const,
		'The type of external connection',
	),
	'ConnectionType',
);
export const ConnectionResponse = z.object({
	id: z.string().describe('The unique identifier for this connection'),
	type: ConnectionTypeSchema.describe('The type of connection'),
	name: z.string().describe('The display name of the connection (handle or domain)'),
	verified: z.boolean().describe('Whether the connection has been verified'),
	visibility_flags: createBitflagInt32Type(
		ConnectionVisibilityFlags,
		ConnectionVisibilityFlagsDescriptions,
		'Bitfield for connection visibility settings',
		'ConnectionVisibilityFlags',
	).describe('Bitfield controlling who can see this connection'),
	sort_order: Int32Type.describe('The display order of this connection'),
});

export type ConnectionResponse = z.infer<typeof ConnectionResponse>;

export const ConnectionListResponse = z.array(ConnectionResponse);

export type ConnectionListResponse = z.infer<typeof ConnectionListResponse>;

export const ConnectionVerificationResponse = z.object({
	token: z.string().describe('The verification token to place in DNS or profile'),
	type: ConnectionTypeSchema.describe('The type of connection being verified'),
	id: z.string().describe('The connection identifier (handle or domain)'),
	instructions: z.string().describe('Human-readable instructions for completing verification'),
	initiation_token: z.string().describe('Signed token the client sends back at verify time'),
});

export type ConnectionVerificationResponse = z.infer<typeof ConnectionVerificationResponse>;

export const VerifyAndCreateConnectionRequest = z.object({
	initiation_token: z.string().describe('The signed initiation token returned from the create endpoint'),
	visibility_flags: Int32Type.optional().describe('Bitfield controlling who can see this connection'),
});

export type VerifyAndCreateConnectionRequest = z.infer<typeof VerifyAndCreateConnectionRequest>;

export const CreateConnectionRequest = z.object({
	type: ConnectionTypeSchema.describe('The type of connection to create'),
	identifier: HostnameType.describe('The connection identifier (handle or domain)'),
	visibility_flags: Int32Type.optional().describe('Bitfield controlling who can see this connection'),
});

export type CreateConnectionRequest = z.infer<typeof CreateConnectionRequest>;

export const UpdateConnectionRequest = z.object({
	visibility_flags: Int32Type.optional().describe('Bitfield controlling who can see this connection'),
	sort_order: Int32Type.optional().describe('The display order of this connection'),
});

export type UpdateConnectionRequest = z.infer<typeof UpdateConnectionRequest>;

export const ReorderConnectionsRequest = z.object({
	connection_ids: z
		.array(z.string())
		.min(1)
		.max(20)
		.describe('Ordered list of connection IDs defining the new display order'),
});

export type ReorderConnectionsRequest = z.infer<typeof ReorderConnectionsRequest>;

export const ConnectionTypeParam = z.object({
	type: ConnectionTypeSchema,
	connection_id: z.string().describe('The unique identifier of the connection'),
});

export type ConnectionTypeParam = z.infer<typeof ConnectionTypeParam>;

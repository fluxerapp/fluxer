// SPDX-License-Identifier: AGPL-3.0-or-later

import {WebAuthnRegistrationResponse} from '@fluxer/schema/src/domains/auth/WebAuthnSchemas';
import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const PasskeyMigrationResponse = z.object({
	pending: z
		.object({
			credential_id: z.string().describe('ID of the passkey waiting to be updated'),
			name: z.string().describe('User-assigned name of the passkey'),
			cross_device: z.boolean().describe('Whether the passkey was used from another device'),
		})
		.nullable()
		.describe('The passkey this session can update, or null'),
});

export type PasskeyMigrationResponse = z.infer<typeof PasskeyMigrationResponse>;

export const PasskeyMigrationCompleteRequest = z.object({
	response: WebAuthnRegistrationResponse.describe('WebAuthn registration response'),
	challenge: createStringType(1, 1024).describe('The challenge from registration options'),
});

export type PasskeyMigrationCompleteRequest = z.infer<typeof PasskeyMigrationCompleteRequest>;

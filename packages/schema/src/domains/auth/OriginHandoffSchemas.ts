// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const ORIGIN_HANDOFF_MAX_PAYLOAD_LENGTH = 8 * 1024 * 1024;

const Base64UrlPattern = /^[A-Za-z0-9_-]+$/u;

export const OriginHandoffCreateRequest = z.object({
	nonce_hash: z
		.string()
		.regex(/^[0-9a-f]{64}$/u)
		.describe('Lowercase hex SHA-256 digest of the nonce the receiving origin holds'),
	payload: z
		.string()
		.min(1)
		.max(ORIGIN_HANDOFF_MAX_PAYLOAD_LENGTH)
		.regex(Base64UrlPattern)
		.describe('Encrypted client state encoded as base64url'),
});

export type OriginHandoffCreateRequest = z.infer<typeof OriginHandoffCreateRequest>;

export const OriginHandoffCreateResponse = z.object({
	handoff_id: z.string().describe('Single-use identifier the receiving origin redeems'),
});

export type OriginHandoffCreateResponse = z.infer<typeof OriginHandoffCreateResponse>;

export const OriginHandoffRedeemRequest = z.object({
	handoff_id: z
		.string()
		.regex(/^[A-Za-z0-9_-]{43}$/u)
		.describe('Identifier returned when the handoff was created'),
	nonce: z
		.string()
		.min(16)
		.max(256)
		.regex(Base64UrlPattern)
		.describe('Nonce whose SHA-256 digest was sent when the handoff was created'),
});

export type OriginHandoffRedeemRequest = z.infer<typeof OriginHandoffRedeemRequest>;

export const OriginHandoffRedeemResponse = z.object({
	payload: z.string().describe('Encrypted client state encoded as base64url'),
});

export type OriginHandoffRedeemResponse = z.infer<typeof OriginHandoffRedeemResponse>;

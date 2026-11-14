// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const EnforcementKindType = z
	.enum(['warning', 'suspension', 'closure', 'feature_limit', 'content_removal', 'profile_reset', 'community_removal'])
	.describe('What the decision did');

export const EnforcementNotice = z.object({
	decision_id: SnowflakeStringType.nullable().describe(
		'The decision reference. Null for restrictions from before decisions',
	),
	kind: EnforcementKindType,
	permanent: z.boolean().describe('Whether this is a suspension with no end'),
	issued_at: z.string().nullable().describe('When the decision was made, as an ISO 8601 timestamp'),
	ends_at: z.string().nullable().describe('When a suspension or feature limit ends, as an ISO 8601 timestamp'),
	deletion_at: z.string().nullable().describe('When a closed account or bot is deleted, as an ISO 8601 timestamp'),
	summary: z.string().describe('One sentence saying what happened'),
	effects: z.array(z.string()).describe('What the decision means for the holder, one sentence each'),
	rule: z
		.object({
			title: z.string().describe('Title of the rule that was broken'),
			url: z.string().nullable().describe('Link to the rule, null when the instance publishes none'),
		})
		.nullable()
		.describe('The rule the decision relies on, null when none was recorded'),
	facts: z.array(z.string()).describe('The finding first, then where and when it happened'),
	statement: z.string().nullable().describe('Note to the holder from the team'),
	how_we_decided: z.array(z.string()).describe('How the case reached the team and who decided it'),
	appeal: z
		.object({
			email: z.string().describe('Address that takes appeals'),
			deadline: z.string().nullable().describe('Last day to appeal, already formatted for the reader'),
			account_has_email: z.boolean().describe('Whether the account has an email address to write from'),
		})
		.nullable()
		.describe('How to appeal, null when the instance offers no appeal route'),
	redress: z.array(z.string()).describe('Other ways to challenge the decision'),
});

export type EnforcementNotice = z.infer<typeof EnforcementNotice>;

export const NetworkBlock = z.object({
	ip_address: z.string().describe('The blocked address the request came from'),
	appeal_email: z
		.string()
		.nullable()
		.describe('Address to write to about the block, null when the instance names none'),
	appeals_supported: z.boolean().describe('Whether the instance takes appeals'),
	ban_kind: z.enum(['permanent', 'temporary_24h']).describe('Whether the block has an end'),
	expires_at: z.string().nullable().describe('When the block ends, as an ISO 8601 timestamp'),
	reason: z
		.object({
			title: z.string().describe('Title of the rule behind the block'),
			url: z.string().nullable().describe('Link to the rule, null when the instance publishes none'),
		})
		.nullable()
		.describe('The rule behind the block, null when it is not shown'),
	product_name: z.string().describe('Name of the instance, as the server writes it in its own messages'),
});

export type NetworkBlock = z.infer<typeof NetworkBlock>;

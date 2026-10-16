// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {isEnforcementReasonCode} from '@fluxer/constants/src/EnforcementReasons';
import {EnforcementKindType, EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {createQueryIntegerType, QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	createInt32EnumType,
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const EnforcementReasonCodeType = createInt32EnumType(
	Object.entries(DeletionReasons).flatMap(([name, value]) =>
		isEnforcementReasonCode(value) ? [[value, name] as const] : [],
	),
	'Reason code of an enforcement decision',
	'EnforcementReasonCode',
);

export const EnforcementDecisionInput = z
	.object({
		reason_code: EnforcementReasonCodeType,
		source: z
			.enum(['report', 'own_review'])
			.describe('Whether the team reviewed this after a report or found it in its own review'),
		automation: z
			.enum(['none', 'flagged'])
			.describe('Whether automated tools flagged this before a person on the team decided'),
		report_ids: z
			.array(SnowflakeType)
			.max(20)
			.optional()
			.describe('Reports about the subject that this decision resolves. Required when the source is a report'),
		evidence: z
			.array(z.object({channel_id: SnowflakeType, message_id: SnowflakeType}))
			.max(50)
			.optional()
			.describe(
				'Messages from the subject that the decision relies on. They are snapshotted when the decision is made',
			),
		statement: createStringType(1, 500)
			.optional()
			.describe('Note to the user, shown in the notice. Never name or describe other people'),
		rights_holder: createStringType(1, 200)
			.optional()
			.describe('The rights holder or the right concerned. Required for copyright decisions and refused otherwise'),
		confirmations: z
			.array(z.enum(['evidence_preserved', 'reports_filed_or_not_required']))
			.max(2)
			.optional()
			.describe('Required for child safety and violent extremism decisions'),
	})
	.describe('Why the restriction is applied. The X-Audit-Log-Reason header is required and becomes the first note');

export type EnforcementDecisionInput = z.infer<typeof EnforcementDecisionInput>;

export const BulkEnforcementDecisionInput = z
	.strictObject({
		reason_code: EnforcementReasonCodeType,
		source: z
			.literal('own_review')
			.describe('Bulk decisions cannot link reports, so the team always found them in its own review'),
		automation: EnforcementDecisionInput.shape.automation,
		statement: createStringType(1, 500).describe(
			'Note to the user, shown in every notice. It is the only fact a bulk decision can give',
		),
	})
	.describe('Why every account in the job is restricted. Reports and evidence messages are refused in bulk');

export type BulkEnforcementDecisionInput = z.infer<typeof BulkEnforcementDecisionInput>;

function enforcementBasis<Decision extends z.ZodType>(decision: Decision) {
	return z
		.object({type: z.literal('enforcement'), decision})
		.describe('A rule was broken. Records an enforcement decision and sends its notice');
}

const HolderRequestBasis = z
	.object({type: z.literal('holder_request')})
	.describe('The account holder or community owner asked');

const HousekeepingBasis = z
	.object({type: z.literal('housekeeping')})
	.describe("Nobody's conduct is involved, such as inactivity or cleanup");

export const ModerationBasis = z
	.discriminatedUnion('type', [enforcementBasis(EnforcementDecisionInput), HolderRequestBasis, HousekeepingBasis])
	.describe('Why the action is taken');

export type ModerationBasis = z.infer<typeof ModerationBasis>;

export const BulkModerationBasis = z
	.discriminatedUnion('type', [enforcementBasis(BulkEnforcementDecisionInput), HolderRequestBasis, HousekeepingBasis])
	.describe('Why the action is taken');

export type BulkModerationBasis = z.infer<typeof BulkModerationBasis>;

export const MessageDeletionBasis = z
	.discriminatedUnion('type', [HolderRequestBasis, HousekeepingBasis])
	.describe(
		'Why the message is deleted. To remove a message for breaking a rule, shred it with an enforcement basis instead',
	);

export const CommunityRemovalBasis = z
	.discriminatedUnion('type', [enforcementBasis(EnforcementDecisionInput), HolderRequestBasis])
	.describe('Why the community is removed');

export type CommunityRemovalBasis = z.infer<typeof CommunityRemovalBasis>;

const NETWORK_BLOCK_DEFAULT_DAYS = 90;

const IpAddressType = z.union([z.ipv4(), z.ipv6()]);

export const NetworkBlockRequest = z
	.object({
		ips: z
			.array(
				createStringType(1, 45).refine(
					(value) => IpAddressType.safeParse(value).success,
					'Must be a valid IPv4 or IPv6 address',
				),
			)
			.min(1)
			.max(20)
			.describe(
				'Addresses the account used in the last 30 days. An address on which another account that is not suspended or closed was last active is refused',
			),
		days: z
			.number()
			.int()
			.min(1)
			.max(365)
			.default(NETWORK_BLOCK_DEFAULT_DAYS)
			.describe(
				'How many days the blocks last. Ending the decision or deleting the account removes them sooner. Defaults to 90',
			),
	})
	.describe(
		'Block networks the account used. Only with a permanent suspension or a closure for a broken rule. Requires BAN_IP_ADD and USER_VIEW_IP',
	);

export type NetworkBlockRequest = z.infer<typeof NetworkBlockRequest>;

export const SkippedNetworkBlock = z.object({
	ip: z.string(),
	reason: z
		.enum(['exempt', 'special_purpose', 'already_blocked'])
		.describe(
			'Exempt addresses and special-purpose ranges such as private, shared carrier and documentation ranges are never blocked. An address that is already blocked permanently or by another decision is left as it is',
		),
});

export const NetworkBlockCandidatesResponse = z.object({
	networks: z
		.array(
			z.object({
				ip: z.string(),
				last_seen_at: z.string().describe('When the account last used this address'),
				skip_reason: z
					.enum(['exempt', 'special_purpose'])
					.nullable()
					.describe('Why this address would be skipped. Null when it can be blocked'),
				other_unrestricted_accounts: Int32Type.describe(
					'Other accounts that are not suspended or closed and were last active on this address. Blocking is refused when this is above zero',
				),
			}),
		)
		.describe('Addresses the account used in the last 30 days, most recent first'),
});

export type NetworkBlockCandidatesResponse = z.infer<typeof NetworkBlockCandidatesResponse>;

export const AdminUserBanRequest = z.object({
	duration_hours: z
		.number()
		.int()
		.min(0)
		.max(8760)
		.describe('Duration of the suspension in hours. Use 0 for a permanent suspension'),
	decision: EnforcementDecisionInput,
	remove_evidence: z
		.boolean()
		.optional()
		.describe('Remove the evidence messages after they are snapshotted into the decision'),
	block_networks: NetworkBlockRequest.optional(),
});

export type AdminUserBanRequest = z.infer<typeof AdminUserBanRequest>;

export const AdminUserWarningRequest = z.object({
	decision: EnforcementDecisionInput,
});

export type AdminUserWarningRequest = z.infer<typeof AdminUserWarningRequest>;

export const AdminUserWarningResponse = z.object({
	decision_id: SnowflakeStringType.describe('The warning decision'),
});

export const AdminUserFeatureLimitRequest = z.object({
	limit: z
		.enum(['direct_messages', 'reports', 'purchases'])
		.describe(
			'The one feature this decision limits. Direct messages are for spam, automation, fraud and malware reasons only, reports for misuse of reporting, purchases for payment reasons',
		),
	ends_at: z.iso
		.datetime()
		.optional()
		.describe('When a reports limit ends, at most 365 days away. Defaults to 90 days. Refused for other limits'),
	decision: EnforcementDecisionInput,
});

export type AdminUserFeatureLimitRequest = z.infer<typeof AdminUserFeatureLimitRequest>;

export const EnforcementDecisionIdParam = z.object({
	decision_id: SnowflakeType.describe('The ID of the enforcement decision'),
});

export const EnforcementDecisionListQuery = z
	.object({
		review_queue: QueryBooleanType.describe(
			'List the automated decisions waiting for a person to review them instead of one subject',
		),
		subject_type: z.enum(['user', 'guild']).optional().describe('Kind of subject whose decisions to list'),
		subject_id: SnowflakeType.optional().describe('ID of the user or community whose decisions to list'),
		limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 100}).describe(
			'Maximum number of decisions to return',
		),
		before: SnowflakeType.optional().describe('Return decisions with IDs lower than this decision ID'),
	})
	.refine((query) => query.review_queue || (query.subject_type !== undefined && query.subject_id !== undefined), {
		error: 'Give subject_type and subject_id, or review_queue=true',
	});

export type EnforcementDecisionListQuery = z.infer<typeof EnforcementDecisionListQuery>;

export const EnforcementDecisionNoteRequest = z.object({
	body: createStringType(1, 2000).describe('The internal note. Staff only'),
});

export type EnforcementDecisionNoteRequest = z.infer<typeof EnforcementDecisionNoteRequest>;

export const EnforcementDecisionEndRequest = z.object({
	outcome: z
		.enum(['lifted', 'reversed'])
		.describe('Lifted ends a restriction that was correct. Reversed says the decision was wrong'),
	statement: createStringType(1, 500).optional().describe('Note to the user, shown in the email'),
});

export type EnforcementDecisionEndRequest = z.infer<typeof EnforcementDecisionEndRequest>;

const EnforcementDecisionSummary = z.object({
	decision_id: SnowflakeStringType,
	subject_type: z.enum(['user', 'guild']),
	subject_id: SnowflakeStringType,
	subject_name: z.string().nullable().describe('Community name or bot username when the decision was made'),
	recipient_user_id: SnowflakeStringType.nullable().describe('Who receives the notice'),
	kind: EnforcementKindType,
	reason_code: Int32Type,
	source: z.enum(['report', 'own_review']),
	automation: z.enum(['none', 'flagged', 'automated']),
	statement: z.string().nullable(),
	rights_holder: z.string().nullable(),
	ends_at: z.string().nullable(),
	deletion_at: z.string().nullable(),
	limit: z.enum(['direct_messages', 'reports', 'purchases']).nullable(),
	profile_fields: z.array(z.string()),
	removed_message_count: Int32Type.nullable(),
	removed_all_messages: z.boolean(),
	payment_at: z.string().nullable(),
	report_ids: z.array(SnowflakeStringType).describe('Empty without the report view permission'),
	decided_by: SnowflakeStringType,
	created_at: z.string(),
	status: z.enum(['active', 'ended']),
	end_outcome: z
		.enum(['lifted', 'reversed', 'expired'])
		.nullable()
		.describe('Expired is recorded by Fluxer when a temporary suspension runs out'),
	ended_at: z.string().nullable(),
	ended_by: SnowflakeStringType.nullable(),
	end_statement: z.string().nullable(),
	notice_status: z.enum(['pending', 'sent', 'undelivered']),
	notice_channels: z.array(z.enum(['email', 'system_dm', 'sign_in'])),
	notice_locale: z.string().nullable(),
	notice_text: z.string().nullable().describe('The plain text that was sent'),
	notice_sent_at: z.string().nullable(),
	expired: z.boolean().describe('An active suspension or feature limit whose end has passed'),
	replaced: z.boolean().describe('An active suspension or closure that a later decision replaced'),
});

const EnforcementDecisionNote = z.object({
	note_id: SnowflakeStringType,
	author_id: SnowflakeStringType,
	body: z.string(),
	created_at: z.string(),
});

const EnforcementEvidenceMessage = z.object({
	message_id: SnowflakeStringType,
	channel_id: SnowflakeStringType.nullable(),
	timestamp: z.string(),
	content: z.string().nullable().describe('Null without the message lookup permission, and for restricted decisions'),
	attachments: z
		.array(
			z.object({
				filename: z.string(),
				size: z.string().nullable(),
				content_type: z.string().nullable(),
			}),
		)
		.nullable()
		.describe('Attachment details without the files. Null without the message lookup permission'),
	attachment_count: Int32Type,
});

export const EnforcementDecisionResponse = EnforcementDecisionSummary.extend({
	notes: z.array(EnforcementDecisionNote),
	evidence: z.array(EnforcementEvidenceMessage).describe('Empty once the evidence has expired'),
	profile_values: z
		.record(z.string(), z.string())
		.nullable()
		.describe('Profile values before a reset. Null without the message lookup permission'),
	network_blocks: z
		.array(
			z.object({
				ip: z.string().describe('Masked without the IP view permission'),
				expires_at: z.string().nullable(),
			}),
		)
		.describe('Network blocks that belong to this decision and have not expired'),
	notice: EnforcementNotice.describe('What the holder sees now, rendered in English'),
});

export type EnforcementDecisionResponse = z.infer<typeof EnforcementDecisionResponse>;

export const EnforcementDecisionListResponse = z.object({
	decisions: z.array(EnforcementDecisionSummary),
});

export type EnforcementDecisionListResponse = z.infer<typeof EnforcementDecisionListResponse>;

export const EnforcementDecisionNoteResponse = EnforcementDecisionNote;

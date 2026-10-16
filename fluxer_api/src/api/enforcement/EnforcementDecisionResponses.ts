// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BannedIpEntry} from '@app/api/admin/AdminRepository';
import type {
	EnforcementDecisionEvidenceRow,
	EnforcementDecisionNoteRow,
	EnforcementDecisionRow,
} from '@app/api/database/types/EnforcementTypes';
import {isDecisionExpired, isDecisionReplaced} from '@app/api/enforcement/DecisionStatus';
import {hasAdminACL} from '@app/api/middleware/AdminMiddleware';
import type {User} from '@app/api/models/User';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {maskIpForDisplay} from '@fluxer/ip_utils/src/IpAddress';
import type {EnforcementDecisionResponse} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';

type EnforcementDecisionSummary = Omit<
	EnforcementDecisionResponse,
	'notes' | 'evidence' | 'profile_values' | 'network_blocks' | 'notice'
>;

function iso(value: Date | null): string | null {
	return value?.toISOString() ?? null;
}

export function toNoteResponse(note: EnforcementDecisionNoteRow): EnforcementDecisionResponse['notes'][number] {
	return {
		note_id: note.note_id.toString(),
		author_id: note.author_id.toString(),
		body: note.body,
		created_at: note.created_at.toISOString(),
	};
}

export function toDecisionSummary(
	decision: EnforcementDecisionRow,
	subject: User | null,
	acls: ReadonlySet<string>,
): EnforcementDecisionSummary {
	return {
		decision_id: decision.decision_id.toString(),
		subject_type: decision.subject_type,
		subject_id: decision.subject_id.toString(),
		subject_name: decision.subject_name,
		recipient_user_id: decision.recipient_user_id?.toString() ?? null,
		kind: decision.kind,
		reason_code: decision.reason_code,
		source: decision.source,
		automation: decision.automation,
		statement: decision.statement,
		rights_holder: decision.rights_holder,
		ends_at: iso(decision.ends_at),
		deletion_at: iso(decision.deletion_at),
		limit: decision.feature_limit,
		profile_fields: [...(decision.profile_fields ?? [])].sort(),
		removed_message_count: decision.removed_message_count,
		removed_all_messages: decision.removed_all_messages ?? false,
		payment_at: iso(decision.payment_at),
		report_ids: hasAdminACL(acls, AdminACLs.REPORT_VIEW)
			? [...(decision.report_ids ?? [])].map((reportId) => reportId.toString())
			: [],
		decided_by: decision.decided_by.toString(),
		created_at: decision.created_at.toISOString(),
		status: decision.status,
		end_outcome: decision.end_outcome,
		ended_at: iso(decision.ended_at),
		ended_by: decision.ended_by?.toString() ?? null,
		end_statement: decision.end_statement,
		notice_status: decision.notice_status,
		notice_channels: [...(decision.notice_channels ?? [])].sort(),
		notice_locale: decision.notice_locale,
		notice_text: decision.notice_text,
		notice_sent_at: iso(decision.notice_sent_at),
		expired: isDecisionExpired(decision),
		replaced: isDecisionReplaced(decision, subject),
	};
}

export function toAdminDecisionResponse({
	decision,
	subject,
	notes,
	evidence,
	networkBlocks,
	notice,
	acls,
}: {
	decision: EnforcementDecisionRow;
	subject: User | null;
	notes: ReadonlyArray<EnforcementDecisionNoteRow>;
	evidence: EnforcementDecisionEvidenceRow | null;
	networkBlocks: ReadonlyArray<BannedIpEntry>;
	notice: EnforcementNotice;
	acls: ReadonlySet<string>;
}): EnforcementDecisionResponse {
	const canViewContent = hasAdminACL(acls, AdminACLs.MESSAGE_LOOKUP);
	const canViewIp = hasAdminACL(acls, AdminACLs.USER_VIEW_IP);
	return {
		...toDecisionSummary(decision, subject, acls),
		notes: notes.map(toNoteResponse),
		evidence: (evidence?.messages ?? []).map((message) => ({
			message_id: message.message_id.toString(),
			channel_id: message.channel_id?.toString() ?? null,
			timestamp: message.timestamp.toISOString(),
			content: canViewContent ? message.content : null,
			attachments: canViewContent
				? (message.missing_attachments ?? []).map((attachment) => ({
						filename: attachment.filename,
						size: attachment.size.toString(),
						content_type: attachment.content_type || null,
					}))
				: null,
			attachment_count: message.missing_attachments?.length ?? 0,
		})),
		profile_values: canViewContent && evidence?.profile_values ? Object.fromEntries(evidence.profile_values) : null,
		network_blocks: networkBlocks.map((block) => ({
			ip: canViewIp ? block.ip : (maskIpForDisplay(block.ip) ?? '[redacted]'),
			expires_at: iso(block.expiresAt),
		})),
		notice,
	};
}

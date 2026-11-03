// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {buildEnforcementNotice, type EvidenceMessageLocation} from '@app/api/enforcement/EnforcementNotice';
import {renderNoticeText} from '@app/api/enforcement/EnforcementNoticeDelivery';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getInstanceProductName, setCachedProductName} from '@app/api/instance/ProductName';
import {User} from '@app/api/models/User';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {ENFORCEMENT_RULES, type EnforcementRuleKey} from '@fluxer/constants/src/EnforcementReasons';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {AdminUserClearFieldsRequest} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {enforcementEndedSlots, renderEmailTemplateText, reportResolvedSlots} from '@pkgs/email/src/EmailService';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {afterEach, describe, expect, it} from 'vitest';

const REPORT_REFERENCE = {
	target: 'user',
	reportedAt: new Date('2026-10-24T18:12:00Z'),
	username: 'example',
	communityName: '',
	channelName: '',
} as const;

const CREATED_AT = new Date('2026-10-14T10:00:00.000Z');
const NOW = new Date('2026-10-14T12:00:00.000Z');
const STAFF_ID = 987654321987n;
const REPORT_ID = 555444333222n;

function decision(overrides: Partial<EnforcementDecisionRow> = {}): EnforcementDecisionRow {
	return {
		decision_id: 1234567890123n,
		subject_type: 'user',
		subject_id: 40n,
		subject_name: null,
		recipient_user_id: 40n,
		kind: 'suspension',
		reason_code: DeletionReasons.SPAM,
		source: 'own_review',
		automation: 'none',
		statement: null,
		rights_holder: null,
		ends_at: new Date('2026-10-21T10:00:00.000Z'),
		deletion_at: null,
		feature_limit: null,
		profile_fields: null,
		removed_message_count: null,
		removed_all_messages: null,
		payment_at: null,
		report_ids: new Set([REPORT_ID]),
		decided_by: STAFF_ID,
		created_at: CREATED_AT,
		status: 'active',
		end_outcome: null,
		ended_at: null,
		ended_by: null,
		end_statement: null,
		notice_status: 'sent',
		notice_channels: new Set(['email']),
		notice_locale: 'en-US',
		notice_text: null,
		notice_sent_at: CREATED_AT,
		...overrides,
	};
}

const IN_CHANNEL: Array<EvidenceMessageLocation> = [
	{
		locationKey: '20',
		location: {type: 'channel', channelName: 'general', communityName: 'Plant Club'},
		timestamp: new Date('2026-10-01T09:00:00.000Z'),
	},
	{
		locationKey: '20',
		location: {type: 'channel', channelName: 'general', communityName: 'Plant Club'},
		timestamp: new Date('2026-10-02T18:30:00.000Z'),
	},
];

const IN_DIRECT_MESSAGE: Array<EvidenceMessageLocation> = [
	{locationKey: '21', location: {type: 'direct_message'}, timestamp: new Date('2026-09-03T08:15:00.000Z')},
	{locationKey: '22', location: {type: 'group_message'}, timestamp: new Date('2026-09-04T08:15:00.000Z')},
	{locationKey: '23', location: {type: 'unknown'}, timestamp: new Date('2026-09-05T08:15:00.000Z')},
];

function notice(
	row: EnforcementDecisionRow,
	evidence: ReadonlyArray<EvidenceMessageLocation> = IN_CHANNEL,
	locale = 'en-US',
): EnforcementNotice {
	return buildEnforcementNotice(
		{type: 'decision', decision: row, evidence, networksBlockedUntil: null, accountHasEmail: true},
		locale,
		NOW,
	);
}

function legacyUser(overrides: Partial<typeof EMPTY_USER_ROW>): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: 41n as UserID,
		username: 'legacy',
		email: 'legacy@example.com',
		...overrides,
	});
}

const KINDS: Record<string, EnforcementDecisionRow> = {
	suspension: decision({statement: 'You posted the same link in ten communities.'}),
	permanent_suspension: decision({ends_at: null, removed_message_count: 3}),
	bot_suspension: decision({subject_name: 'helperbot', recipient_user_id: 77n}),
	warning: decision({kind: 'warning', ends_at: null}),
	closure: decision({kind: 'closure', ends_at: null, deletion_at: new Date('2026-12-27T10:00:00.000Z')}),
	feature_limit_direct_messages: decision({kind: 'feature_limit', ends_at: null, feature_limit: 'direct_messages'}),
	feature_limit_reports: decision({
		kind: 'feature_limit',
		reason_code: DeletionReasons.REPORTING_ABUSE,
		feature_limit: 'reports',
	}),
	feature_limit_purchases: decision({
		kind: 'feature_limit',
		reason_code: DeletionReasons.PAYMENT_FRAUD,
		ends_at: null,
		feature_limit: 'purchases',
		automation: 'automated',
		payment_at: new Date('2026-10-10T00:00:00.000Z'),
	}),
	content_removal: decision({kind: 'content_removal', ends_at: null, removed_message_count: 2}),
	content_removal_all: decision({kind: 'content_removal', ends_at: null, removed_all_messages: true}),
	profile_reset: decision({kind: 'profile_reset', ends_at: null, profile_fields: new Set(['bio', 'global_name'])}),
	community_removal: decision({
		kind: 'community_removal',
		subject_type: 'guild',
		subject_name: 'Plant Club',
		ends_at: null,
	}),
	protected_harassment: decision({reason_code: DeletionReasons.HARASSMENT_OR_BULLYING, source: 'report'}),
	restricted_child_safety: decision({reason_code: DeletionReasons.CHILD_SAFETY_VIOLATION, ends_at: null}),
	copyright: decision({
		kind: 'warning',
		ends_at: null,
		reason_code: DeletionReasons.COPYRIGHT,
		rights_holder: 'Example Records',
	}),
	self_harm: decision({
		kind: 'warning',
		ends_at: null,
		reason_code: DeletionReasons.SELF_HARM_PROMOTION,
		automation: 'flagged',
	}),
};

describe('buildEnforcementNotice', () => {
	const config = getConfig();
	const selfHosted = config.instance.selfHosted;

	afterEach(() => {
		config.instance.selfHosted = selfHosted;
		setCachedProductName(null);
	});

	it('renders conversations with dates under standard handling and the month only under protected handling', () => {
		expect(notice(decision(), IN_DIRECT_MESSAGE).facts.slice(1)).toEqual([
			'1 message in a direct message conversation, on Sep 3, 2026, 8:15 AM UTC.',
			'1 message in a group conversation, on Sep 4, 2026, 8:15 AM UTC.',
			'1 message in a conversation that no longer exists, on Sep 5, 2026, 8:15 AM UTC.',
		]);
		const protectedNotice = notice(KINDS.protected_harassment!, IN_DIRECT_MESSAGE);
		expect(protectedNotice.facts.slice(1)).toEqual([
			'1 message in a direct message conversation in September 2026.',
			'1 message in a group conversation in September 2026.',
			'1 message in a conversation that no longer exists, on Sep 5, 2026, 8:15 AM UTC.',
		]);
	});

	it('gives restricted decisions the rule lead only', () => {
		const restricted = notice(
			decision({reason_code: DeletionReasons.VIOLENT_EXTREMISM, ends_at: null, statement: 'x'}),
		);
		expect(restricted.facts).toEqual([
			'We found content or activity on your account that breaks our rules on terrorism and violent extremism.',
		]);
		expect(restricted.statement).toBeNull();
	});

	it('never shows private fields', () => {
		const rendered = JSON.stringify(
			Object.values(KINDS).map((row) => notice({...row, decided_by: STAFF_ID, report_ids: new Set([REPORT_ID])})),
		);
		for (const secret of [STAFF_ID.toString(), REPORT_ID.toString(), '127.0.0.1', 'NCMEC', 'police', 'hash']) {
			expect(rendered).not.toContain(secret);
		}
	});

	it('renders legacy restrictions from the user row', () => {
		const suspended = buildEnforcementNotice(
			{
				type: 'legacy',
				user: legacyUser({temp_banned_until: new Date('2026-10-20T00:00:00.000Z'), flags: UserFlags.DISABLED}),
			},
			'en-US',
			NOW,
		);
		expect(suspended).toMatchObject({
			decision_id: null,
			kind: 'suspension',
			permanent: false,
			rule: null,
			facts: ["We don't have more detail on record for this decision."],
			statement: null,
			how_we_decided: [],
			appeal: {email: 'appeals@fluxer.com', deadline: null, account_has_email: true},
		});
		const banned = buildEnforcementNotice(
			{
				type: 'legacy',
				user: legacyUser({temp_banned_until: new Date('2126-10-14T00:00:00.000Z'), flags: UserFlags.DISABLED}),
			},
			'en-US',
			NOW,
		);
		expect(banned).toMatchObject({kind: 'suspension', permanent: true, summary: 'We banned your account.'});
		const closed = buildEnforcementNotice(
			{
				type: 'legacy',
				user: legacyUser({
					flags: UserFlags.DELETED,
					pending_deletion_at: new Date('2026-12-01T00:00:00.000Z'),
					deletion_reason_code: DeletionReasons.SPAM,
					deletion_public_reason: 'Bulk invites',
					deletion_scheduled_at: new Date('2026-10-01T00:00:00.000Z'),
				}),
			},
			'en-US',
			NOW,
		);
		expect(closed).toMatchObject({
			kind: 'closure',
			rule: {title: 'Spam'},
			facts: [],
			statement: 'Bulk invites',
			appeal: {deadline: 'Nov 30, 2026'},
		});
	});

	it('names no product, address or support line on an unnamed self-hosted instance', () => {
		config.instance.selfHosted = true;
		setCachedProductName(null);
		for (const row of Object.values(KINDS)) {
			const rendered = notice(row);
			const text = JSON.stringify(rendered);
			expect(text).not.toMatch(/fluxer/i);
			expect(text).not.toContain('@');
			expect(rendered.appeal).toBeNull();
			expect(rendered.redress).toEqual([]);
			expect(rendered.rule?.url ?? null).toBeNull();
			expect(text).not.toContain('support lines');
		}
	});

	it('names no product or address in any email or reporter notice on an unnamed self-hosted instance', () => {
		config.instance.selfHosted = true;
		setCachedProductName(null);
		const productName = getInstanceProductName();
		const contacts = resolveContactEmails();
		const full = <T extends Parameters<typeof getEmailTemplate>[0]>(
			key: T,
			variables: Parameters<typeof getEmailTemplate<T>>[2],
			slots?: Parameters<typeof renderEmailTemplateText<T>>[4],
		) => {
			const template = getEmailTemplate(key, 'en-US', {...variables, reply: 'no'}, productName);
			const text = renderEmailTemplateText(key, 'en-US', variables, productName, slots);
			return template.ok && text !== null ? `${template.value.subject}\n${text}` : null;
		};
		const rendered = [
			...Object.values(KINDS).map((row) => renderNoticeText(row, notice(row), 'en-US')),
			...(['lifted', 'reversed'] as const).map((outcome) =>
				full(
					'enforcement_ended',
					{outcome, decision_id: '1', restores_access: 'yes', statement: null, content_removed: 'yes'},
					enforcementEndedSlots,
				),
			),
			full('account_deletion_scheduled_requested', {deletion_date: 'Dec 13, 2026'}),
			full('account_deletion_scheduled_inactivity', {deletion_date: 'Dec 13, 2026'}),
			...(['removed', 'restricted', 'action_taken', 'no_violation'] as const).flatMap((outcome) => [
				full('report_resolved', {...REPORT_REFERENCE, outcome, appeals_email: contacts.appealsEmail}, (template) =>
					reportResolvedSlots(template, '1', ''),
				),
				full('dsa_report_resolved', {...REPORT_REFERENCE, outcome, appeals_email: contacts.appealsEmail}, (template) =>
					reportResolvedSlots(template, '1', ''),
				),
			]),
		];
		for (const text of rendered) {
			expect(text).not.toBeNull();
			expect(text).not.toMatch(/fluxer/i);
			expect(text).not.toContain('@');
		}
	});
});

describe('enforcement content catalog', () => {
	const keys = new Set(Object.keys(CONTENT_I18N_MESSAGES));

	it('has a title and a lead for every rule', () => {
		for (const rule of Object.keys(ENFORCEMENT_RULES) as Array<EnforcementRuleKey>) {
			expect(keys.has(`enforcement.rule.${rule}`), rule).toBe(true);
			expect(keys.has(`enforcement.lead.${rule}`), rule).toBe(true);
		}
	});

	it('has a label for every profile field', () => {
		for (const field of AdminUserClearFieldsRequest.shape.fields.element.options.map((option) => option.value)) {
			expect(keys.has(`enforcement.profile_field.${field}`), field).toBe(true);
		}
	});
});

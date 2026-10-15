// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {type ContentI18nMessageArgs, getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import {joinLocalizedList} from '@app/api/content_i18n/JoinLocalizedList';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import {
	formatNoticeDate,
	formatNoticeMonth,
	formatNoticeTimeUtc,
	isPermanentSuspensionEnd,
} from '@app/api/enforcement/NoticeDates';
import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {getLegalUrls} from '@app/api/instance/LegalUrls';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import type {User} from '@app/api/models/User';
import {isAccountClosed} from '@app/api/user/UserHelpers';
import {
	ENFORCEMENT_APPEAL_WINDOW_DAYS,
	ENFORCEMENT_REASONS,
	ENFORCEMENT_RULES,
	type EnforcementRuleKey,
	enforcementReasonOf,
	isAccountKind,
} from '@fluxer/constants/src/EnforcementReasons';
import type {UserProfileField} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {ms} from 'itty-time';

type EnforcementContentKey = Extract<ContentI18nKey, `enforcement.${string}`>;
type NoticeArgs<K extends EnforcementContentKey> = K extends EnforcementContentKey
	? ContentI18nMessageArgs<K> extends [variables: infer Variables]
		? [keyof Omit<Variables, 'product_name'>] extends [never]
			? [variables?: undefined]
			: [variables: Omit<Variables, 'product_name'>]
		: [variables?: undefined]
	: never;

interface NoticeTarget {
	target: 'account' | 'bot';
	bot_name: string;
}

type EvidenceLocation =
	| {type: 'channel'; channelName: string; communityName: string}
	| {type: 'direct_message'}
	| {type: 'group_message'}
	| {type: 'unknown'};

export interface EvidenceMessageLocation {
	locationKey: string;
	location: EvidenceLocation;
	timestamp: Date;
}

type EnforcementNoticeSource =
	| {
			type: 'decision';
			decision: EnforcementDecisionRow;
			evidence: ReadonlyArray<EvidenceMessageLocation>;
			networksBlockedUntil: Date | null;
			accountHasEmail: boolean;
	  }
	| {type: 'legacy'; user: User};

const OUT_OF_COURT_URL = 'https://digital-strategy.ec.europa.eu/en/policies/dsa-out-court-dispute-settlement';
const SELF_HARM_ANCHOR = ENFORCEMENT_RULES.self_harm.anchor;
const DOUBLED_PERIOD = /(?<!\.)\.\.(?!\.)/g;

interface NoticeContext {
	locale: string | null;
	hosted: boolean;
	appealsEmail: string | null;
	productName: string;
}

export function isBotDecision(decision: EnforcementDecisionRow): boolean {
	return decision.subject_type === 'user' && decision.subject_name !== null;
}

function text<K extends EnforcementContentKey>(ctx: NoticeContext, key: K, ...args: NoticeArgs<K>): string {
	const variables = [{product_name: ctx.productName, ...args[0]}] as ContentI18nMessageArgs<K>;
	return getContentMessage(key, ctx.locale, ...variables).replace(DOUBLED_PERIOD, '.');
}

function ruleUrl(ctx: NoticeContext, rule: EnforcementRuleKey): string | null {
	const {policy, anchor} = ENFORCEMENT_RULES[rule];
	const legal = getLegalUrls();
	const base = policy === 'terms' ? legal.termsUrl : legal.guidelinesUrl;
	if (!base) return null;
	return ctx.hosted ? `${base}#${anchor}` : base;
}

type RuleReference = NonNullable<EnforcementNotice['rule']>;

function ruleOf(ctx: NoticeContext, rule: EnforcementRuleKey): RuleReference {
	return {title: text(ctx, `enforcement.rule.${rule}`), url: ruleUrl(ctx, rule)};
}

function redressLines(ctx: NoticeContext): Array<string> {
	if (!ctx.hosted) return [];
	return [
		text(ctx, 'enforcement.redress.out_of_court', {url: OUT_OF_COURT_URL}),
		text(ctx, 'enforcement.redress.regulator_and_court'),
	];
}

function appealOf(ctx: NoticeContext, from: Date | null, accountHasEmail: boolean): EnforcementNotice['appeal'] {
	if (!ctx.appealsEmail) return null;
	return {
		email: ctx.appealsEmail,
		deadline: from
			? formatNoticeDate(new Date(from.getTime() + ms(`${ENFORCEMENT_APPEAL_WINDOW_DAYS} days`)), ctx.locale)
			: null,
		account_has_email: accountHasEmail,
	};
}

function profileFieldList(ctx: NoticeContext, fields: ReadonlySet<string> | null): string {
	const labels = [...(fields ?? [])].map((field) =>
		text(ctx, `enforcement.profile_field.${field as UserProfileField}`),
	);
	return joinLocalizedList(labels, ctx.locale);
}

interface LocationGroup {
	location: EvidenceLocation;
	first: Date;
	last: Date;
	count: number;
}

function isPrivateLocation(location: EvidenceLocation): boolean {
	return location.type === 'direct_message' || location.type === 'group_message';
}

function groupLocations(
	ctx: NoticeContext,
	evidence: ReadonlyArray<EvidenceMessageLocation>,
	byMonth: boolean,
): Array<LocationGroup> {
	const groups = new Map<string, LocationGroup>();
	const sorted = [...evidence].sort((left, right) => left.timestamp.getTime() - right.timestamp.getTime());
	for (const message of sorted) {
		const key =
			byMonth && isPrivateLocation(message.location)
				? `${message.locationKey}:${formatNoticeMonth(message.timestamp, ctx.locale)}`
				: message.locationKey;
		const group = groups.get(key);
		if (group) {
			group.last = message.timestamp;
			group.count += 1;
		} else {
			groups.set(key, {location: message.location, first: message.timestamp, last: message.timestamp, count: 1});
		}
	}
	return [...groups.values()];
}

function locationLine(ctx: NoticeContext, group: LocationGroup, community: boolean, byMonth: boolean): string {
	const start = formatNoticeTimeUtc(group.first, ctx.locale);
	const end = formatNoticeTimeUtc(group.last, ctx.locale);
	const range = {count: group.count, range: start === end ? 'single' : 'other', start, end};
	const month = {count: group.count, month: formatNoticeMonth(group.first, ctx.locale)};
	switch (group.location.type) {
		case 'channel':
			return community
				? text(ctx, 'enforcement.facts.community_channel', {...range, channel: group.location.channelName})
				: text(ctx, 'enforcement.facts.channel', {
						...range,
						channel: group.location.channelName,
						community: group.location.communityName,
					});
		case 'direct_message':
			return byMonth
				? text(ctx, 'enforcement.facts.direct_message_month', month)
				: text(ctx, 'enforcement.facts.direct_message', range);
		case 'group_message':
			return byMonth
				? text(ctx, 'enforcement.facts.group_message_month', month)
				: text(ctx, 'enforcement.facts.group_message', range);
		case 'unknown':
			return text(ctx, 'enforcement.facts.unknown_location', range);
	}
}

function decisionSummary(ctx: NoticeContext, decision: EnforcementDecisionRow, target: NoticeTarget): string {
	switch (decision.kind) {
		case 'suspension':
			return decision.ends_at
				? text(ctx, 'enforcement.summary.suspension', {
						...target,
						ends_at: formatNoticeTimeUtc(decision.ends_at, ctx.locale),
					})
				: text(ctx, 'enforcement.summary.suspension_permanent', target);
		case 'warning':
			return text(ctx, 'enforcement.summary.warning');
		case 'closure':
			return text(ctx, 'enforcement.summary.closure', target);
		case 'feature_limit':
			return text(ctx, 'enforcement.summary.feature_limit', {
				ends: decision.ends_at ? 'yes' : 'no',
				ends_at: decision.ends_at ? formatNoticeTimeUtc(decision.ends_at, ctx.locale) : '',
			});
		case 'content_removal':
			return text(ctx, 'enforcement.summary.content_removal', {
				all: decision.removed_all_messages ? 'yes' : 'no',
				count: decision.removed_message_count ?? 0,
			});
		case 'profile_reset':
			return text(ctx, 'enforcement.summary.profile_reset', {
				fields: profileFieldList(ctx, decision.profile_fields),
			});
		case 'community_removal':
			return text(ctx, 'enforcement.summary.community_removal', {community_name: decision.subject_name ?? ''});
	}
}

function kindEffect(ctx: NoticeContext, decision: EnforcementDecisionRow, target: NoticeTarget): string {
	switch (decision.kind) {
		case 'suspension':
			return text(
				ctx,
				decision.ends_at ? 'enforcement.effect.suspension' : 'enforcement.effect.suspension_permanent',
				target,
			);
		case 'warning':
			return text(ctx, 'enforcement.effect.warning');
		case 'closure':
			return text(ctx, 'enforcement.effect.closure', {
				...target,
				deletion_at: decision.deletion_at ? formatNoticeDate(decision.deletion_at, ctx.locale) : '',
			});
		case 'feature_limit':
			switch (decision.feature_limit) {
				case 'reports':
					return text(ctx, 'enforcement.effect.limit_reports', {
						safety_route: ctx.hosted ? 'yes' : 'no',
						url: `${Config.endpoints.marketing}/company-information`,
					});
				case 'purchases':
					return text(ctx, 'enforcement.effect.limit_purchases', {
						review: decision.automation === 'automated' ? 'yes' : 'no',
					});
				default:
					return text(ctx, 'enforcement.effect.limit_direct_messages');
			}
		case 'content_removal':
		case 'profile_reset':
			return text(ctx, 'enforcement.effect.account_not_restricted');
		case 'community_removal':
			return text(ctx, 'enforcement.effect.community_removed');
	}
}

function decisionEffects(
	ctx: NoticeContext,
	decision: EnforcementDecisionRow,
	target: NoticeTarget,
	networksBlockedUntil: Date | null,
): Array<string> {
	const reason = ENFORCEMENT_REASONS[decision.reason_code];
	const ownAccount = decision.subject_type === 'user' && target.target === 'account';
	const accountKind = isAccountKind(decision.kind);
	const effects = [kindEffect(ctx, decision, target)];
	if (!accountKind) {
		effects.push(text(ctx, 'enforcement.effect.scope'));
	}
	const barsNewAccounts =
		(decision.kind === 'closure' || (decision.kind === 'suspension' && !decision.ends_at)) &&
		ENFORCEMENT_RULES[reason.rule].barsNewAccounts;
	if (ownAccount && barsNewAccounts) {
		effects.push(text(ctx, 'enforcement.effect.no_other_accounts'));
	}
	if (accountKind && (decision.removed_message_count ?? 0) > 0) {
		effects.push(text(ctx, 'enforcement.effect.messages_removed', {count: decision.removed_message_count ?? 0}));
	}
	if (accountKind && networksBlockedUntil) {
		effects.push(
			text(ctx, 'enforcement.effect.networks_blocked', {date: formatNoticeDate(networksBlockedUntil, ctx.locale)}),
		);
	}
	return effects;
}

function leadKey(decision: EnforcementDecisionRow, rule: EnforcementRuleKey): EnforcementContentKey {
	if (decision.kind === 'community_removal') return 'enforcement.lead.community';
	if (isBotDecision(decision)) return 'enforcement.lead.bot';
	return `enforcement.lead.${rule}`;
}

function decisionFacts(
	ctx: NoticeContext,
	decision: EnforcementDecisionRow,
	evidence: ReadonlyArray<EvidenceMessageLocation>,
): Array<string> {
	const reason = ENFORCEMENT_REASONS[decision.reason_code];
	const community = decision.kind === 'community_removal';
	const facts = [text(ctx, leadKey(decision, reason.rule))];
	if (reason.handling !== 'restricted') {
		const byMonth = reason.handling === 'protected';
		for (const group of groupLocations(ctx, evidence, byMonth)) {
			facts.push(locationLine(ctx, group, community, byMonth));
		}
	}
	if (decision.kind === 'profile_reset') {
		facts.push(text(ctx, 'enforcement.facts.profile', {fields: profileFieldList(ctx, decision.profile_fields)}));
	}
	if (decision.payment_at) {
		facts.push(text(ctx, 'enforcement.facts.payment', {date: formatNoticeDate(decision.payment_at, ctx.locale)}));
	}
	if (decision.rights_holder) {
		facts.push(text(ctx, 'enforcement.facts.copyright', {rights_holder: decision.rights_holder}));
	}
	const guidelinesUrl = getLegalUrls().guidelinesUrl;
	if (reason.rule === 'self_harm' && ctx.hosted && guidelinesUrl) {
		facts.push(text(ctx, 'enforcement.welfare.self_harm', {url: `${guidelinesUrl}#${SELF_HARM_ANCHOR}`}));
	}
	return facts;
}

function howWeDecided(ctx: NoticeContext, decision: EnforcementDecisionRow): Array<string> {
	return [
		text(ctx, `enforcement.source.${decision.source}`),
		text(ctx, `enforcement.automation.${decision.automation}`),
	];
}

function buildDecisionNotice(
	ctx: NoticeContext,
	{decision, evidence, networksBlockedUntil, accountHasEmail}: Extract<EnforcementNoticeSource, {type: 'decision'}>,
): EnforcementNotice {
	const reason = ENFORCEMENT_REASONS[decision.reason_code];
	const target: NoticeTarget = {
		target: isBotDecision(decision) ? 'bot' : 'account',
		bot_name: decision.subject_name ?? '',
	};
	return {
		decision_id: decision.decision_id.toString(),
		kind: decision.kind,
		permanent: decision.kind === 'suspension' && decision.ends_at === null,
		issued_at: decision.created_at.toISOString(),
		ends_at: decision.ends_at?.toISOString() ?? null,
		deletion_at: decision.deletion_at?.toISOString() ?? null,
		summary: decisionSummary(ctx, decision, target),
		effects: decisionEffects(ctx, decision, target, networksBlockedUntil),
		rule: ruleOf(ctx, reason.rule),
		facts: decisionFacts(ctx, decision, evidence),
		statement: reason.handling === 'restricted' ? null : decision.statement,
		how_we_decided: howWeDecided(ctx, decision),
		appeal: appealOf(ctx, decision.notice_sent_at, accountHasEmail),
		redress: redressLines(ctx),
	};
}

function buildLegacyNotice(ctx: NoticeContext, user: User, now: Date): EnforcementNotice {
	const closed = isAccountClosed(user);
	const permanent = !closed && (user.tempBannedUntil === null || isPermanentSuspensionEnd(user.tempBannedUntil, now));
	const target: NoticeTarget = {target: 'account', bot_name: ''};
	const reason = closed ? enforcementReasonOf(user.deletionReasonCode) : null;
	const rule = reason ? ruleOf(ctx, reason.rule) : null;
	const statement = closed && reason?.handling === 'standard' ? user.deletionPublicReason : null;
	const summary = closed
		? text(ctx, 'enforcement.summary.closure', target)
		: permanent
			? text(ctx, 'enforcement.summary.suspension_permanent', target)
			: text(ctx, 'enforcement.summary.suspension', {
					...target,
					ends_at: formatNoticeTimeUtc(user.tempBannedUntil!, ctx.locale),
				});
	const effect = closed
		? text(ctx, 'enforcement.effect.closure', {
				...target,
				deletion_at: user.pendingDeletionAt ? formatNoticeDate(user.pendingDeletionAt, ctx.locale) : '',
			})
		: text(ctx, permanent ? 'enforcement.effect.suspension_permanent' : 'enforcement.effect.suspension', target);
	return {
		decision_id: null,
		kind: closed ? 'closure' : 'suspension',
		permanent,
		issued_at: null,
		ends_at: closed || permanent ? null : (user.tempBannedUntil?.toISOString() ?? null),
		deletion_at: closed ? (user.pendingDeletionAt?.toISOString() ?? null) : null,
		summary,
		effects: [effect],
		rule,
		facts: statement ? [] : [text(ctx, 'enforcement.lead.legacy')],
		statement,
		how_we_decided: [],
		appeal: appealOf(ctx, closed ? user.deletionScheduledAt : null, user.email !== null),
		redress: redressLines(ctx),
	};
}

function noticeContext(locale: string | null): NoticeContext {
	return {
		locale,
		hosted: !Config.instance.selfHosted,
		appealsEmail: resolveContactEmails().appealsEmail,
		productName: getInstanceProductName(),
	};
}

export function describeRule(rule: EnforcementRuleKey, locale: string | null): RuleReference {
	return ruleOf(noticeContext(locale), rule);
}

export function buildEnforcementNotice(
	source: EnforcementNoticeSource,
	locale: string | null,
	now: Date = new Date(),
): EnforcementNotice {
	const ctx = noticeContext(locale);
	return source.type === 'decision' ? buildDecisionNotice(ctx, source) : buildLegacyNotice(ctx, source.user, now);
}

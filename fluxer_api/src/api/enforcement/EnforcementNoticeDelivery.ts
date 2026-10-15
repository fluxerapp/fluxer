// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {trySendAdminNotification} from '@app/api/admin/services/AdminNotification';
import {createApplicationID, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {EnforcementDecisionRow, EnforcementNoticeChannel} from '@app/api/database/types/EnforcementTypes';
import type {EnforcementNoticeRecord} from '@app/api/enforcement/EnforcementDecisionRepository';
import {
	buildEnforcementNotice,
	type EvidenceMessageLocation,
	isBotDecision,
} from '@app/api/enforcement/EnforcementNotice';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {Logger} from '@app/api/Logger';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import {type SystemDmDeps, systemDmBody, trySendSystemDm} from '@app/api/user/services/SystemDmMessage';
import {ENFORCEMENT_REASONS, isAccountKind} from '@fluxer/constants/src/EnforcementReasons';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import type {EmailTemplateVariables, EnforcementNoticeKindLabel} from '@pkgs/email/src/email_i18n/EmailI18nTypes';

interface EnforcementNoticeDeliveryDeps {
	apiContext: ApiContext;
	applicationRepository: ApplicationRepository;
	systemDm: SystemDmDeps;
}

function sendsSystemDm(decision: EnforcementDecisionRow): boolean {
	return !isAccountKind(decision.kind) || isBotDecision(decision);
}

type NoticeFormat = 'email' | 'system_dm';

const MARKDOWN_LINK = /\[([^\]]+)\]\((\S+?)\)/g;

function lineText(line: string, format: NoticeFormat): string {
	return format === 'email' ? line.replace(MARKDOWN_LINK, '$1 ($2)') : line;
}

function joinLines(lines: ReadonlyArray<string>, format: NoticeFormat): string {
	return lines.map((line) => lineText(line, format)).join('\n');
}

function ruleTitle(rule: EnforcementNotice['rule'], format: NoticeFormat): string {
	if (!rule) return '';
	return format === 'system_dm' && rule.url ? `[${rule.title}](${rule.url})` : rule.title;
}

function kindLabel(decision: EnforcementDecisionRow, notice: EnforcementNotice): EnforcementNoticeKindLabel {
	if (isBotDecision(decision)) return 'bot';
	return notice.permanent ? 'ban' : decision.kind;
}

function noticeVariables(
	decision: EnforcementDecisionRow,
	notice: EnforcementNotice,
	username: string,
	format: NoticeFormat,
): EmailTemplateVariables['enforcement_notice'] {
	const {handling} = ENFORCEMENT_REASONS[decision.reason_code];
	return {
		username,
		kind_label: kindLabel(decision, notice),
		neutral: handling === 'restricted' ? 'yes' : 'no',
		summary: lineText(notice.summary, format),
		effects: joinLines(notice.effects, format),
		rule_title: ruleTitle(notice.rule, format),
		rule_url: format === 'email' ? (notice.rule?.url ?? null) : null,
		facts: joinLines(notice.facts, format),
		statement: notice.statement,
		how_we_decided: joinLines(notice.how_we_decided, format),
		appeals_email: notice.appeal?.email ?? null,
		appeal_deadline: notice.appeal?.deadline ?? '',
		redress: notice.redress.length > 0 ? joinLines(notice.redress, format) : null,
		decision_id: decision.decision_id.toString(),
	};
}

function renderNotice(variables: EmailTemplateVariables['enforcement_notice'], locale: string | null) {
	return getEmailTemplate('enforcement_notice', locale, variables, getInstanceProductName());
}

export function renderNoticeText(
	decision: EnforcementDecisionRow,
	notice: EnforcementNotice,
	username: string,
	locale: string | null,
): string | null {
	const rendered = renderNotice(noticeVariables(decision, notice, username, 'system_dm'), locale);
	return rendered.ok ? systemDmBody(rendered.value.body) : null;
}

export class EnforcementNoticeDelivery {
	constructor(private readonly deps: EnforcementNoticeDeliveryDeps) {}

	async findRecipient(decision: EnforcementDecisionRow): Promise<User | null> {
		const {users} = this.deps.apiContext.services;
		if (decision.recipient_user_id === null) return null;
		return users.findUnique(createUserID(decision.recipient_user_id));
	}

	async findBotOwner(botUserId: UserID): Promise<UserID | null> {
		const application = await this.deps.applicationRepository.getApplication(createApplicationID(BigInt(botUserId)));
		return application?.botUserId === botUserId ? application.ownerUserId : null;
	}

	async deliver(
		decision: EnforcementDecisionRow,
		evidence: ReadonlyArray<EvidenceMessageLocation>,
		networksBlockedUntil: Date | null,
	): Promise<EnforcementNoticeRecord | null> {
		const recipient = await this.findRecipient(decision);
		const email = (await getInstanceConfigRepository().getEffectiveEmailConfig()).enabled ? recipient?.email : null;
		const systemDm = sendsSystemDm(decision);
		if (!recipient || (!email && !systemDm)) {
			return {status: 'undelivered', channels: new Set(), locale: recipient?.locale ?? null, text: null, sentAt: null};
		}
		const sentAt = new Date();
		const notice = buildEnforcementNotice(
			{
				type: 'decision',
				decision: {...decision, notice_sent_at: sentAt},
				evidence,
				networksBlockedUntil,
				accountHasEmail: Boolean(email),
			},
			recipient.locale,
		);
		const variables = noticeVariables(decision, notice, recipient.username, 'email');
		const rendered = renderNotice(variables, recipient.locale);
		const dmText = renderNoticeText(decision, notice, recipient.username, recipient.locale);
		if (!rendered.ok || dmText === null) {
			Logger.error(
				{decisionId: decision.decision_id.toString(), error: rendered.ok ? null : rendered.error},
				'Failed to render an enforcement notice',
			);
			return null;
		}
		const channels = new Set<EnforcementNoticeChannel>();
		const targetId = decision.decision_id.toString();
		if (
			email &&
			(await trySendAdminNotification(
				() => this.deps.apiContext.services.email.sendEnforcementNoticeEmail(email, variables, recipient.locale),
				{action: 'enforcement_notice', targetId},
			))
		) {
			channels.add('email');
		}
		if (systemDm && (await this.sendSystemDm(recipient, dmText))) {
			channels.add('system_dm');
		}
		if (channels.size === 0) return null;
		return {
			status: 'sent',
			channels,
			locale: recipient.locale,
			text: channels.has('email') ? rendered.value.body : dmText,
			sentAt,
		};
	}

	async deliverEnded(
		decision: EnforcementDecisionRow,
		outcome: EmailTemplateVariables['enforcement_ended']['outcome'],
		inForce: boolean,
	): Promise<void> {
		const recipient = await this.findRecipient(decision);
		if (!recipient) return;
		const variables: EmailTemplateVariables['enforcement_ended'] = {
			username: recipient.username,
			outcome,
			decision_id: decision.decision_id.toString(),
			restores_access: inForce && isAccountKind(decision.kind) ? 'yes' : 'no',
			statement: decision.end_statement,
			content_removed:
				decision.kind === 'community_removal' ||
				(decision.removed_message_count ?? 0) > 0 ||
				decision.removed_all_messages
					? 'yes'
					: 'no',
		};
		await this.sendEnded(recipient, variables, sendsSystemDm(decision));
	}

	async sendLegacyEnded(recipient: User): Promise<void> {
		await this.sendEnded(
			recipient,
			{
				username: recipient.username,
				outcome: 'lifted',
				decision_id: null,
				restores_access: 'yes',
				statement: null,
				content_removed: 'no',
			},
			false,
		);
	}

	private async sendEnded(
		recipient: User,
		variables: EmailTemplateVariables['enforcement_ended'],
		systemDm: boolean,
	): Promise<void> {
		const targetId = variables.decision_id ?? recipient.id.toString();
		const email = recipient.email;
		if (email) {
			await trySendAdminNotification(
				() => this.deps.apiContext.services.email.sendEnforcementEndedEmail(email, variables, recipient.locale),
				{action: 'enforcement_ended', targetId},
			);
		}
		if (!systemDm) return;
		const rendered = getEmailTemplate('enforcement_ended', recipient.locale, variables, getInstanceProductName());
		if (rendered.ok) {
			await this.sendSystemDm(recipient, systemDmBody(rendered.value.body));
		}
	}

	private sendSystemDm(recipient: User, content: string): Promise<boolean> {
		return trySendSystemDm(this.deps.systemDm, this.deps.apiContext.services.users, recipient.id, content);
	}
}

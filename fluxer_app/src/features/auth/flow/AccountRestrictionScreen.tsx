// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	NETWORK_BLOCK_LEAD_DESCRIPTOR,
	NETWORK_BLOCK_LEAD_WITH_REASON_DESCRIPTOR,
	NETWORK_BLOCKED_DESCRIPTOR,
	SHARED_NETWORK_DESCRIPTOR,
} from '@app/features/auth/AuthMessageDescriptors';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/flow/AccountRestrictionScreen.module.css';
import {useAuthPresentation} from '@app/features/auth/flow/useAuthPresentation';
import {type StepTitleChangeHandler, useReportStepTitle} from '@app/features/auth/flow/useReportStepTitle';
import type {AccountRestriction} from '@app/features/auth/state/AccountRestriction';
import Authentication from '@app/features/auth/state/Authentication';
import {AuthCardVariant} from '@app/features/auth/state/AuthLayoutContext';
import {BACK_TO_SIGN_IN_DESCRIPTOR, TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Button} from '@app/features/ui/button/Button';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import type {EnforcementNotice, NetworkBlock} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useEffect} from 'react';

const ACCOUNT_SUSPENDED_DESCRIPTOR = msg({
	message: 'Account suspended',
	comment: 'Title of the sign-in screen for an account suspended until a set date.',
});
const ACCOUNT_BANNED_DESCRIPTOR = msg({
	message: 'Account banned',
	comment: 'Title of the sign-in screen for an account suspended with no end date.',
});
const ACCOUNT_CLOSED_DESCRIPTOR = msg({
	message: 'Account closed',
	comment: 'Title of the sign-in screen for an account the team closed and will delete.',
});
const WHAT_HAPPENED_DESCRIPTOR = msg({
	message: 'What happened',
	comment: 'Heading on the account restriction screen above what the team did and what it means.',
});
const WHY_DESCRIPTOR = msg({
	message: 'Why',
	comment: 'Heading on the account restriction screen above the rule that was broken and the findings.',
});
const HOW_WE_DECIDED_DESCRIPTOR = msg({
	message: 'How we decided',
	comment: 'Heading on the account restriction screen above how the case reached the team and who decided it.',
});
const HOW_TO_APPEAL_DESCRIPTOR = msg({
	message: 'How to appeal',
	comment: 'Heading on the account restriction screen above the appeal instructions.',
});
const NOTE_FROM_OUR_TEAM_DESCRIPTOR = msg({
	message: 'Note from our team: {statement}',
	comment: 'Line on the account restriction screen with a note the team wrote to the account holder.',
});
const DECISION_ID_DESCRIPTOR = msg({
	message: 'Decision ID: {decisionId}',
	comment: 'Reference line on the account restriction screen. People quote this ID when they appeal.',
});
const RULE_DESCRIPTOR = msg({
	message: 'Rule: {title}',
	comment: 'Names the rule behind a restriction when the instance publishes no link for it. title is the rule title.',
});
const NETWORK_BLOCK_ENDS_DESCRIPTOR = msg({
	message: 'The block ends on {date}.',
	comment: 'Line on the network blocked screen for a block with an end. date is a date and time.',
});
const MARKDOWN_LINK = /\[([^\]]+)\]\((https?:\/\/[^\s)]+)\)/g;

function resolveNoticeTitle(notice: EnforcementNotice) {
	if (notice.kind === 'closure') return ACCOUNT_CLOSED_DESCRIPTOR;
	return notice.permanent ? ACCOUNT_BANNED_DESCRIPTOR : ACCOUNT_SUSPENDED_DESCRIPTOR;
}

function mailtoHref(email: string): string {
	return `mailto:${email}`;
}

function RuleLine({rule}: {rule: NonNullable<EnforcementNotice['rule']>}) {
	const {i18n} = useLingui();
	const {title, url} = rule;
	if (url == null) {
		return (
			<p className={styles.text} data-flx="auth.flow.account-restriction-screen.rule-line.text">
				{i18n._(RULE_DESCRIPTOR, {title})}
			</p>
		);
	}
	return (
		<p className={styles.text} data-flx="auth.flow.account-restriction-screen.rule-line.text--2">
			<Trans comment="Names the rule behind a restriction, with the rule title linked to the rule. title is the rule title.">
				Rule:{' '}
				<ExternalLink href={url} data-flx="auth.flow.account-restriction-screen.rule-line.external-link">
					{title}
				</ExternalLink>
			</Trans>
		</p>
	);
}

function AppealSentence({appeal}: {appeal: NonNullable<EnforcementNotice['appeal']>}) {
	const {email} = appeal;
	const deadline = appeal.deadline?.replace(/\.$/, '');
	const href = mailtoHref(email);
	if (deadline == null) {
		return (
			<Trans comment="Appeal line for a restriction from before the team recorded decisions, with no appeal deadline. The link is the appeals email address.">
				If you think we got this wrong, email{' '}
				<ExternalLink href={href} data-flx="auth.flow.account-restriction-screen.appeal-sentence.external-link">
					{email}
				</ExternalLink>{' '}
				and include your username.
			</Trans>
		);
	}
	if (!appeal.account_has_email) {
		return (
			<Trans comment="Appeal line for an account with no email address. The link is the appeals email address and deadline is a date.">
				If you think we got this wrong, email{' '}
				<ExternalLink href={href} data-flx="auth.flow.account-restriction-screen.appeal-sentence.external-link--2">
					{email}
				</ExternalLink>{' '}
				by {deadline}. Include the decision ID and your username. Appeals are free, and a person reviews each one.
			</Trans>
		);
	}
	return (
		<Trans comment="Appeal line on the account restriction screen. The link is the appeals email address and deadline is a date.">
			If you think we got this wrong, email{' '}
			<ExternalLink href={href} data-flx="auth.flow.account-restriction-screen.appeal-sentence.external-link--3">
				{email}
			</ExternalLink>{' '}
			by {deadline}. Write from the email address on your account if you can, and include the decision ID. Appeals are
			free, and a person reviews each one.
		</Trans>
	);
}

function Section({heading, children}: {heading: string; children: ReactNode}) {
	return (
		<section className={styles.section} data-flx="auth.flow.account-restriction-screen.section">
			<h2 className={styles.heading} data-flx="auth.flow.account-restriction-screen.section.heading">
				{heading}
			</h2>
			{children}
		</section>
	);
}

function LineText({line}: {line: string}) {
	const parts: Array<ReactNode> = [];
	let index = 0;
	for (const match of line.matchAll(MARKDOWN_LINK)) {
		parts.push(line.slice(index, match.index));
		parts.push(
			<ExternalLink key={match.index} href={match[2]} data-flx="auth.flow.account-restriction-screen.line-text.link">
				{match[1]}
			</ExternalLink>,
		);
		index = match.index + match[0].length;
	}
	parts.push(line.slice(index));
	return parts;
}

function Lines({lines, className = styles.text}: {lines: ReadonlyArray<string>; className?: string}) {
	return lines.map((line) => (
		<p key={line} className={className} data-flx="auth.flow.account-restriction-screen.lines.text">
			<LineText line={line} data-flx="auth.flow.account-restriction-screen.lines.line-text" />
		</p>
	));
}

function ScreenRoot({children, footer}: {children: ReactNode; footer: ReactNode}) {
	const {i18n} = useLingui();
	return (
		<div
			className={clsx(styles.container, styles.selectable, isMobileExperienceEnabled() && styles.surface)}
			dir={getLocaleDirection(i18n.locale)}
			data-flx="auth.flow.account-restriction-screen.root"
		>
			{children}
			<div className={styles.footer} data-flx="auth.flow.account-restriction-screen.footer">
				{footer}
			</div>
		</div>
	);
}

function NoticeView({
	notice,
	showTitle,
	onBackToSignIn,
}: {
	notice: EnforcementNotice;
	showTitle: boolean;
	onBackToSignIn: () => void;
}) {
	const {i18n} = useLingui();
	const showWhy = notice.rule != null || notice.facts.length > 0 || notice.statement != null;
	return (
		<ScreenRoot
			footer={
				<Button
					fitContainer
					onClick={onBackToSignIn}
					data-flx="auth.flow.account-restriction-screen.notice.button.back"
				>
					{i18n._(BACK_TO_SIGN_IN_DESCRIPTOR)}
				</Button>
			}
			data-flx="auth.flow.account-restriction-screen.notice-view.screen-root"
		>
			{showTitle ? (
				<h1 className={styles.title} data-flx="auth.flow.account-restriction-screen.notice.title">
					{i18n._(resolveNoticeTitle(notice))}
				</h1>
			) : null}
			<Section
				heading={i18n._(WHAT_HAPPENED_DESCRIPTOR)}
				data-flx="auth.flow.account-restriction-screen.notice-view.section"
			>
				<Lines
					lines={[notice.summary]}
					className={styles.summary}
					data-flx="auth.flow.account-restriction-screen.notice-view.summary"
				/>
				<Lines lines={notice.effects} data-flx="auth.flow.account-restriction-screen.notice-view.lines" />
			</Section>
			{showWhy ? (
				<Section
					heading={i18n._(WHY_DESCRIPTOR)}
					data-flx="auth.flow.account-restriction-screen.notice-view.section--2"
				>
					{notice.rule == null ? null : (
						<RuleLine rule={notice.rule} data-flx="auth.flow.account-restriction-screen.notice-view.rule-line" />
					)}
					<Lines lines={notice.facts} data-flx="auth.flow.account-restriction-screen.notice-view.lines--2" />
					{notice.statement == null ? null : (
						<p className={styles.text} data-flx="auth.flow.account-restriction-screen.notice.statement">
							{i18n._(NOTE_FROM_OUR_TEAM_DESCRIPTOR, {statement: notice.statement})}
						</p>
					)}
				</Section>
			) : null}
			{notice.how_we_decided.length > 0 ? (
				<Section
					heading={i18n._(HOW_WE_DECIDED_DESCRIPTOR)}
					data-flx="auth.flow.account-restriction-screen.notice-view.section--3"
				>
					<Lines lines={notice.how_we_decided} data-flx="auth.flow.account-restriction-screen.notice-view.lines--3" />
				</Section>
			) : null}
			{notice.appeal == null ? null : (
				<Section
					heading={i18n._(HOW_TO_APPEAL_DESCRIPTOR)}
					data-flx="auth.flow.account-restriction-screen.notice-view.section--4"
				>
					<p className={styles.text} data-flx="auth.flow.account-restriction-screen.notice.appeal">
						<AppealSentence
							appeal={notice.appeal}
							data-flx="auth.flow.account-restriction-screen.notice-view.appeal-sentence"
						/>
					</p>
					<Lines
						lines={notice.redress}
						className={styles.fineText}
						data-flx="auth.flow.account-restriction-screen.notice-view.lines--4"
					/>
				</Section>
			)}
			{notice.decision_id == null ? null : (
				<p className={styles.reference} data-flx="auth.flow.account-restriction-screen.notice.reference">
					{i18n._(DECISION_ID_DESCRIPTOR, {decisionId: notice.decision_id})}
				</p>
			)}
		</ScreenRoot>
	);
}

export function NetworkBlockDetails({block}: {block: NetworkBlock}) {
	const {i18n} = useLingui();
	const {appeal_email: email, ip_address: ip, reason, expires_at: expiresAt, product_name: productName} = block;
	return (
		<>
			<p className={styles.text} data-flx="auth.flow.account-restriction-screen.network-block-details.lead">
				{i18n._(reason == null ? NETWORK_BLOCK_LEAD_DESCRIPTOR : NETWORK_BLOCK_LEAD_WITH_REASON_DESCRIPTOR, {
					productName,
				})}
			</p>
			{reason == null ? null : (
				<RuleLine rule={reason} data-flx="auth.flow.account-restriction-screen.network-block-details.rule-line" />
			)}
			{expiresAt == null ? null : (
				<p className={styles.text} data-flx="auth.flow.account-restriction-screen.network-block-details.ends">
					{i18n._(NETWORK_BLOCK_ENDS_DESCRIPTOR, {
						date: `${getDateFormatter(i18n.locale, {dateStyle: 'medium', timeStyle: 'short', timeZone: 'UTC'}).format(new Date(expiresAt))} UTC`,
					})}
				</p>
			)}
			<p className={styles.text} data-flx="auth.flow.account-restriction-screen.network-block-details.shared">
				{i18n._(SHARED_NETWORK_DESCRIPTOR)}
			</p>
			{email == null ? null : (
				<p className={styles.text} data-flx="auth.flow.account-restriction-screen.network-block-details.appeal">
					<Trans comment="Line on the network blocked screen. The link is the contact email address and ip is the blocked IP address.">
						If you think this block is a mistake, email{' '}
						<ExternalLink
							href={mailtoHref(email)}
							data-flx="auth.flow.account-restriction-screen.network-block-details.external-link"
						>
							{email}
						</ExternalLink>{' '}
						and include your IP address, {ip}.
					</Trans>
				</p>
			)}
		</>
	);
}

interface AccountRestrictionScreenProps {
	restriction: AccountRestriction;
	onBackToSignIn: () => void;
	onTryAgain: () => void;
	onStepTitleChange?: StepTitleChangeHandler | null;
}

export function AccountRestrictionScreen({
	restriction,
	onBackToSignIn,
	onTryAgain,
	onStepTitleChange = null,
}: AccountRestrictionScreenProps) {
	const {i18n} = useLingui();
	useAuthPresentation({variant: AuthCardVariant.STANDARD});
	useReportStepTitle(
		onStepTitleChange,
		i18n._(restriction.type === 'notice' ? resolveNoticeTitle(restriction.notice) : NETWORK_BLOCKED_DESCRIPTOR),
	);
	const showTitle = onStepTitleChange == null;
	if (restriction.type === 'notice') {
		return (
			<NoticeView
				notice={restriction.notice}
				showTitle={showTitle}
				onBackToSignIn={onBackToSignIn}
				data-flx="auth.flow.account-restriction-screen.notice-view"
			/>
		);
	}
	return (
		<ScreenRoot
			footer={
				<Button
					fitContainer
					onClick={onTryAgain}
					data-flx="auth.flow.account-restriction-screen.network-blocked.button"
				>
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
			}
			data-flx="auth.flow.account-restriction-screen.network-blocked"
		>
			{showTitle ? (
				<h1 className={styles.title} data-flx="auth.flow.account-restriction-screen.network-blocked.title">
					{i18n._(NETWORK_BLOCKED_DESCRIPTOR)}
				</h1>
			) : null}
			<NetworkBlockDetails
				block={restriction.block}
				data-flx="auth.flow.account-restriction-screen.network-block-details"
			/>
		</ScreenRoot>
	);
}

export const AccountRestrictionGate = observer(function AccountRestrictionGate({children}: {children: ReactNode}) {
	const location = useLocation();
	const restriction = Authentication.accountRestriction;
	useEffect(() => AuthenticationCommands.clearAccountRestriction, []);
	if (restriction == null) {
		return children;
	}
	const handleBackToSignIn = () => {
		AuthenticationCommands.clearAccountRestriction();
		if (!location.pathname.endsWith(Routes.LOGIN)) {
			RouterUtils.replaceWith(Routes.LOGIN);
		}
	};
	return (
		<AccountRestrictionScreen
			restriction={restriction}
			onBackToSignIn={handleBackToSignIn}
			onTryAgain={AuthenticationCommands.clearAccountRestriction}
			data-flx="auth.flow.account-restriction-screen.account-restriction-gate.account-restriction-screen"
		/>
	);
});

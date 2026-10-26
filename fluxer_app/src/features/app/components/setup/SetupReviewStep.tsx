// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/setup/SelfHostedSetupWizardGate.module.css';
import type {BrandingAssetState} from '@app/features/app/components/setup/SetupBrandingDraft';
import {DEFAULT_THEME_COLOR} from '@app/features/app/components/setup/SetupBrandingFields';
import type {FinishDraft} from '@app/features/app/components/setup/SetupPolicySteps';
import type {WizardStep} from '@app/features/app/components/setup/SetupWizardStateMachine';
import {StepHeader, SUMMARY_DESCRIPTORS, TextButton} from '@app/features/app/components/setup/SetupWizardSteps';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {int2hex} from '@app/features/theme/utils/ColorUtils';
import Users from '@app/features/user/state/Users';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const REVIEW_TITLE_DESCRIPTOR = msg({
	message: 'Check your choices',
	comment: 'Setup wizard title for the review screen before finishing.',
});
const REVIEW_BODY_DESCRIPTOR = msg({
	message: "How people sign in can't change after setup. You can change everything else later in the admin panel.",
	comment: 'Setup wizard body on the review screen.',
});
const REVIEW_CHANGE_DESCRIPTOR = msg({
	message: 'Change',
	comment: 'Link on the setup wizard review screen that goes back to one answer.',
});
const REVIEW_CHANGE_LABEL_DESCRIPTOR = msg({
	message: 'Change {row}',
	comment: 'Accessible label of a review link. row is the name of the answer, such as Name or Direct messages.',
});
const FINISH_ERROR_DESCRIPTOR = msg({
	message: "Setup didn't finish. {error}",
	comment: 'Alert when saving the setup fails. error is the reason from the server.',
});

interface ReviewStepProps {
	config: Pick<InstanceConfigResponse, 'account_identity' | 'integrations'> | null;
	finishDraft: FinishDraft;
	productName: string;
	themeColor: number | null;
	assets: ReadonlyArray<BrandingAssetState>;
	attachmentsEnabled: boolean;
	accountUsername: string;
	error: string | null;
	onChange: (step: WizardStep) => void;
	onEmailChange: () => void;
}

interface ReviewRow {
	key: string;
	label: string;
	value: React.ReactNode;
	onChange?: () => void;
}

function reviewRows(
	{
		config,
		finishDraft,
		productName,
		themeColor,
		assets,
		attachmentsEnabled,
		accountUsername,
		onChange,
		onEmailChange,
	}: ReviewStepProps,
	i18n: I18n,
): ReadonlyArray<ReviewRow> {
	if (!config) return [];
	const t = (descriptor: MessageDescriptor, values?: Record<string, unknown>) => i18n._(descriptor, values);
	const usernameMode = config.account_identity.mode === AccountIdentityModes.USERNAME;
	const logo = assets.find((asset) => asset.kind === 'logo');
	const logoSource = logo?.preview ?? logo?.url ?? null;
	const emailService = config.integrations.email;
	const signInLabel = t(SUMMARY_DESCRIPTORS.signIn);
	let signInDescriptor = SUMMARY_DESCRIPTORS.emailAddressesUnique;
	if (usernameMode) signInDescriptor = SUMMARY_DESCRIPTORS.usernames;
	else if (config.account_identity.tag_style === TagStyles.RANDOM)
		signInDescriptor = SUMMARY_DESCRIPTORS.emailAddressesShared;
	const registrationSummary = {
		closed: SUMMARY_DESCRIPTORS.registrationClosed,
		approval: SUMMARY_DESCRIPTORS.registrationApproval,
		open: SUMMARY_DESCRIPTORS.registrationOpen,
	}[finishDraft.registrationMode];
	const rows: Array<ReviewRow> = [
		{
			key: 'sign_in',
			label: signInLabel,
			value: t(signInDescriptor),
		},
		{
			key: 'account',
			label: t(SUMMARY_DESCRIPTORS.account),
			value: (
				<bdi data-flx="app.setup.self-hosted-setup-wizard-gate.review-rows.bdi">
					{`@${Users.getCurrentUser()?.username ?? accountUsername}`}
				</bdi>
			),
		},
	];
	if (!usernameMode) {
		const label = t(SUMMARY_DESCRIPTORS.email);
		let value: string;
		if (RuntimeConfig.emailsEnabled) value = t(SUMMARY_DESCRIPTORS.emailAlreadySet);
		else if (emailService.effective_enabled) {
			value = t(SUMMARY_DESCRIPTORS.emailFrom, {
				host: emailService.smtp.host ?? '',
				from: emailService.from_email ?? '',
			});
		} else value = t(SUMMARY_DESCRIPTORS.emailLater);
		rows.push({key: 'email', label, value, onChange: RuntimeConfig.emailsEnabled ? undefined : onEmailChange});
	}
	rows.push(
		{key: 'name', label: t(SUMMARY_DESCRIPTORS.name), value: productName.trim(), onChange: () => onChange('name')},
		{
			key: 'look',
			label: t(SUMMARY_DESCRIPTORS.look),
			value: (
				<span
					className={styles.lookSummary}
					data-flx="app.setup.self-hosted-setup-wizard-gate.review-rows.look-summary"
				>
					<span
						className={styles.colorSwatch}
						style={{background: int2hex(themeColor ?? DEFAULT_THEME_COLOR)}}
						aria-hidden="true"
						data-flx="app.setup.self-hosted-setup-wizard-gate.review-rows.color-swatch"
					/>
					<span data-flx="app.setup.self-hosted-setup-wizard-gate.review-rows.span">
						{themeColor === null ? t(SUMMARY_DESCRIPTORS.defaultColor) : int2hex(themeColor)}
					</span>
					{logoSource && (
						<img
							className={styles.logoThumb}
							src={logoSource}
							alt=""
							data-flx="app.setup.self-hosted-setup-wizard-gate.review-rows.logo-thumb"
						/>
					)}
				</span>
			),
			onChange: () => onChange('look'),
		},
		{
			key: 'communities',
			label: t(SUMMARY_DESCRIPTORS.communities),
			value: finishDraft.singleCommunityEnabled
				? t(SUMMARY_DESCRIPTORS.singleCommunity, {name: finishDraft.singleCommunityName.trim()})
				: t(SUMMARY_DESCRIPTORS.manyCommunities),
			onChange: () => onChange('communities'),
		},
		{
			key: 'direct_messages',
			label: t(SUMMARY_DESCRIPTORS.directMessages),
			value: t(finishDraft.directMessagesDisabled ? SUMMARY_DESCRIPTORS.off : SUMMARY_DESCRIPTORS.allowed),
			onChange: () => onChange('direct_messages'),
		},
		{
			key: 'personal_notes',
			label: t(SUMMARY_DESCRIPTORS.personalNotes),
			value: t(finishDraft.personalNotesEnabled ? SUMMARY_DESCRIPTORS.on : SUMMARY_DESCRIPTORS.off),
			onChange: () => onChange('personal_notes'),
		},
		{
			key: 'registration',
			label: t(SUMMARY_DESCRIPTORS.registration),
			value: t(registrationSummary),
			onChange: () => onChange('registration'),
		},
		{
			key: 'push',
			label: t(SUMMARY_DESCRIPTORS.push),
			value: t(finishDraft.pushRelayConsentAccepted ? SUMMARY_DESCRIPTORS.pushOn : SUMMARY_DESCRIPTORS.pushOff),
			onChange: () => onChange('push_relay'),
		},
		{
			key: 'gif',
			label: t(SUMMARY_DESCRIPTORS.gif),
			value: t(config.integrations.gif.klipy_api_key_set ? SUMMARY_DESCRIPTORS.on : SUMMARY_DESCRIPTORS.skipped),
			onChange: () => onChange('gif_youtube'),
		},
		{
			key: 'youtube',
			label: t(SUMMARY_DESCRIPTORS.youtube),
			value: t(config.integrations.youtube.api_key_set ? SUMMARY_DESCRIPTORS.on : SUMMARY_DESCRIPTORS.skipped),
			onChange: () => onChange('gif_youtube'),
		},
		{
			key: 'attachments',
			label: t(SUMMARY_DESCRIPTORS.attachments),
			value: t(attachmentsEnabled ? SUMMARY_DESCRIPTORS.deleteOld : SUMMARY_DESCRIPTORS.keepAll),
			onChange: () => onChange('attachments'),
		},
	);
	return rows;
}

export const ReviewStep = observer((props: ReviewStepProps) => {
	const {i18n} = useLingui();
	const {error} = props;
	const rows = reviewRows(props, i18n);
	return (
		<section className={styles.step} data-flx="app.self-hosted-setup-wizard-gate.review-step">
			<StepHeader
				title={i18n._(REVIEW_TITLE_DESCRIPTOR)}
				body={i18n._(REVIEW_BODY_DESCRIPTOR)}
				data-flx="app.setup.setup-wizard-steps.review-step.step-header"
			/>
			<dl className={styles.summary} data-flx="app.self-hosted-setup-wizard-gate.summary">
				{rows.map((row) => (
					<div key={row.key} className={styles.summaryRow} data-flx="app.self-hosted-setup-wizard-gate.summary-row">
						<dt className={styles.summaryLabel} data-flx="app.self-hosted-setup-wizard-gate.summary-label">
							{row.label}
						</dt>
						<dd className={styles.summaryValue} data-flx="app.self-hosted-setup-wizard-gate.summary-value">
							<span
								className={styles.summaryValueText}
								data-flx="app.setup.setup-wizard-steps.review-step.summary-value-text"
							>
								{row.value}
							</span>
							{row.onChange && (
								<TextButton
									onClick={row.onChange}
									ariaLabel={i18n._(REVIEW_CHANGE_LABEL_DESCRIPTOR, {row: row.label})}
									data-flx="app.setup.setup-wizard-steps.review-step.text-button.change"
								>
									{i18n._(REVIEW_CHANGE_DESCRIPTOR)}
								</TextButton>
							)}
						</dd>
					</div>
				))}
			</dl>
			{error && (
				<p className={styles.alert} role="alert" data-flx="app.self-hosted-setup-wizard-gate.finish-error">
					{i18n._(FINISH_ERROR_DESCRIPTOR, {error})}
				</p>
			)}
		</section>
	);
});

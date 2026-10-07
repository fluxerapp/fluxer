// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {EXAMPLE_INVITE_CODE, EXAMPLE_REPORT_USER_TAG} from '@app/features/app/config/I18nDisplayConstants';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import type {FormValues, ReportField, ReportType} from '@app/features/moderation/components/report/ReportTypes';
import {ReportFlowBanner} from '@app/features/moderation/components/report_flow/ReportFlowBanner';
import {REPORT_CATEGORY_DESCRIPTOR} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import {ReportFlowAnswerList} from '@app/features/moderation/components/report_flow/ReportFlowSummary';
import {Button} from '@app/features/ui/button/Button';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {ReportFlowResponse, ReportFlowStep} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useEffect, useRef} from 'react';

const MESSAGE_LINK_DESCRIPTOR = msg({
	message: 'Message link',
	comment:
		'Field label on the details step of the DSA report form (message report). The reporter pastes a direct link to the offending message.',
});
const REPORTED_USER_TAG_OPTIONAL_DESCRIPTOR = msg({
	message: 'Reported user tag (optional)',
	comment:
		'Field label on the details step of the DSA report form (message report). Optional FluxerTag of the user who sent the message.',
});
const USER_ID_OPTIONAL_DESCRIPTOR = msg({
	message: 'User ID (optional)',
	comment:
		'Field label on the details step of the DSA report form (user report). Optional Fluxer user snowflake ID. "ID" is conventional.',
});
const USER_TAG_OPTIONAL_DESCRIPTOR = msg({
	message: 'User tag (optional)',
	comment:
		'Field label on the details step of the DSA report form (user report). Optional FluxerTag of the reported user.',
});
const COMMUNITY_ID_DESCRIPTOR = msg({
	message: 'Community ID',
	comment:
		"Field label on the details step of the DSA report form (community report). The reported community's snowflake ID.",
});
const INVITE_CODE_OPTIONAL_DESCRIPTOR = msg({
	message: 'Invite code (optional)',
	comment:
		'Field label on the details step of the DSA report form (community report). Optional invite code to the reported community.',
});
const EXPLAIN_THE_PROBLEM_DESCRIPTOR = msg({
	message: 'Explain the problem',
	comment:
		'Field label on the details step of the DSA report form. Required free text where the reporter explains what is wrong with the reported content.',
});
const EXPLAIN_THE_PROBLEM_HELPER_DESCRIPTOR = msg({
	message: 'If you believe it is illegal, say which law and why.',
	comment: 'Helper text under the Explain the problem field on the details step of the DSA report form.',
});
const FULL_LEGAL_NAME_OPTIONAL_FOR_CSAM_DESCRIPTOR = msg({
	message: 'Full legal name (optional for child sexual abuse reports)',
	comment:
		'Field label on the details step of the DSA report form. Legal name of the reporter for the formal declaration. The law lets people who report child sexual abuse material leave it empty. Keep the tone neutral.',
});
const FIRST_AND_LAST_NAME_DESCRIPTOR = msg({
	message: 'First and last name',
	comment: 'Placeholder in the Full legal name input on the DSA report form.',
});
const COUNTRY_OF_RESIDENCE_DESCRIPTOR = msg({
	message: 'Country of residence',
	comment:
		'Field label on the details step of the DSA report form. The country where the reporter resides, used for the legal declaration.',
});
const GOOD_FAITH_STATEMENT_DESCRIPTOR = msg({
	message: 'I confirm in good faith that the information and allegations in this notice are accurate and complete.',
	comment:
		'Required checkbox on the details step of the DSA report form. A formal statement the law asks every reporter to make. Keep the tone formal.',
});
const CHOOSE_THE_REASON_AGAIN_DESCRIPTOR = msg({
	message: 'Choose the reason again',
	comment:
		'Button on the details step of the DSA report form, shown when the server no longer accepts the chosen reason. Reloads the questions and returns to the reason step.',
});

interface Props {
	selectedType: ReportType;
	formValues: FormValues;
	flow: ReportFlowResponse;
	steps: ReadonlyArray<ReportFlowStep>;
	urgent: boolean;
	goodFaithConfirmed: boolean;
	countryOptions: Array<ComboboxOption<string>>;
	fieldErrors: Partial<Record<ReportField, string>>;
	errorMessage: string | null;
	answersRejected: boolean;
	canSubmit: boolean;
	isSubmitting: boolean;
	onFieldChange: (field: keyof FormValues, value: string) => void;
	onGoodFaithChange: (value: boolean) => void;
	onChooseReasonAgain: () => void;
	onSubmit: () => void;
	onStartOver: () => void;
	onBack: () => void;
	messageLinkOk: boolean;
	userTargetOk: boolean;
	guildTargetOk: boolean;
}

export const ReportStepDetails: React.FC<Props> = ({
	selectedType,
	formValues,
	flow,
	steps,
	urgent,
	goodFaithConfirmed,
	countryOptions,
	fieldErrors,
	errorMessage,
	answersRejected,
	canSubmit,
	isSubmitting,
	onFieldChange,
	onGoodFaithChange,
	onChooseReasonAgain,
	onSubmit,
	onStartOver,
	onBack,
	messageLinkOk,
	userTargetOk,
	guildTargetOk,
}) => {
	const {i18n} = useLingui();
	const hasFieldErrors = Object.values(fieldErrors).some((value) => Boolean(value));
	const showGeneralError = Boolean(errorMessage && (answersRejected || !hasFieldErrors));
	const errorBoxRef = useRef<HTMLDivElement>(null);
	useEffect(() => {
		if (!showGeneralError) return;
		errorBoxRef.current?.scrollIntoView({
			block: 'nearest',
			behavior: Accessibility.useSmoothScrolling ? 'smooth' : 'auto',
		});
	}, [showGeneralError, errorMessage]);
	return (
		<div className={styles.card} data-flx="moderation.report.report-step-details.card">
			<header className={styles.cardHeader} data-flx="moderation.report.report-step-details.card-header">
				<p className={styles.eyebrow} data-flx="moderation.report.report-step-details.eyebrow">
					<Trans>Step 5</Trans>
				</p>
				<h1 className={styles.title} data-flx="moderation.report.report-step-details.title">
					<Trans>Report details</Trans>
				</h1>
				<p className={styles.description} data-flx="moderation.report.report-step-details.description">
					<Trans>Share what our team needs to assess this.</Trans>
				</p>
			</header>
			<div className={styles.cardBody} data-flx="moderation.report.report-step-details.card-body">
				{showGeneralError && (
					<div
						ref={errorBoxRef}
						className={styles.errorBox}
						role="alert"
						aria-live="polite"
						data-flx="moderation.report.report-step-details.error-box"
					>
						{errorMessage}
						{answersRejected && (
							<div className={styles.actionRow} data-flx="moderation.report.report-step-details.action-row--2">
								<Button
									variant="secondary"
									small
									fitContent
									type="button"
									onClick={onChooseReasonAgain}
									disabled={isSubmitting}
									data-flx="moderation.report.report-step-details.button.choose-reason-again"
								>
									{i18n._(CHOOSE_THE_REASON_AGAIN_DESCRIPTOR)}
								</Button>
							</div>
						)}
					</div>
				)}
				{urgent && <ReportFlowBanner data-flx="moderation.report.report-step-details.report-flow-banner" />}
				<section className={styles.cardHeader} data-flx="moderation.report.report-step-details.answers">
					<h2 className={styles.eyebrow} data-flx="moderation.report.report-step-details.answers-heading">
						{i18n._(REPORT_CATEGORY_DESCRIPTOR)}
					</h2>
					<ReportFlowAnswerList
						flow={flow}
						steps={steps}
						data-flx="moderation.report.report-step-details.report-flow-answer-list"
					/>
				</section>
				<form
					className={styles.form}
					onSubmit={(e) => {
						e.preventDefault();
						onSubmit();
					}}
					data-flx="moderation.report.report-step-details.form.prevent-default"
				>
					{selectedType === 'message' && (
						<>
							<Input
								label={i18n._(MESSAGE_LINK_DESCRIPTOR)}
								type="url"
								dir="ltr"
								value={formValues.messageLink}
								onChange={(e) => onFieldChange('messageLink', e.target.value)}
								placeholder={`${window.location.origin}/channels/...`}
								autoComplete="off"
								error={fieldErrors.messageLink}
								footer={
									!formValues.messageLink.trim() ? undefined : !messageLinkOk ? (
										<span className={styles.helperText} data-flx="moderation.report.report-step-details.helper-text">
											<Trans>That doesn't look like a valid URL.</Trans>
										</span>
									) : undefined
								}
								data-flx="moderation.report.report-step-details.input.field-change.url"
							/>
							<Input
								label={i18n._(REPORTED_USER_TAG_OPTIONAL_DESCRIPTOR)}
								type="text"
								dir="ltr"
								value={formValues.messageUserTag}
								onChange={(e) => onFieldChange('messageUserTag', e.target.value)}
								placeholder={EXAMPLE_REPORT_USER_TAG}
								autoComplete="off"
								error={fieldErrors.messageUserTag}
								data-flx="moderation.report.report-step-details.input.field-change.text"
							/>
						</>
					)}
					{selectedType === 'user' && (
						<>
							<Input
								label={i18n._(USER_ID_OPTIONAL_DESCRIPTOR)}
								type="text"
								dir="ltr"
								value={formValues.userId}
								onChange={(e) => onFieldChange('userId', e.target.value)}
								placeholder="123456789012345678"
								autoComplete="off"
								error={fieldErrors.userId}
								data-flx="moderation.report.report-step-details.input.field-change.text--2"
							/>
							<Input
								label={i18n._(USER_TAG_OPTIONAL_DESCRIPTOR)}
								type="text"
								dir="ltr"
								value={formValues.userTag}
								onChange={(e) => onFieldChange('userTag', e.target.value)}
								placeholder={EXAMPLE_REPORT_USER_TAG}
								autoComplete="off"
								error={fieldErrors.userTag}
								footer={
									userTargetOk ? undefined : (
										<span className={styles.helperText} data-flx="moderation.report.report-step-details.helper-text--2">
											<Trans>Provide at least a user ID or a user tag.</Trans>
										</span>
									)
								}
								data-flx="moderation.report.report-step-details.input.field-change.text--3"
							/>
						</>
					)}
					{selectedType === 'guild' && (
						<>
							<Input
								label={i18n._(COMMUNITY_ID_DESCRIPTOR)}
								type="text"
								dir="ltr"
								value={formValues.guildId}
								onChange={(e) => onFieldChange('guildId', e.target.value)}
								placeholder="123456789012345678"
								autoComplete="off"
								error={fieldErrors.guildId}
								footer={
									guildTargetOk ? undefined : (
										<span className={styles.helperText} data-flx="moderation.report.report-step-details.helper-text--3">
											<Trans>Community ID is required.</Trans>
										</span>
									)
								}
								data-flx="moderation.report.report-step-details.input.field-change.text--4"
							/>
							<Input
								label={i18n._(INVITE_CODE_OPTIONAL_DESCRIPTOR)}
								type="text"
								dir="ltr"
								value={formValues.inviteCode}
								onChange={(e) => onFieldChange('inviteCode', e.target.value)}
								placeholder={EXAMPLE_INVITE_CODE}
								autoComplete="off"
								error={fieldErrors.inviteCode}
								data-flx="moderation.report.report-step-details.input.field-change.text--5"
							/>
						</>
					)}
					<Textarea
						label={i18n._(EXPLAIN_THE_PROBLEM_DESCRIPTOR)}
						value={formValues.additionalInfo}
						onChange={(e) => onFieldChange('additionalInfo', e.target.value)}
						maxLength={1000}
						minRows={3}
						maxRows={6}
						required
						error={fieldErrors.additionalInfo}
						footer={
							<span className={styles.helperText} data-flx="moderation.report.report-step-details.helper-text--4">
								{i18n._(EXPLAIN_THE_PROBLEM_HELPER_DESCRIPTOR)}
							</span>
						}
						data-flx="moderation.report.report-step-details.textarea.field-change"
					/>
					<Input
						label={i18n._(FULL_LEGAL_NAME_OPTIONAL_FOR_CSAM_DESCRIPTOR)}
						type="text"
						value={formValues.reporterFullName}
						onChange={(e) => onFieldChange('reporterFullName', e.target.value)}
						placeholder={i18n._(FIRST_AND_LAST_NAME_DESCRIPTOR)}
						autoComplete="name"
						error={fieldErrors.reporterFullName}
						data-flx="moderation.report.report-step-details.input.field-change.text--6"
					/>
					<Combobox<string>
						label={i18n._(COUNTRY_OF_RESIDENCE_DESCRIPTOR)}
						value={formValues.reporterCountry}
						options={countryOptions}
						error={fieldErrors.reporterCountry}
						onChange={(value) => onFieldChange('reporterCountry', value)}
						dir={getLocaleDirection(i18n.locale)}
						data-flx="moderation.report.report-step-details.select.field-change--2"
					/>
					<Checkbox
						checked={goodFaithConfirmed}
						onChange={onGoodFaithChange}
						data-flx="moderation.report.report-step-details.checkbox.good-faith-change"
					>
						<span className={styles.description} data-flx="moderation.report.report-step-details.good-faith-label">
							{i18n._(GOOD_FAITH_STATEMENT_DESCRIPTOR)}
						</span>
					</Checkbox>
					{fieldErrors.goodFaithConfirmed && (
						<div
							className={styles.errorBox}
							role="alert"
							data-flx="moderation.report.report-step-details.good-faith-error"
						>
							{fieldErrors.goodFaithConfirmed}
						</div>
					)}
					<div className={styles.actionRow} data-flx="moderation.report.report-step-details.action-row">
						<Button
							fitContent
							type="submit"
							disabled={!canSubmit || isSubmitting}
							submitting={isSubmitting}
							className={styles.actionButton}
							data-flx="moderation.report.report-step-details.action-button.submit"
						>
							<Trans>Send DSA report</Trans>
						</Button>
						<Button
							variant="secondary"
							fitContent
							type="button"
							onClick={onBack}
							disabled={isSubmitting}
							data-flx="moderation.report.report-step-details.button.back"
						>
							<Trans>Back</Trans>
						</Button>
					</div>
				</form>
			</div>
			<footer className={styles.footerLinks} data-flx="moderation.report.report-step-details.footer-links">
				<p className={styles.linkRow} data-flx="moderation.report.report-step-details.link-row">
					<FocusRing offset={-2} data-flx="moderation.report.report-step-details.focus-ring.start-over">
						<button
							type="button"
							className={styles.linkButton}
							onClick={onStartOver}
							disabled={isSubmitting}
							data-flx="moderation.report.report-step-details.link-button.start-over"
						>
							<Trans>Start over</Trans>
						</button>
					</FocusRing>
				</p>
			</footer>
		</div>
	);
};

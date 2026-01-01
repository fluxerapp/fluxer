/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Input, Textarea} from '~/components/form/Input';
import {Select, type SelectOption} from '~/components/form/Select';
import {Button} from '~/components/uikit/Button/Button';
import {RadioGroup, type RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {Endpoints} from '~/Endpoints';
import {useFluxerDocumentTitle} from '~/hooks/useFluxerDocumentTitle';
import HttpClient from '~/lib/HttpClient';
import styles from './ReportPage.module.css';

type ReportType = 'message' | 'user' | 'guild';
type FlowStep = 'selection' | 'email' | 'verification' | 'details' | 'complete';

const EMAIL_REGEX = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
const VERIFICATION_CODE_REGEX = /^[A-Z0-9]{4}-[A-Z0-9]{4}$/;

const FLOW_TOTAL_STEPS = 4;

const INITIAL_FORM_VALUES = {
	category: '',
	reporterFullName: '',
	reporterCountry: '',
	reporterFluxerTag: '',
	messageLink: '',
	messageUserTag: '',
	userId: '',
	userTag: '',
	guildId: '',
	inviteCode: '',
	additionalInfo: '',
};

type FormValues = typeof INITIAL_FORM_VALUES;

type State = {
	selectedType: ReportType | null;
	flowStep: FlowStep;

	email: string;
	verificationCode: string;
	ticket: string | null;

	formValues: FormValues;

	isSendingCode: boolean;
	isVerifying: boolean;
	isSubmitting: boolean;

	errorMessage: string | null;
	successReportId: string | null;
};

type Action =
	| {type: 'RESET_ALL'}
	| {type: 'SELECT_TYPE'; reportType: ReportType}
	| {type: 'GO_TO_SELECTION'}
	| {type: 'GO_TO_EMAIL'}
	| {type: 'GO_TO_VERIFICATION'}
	| {type: 'GO_TO_DETAILS'}
	| {type: 'SET_ERROR'; message: string | null}
	| {type: 'SET_EMAIL'; email: string}
	| {type: 'SET_VERIFICATION_CODE'; code: string}
	| {type: 'SET_TICKET'; ticket: string | null}
	| {type: 'SET_FORM_FIELD'; field: keyof FormValues; value: string}
	| {type: 'SENDING_CODE'; value: boolean}
	| {type: 'VERIFYING'; value: boolean}
	| {type: 'SUBMITTING'; value: boolean}
	| {type: 'SUBMIT_SUCCESS'; reportId: string};

const createInitialState = (): State => ({
	selectedType: null,
	flowStep: 'selection',

	email: '',
	verificationCode: '',
	ticket: null,

	formValues: {...INITIAL_FORM_VALUES},

	isSendingCode: false,
	isVerifying: false,
	isSubmitting: false,

	errorMessage: null,
	successReportId: null,
});

function reducer(state: State, action: Action): State {
	switch (action.type) {
		case 'RESET_ALL':
			return createInitialState();

		case 'SELECT_TYPE':
			return {
				...createInitialState(),
				selectedType: action.reportType,
				flowStep: 'email',
			};

		case 'GO_TO_SELECTION':
			return {
				...createInitialState(),
			};

		case 'GO_TO_EMAIL':
			return {
				...state,
				flowStep: 'email',
				verificationCode: '',
				ticket: null,
				isVerifying: false,
				errorMessage: null,
			};

		case 'GO_TO_VERIFICATION':
			return {
				...state,
				flowStep: 'verification',
				verificationCode: '',
				ticket: null,
				errorMessage: null,
			};

		case 'GO_TO_DETAILS':
			return {
				...state,
				flowStep: 'details',
				errorMessage: null,
			};

		case 'SET_ERROR':
			return {...state, errorMessage: action.message};

		case 'SET_EMAIL':
			return {...state, email: action.email, errorMessage: null};

		case 'SET_VERIFICATION_CODE':
			return {...state, verificationCode: action.code, errorMessage: null};

		case 'SET_TICKET':
			return {...state, ticket: action.ticket};

		case 'SET_FORM_FIELD':
			return {
				...state,
				formValues: {...state.formValues, [action.field]: action.value},
				errorMessage: null,
			};

		case 'SENDING_CODE':
			return {...state, isSendingCode: action.value};

		case 'VERIFYING':
			return {...state, isVerifying: action.value};

		case 'SUBMITTING':
			return {...state, isSubmitting: action.value};

		case 'SUBMIT_SUCCESS':
			return {
				...state,
				successReportId: action.reportId,
				flowStep: 'complete',
				isSubmitting: false,
				errorMessage: null,
			};

		default:
			return state;
	}
}

function getStepNumber(step: FlowStep): number | null {
	switch (step) {
		case 'selection':
			return 1;
		case 'email':
			return 2;
		case 'verification':
			return 3;
		case 'details':
			return 4;
		case 'complete':
			return null;
	}
}

function formatVerificationCodeInput(raw: string): string {
	const cleaned = raw
		.toUpperCase()
		.replace(/[^A-Z0-9]/g, '')
		.slice(0, 8);
	if (cleaned.length <= 4) return cleaned;
	return `${cleaned.slice(0, 4)}-${cleaned.slice(4)}`;
}

function normalizeLikelyUrl(raw: string): string {
	const trimmed = raw.trim();
	if (!trimmed) return '';

	if (!/^[a-zA-Z][a-zA-Z\d+\-.]*:\/\//.test(trimmed)) {
		return `https://${trimmed}`;
	}

	return trimmed;
}

function isValidHttpUrl(raw: string): boolean {
	try {
		const url = new URL(raw);
		return url.protocol === 'http:' || url.protocol === 'https:';
	} catch {
		return false;
	}
}

export const ReportPage = observer(() => {
	const {t} = useLingui();

	useFluxerDocumentTitle(t`Report Illegal Content`);

	const [state, dispatch] = React.useReducer(reducer, undefined, createInitialState);

	const reportTypeOptions = React.useMemo<ReadonlyArray<RadioOption<ReportType>>>(
		() => [
			{value: 'message', name: t`Report a Message`},
			{value: 'user', name: t`Report a User Profile`},
			{value: 'guild', name: t`Report a Community`},
		],
		[t],
	);

	const reportTypeLabel = React.useMemo(() => {
		const match = reportTypeOptions.find((o) => o.value === state.selectedType);
		return match?.name ?? '';
	}, [reportTypeOptions, state.selectedType]);

	const messageCategoryOptions = React.useMemo<Array<SelectOption<string>>>(
		() => [
			{value: '', label: t`Select a category`},
			{value: 'harassment', label: t`Harassment or Bullying`},
			{value: 'hate_speech', label: t`Hate Speech`},
			{value: 'violent_content', label: t`Violent or Graphic Content`},
			{value: 'spam', label: t`Spam or Scam`},
			{value: 'nsfw_violation', label: t`NSFW Policy Violation`},
			{value: 'illegal_activity', label: t`Illegal Activity`},
			{value: 'doxxing', label: t`Sharing Personal Information`},
			{value: 'self_harm', label: t`Self-Harm or Suicide`},
			{value: 'child_safety', label: t`Child Safety Concerns`},
			{value: 'malicious_links', label: t`Malicious Links`},
			{value: 'impersonation', label: t`Impersonation`},
			{value: 'other', label: t`Other`},
		],
		[t],
	);

	const userCategoryOptions = React.useMemo<Array<SelectOption<string>>>(
		() => [
			{value: '', label: t`Select a category`},
			{value: 'harassment', label: t`Harassment or Bullying`},
			{value: 'hate_speech', label: t`Hate Speech`},
			{value: 'spam_account', label: t`Spam Account`},
			{value: 'impersonation', label: t`Impersonation`},
			{value: 'underage_user', label: t`Underage User`},
			{value: 'inappropriate_profile', label: t`Inappropriate Profile`},
			{value: 'other', label: t`Other`},
		],
		[t],
	);

	const guildCategoryOptions = React.useMemo<Array<SelectOption<string>>>(
		() => [
			{value: '', label: t`Select a category`},
			{value: 'harassment', label: t`Harassment`},
			{value: 'hate_speech', label: t`Hate Speech`},
			{value: 'extremist_community', label: t`Extremist Community`},
			{value: 'illegal_activity', label: t`Illegal Activity`},
			{value: 'child_safety', label: t`Child Safety Concerns`},
			{value: 'raid_coordination', label: t`Raid Coordination`},
			{value: 'spam', label: t`Spam or Scam Community`},
			{value: 'malware_distribution', label: t`Malware Distribution`},
			{value: 'other', label: t`Other`},
		],
		[t],
	);

	const countryOptions = React.useMemo<Array<SelectOption<string>>>(
		() => [
			{value: '', label: t`Select a country`},
			{value: 'AT', label: t`Austria`},
			{value: 'BE', label: t`Belgium`},
			{value: 'BG', label: t`Bulgaria`},
			{value: 'HR', label: t`Croatia`},
			{value: 'CY', label: t`Cyprus`},
			{value: 'CZ', label: t`Czech Republic`},
			{value: 'DK', label: t`Denmark`},
			{value: 'EE', label: t`Estonia`},
			{value: 'FI', label: t`Finland`},
			{value: 'FR', label: t`France`},
			{value: 'DE', label: t`Germany`},
			{value: 'GR', label: t`Greece`},
			{value: 'HU', label: t`Hungary`},
			{value: 'IE', label: t`Ireland`},
			{value: 'IT', label: t`Italy`},
			{value: 'LV', label: t`Latvia`},
			{value: 'LT', label: t`Lithuania`},
			{value: 'LU', label: t`Luxembourg`},
			{value: 'MT', label: t`Malta`},
			{value: 'NL', label: t`Netherlands`},
			{value: 'PL', label: t`Poland`},
			{value: 'PT', label: t`Portugal`},
			{value: 'RO', label: t`Romania`},
			{value: 'SK', label: t`Slovakia`},
			{value: 'SI', label: t`Slovenia`},
			{value: 'ES', label: t`Spain`},
			{value: 'SE', label: t`Sweden`},
		],
		[t],
	);

	const categoryOptionsByType = React.useMemo(() => {
		return {
			message: messageCategoryOptions,
			user: userCategoryOptions,
			guild: guildCategoryOptions,
		} satisfies Record<ReportType, Array<SelectOption<string>>>;
	}, [messageCategoryOptions, userCategoryOptions, guildCategoryOptions]);

	const categoryOptions = state.selectedType ? categoryOptionsByType[state.selectedType] : [];

	const isBusy = state.isSendingCode || state.isVerifying || state.isSubmitting;

	React.useEffect(() => {
		if (state.flowStep === 'selection') return;

		if (!state.selectedType) {
			dispatch({type: 'GO_TO_SELECTION'});
			return;
		}

		if (state.flowStep === 'verification' && !state.email.trim()) {
			dispatch({type: 'GO_TO_EMAIL'});
			return;
		}

		if (state.flowStep === 'details' && !state.ticket) {
			dispatch({type: 'GO_TO_EMAIL'});
			return;
		}

		if (state.flowStep === 'complete' && !state.successReportId) {
			dispatch({type: 'GO_TO_SELECTION'});
		}
	}, [state.flowStep, state.selectedType, state.email, state.ticket, state.successReportId]);

	React.useEffect(() => {
		window.scrollTo({top: 0, behavior: 'smooth'});
	}, [state.flowStep]);

	const onSelectType = React.useCallback((type: ReportType) => {
		dispatch({type: 'SELECT_TYPE', reportType: type});
	}, []);

	const sendVerificationCode = React.useCallback(async () => {
		if (state.isSendingCode || state.isVerifying || state.isSubmitting) return;

		const normalizedEmail = state.email.trim();

		if (!normalizedEmail) {
			dispatch({type: 'SET_ERROR', message: t`Please provide an email address.`});
			return;
		}

		if (!EMAIL_REGEX.test(normalizedEmail)) {
			dispatch({type: 'SET_ERROR', message: t`Please enter a valid email address.`});
			return;
		}

		dispatch({type: 'SET_ERROR', message: null});
		dispatch({type: 'SENDING_CODE', value: true});

		try {
			await HttpClient.post({
				url: Endpoints.DSA_REPORT_EMAIL_SEND,
				body: {email: normalizedEmail},
			});

			dispatch({type: 'SET_EMAIL', email: normalizedEmail});
			dispatch({type: 'GO_TO_VERIFICATION'});
		} catch (_error) {
			dispatch({type: 'SET_ERROR', message: t`Failed to send verification code. Please try again.`});
		} finally {
			dispatch({type: 'SENDING_CODE', value: false});
		}
	}, [state.email, state.isSendingCode, state.isVerifying, state.isSubmitting, t]);

	const verifyCode = React.useCallback(async () => {
		if (state.isSendingCode || state.isVerifying || state.isSubmitting) return;

		const code = state.verificationCode.trim().toUpperCase();

		if (!code) {
			dispatch({type: 'SET_ERROR', message: t`Enter the code before continuing.`});
			return;
		}

		if (!VERIFICATION_CODE_REGEX.test(code)) {
			dispatch({type: 'SET_ERROR', message: t`Enter a code in the format ABCD-1234.`});
			return;
		}

		const normalizedEmail = state.email.trim();

		if (!normalizedEmail || !EMAIL_REGEX.test(normalizedEmail)) {
			dispatch({type: 'SET_ERROR', message: t`Please go back and enter a valid email address.`});
			return;
		}

		dispatch({type: 'SET_ERROR', message: null});
		dispatch({type: 'VERIFYING', value: true});

		try {
			const response = await HttpClient.post<{ticket: string}>({
				url: Endpoints.DSA_REPORT_EMAIL_VERIFY,
				body: {email: normalizedEmail, code},
			});

			dispatch({type: 'SET_TICKET', ticket: response.body.ticket});
			dispatch({type: 'GO_TO_DETAILS'});
		} catch (_error) {
			dispatch({type: 'SET_ERROR', message: t`The verification code is invalid or expired.`});
		} finally {
			dispatch({type: 'VERIFYING', value: false});
		}
	}, [state.email, state.verificationCode, state.isSendingCode, state.isVerifying, state.isSubmitting, t]);

	const handleSubmit = React.useCallback(async () => {
		if (!state.selectedType) return;
		if (state.isSubmitting || state.isSendingCode || state.isVerifying) return;

		if (!state.ticket) {
			dispatch({type: 'SET_ERROR', message: t`You must verify your email before submitting a report.`});
			return;
		}

		const reporterFullName = state.formValues.reporterFullName.trim();
		const reporterCountry = state.formValues.reporterCountry;
		const reporterFluxerTag = state.formValues.reporterFluxerTag.trim();
		const additionalInfo = state.formValues.additionalInfo.trim();

		if (!state.formValues.category) {
			dispatch({type: 'SET_ERROR', message: t`Select a violation category.`});
			return;
		}

		if (!reporterFullName) {
			dispatch({type: 'SET_ERROR', message: t`Provide your full legal name for the declaration.`});
			return;
		}

		if (!reporterCountry) {
			dispatch({type: 'SET_ERROR', message: t`Select your country of residence.`});
			return;
		}

		const payload: Record<string, unknown> = {
			ticket: state.ticket,
			report_type: state.selectedType,
			category: state.formValues.category,
			reporter_full_legal_name: reporterFullName,
			reporter_country_of_residence: reporterCountry,
		};

		if (reporterFluxerTag) payload.reporter_fluxer_tag = reporterFluxerTag;
		if (additionalInfo) payload.additional_info = additionalInfo;

		switch (state.selectedType) {
			case 'message': {
				const raw = state.formValues.messageLink;
				const normalized = normalizeLikelyUrl(raw);

				if (!raw.trim()) {
					dispatch({type: 'SET_ERROR', message: t`Please paste the message link you are reporting.`});
					return;
				}

				if (!isValidHttpUrl(normalized)) {
					dispatch({type: 'SET_ERROR', message: t`Please enter a valid message link URL.`});
					return;
				}

				payload.message_link = normalized;

				const reportedUserTag = state.formValues.messageUserTag.trim();
				if (reportedUserTag) payload.reported_user_tag = reportedUserTag;
				break;
			}

			case 'user': {
				const userId = state.formValues.userId.trim();
				const userTag = state.formValues.userTag.trim();

				if (!userId && !userTag) {
					dispatch({
						type: 'SET_ERROR',
						message: t`Provide either a user ID or a FluxerTag for the person you are reporting.`,
					});
					return;
				}

				if (userId) payload.user_id = userId;
				if (userTag) payload.user_tag = userTag;
				break;
			}

			case 'guild': {
				const guildId = state.formValues.guildId.trim();
				const inviteCode = state.formValues.inviteCode.trim();

				if (!guildId) {
					dispatch({type: 'SET_ERROR', message: t`Please include the community (guild) ID you are reporting.`});
					return;
				}

				payload.guild_id = guildId;
				if (inviteCode) payload.invite_code = inviteCode;
				break;
			}
		}

		dispatch({type: 'SET_ERROR', message: null});
		dispatch({type: 'SUBMITTING', value: true});

		try {
			const response = await HttpClient.post<{report_id: string}>({
				url: Endpoints.DSA_REPORT_CREATE,
				body: payload,
			});

			dispatch({type: 'SUBMIT_SUCCESS', reportId: response.body.report_id});
		} catch (_error) {
			dispatch({type: 'SET_ERROR', message: t`Something went wrong while submitting the report. Please try again.`});
			dispatch({type: 'SUBMITTING', value: false});
		}
	}, [state, t]);

	const stepNumber = getStepNumber(state.flowStep);

	const renderStepHeader = (title: React.ReactNode, description: React.ReactNode) => (
		<>
			{stepNumber !== null && (
				<div className={styles.stepIndicator}>
					<Trans>
						Step {stepNumber} of {FLOW_TOTAL_STEPS}
					</Trans>
				</div>
			)}
			<h1 className={styles.title}>{title}</h1>
			<p className={styles.description}>{description}</p>
		</>
	);

	if (state.flowStep === 'complete' && state.successReportId) {
		return (
			<div className={styles.container}>
				{renderStepHeader(
					<Trans>Report Submitted</Trans>,
					<Trans>Thank you for filing a report. We&apos;ll review it as soon as possible.</Trans>,
				)}

				<div className={styles.successBox}>
					<div className={styles.successLabel}>
						<Trans>Your submission ID</Trans>
					</div>
					<div className={styles.successValue}>{state.successReportId}</div>
				</div>

				<div className={styles.footer}>
					<button type="button" className={styles.link} onClick={() => dispatch({type: 'RESET_ALL'})} disabled={isBusy}>
						<Trans>Submit another report</Trans>
					</button>
				</div>
			</div>
		);
	}

	if (state.flowStep === 'selection') {
		return (
			<div className={styles.container}>
				{renderStepHeader(
					<Trans>Report Illegal Content</Trans>,
					<Trans>
						Use this form to report illegal content under the Digital Services Act (DSA). Select what you want to
						report.
					</Trans>,
				)}

				<div className={styles.form}>
					<RadioGroup<ReportType>
						options={reportTypeOptions}
						value={state.selectedType}
						onChange={onSelectType}
						aria-label={t`Report Type`}
					/>
				</div>
			</div>
		);
	}

	if (state.flowStep === 'email') {
		const normalizedEmail = state.email.trim();
		const emailLooksValid = normalizedEmail.length > 0 && EMAIL_REGEX.test(normalizedEmail);

		return (
			<div className={styles.container}>
				{renderStepHeader(
					<Trans>Enter Your Email</Trans>,
					<Trans>We need to verify your email address before you can submit a report.</Trans>,
				)}

				{state.selectedType && (
					<div className={styles.metaLine}>
						<Trans>Reporting:</Trans> <span className={styles.metaValue}>{reportTypeLabel}</span>
					</div>
				)}

				{state.errorMessage && (
					<div className={styles.errorBox} role="alert" aria-live="polite">
						{state.errorMessage}
					</div>
				)}

				<form
					className={styles.form}
					onSubmit={(e) => {
						e.preventDefault();
						void sendVerificationCode();
					}}
				>
					<Input
						label={t`Email Address`}
						type="email"
						value={state.email}
						onChange={(e) => dispatch({type: 'SET_EMAIL', email: e.target.value})}
						placeholder="you@example.com"
						autoComplete="email"
					/>

					<Button
						fitContainer
						type="submit"
						disabled={!emailLooksValid || state.isSendingCode}
						submitting={state.isSendingCode}
					>
						<Trans>Send Verification Code</Trans>
					</Button>
				</form>

				<div className={styles.footer}>
					<button
						type="button"
						className={styles.link}
						onClick={() => dispatch({type: 'GO_TO_SELECTION'})}
						disabled={isBusy}
					>
						<Trans>Start over</Trans>
					</button>
				</div>
			</div>
		);
	}

	if (state.flowStep === 'verification') {
		const codeForValidation = state.verificationCode.trim().toUpperCase();
		const codeLooksValid = VERIFICATION_CODE_REGEX.test(codeForValidation);

		return (
			<div className={styles.container}>
				{renderStepHeader(
					<Trans>Enter Verification Code</Trans>,
					<Trans>We sent a verification code to {state.email}.</Trans>,
				)}

				{state.errorMessage && (
					<div className={styles.errorBox} role="alert" aria-live="polite">
						{state.errorMessage}
					</div>
				)}

				<form
					className={styles.form}
					onSubmit={(e) => {
						e.preventDefault();
						void verifyCode();
					}}
				>
					<Input
						label={t`Verification Code`}
						type="text"
						value={state.verificationCode}
						onChange={(e) =>
							dispatch({type: 'SET_VERIFICATION_CODE', code: formatVerificationCodeInput(e.target.value)})
						}
						placeholder="ABCD-1234"
						autoComplete="one-time-code"
						footer={
							<span className={styles.helperText}>
								<Trans>Letters and numbers only. You can paste the code—formatting is automatic.</Trans>
							</span>
						}
					/>

					<Button
						fitContainer
						type="submit"
						disabled={!codeLooksValid || state.isVerifying}
						submitting={state.isVerifying}
					>
						<Trans>Verify Code</Trans>
					</Button>
				</form>

				<div className={styles.footer}>
					<div className={styles.footerRow}>
						<button
							type="button"
							className={styles.link}
							onClick={() => dispatch({type: 'GO_TO_EMAIL'})}
							disabled={isBusy}
						>
							<Trans>Change email</Trans>
						</button>

						<button
							type="button"
							className={styles.link}
							onClick={() => void sendVerificationCode()}
							disabled={state.isSendingCode || state.isVerifying || state.isSubmitting}
						>
							<Trans>Resend code</Trans>
						</button>
					</div>

					<button
						type="button"
						className={styles.link}
						onClick={() => dispatch({type: 'GO_TO_SELECTION'})}
						disabled={isBusy}
					>
						<Trans>Start over</Trans>
					</button>
				</div>
			</div>
		);
	}

	if (state.flowStep === 'details' && state.selectedType) {
		const reporterFullName = state.formValues.reporterFullName.trim();
		const reporterCountry = state.formValues.reporterCountry;
		const category = state.formValues.category;

		const messageLinkNormalized = normalizeLikelyUrl(state.formValues.messageLink);
		const messageLinkOk = state.selectedType !== 'message' ? true : isValidHttpUrl(messageLinkNormalized);

		const userTargetOk =
			state.selectedType !== 'user' ? true : Boolean(state.formValues.userId.trim() || state.formValues.userTag.trim());

		const guildTargetOk = state.selectedType !== 'guild' ? true : Boolean(state.formValues.guildId.trim());

		const canSubmit =
			Boolean(category) &&
			Boolean(reporterFullName) &&
			Boolean(reporterCountry) &&
			messageLinkOk &&
			userTargetOk &&
			guildTargetOk;

		return (
			<div className={styles.container}>
				{renderStepHeader(<Trans>Report Details</Trans>, <Trans>Please provide the details of your report.</Trans>)}

				<div className={styles.metaLine}>
					<Trans>Reporting:</Trans> <span className={styles.metaValue}>{reportTypeLabel}</span>
					<span className={styles.metaSpacer} aria-hidden="true">
						•
					</span>
					<span className={styles.metaValue}>
						<Trans>Email verified</Trans>
					</span>
				</div>

				{state.errorMessage && (
					<div className={styles.errorBox} role="alert" aria-live="polite">
						{state.errorMessage}
					</div>
				)}

				<form
					className={styles.form}
					onSubmit={(e) => {
						e.preventDefault();
						void handleSubmit();
					}}
				>
					<Select<string>
						label={t`Violation Category`}
						value={state.formValues.category}
						options={categoryOptions}
						onChange={(value) => dispatch({type: 'SET_FORM_FIELD', field: 'category', value})}
						isSearchable={false}
					/>

					{state.selectedType === 'message' && (
						<>
							<Input
								label={t`Message Link`}
								type="url"
								value={state.formValues.messageLink}
								onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'messageLink', value: e.target.value})}
								placeholder="https://fluxer.app/channels/..."
								autoComplete="off"
								footer={
									!state.formValues.messageLink.trim() ? undefined : !messageLinkOk ? (
										<span className={styles.helperText}>
											<Trans>That doesn&apos;t look like a valid URL.</Trans>
										</span>
									) : undefined
								}
							/>
							<Input
								label={t`Reported User Tag (optional)`}
								type="text"
								value={state.formValues.messageUserTag}
								onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'messageUserTag', value: e.target.value})}
								placeholder="username#1234"
								autoComplete="off"
							/>
						</>
					)}

					{state.selectedType === 'user' && (
						<>
							<Input
								label={t`User ID (optional)`}
								type="text"
								value={state.formValues.userId}
								onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'userId', value: e.target.value})}
								placeholder="123456789012345678"
								autoComplete="off"
							/>
							<Input
								label={t`User Tag (optional)`}
								type="text"
								value={state.formValues.userTag}
								onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'userTag', value: e.target.value})}
								placeholder="username#1234"
								autoComplete="off"
								footer={
									userTargetOk ? undefined : (
										<span className={styles.helperText}>
											<Trans>Provide at least a user ID or a user tag.</Trans>
										</span>
									)
								}
							/>
						</>
					)}

					{state.selectedType === 'guild' && (
						<>
							<Input
								label={t`Guild (Community) ID`}
								type="text"
								value={state.formValues.guildId}
								onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'guildId', value: e.target.value})}
								placeholder="123456789012345678"
								autoComplete="off"
								footer={
									guildTargetOk ? undefined : (
										<span className={styles.helperText}>
											<Trans>Guild ID is required.</Trans>
										</span>
									)
								}
							/>
							<Input
								label={t`Invite Code (optional)`}
								type="text"
								value={state.formValues.inviteCode}
								onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'inviteCode', value: e.target.value})}
								placeholder="abcDEF12"
								autoComplete="off"
							/>
						</>
					)}

					<Input
						label={t`Full Legal Name`}
						type="text"
						value={state.formValues.reporterFullName}
						onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'reporterFullName', value: e.target.value})}
						placeholder={t`First and last name`}
						autoComplete="name"
					/>

					<Select<string>
						label={t`Country of Residence`}
						value={state.formValues.reporterCountry}
						options={countryOptions}
						onChange={(value) => dispatch({type: 'SET_FORM_FIELD', field: 'reporterCountry', value})}
					/>

					<Input
						label={t`Your FluxerTag (optional)`}
						type="text"
						value={state.formValues.reporterFluxerTag}
						onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'reporterFluxerTag', value: e.target.value})}
						placeholder="username#1234"
					/>

					<Textarea
						label={t`Additional Comments (optional)`}
						value={state.formValues.additionalInfo}
						onChange={(e) => dispatch({type: 'SET_FORM_FIELD', field: 'additionalInfo', value: e.target.value})}
						placeholder={t`Describe what makes the content illegal`}
						maxLength={1000}
						minRows={3}
						maxRows={6}
					/>

					<Button
						fitContainer
						type="submit"
						disabled={!canSubmit || state.isSubmitting}
						submitting={state.isSubmitting}
					>
						<Trans>Submit DSA Report</Trans>
					</Button>
				</form>

				<div className={styles.footer}>
					<button type="button" className={styles.link} onClick={() => dispatch({type: 'RESET_ALL'})} disabled={isBusy}>
						<Trans>Start over</Trans>
					</button>
				</div>
			</div>
		);
	}

	return null;
});

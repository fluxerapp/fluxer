// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import {
	COUNTRY_OPTIONS,
	GUILD_CATEGORY_OPTIONS,
	MESSAGE_CATEGORY_OPTIONS,
	REPORT_TYPE_OPTION_DESCRIPTORS,
	USER_CATEGORY_OPTIONS,
} from '@app/features/moderation/components/report/OptionDescriptors';
import {ReportBreadcrumbs} from '@app/features/moderation/components/report/ReportBreadcrumbs';
import {
	createReportSnapshot,
	selectReportState,
	transitionReportSnapshot,
} from '@app/features/moderation/components/report/ReportState';
import {ReportStepComplete} from '@app/features/moderation/components/report/ReportStepComplete';
import {ReportStepDetails} from '@app/features/moderation/components/report/ReportStepDetails';
import {ReportStepEmail} from '@app/features/moderation/components/report/ReportStepEmail';
import {ReportStepSelection} from '@app/features/moderation/components/report/ReportStepSelection';
import {ReportStepVerification} from '@app/features/moderation/components/report/ReportStepVerification';
import type {Action, FlowStep, FormValues, ReportType} from '@app/features/moderation/components/report/ReportTypes';
import {
	EMAIL_REGEX,
	formatVerificationCodeInput,
	isValidHttpUrl,
	normalizeLikelyUrl,
	VERIFICATION_CODE_REGEX,
} from '@app/features/moderation/components/report/Validators';
import {http} from '@app/features/platform/transport/RestTransport';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const REPORT_ILLEGAL_CONTENT_DESCRIPTOR = msg({
	message: 'Report illegal content',
	comment:
		'Document/page title for the public DSA illegal-content report page. Sensitive/legal flow; keep tone plain and factual.',
});
const SOMETHING_WENT_WRONG_WHILE_SENDING_THE_REPORT_PLEASE_DESCRIPTOR = msg({
	message: 'Failed to send report. Try again.',
	comment: 'Generic error shown on the DSA report submission page when the report could not be sent.',
});
const PLEASE_PROVIDE_AN_EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'Email address required.',
	comment: 'Inline validation error on the email step of the DSA report flow when the email field is empty.',
});
const PLEASE_ENTER_A_VALID_EMAIL_ADDRESS_DESCRIPTOR = msg({
	message: 'Enter a valid email address.',
	comment: 'Inline validation error on the email step of the DSA report flow when the email is malformed.',
});
const CODE_RESENT_DESCRIPTOR = msg({
	message: 'Code resent',
	comment: 'Success toast on the verification step of the DSA report flow after resending the email verification code.',
});
const FAILED_TO_SEND_VERIFICATION_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to send code. Try again.',
	comment: 'Error shown when the initial email verification code send fails on the DSA report flow.',
});
const FAILED_TO_RESEND_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to resend code. Try again.',
	comment: 'Error toast when the resend-code request fails on the verification step of the DSA report flow.',
});
const ENTER_THE_CODE_BEFORE_CONTINUING_DESCRIPTOR = msg({
	message: 'Enter the code before continuing.',
	comment: 'Inline validation error on the verification step of the DSA report flow when the code field is empty.',
});
const ENTER_A_CODE_IN_THE_FORMAT_ABCD_1234_DESCRIPTOR = msg({
	message: 'Enter a code in the format ABCD-1234.',
	comment:
		'Inline validation error on the verification step of the DSA report flow when the code does not match the expected 4-letter-dash-4-digit format. "ABCD-1234" is a literal example pattern.',
});
const PLEASE_GO_BACK_AND_ENTER_A_VALID_EMAIL_DESCRIPTOR = msg({
	message: 'Go back and enter a valid email.',
	comment:
		'Inline error on the verification step of the DSA report flow when the stored email is missing or malformed and the user must return to the email step.',
});
const THE_VERIFICATION_CODE_IS_INVALID_OR_EXPIRED_DESCRIPTOR = msg({
	message: 'The verification code is invalid or expired.',
	comment: 'Inline error on the verification step of the DSA report flow when the entered code is rejected.',
});
const YOU_MUST_VERIFY_YOUR_EMAIL_BEFORE_SENDING_A_DESCRIPTOR = msg({
	message: 'You must verify your email before sending a report.',
	comment:
		'Inline error on the details step of the DSA report flow when the user has not completed email verification.',
});
const SELECT_A_VIOLATION_CATEGORY_DESCRIPTOR = msg({
	message: 'Select a violation category.',
	comment: 'Inline validation error on the details step of the DSA report flow when the category field is empty.',
});
const PROVIDE_YOUR_FULL_LEGAL_NAME_FOR_THE_DECLARATION_DESCRIPTOR = msg({
	message: 'Provide your full legal name for the declaration.',
	comment:
		'Inline validation error on the details step of the DSA report flow. The reporter must give their legal name for the legal declaration; keep tone neutral and factual.',
});
const SELECT_YOUR_COUNTRY_OF_RESIDENCE_DESCRIPTOR = msg({
	message: 'Select your country of residence.',
	comment:
		'Inline validation error on the details step of the DSA report flow when the country dropdown has no selection.',
});
const PLEASE_PASTE_THE_MESSAGE_LINK_YOU_ARE_REPORTING_DESCRIPTOR = msg({
	message: 'Paste the message link.',
	comment:
		'Inline validation error on the details step of the DSA report flow (message report) when the message link is empty.',
});
const PLEASE_ENTER_A_VALID_MESSAGE_LINK_URL_DESCRIPTOR = msg({
	message: 'Enter a valid message link.',
	comment:
		'Inline validation error on the details step of the DSA report flow (message report) when the message link is not a valid URL.',
});
const PROVIDE_EITHER_A_USER_ID_OR_A_USERNAME_DESCRIPTOR = msg({
	message: 'Provide either a user ID or a username for the person you are reporting.',
	comment:
		'Inline validation error on the details step of the DSA report flow (user report) when both user ID and username fields are empty.',
});
const PLEASE_INCLUDE_THE_COMMUNITY_ID_YOU_ARE_REPORTING_DESCRIPTOR = msg({
	message: 'Include the community ID.',
	comment:
		'Inline validation error on the details step of the DSA report flow (community report) when the community ID field is empty.',
});

interface ValidationError {
	path: string;
	message: string;
}

export const ReportPage = observer(() => {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(REPORT_ILLEGAL_CONTENT_DESCRIPTOR));
	const [reportSnapshot, setReportSnapshot] = useState(createReportSnapshot);
	const state = useMemo(() => selectReportState(reportSnapshot), [reportSnapshot]);
	const dispatch = useCallback((event: Action) => {
		setReportSnapshot((snapshot) => transitionReportSnapshot(snapshot, event));
	}, []);
	const parseValidationErrors = useCallback(
		(
			error: unknown,
		): {fieldErrors: Partial<Record<keyof FormValues, string>>; generalMessage: string | null} | null => {
			if (error && typeof error === 'object' && 'body' in error && (error as {body?: unknown}).body) {
				const body = (error as {body?: Record<string, unknown>}).body;
				const pathMap: Record<string, keyof FormValues> = {
					category: 'category',
					reporter_full_legal_name: 'reporterFullName',
					reporter_country_of_residence: 'reporterCountry',
					reporter_fluxer_tag: 'reporterFluxerTag',
					message_link: 'messageLink',
					reported_user_tag: 'messageUserTag',
					user_id: 'userId',
					user_tag: 'userTag',
					guild_id: 'guildId',
					invite_code: 'inviteCode',
					additional_info: 'additionalInfo',
				};
				if (body?.code === APIErrorCodes.INVALID_FORM_BODY && Array.isArray(body.errors)) {
					const fieldErrors: Partial<Record<keyof FormValues, string>> = {};
					const errors = body.errors as Array<ValidationError>;
					for (const err of errors) {
						const mapped = pathMap[err.path];
						if (mapped) {
							fieldErrors[mapped] = err.message;
						}
					}
					const hasFieldErrors = Object.keys(fieldErrors).length > 0;
					const generalMessage = hasFieldErrors
						? null
						: (errors[0]?.message ?? i18n._(SOMETHING_WENT_WRONG_WHILE_SENDING_THE_REPORT_PLEASE_DESCRIPTOR));
					return {fieldErrors, generalMessage};
				}
				if (typeof body?.message === 'string') {
					return {fieldErrors: {}, generalMessage: body.message};
				}
			}
			return null;
		},
		[i18n],
	);
	const reportTypeOptions = useMemo<ReadonlyArray<RadioOption<ReportType>>>(() => {
		return REPORT_TYPE_OPTION_DESCRIPTORS.map((option: {value: ReportType; name: MessageDescriptor}) => ({
			value: option.value,
			name: i18n._(option.name),
		}));
	}, [i18n.locale]);
	const messageCategoryOptions = useMemo<Array<ComboboxOption<string>>>(() => {
		return MESSAGE_CATEGORY_OPTIONS.map((option: {value: string; label: MessageDescriptor}) => ({
			value: option.value,
			label: i18n._(option.label),
		}));
	}, [i18n.locale]);
	const userCategoryOptions = useMemo<Array<ComboboxOption<string>>>(() => {
		return USER_CATEGORY_OPTIONS.map((option: {value: string; label: MessageDescriptor}) => ({
			value: option.value,
			label: i18n._(option.label),
		}));
	}, [i18n.locale]);
	const guildCategoryOptions = useMemo<Array<ComboboxOption<string>>>(() => {
		return GUILD_CATEGORY_OPTIONS.map((option: {value: string; label: MessageDescriptor}) => ({
			value: option.value,
			label: i18n._(option.label),
		}));
	}, [i18n.locale]);
	const countryOptions = useMemo<Array<ComboboxOption<string>>>(() => {
		return COUNTRY_OPTIONS.map((option: {value: string; label: MessageDescriptor}) => ({
			value: option.value,
			label: i18n._(option.label),
		}));
	}, [i18n.locale]);
	const categoryOptionsByType = useMemo(() => {
		return {
			message: messageCategoryOptions,
			user: userCategoryOptions,
			guild: guildCategoryOptions,
		} satisfies Record<ReportType, Array<ComboboxOption<string>>>;
	}, [messageCategoryOptions, userCategoryOptions, guildCategoryOptions]);
	const categoryOptions = state.selectedType ? categoryOptionsByType[state.selectedType] : [];
	useEffect(() => {
		if (state.resendCooldownSeconds <= 0) return;
		const timer = window.setInterval(() => dispatch({type: 'TICK_RESEND_COOLDOWN'}), 1000);
		return () => window.clearInterval(timer);
	}, [state.resendCooldownSeconds, dispatch]);
	useEffect(() => {
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
	useEffect(() => {
		window.scrollTo({top: 0, behavior: Accessibility.useSmoothScrolling ? 'smooth' : 'auto'});
	}, [state.flowStep]);
	const onSelectType = useCallback((type: ReportType) => {
		dispatch({type: 'SELECT_TYPE', reportType: type});
	}, []);
	const sendVerificationCode = useCallback(async () => {
		if (state.isSendingCode || state.isVerifying || state.isSubmitting) return;
		const normalizedEmail = state.email.trim();
		if (!normalizedEmail) {
			dispatch({type: 'SET_ERROR', message: i18n._(PLEASE_PROVIDE_AN_EMAIL_ADDRESS_DESCRIPTOR)});
			return;
		}
		if (!EMAIL_REGEX.test(normalizedEmail)) {
			dispatch({type: 'SET_ERROR', message: i18n._(PLEASE_ENTER_A_VALID_EMAIL_ADDRESS_DESCRIPTOR)});
			return;
		}
		dispatch({type: 'SET_ERROR', message: null});
		dispatch({type: 'SENDING_CODE', value: true});
		if (state.flowStep === 'verification') {
			dispatch({type: 'START_RESEND_COOLDOWN', seconds: 30});
		}
		try {
			await http.post(Endpoints.DSA_REPORT_EMAIL_SEND, {
				body: {email: normalizedEmail},
			});
			dispatch({type: 'SET_EMAIL', email: normalizedEmail});
			dispatch({type: 'GO_TO_VERIFICATION'});
			if (state.flowStep === 'verification') {
				ToastCommands.createToast({type: 'success', children: i18n._(CODE_RESENT_DESCRIPTOR)});
			}
		} catch (_error) {
			dispatch({type: 'SET_ERROR', message: i18n._(FAILED_TO_SEND_VERIFICATION_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR)});
			if (state.flowStep === 'verification') {
				showModerationErrorModal(
					i18n,
					() => i18n._(FAILED_TO_RESEND_CODE_PLEASE_TRY_AGAIN_DESCRIPTOR),
					'moderation.report-page.resend-code-error-modal',
				);
			}
		} finally {
			dispatch({type: 'SENDING_CODE', value: false});
		}
	}, [state.email, state.isSendingCode, state.isVerifying, state.isSubmitting, state.flowStep, i18n]);
	const verifyCode = useCallback(async () => {
		if (state.isSendingCode || state.isVerifying || state.isSubmitting) return;
		const code = state.verificationCode.trim().toUpperCase();
		if (!code) {
			dispatch({type: 'SET_ERROR', message: i18n._(ENTER_THE_CODE_BEFORE_CONTINUING_DESCRIPTOR)});
			return;
		}
		if (!VERIFICATION_CODE_REGEX.test(code)) {
			dispatch({type: 'SET_ERROR', message: i18n._(ENTER_A_CODE_IN_THE_FORMAT_ABCD_1234_DESCRIPTOR)});
			return;
		}
		const normalizedEmail = state.email.trim();
		if (!normalizedEmail || !EMAIL_REGEX.test(normalizedEmail)) {
			dispatch({type: 'SET_ERROR', message: i18n._(PLEASE_GO_BACK_AND_ENTER_A_VALID_EMAIL_DESCRIPTOR)});
			return;
		}
		dispatch({type: 'SET_ERROR', message: null});
		dispatch({type: 'VERIFYING', value: true});
		try {
			const response = await http.post<{ticket: string}>(Endpoints.DSA_REPORT_EMAIL_VERIFY, {
				body: {email: normalizedEmail, code},
			});
			dispatch({type: 'SET_TICKET', ticket: response.body.ticket});
			dispatch({type: 'GO_TO_DETAILS'});
		} catch (_error) {
			dispatch({type: 'SET_ERROR', message: i18n._(THE_VERIFICATION_CODE_IS_INVALID_OR_EXPIRED_DESCRIPTOR)});
		} finally {
			dispatch({type: 'VERIFYING', value: false});
		}
	}, [state.email, state.verificationCode, state.isSendingCode, state.isVerifying, state.isSubmitting, i18n]);
	const handleSubmit = useCallback(async () => {
		if (!state.selectedType) return;
		if (state.isSubmitting || state.isSendingCode || state.isVerifying) return;
		if (!state.ticket) {
			dispatch({type: 'SET_ERROR', message: i18n._(YOU_MUST_VERIFY_YOUR_EMAIL_BEFORE_SENDING_A_DESCRIPTOR)});
			return;
		}
		dispatch({type: 'CLEAR_FIELD_ERRORS'});
		const reporterFullName = state.formValues.reporterFullName.trim();
		const reporterCountry = state.formValues.reporterCountry;
		const reporterFluxerTag = state.formValues.reporterFluxerTag.trim();
		const additionalInfo = state.formValues.additionalInfo.trim();
		if (!state.formValues.category) {
			dispatch({type: 'SET_ERROR', message: i18n._(SELECT_A_VIOLATION_CATEGORY_DESCRIPTOR)});
			return;
		}
		if (!reporterFullName) {
			dispatch({type: 'SET_ERROR', message: i18n._(PROVIDE_YOUR_FULL_LEGAL_NAME_FOR_THE_DECLARATION_DESCRIPTOR)});
			return;
		}
		if (!reporterCountry) {
			dispatch({type: 'SET_ERROR', message: i18n._(SELECT_YOUR_COUNTRY_OF_RESIDENCE_DESCRIPTOR)});
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
					dispatch({type: 'SET_ERROR', message: i18n._(PLEASE_PASTE_THE_MESSAGE_LINK_YOU_ARE_REPORTING_DESCRIPTOR)});
					return;
				}
				if (!isValidHttpUrl(normalized)) {
					dispatch({type: 'SET_ERROR', message: i18n._(PLEASE_ENTER_A_VALID_MESSAGE_LINK_URL_DESCRIPTOR)});
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
						message: i18n._(PROVIDE_EITHER_A_USER_ID_OR_A_USERNAME_DESCRIPTOR),
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
					dispatch({type: 'SET_ERROR', message: i18n._(PLEASE_INCLUDE_THE_COMMUNITY_ID_YOU_ARE_REPORTING_DESCRIPTOR)});
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
			const response = await http.post<{report_id: string}>(Endpoints.DSA_REPORT_CREATE, {
				body: payload,
			});
			dispatch({type: 'SUBMIT_SUCCESS', reportId: response.body.report_id});
		} catch (_error) {
			const parsed = parseValidationErrors(_error);
			if (parsed) {
				dispatch({type: 'SET_FIELD_ERRORS', errors: parsed.fieldErrors});
				dispatch({type: 'SET_ERROR', message: parsed.generalMessage});
			} else {
				dispatch({type: 'SET_ERROR', message: i18n._(SOMETHING_WENT_WRONG_WHILE_SENDING_THE_REPORT_PLEASE_DESCRIPTOR)});
			}
			dispatch({type: 'SUBMITTING', value: false});
		}
	}, [state, i18n]);
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
	const handleBreadcrumbSelect = (step: FlowStep) => {
		switch (step) {
			case 'selection':
				dispatch({type: 'GO_TO_SELECTION'});
				break;
			case 'email':
				dispatch({type: 'GO_TO_EMAIL'});
				break;
			case 'verification':
				dispatch({type: 'GO_TO_VERIFICATION'});
				break;
			case 'details':
				dispatch({type: 'GO_TO_DETAILS'});
				break;
			default:
				break;
		}
	};
	const renderStep = () => {
		switch (state.flowStep) {
			case 'selection':
				return (
					<ReportStepSelection
						reportTypeOptions={reportTypeOptions}
						selectedType={state.selectedType}
						onSelect={onSelectType}
						data-flx="moderation.report-page.render-step.report-step-selection.select-type"
					/>
				);
			case 'email':
				return (
					<ReportStepEmail
						email={state.email}
						errorMessage={state.errorMessage}
						isSending={state.isSendingCode}
						onEmailChange={(value) => dispatch({type: 'SET_EMAIL', email: value})}
						onSubmit={() => void sendVerificationCode()}
						onStartOver={() => dispatch({type: 'GO_TO_SELECTION'})}
						data-flx="moderation.report-page.render-step.report-step-email"
					/>
				);
			case 'verification':
				return (
					<ReportStepVerification
						email={state.email}
						verificationCode={state.verificationCode}
						errorMessage={state.errorMessage}
						isVerifying={state.isVerifying}
						isResending={state.isSendingCode}
						resendCooldownSeconds={state.resendCooldownSeconds}
						onChangeEmail={() => dispatch({type: 'GO_TO_EMAIL'})}
						onResend={() => void sendVerificationCode()}
						onVerify={() => void verifyCode()}
						onCodeChange={(value) =>
							dispatch({type: 'SET_VERIFICATION_CODE', code: formatVerificationCodeInput(value)})
						}
						onStartOver={() => dispatch({type: 'GO_TO_SELECTION'})}
						data-flx="moderation.report-page.render-step.report-step-verification"
					/>
				);
			case 'details':
				return (
					<ReportStepDetails
						selectedType={state.selectedType as ReportType}
						formValues={state.formValues}
						categoryOptions={categoryOptions}
						countryOptions={countryOptions}
						fieldErrors={state.fieldErrors}
						errorMessage={state.errorMessage}
						canSubmit={canSubmit}
						isSubmitting={state.isSubmitting}
						onFieldChange={(field, value) => dispatch({type: 'SET_FORM_FIELD', field, value})}
						onSubmit={() => void handleSubmit()}
						onStartOver={() => dispatch({type: 'RESET_ALL'})}
						onBack={() => dispatch({type: 'GO_TO_VERIFICATION'})}
						messageLinkOk={messageLinkOk}
						userTargetOk={userTargetOk}
						guildTargetOk={guildTargetOk}
						data-flx="moderation.report-page.render-step.report-step-details"
					/>
				);
			case 'complete':
				return state.successReportId ? (
					<ReportStepComplete
						onStartOver={() => dispatch({type: 'RESET_ALL'})}
						data-flx="moderation.report-page.render-step.report-step-complete"
					/>
				) : null;
			default:
				return null;
		}
	};
	const breadcrumbs =
		state.flowStep === 'complete' ? null : (
			<ReportBreadcrumbs
				current={state.flowStep}
				hasSelection={Boolean(state.selectedType)}
				hasEmail={Boolean(state.email.trim())}
				hasTicket={Boolean(state.ticket)}
				onSelect={handleBreadcrumbSelect}
				data-flx="moderation.report-page.report-breadcrumbs.breadcrumb-select"
			/>
		);
	const breadcrumbShell =
		state.flowStep === 'complete' ? null : (
			<div className={styles.breadcrumbShell} data-flx="moderation.report-page.breadcrumb-shell">
				{breadcrumbs ?? (
					<span
						className={styles.breadcrumbPlaceholder}
						aria-hidden="true"
						data-flx="moderation.report-page.breadcrumb-placeholder"
					/>
				)}
			</div>
		);
	return (
		<div className={styles.page} data-flx="moderation.report-page.page">
			{breadcrumbShell}
			<div className={styles.mainColumn} data-flx="moderation.report-page.main-column">
				{renderStep()}
			</div>
		</div>
	);
});

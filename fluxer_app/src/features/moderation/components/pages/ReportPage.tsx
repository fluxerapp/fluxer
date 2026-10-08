// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {getLocaleDirection} from '@app/features/i18n/utils/LocaleDirection';
import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import styles from '@app/features/moderation/components/pages/ReportPage.module.css';
import {
	COUNTRY_OPTIONS,
	REPORT_TYPE_OPTION_DESCRIPTORS,
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
import {ReportStepReason} from '@app/features/moderation/components/report/ReportStepReason';
import {ReportStepSelection} from '@app/features/moderation/components/report/ReportStepSelection';
import {ReportStepUnavailable} from '@app/features/moderation/components/report/ReportStepUnavailable';
import {
	formatCooldownDuration,
	ReportStepVerification,
} from '@app/features/moderation/components/report/ReportStepVerification';
import type {
	Action,
	FlowStep,
	FormValues,
	ReportField,
	ReportType,
} from '@app/features/moderation/components/report/ReportTypes';
import {
	EMAIL_REGEX,
	formatVerificationCodeInput,
	isValidHttpUrl,
	normalizeLikelyUrl,
	VERIFICATION_CODE_REGEX,
} from '@app/features/moderation/components/report/Validators';
import {
	getReportFlowStepKey,
	isReportFlowWalkUrgent,
	type ReportFlowWalk,
} from '@app/features/moderation/components/report_flow/ReportFlowWalk';
import ReportFlows from '@app/features/moderation/state/ReportFlows';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
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
const TOO_MANY_CODES_SENT_DESCRIPTOR = msg({
	message: 'Too many codes sent. Wait {duration} and try again.',
	comment:
		'Inline error on the email or code step of the DSA report flow when the verification code send was rate limited. {duration} is a localized duration such as "45 seconds" or "12 minutes".',
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
const EXPLAIN_THE_PROBLEM_TO_SEND_DESCRIPTOR = msg({
	message: 'Explain the problem to send the report.',
	comment:
		'Inline validation error on the details step of the DSA report flow when the Explain the problem field is empty.',
});
const CONFIRM_THE_STATEMENT_TO_SEND_DESCRIPTOR = msg({
	message: 'Confirm the statement to send the report.',
	comment:
		'Inline validation error on the details step of the DSA report flow when the good-faith statement checkbox is not ticked.',
});
const ENTER_YOUR_FULL_LEGAL_NAME_DESCRIPTOR = msg({
	message: 'Enter your full legal name to send the report.',
	comment:
		'Inline validation error on the Full legal name field of the DSA report form, shown when the server requires the name for the chosen reason. The name is required for every reason except child sexual abuse.',
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
	code?: string;
}

interface ParsedSubmitError {
	fieldErrors: Partial<Record<ReportField, string>>;
	generalMessage: string | null;
	answersRejected: boolean;
}

interface ReportPrefill {
	reportType: ReportType;
	values: Partial<FormValues>;
	option: string | null;
}

const SURFACE = 'dsa';
const REPORT_TYPES: ReadonlyArray<ReportType> = ['message', 'user', 'guild'];
const SNOWFLAKE_REGEX = /^\d{1,20}$/;
const PREFILL_OPTION_REGEX = /^[a-z0-9_]{1,48}$/;
const RESEND_COOLDOWN_SECONDS = 60;
const CODE_COOLDOWNS_STORAGE_KEY = 'dsa_report_code_cooldowns';
const FIELD_BY_ERROR_PATH: Record<string, ReportField> = {
	reporter_full_legal_name: 'reporterFullName',
	reporter_country_of_residence: 'reporterCountry',
	message_link: 'messageLink',
	reported_user_tag: 'messageUserTag',
	user_id: 'userId',
	user_tag: 'userTag',
	guild_id: 'guildId',
	invite_code: 'inviteCode',
	additional_info: 'additionalInfo',
	good_faith_confirmed: 'goodFaithConfirmed',
};
const ANSWER_ERROR_CODES: ReadonlySet<string> = new Set([
	APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS,
	APIErrorCodes.REPORT_FLOW_OUTDATED,
]);

function isAnswerErrorPath(path: string): boolean {
	return path === 'revision_hash' || path === 'steps' || path.startsWith('steps.') || path.startsWith('steps[');
}

function readReportPrefill(search: string): ReportPrefill | null {
	const params = new URLSearchParams(search);
	const reportType = REPORT_TYPES.find((type) => type === params.get('type'));
	if (!reportType) return null;
	const values: Partial<FormValues> = {};
	const messageLink = normalizeLikelyUrl(params.get('message_link') ?? '');
	if (reportType === 'message' && isValidHttpUrl(messageLink)) values.messageLink = messageLink;
	const userId = params.get('user_id')?.trim() ?? '';
	if (reportType === 'user' && SNOWFLAKE_REGEX.test(userId)) values.userId = userId;
	const option = params.get('option') ?? '';
	return {reportType, values, option: PREFILL_OPTION_REGEX.test(option) ? option : null};
}

function readCodeCooldowns(): Record<string, number> {
	try {
		const parsed: unknown = JSON.parse(getProtectedSessionStorage()?.getItem(CODE_COOLDOWNS_STORAGE_KEY) ?? '{}');
		if (!parsed || typeof parsed !== 'object') return {};
		const now = Date.now();
		return Object.fromEntries(
			Object.entries(parsed).filter(
				(entry): entry is [string, number] => typeof entry[1] === 'number' && entry[1] > now,
			),
		);
	} catch {
		return {};
	}
}

function writeCodeCooldowns(cooldowns: Record<string, number>): void {
	try {
		getProtectedSessionStorage()?.setItem(CODE_COOLDOWNS_STORAGE_KEY, JSON.stringify(cooldowns));
	} catch {}
}

function readRateLimitSeconds(error: unknown): number | null {
	if (!(error instanceof HttpError)) return null;
	if (error.status !== HttpStatus.TOO_MANY_REQUESTS && failureCode(error) !== APIErrorCodes.RATE_LIMITED) return null;
	return Math.max(1, Math.ceil((resolveRetryAfterMs(error) ?? RESEND_COOLDOWN_SECONDS * 1000) / 1000));
}

function walkIsComplete(walk: ReportFlowWalk | null): boolean {
	return walk?.phase === 'summary';
}

export const ReportPage = observer(() => {
	const {i18n} = useLingui();
	useFluxerDocumentTitle(i18n._(REPORT_ILLEGAL_CONTENT_DESCRIPTOR));
	const location = useLocation();
	const locale = i18n.locale;
	const [reportSnapshot, setReportSnapshot] = useState(() => {
		const snapshot = createReportSnapshot();
		const prefill = readReportPrefill(location.search);
		return prefill ? transitionReportSnapshot(snapshot, {type: 'PREFILL', ...prefill}) : snapshot;
	});
	const state = useMemo(() => selectReportState(reportSnapshot), [reportSnapshot]);
	const dispatch = useCallback((event: Action) => {
		setReportSnapshot((snapshot) => transitionReportSnapshot(snapshot, event));
	}, []);
	const [codeCooldowns, setCodeCooldowns] = useState(readCodeCooldowns);
	const startCodeCooldown = useCallback((email: string, seconds: number) => {
		setCodeCooldowns((current) => {
			const next = {...current, [email.toLowerCase()]: Date.now() + seconds * 1000};
			writeCodeCooldowns(next);
			return next;
		});
	}, []);
	const parseSubmitError = useCallback(
		(error: unknown): ParsedSubmitError | null => {
			if (!error || typeof error !== 'object' || !('body' in error)) return null;
			const body = (error as {body?: Record<string, unknown>}).body;
			if (!body) return null;
			const fallback = i18n._(SOMETHING_WENT_WRONG_WHILE_SENDING_THE_REPORT_PLEASE_DESCRIPTOR);
			if (typeof body.code === 'string' && ANSWER_ERROR_CODES.has(body.code)) {
				return {
					fieldErrors: {},
					generalMessage: typeof body.message === 'string' ? body.message : fallback,
					answersRejected: true,
				};
			}
			if (body.code === APIErrorCodes.INVALID_FORM_BODY && Array.isArray(body.errors)) {
				const fieldErrors: Partial<Record<ReportField, string>> = {};
				const errors = body.errors as Array<ValidationError>;
				const answerError = errors.find((err) => isAnswerErrorPath(err.path));
				for (const err of errors) {
					const mapped = FIELD_BY_ERROR_PATH[err.path];
					if (mapped === 'reporterFullName' && err.code === ValidationErrorCodes.INVALID_FORMAT) {
						fieldErrors[mapped] = i18n._(ENTER_YOUR_FULL_LEGAL_NAME_DESCRIPTOR);
					} else if (mapped) {
						fieldErrors[mapped] = err.message;
					}
				}
				if (answerError) {
					return {fieldErrors, generalMessage: answerError.message, answersRejected: true};
				}
				const hasFieldErrors = Object.keys(fieldErrors).length > 0;
				return {
					fieldErrors,
					generalMessage: hasFieldErrors ? null : (errors[0]?.message ?? fallback),
					answersRejected: false,
				};
			}
			if (typeof body.message === 'string') {
				return {fieldErrors: {}, generalMessage: body.message, answersRejected: false};
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
	const countryOptions = useMemo<Array<ComboboxOption<string>>>(() => {
		return COUNTRY_OPTIONS.map((option: {value: string; label: MessageDescriptor}) => ({
			value: option.value,
			label: i18n._(option.label),
		}));
	}, [i18n.locale]);
	const loadFlow = useCallback(
		(reportType: ReportType, reload: boolean) => {
			dispatch({type: 'FLOW_REQUESTED'});
			const request = reload
				? ReportFlows.reload(reportType, SURFACE, locale)
				: ReportFlows.load(reportType, SURFACE, locale);
			void request.then((flow) => {
				if (flow) {
					dispatch({type: 'FLOW_LOADED', flow});
				} else if (ReportFlows.getState(reportType, SURFACE, locale)?.status === 'error') {
					dispatch({type: 'FLOW_UNAVAILABLE', reportType});
				}
			});
		},
		[locale, dispatch],
	);
	useEffect(() => {
		if (state.selectedType) loadFlow(state.selectedType, false);
	}, [state.selectedType, loadFlow]);
	useEffect(() => {
		if (state.flowStep !== 'email' && state.flowStep !== 'verification') return;
		const cooldownUntil = codeCooldowns[state.email.trim().toLowerCase()];
		const remainingSeconds = cooldownUntil === undefined ? 0 : Math.ceil((cooldownUntil - Date.now()) / 1000);
		dispatch({type: 'START_RESEND_COOLDOWN', seconds: Math.max(0, remainingSeconds)});
	}, [state.flowStep, state.email, codeCooldowns, dispatch]);
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
		if ((state.flowStep === 'reason' || state.flowStep === 'details') && !state.ticket) {
			dispatch({type: 'GO_TO_EMAIL'});
			return;
		}
		if (state.flowStep === 'details' && !walkIsComplete(state.walk)) {
			dispatch({type: 'GO_TO_REASON'});
			return;
		}
		if (state.flowStep === 'complete' && !state.successReportId) {
			dispatch({type: 'GO_TO_SELECTION'});
		}
	}, [state.flowStep, state.selectedType, state.email, state.ticket, state.successReportId, state.walk]);
	const walkKey = state.walk ? getReportFlowStepKey(state.walk) : null;
	useEffect(() => {
		window.scrollTo({top: 0, behavior: Accessibility.useSmoothScrolling ? 'smooth' : 'auto'});
	}, [state.flowStep, walkKey]);
	const onSelectType = useCallback((type: ReportType) => {
		dispatch({type: 'SELECT_TYPE', reportType: type});
	}, []);
	const continueVerified = useCallback(() => {
		dispatch({type: walkIsComplete(state.walk) ? 'GO_TO_DETAILS' : 'GO_TO_REASON'});
	}, [state.walk, dispatch]);
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
		try {
			await http.post(Endpoints.DSA_REPORT_EMAIL_SEND, {
				body: {email: normalizedEmail},
			});
			dispatch({type: 'SET_EMAIL', email: normalizedEmail});
			dispatch({type: 'GO_TO_VERIFICATION'});
			startCodeCooldown(normalizedEmail, RESEND_COOLDOWN_SECONDS);
			if (state.flowStep === 'verification') {
				ToastCommands.createToast({type: 'success', children: i18n._(CODE_RESENT_DESCRIPTOR)});
			}
		} catch (error) {
			const waitSeconds = readRateLimitSeconds(error);
			if (waitSeconds !== null) {
				startCodeCooldown(normalizedEmail, waitSeconds);
				const duration = formatCooldownDuration(i18n, waitSeconds);
				dispatch({type: 'SET_ERROR', message: i18n._(TOO_MANY_CODES_SENT_DESCRIPTOR, {duration}), rateLimit: true});
				return;
			}
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
	}, [
		state.email,
		state.isSendingCode,
		state.isVerifying,
		state.isSubmitting,
		state.flowStep,
		i18n,
		startCodeCooldown,
	]);
	const verifyCode = useCallback(async () => {
		if (state.isSendingCode || state.isVerifying || state.isSubmitting) return;
		if (state.ticket) {
			continueVerified();
			return;
		}
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
			continueVerified();
		} catch (_error) {
			dispatch({type: 'SET_ERROR', message: i18n._(THE_VERIFICATION_CODE_IS_INVALID_OR_EXPIRED_DESCRIPTOR)});
		} finally {
			dispatch({type: 'VERIFYING', value: false});
		}
	}, [
		state.email,
		state.ticket,
		state.verificationCode,
		state.isSendingCode,
		state.isVerifying,
		state.isSubmitting,
		i18n,
		continueVerified,
	]);
	const handleSubmit = useCallback(async () => {
		if (!state.selectedType) return;
		if (state.isSubmitting || state.isSendingCode || state.isVerifying) return;
		if (!state.ticket) {
			dispatch({type: 'SET_ERROR', message: i18n._(YOU_MUST_VERIFY_YOUR_EMAIL_BEFORE_SENDING_A_DESCRIPTOR)});
			return;
		}
		const {flow, walk} = state;
		if (!flow || !walk || !walkIsComplete(walk)) {
			dispatch({type: 'GO_TO_REASON'});
			return;
		}
		dispatch({type: 'CLEAR_FIELD_ERRORS'});
		const reporterFullName = state.formValues.reporterFullName.trim();
		const reporterCountry = state.formValues.reporterCountry;
		const additionalInfo = state.formValues.additionalInfo.trim();
		if (!additionalInfo) {
			dispatch({type: 'SET_ERROR', message: i18n._(EXPLAIN_THE_PROBLEM_TO_SEND_DESCRIPTOR)});
			return;
		}
		if (!state.goodFaithConfirmed) {
			dispatch({type: 'SET_ERROR', message: i18n._(CONFIRM_THE_STATEMENT_TO_SEND_DESCRIPTOR)});
			return;
		}
		if (!reporterCountry) {
			dispatch({type: 'SET_ERROR', message: i18n._(SELECT_YOUR_COUNTRY_OF_RESIDENCE_DESCRIPTOR)});
			return;
		}
		const payload: Record<string, unknown> = {
			ticket: state.ticket,
			report_type: state.selectedType,
			revision_hash: flow.revision_hash,
			steps: walk.steps,
			locale: flow.locale,
			good_faith_confirmed: true,
			additional_info: additionalInfo,
			reporter_country_of_residence: reporterCountry,
		};
		if (reporterFullName) payload.reporter_full_legal_name = reporterFullName;
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
			const parsed = parseSubmitError(_error);
			if (parsed) {
				dispatch({type: 'SET_FIELD_ERRORS', errors: parsed.fieldErrors});
				if (parsed.answersRejected && parsed.generalMessage) {
					dispatch({type: 'ANSWERS_REJECTED', message: parsed.generalMessage});
				} else {
					dispatch({type: 'SET_ERROR', message: parsed.generalMessage});
				}
			} else {
				dispatch({type: 'SET_ERROR', message: i18n._(SOMETHING_WENT_WRONG_WHILE_SENDING_THE_REPORT_PLEASE_DESCRIPTOR)});
			}
			dispatch({type: 'SUBMITTING', value: false});
		}
	}, [state, i18n, parseSubmitError]);
	const chooseReasonAgain = useCallback(() => {
		if (!state.selectedType) return;
		dispatch({type: 'GO_TO_REASON'});
		loadFlow(state.selectedType, true);
	}, [state.selectedType, loadFlow, dispatch]);
	const reporterCountry = state.formValues.reporterCountry;
	const additionalInfo = state.formValues.additionalInfo.trim();
	const messageLinkNormalized = normalizeLikelyUrl(state.formValues.messageLink);
	const messageLinkOk = state.selectedType !== 'message' ? true : isValidHttpUrl(messageLinkNormalized);
	const userTargetOk =
		state.selectedType !== 'user' ? true : Boolean(state.formValues.userId.trim() || state.formValues.userTag.trim());
	const guildTargetOk = state.selectedType !== 'guild' ? true : Boolean(state.formValues.guildId.trim());
	const canSubmit =
		walkIsComplete(state.walk) &&
		Boolean(additionalInfo) &&
		state.goodFaithConfirmed &&
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
			case 'reason':
				dispatch({type: 'GO_TO_REASON'});
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
						verified={Boolean(state.ticket)}
						resendCooldownSeconds={state.resendCooldownSeconds}
						onEmailChange={(value) => dispatch({type: 'SET_EMAIL', email: value})}
						onSubmit={() => (state.ticket ? continueVerified() : void sendVerificationCode())}
						onStartOver={() => dispatch({type: 'RESET_ALL'})}
						data-flx="moderation.report-page.render-step.report-step-email"
					/>
				);
			case 'verification':
				return (
					<ReportStepVerification
						email={state.email}
						verified={Boolean(state.ticket)}
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
						onStartOver={() => dispatch({type: 'RESET_ALL'})}
						data-flx="moderation.report-page.render-step.report-step-verification"
					/>
				);
			case 'reason':
				return (
					<ReportStepReason
						flow={state.flow}
						flowStatus={state.flowStatus}
						walk={state.walk}
						onWalkChange={(walk) => dispatch({type: 'WALK_CHANGED', walk})}
						onBack={() => dispatch({type: 'GO_TO_VERIFICATION'})}
						onRetryLoad={() => state.selectedType && loadFlow(state.selectedType, true)}
						onStartOver={() => dispatch({type: 'RESET_ALL'})}
						data-flx="moderation.report-page.render-step.report-step-reason"
					/>
				);
			case 'details':
				return state.flow && state.walk ? (
					<ReportStepDetails
						selectedType={state.selectedType as ReportType}
						formValues={state.formValues}
						flow={state.flow}
						steps={state.walk.steps}
						urgent={isReportFlowWalkUrgent(state.flow, state.walk)}
						goodFaithConfirmed={state.goodFaithConfirmed}
						countryOptions={countryOptions}
						fieldErrors={state.fieldErrors}
						errorMessage={state.errorMessage}
						answersRejected={state.answersRejected}
						canSubmit={canSubmit}
						isSubmitting={state.isSubmitting}
						onFieldChange={(field, value) => dispatch({type: 'SET_FORM_FIELD', field, value})}
						onGoodFaithChange={(value) => dispatch({type: 'SET_GOOD_FAITH_CONFIRMED', value})}
						onChooseReasonAgain={chooseReasonAgain}
						onSubmit={() => void handleSubmit()}
						onStartOver={() => dispatch({type: 'RESET_ALL'})}
						onBack={() => dispatch({type: 'GO_TO_REASON'})}
						messageLinkOk={messageLinkOk}
						userTargetOk={userTargetOk}
						guildTargetOk={guildTargetOk}
						data-flx="moderation.report-page.render-step.report-step-details"
					/>
				) : null;
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
				hasEmail={Boolean(state.selectedType && state.email.trim())}
				hasTicket={Boolean(state.selectedType && state.ticket)}
				hasAnswers={walkIsComplete(state.walk)}
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
	if (RuntimeConfig.usesUsernameSignIn) {
		return (
			<div className={styles.page} dir={getLocaleDirection(locale)} data-flx="moderation.report-page.page">
				<div className={styles.mainColumn} data-flx="moderation.report-page.main-column">
					<ReportStepUnavailable data-flx="moderation.report-page.report-step-unavailable" />
				</div>
			</div>
		);
	}
	return (
		<div className={styles.page} dir={getLocaleDirection(locale)} data-flx="moderation.report-page.page">
			{breadcrumbShell}
			<div className={styles.mainColumn} data-flx="moderation.report-page.main-column">
				{renderStep()}
			</div>
		</div>
	);
});

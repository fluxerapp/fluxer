// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import FormField from '@app/features/auth/flow/AuthFormField';
import styles from '@app/features/auth/flow/AuthPageStyles.module.css';
import {DateOfBirthField} from '@app/features/auth/flow/DateOfBirthField';
import {
	getRegistrationLegalConsentConfig,
	RegistrationLegalConsent,
} from '@app/features/auth/flow/RegistrationLegalConsent';
import {type MissingField, SubmitTooltip, shouldDisableSubmit} from '@app/features/auth/flow/SubmitTooltip';
import {useAuthForm} from '@app/features/auth/hooks/useAuthForm';
import {
	type AuthRegisterFormDraft,
	EMPTY_AUTH_REGISTER_FORM_DRAFT,
	useAuthRegisterDraftContext,
} from '@app/features/auth/state/AuthRegisterDraftContext';
import {EMAIL_DESCRIPTOR, PASSWORD_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Button} from '@app/features/ui/button/Button';
import {useUsernameSuggestions} from '@app/features/user/hooks/useUsernameSuggestions';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useId, useMemo, useRef, useState} from 'react';

const PASSWORDS_DO_NOT_MATCH_DESCRIPTOR = msg({
	message: 'Passwords do not match',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const CONFIRM_PASSWORD_DESCRIPTOR = msg({
	message: 'Confirm password',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const DATE_OF_BIRTH_DESCRIPTOR = msg({
	message: 'Date of birth',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const USERNAME_MUST_BE_CHARACTERS_OR_LESS_DESCRIPTOR = msg({
	message: 'Username must be {maxUsernameLength} characters or less',
	comment: 'Registration form validation error when the username exceeds the maximum length. Limit is interpolated.',
});
const ONLY_LETTERS_NUMBERS_AND_UNDERSCORES_DESCRIPTOR = msg({
	message: 'Only letters, numbers, and underscores',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const DISPLAY_NAME_OPTIONAL_DESCRIPTOR = msg({
	message: 'Display name (optional)',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR = msg({
	message: 'What should people call you?',
	comment: 'Question prompt in the authentication auth register form core. Keep the tone plain and specific.',
});
const USERNAME_OPTIONAL_DESCRIPTOR = msg({
	message: 'Username (optional)',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const LEAVE_BLANK_FOR_A_RANDOM_USERNAME_DESCRIPTOR = msg({
	message: 'Leave blank for a random username',
	comment: 'Short label in the authentication auth register form core. Keep the tone plain and specific.',
});
const MAX_USERNAME_LENGTH = 32;

interface FieldConfig {
	showEmail?: boolean;
	showPassword?: boolean;
	showPasswordConfirmation?: boolean;
	showUsernameValidation?: boolean;
}

interface AuthRegisterFormCoreProps {
	fields?: FieldConfig;
	submitLabel: React.ReactNode;
	redirectPath: string;
	onRegister?: (response: AuthenticationCommands.TokenResponse) => Promise<void>;
	inviteCode?: string;
	extraContent?: React.ReactNode;
	showLegalConsent?: boolean;
	theme?: ThemeType;
}

export const AuthRegisterFormCore = observer(function AuthRegisterFormCore({
	fields = {},
	submitLabel,
	redirectPath,
	onRegister,
	inviteCode,
	extraContent,
	showLegalConsent = true,
	theme,
}: AuthRegisterFormCoreProps) {
	const {i18n} = useLingui();
	const {
		showEmail = false,
		showPassword = false,
		showPasswordConfirmation = false,
		showUsernameValidation = false,
	} = fields;
	const location = useLocation();
	const draftKey = `register:${location.pathname}${location.search}`;
	const registrationUrlCode = useMemo(() => {
		const value = new URLSearchParams(location.search).get('registration_url')?.trim();
		return value || undefined;
	}, [location.search]);
	const isPublicRegistrationClosed = RuntimeConfig.registration.mode === 'closed' && !registrationUrlCode;
	const collectDateOfBirth = RuntimeConfig.collectDateOfBirthOnRegistration;
	const {getRegisterFormDraft, setRegisterFormDraft, clearRegisterFormDraft} = useAuthRegisterDraftContext();
	const emailId = useId();
	const globalNameId = useId();
	const usernameId = useId();
	const passwordId = useId();
	const confirmPasswordId = useId();
	const initialDraft = useMemo<AuthRegisterFormDraft>(() => {
		const persistedDraft = getRegisterFormDraft(draftKey);
		if (!persistedDraft) {
			return EMPTY_AUTH_REGISTER_FORM_DRAFT;
		}
		return {
			...persistedDraft,
			formValues: {...persistedDraft.formValues},
		};
	}, [draftKey, getRegisterFormDraft]);
	const draftRef = useRef<AuthRegisterFormDraft>({
		...initialDraft,
		formValues: {...initialDraft.formValues},
	});
	const [selectedMonth, setSelectedMonthState] = useState(initialDraft.selectedMonth);
	const [selectedDay, setSelectedDayState] = useState(initialDraft.selectedDay);
	const [selectedYear, setSelectedYearState] = useState(initialDraft.selectedYear);
	const [consent, setConsentState] = useState(initialDraft.consent);
	const [pendingApprovalUserId, setPendingApprovalUserId] = useState<string | null>(null);
	const legalConsentConfig = getRegistrationLegalConsentConfig(showLegalConsent);
	const effectiveConsent = legalConsentConfig.requirement ? consent : true;
	const initialValues: Record<string, string> = {
		global_name: initialDraft.formValues.global_name ?? '',
		username: initialDraft.formValues.username ?? '',
	};
	if (showEmail) initialValues.email = initialDraft.formValues.email ?? '';
	if (showPassword) initialValues.password = initialDraft.formValues.password ?? '';
	if (showPassword && showPasswordConfirmation) {
		initialValues.confirm_password = initialDraft.formValues.confirm_password ?? '';
	}
	const persistDraft = useCallback(
		(partialDraft: Partial<AuthRegisterFormDraft>) => {
			const currentDraft = draftRef.current;
			const nextDraft: AuthRegisterFormDraft = {
				...currentDraft,
				...partialDraft,
				formValues: partialDraft.formValues ? {...partialDraft.formValues} : currentDraft.formValues,
			};
			draftRef.current = nextDraft;
			setRegisterFormDraft(draftKey, nextDraft);
		},
		[draftKey, setRegisterFormDraft],
	);
	const handleMonthChange = useCallback(
		(month: string) => {
			setSelectedMonthState(month);
			persistDraft({selectedMonth: month});
		},
		[persistDraft],
	);
	const handleDayChange = useCallback(
		(day: string) => {
			setSelectedDayState(day);
			persistDraft({selectedDay: day});
		},
		[persistDraft],
	);
	const handleYearChange = useCallback(
		(year: string) => {
			setSelectedYearState(year);
			persistDraft({selectedYear: year});
		},
		[persistDraft],
	);
	const handleConsentChange = useCallback(
		(nextConsent: boolean) => {
			setConsentState(nextConsent);
			persistDraft({consent: nextConsent});
		},
		[persistDraft],
	);
	const handleRegisterSubmit = async (values: Record<string, string>) => {
		if (showPasswordConfirmation && showPassword && values.password !== values.confirm_password) {
			form.setError('confirm_password', i18n._(PASSWORDS_DO_NOT_MATCH_DESCRIPTOR));
			return false;
		}
		if (isPublicRegistrationClosed) {
			return false;
		}
		setPendingApprovalUserId(null);
		const dateOfBirth =
			collectDateOfBirth && selectedYear && selectedMonth && selectedDay
				? `${selectedYear}-${selectedMonth.padStart(2, '0')}-${selectedDay.padStart(2, '0')}`
				: undefined;
		const response = await AuthenticationCommands.register({
			global_name: values.global_name || undefined,
			username: values.username || undefined,
			email: showEmail ? values.email : undefined,
			password: showPassword ? values.password : undefined,
			date_of_birth: dateOfBirth,
			consent: effectiveConsent,
			invite_code: inviteCode,
			registration_url_code: registrationUrlCode,
			theme,
		});
		if (AuthenticationCommands.isRegistrationPendingApprovalResponse(response)) {
			clearRegisterFormDraft(draftKey);
			setPendingApprovalUserId(response.user_id);
			return false;
		}
		if (onRegister) {
			await onRegister(response);
		} else {
			const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
			await AuthenticationCommands.completeLogin({
				token: response.token,
				userId: response.user_id,
				...(userData ? {userData} : {}),
			});
		}
		clearRegisterFormDraft(draftKey);
		return undefined;
	};
	const {form, isLoading, fieldErrors} = useAuthForm({
		initialValues,
		onSubmit: handleRegisterSubmit,
		redirectPath,
		firstFieldName: showEmail ? 'email' : 'global_name',
	});
	const setDraftedFormValue = useCallback(
		(fieldName: string, value: string) => {
			form.setValue(fieldName, value);
			const nextFormValues = {
				...draftRef.current.formValues,
				[fieldName]: value,
			};
			persistDraft({formValues: nextFormValues});
		},
		[form, persistDraft],
	);
	const {suggestions} = useUsernameSuggestions({
		globalName: form.getValue('global_name'),
		username: form.getValue('username'),
	});
	const missingFields = useMemo(() => {
		const missing: Array<MissingField> = [];
		if (showEmail && !form.getValue('email')) {
			missing.push({key: 'email', label: i18n._(EMAIL_DESCRIPTOR)});
		}
		if (showPassword && !form.getValue('password')) {
			missing.push({key: 'password', label: i18n._(PASSWORD_DESCRIPTOR)});
		}
		if (showPassword && showPasswordConfirmation && !form.getValue('confirm_password')) {
			missing.push({key: 'confirm_password', label: i18n._(CONFIRM_PASSWORD_DESCRIPTOR)});
		}
		if (collectDateOfBirth && (!selectedMonth || !selectedDay || !selectedYear)) {
			missing.push({key: 'date_of_birth', label: i18n._(DATE_OF_BIRTH_DESCRIPTOR)});
		}
		return missing;
	}, [
		form,
		selectedMonth,
		selectedDay,
		selectedYear,
		showEmail,
		showPassword,
		showPasswordConfirmation,
		collectDateOfBirth,
		i18n.locale,
	]);
	type HelperTextState = {type: 'error'; message: string} | {type: 'suggestion'; username: string} | null;
	const usernameValue = form.getValue('username');
	const helperTextState = useMemo<HelperTextState>(() => {
		const trimmed = usernameValue?.trim() || '';
		if (showUsernameValidation && trimmed.length > 0) {
			if (trimmed.length > MAX_USERNAME_LENGTH) {
				return {
					type: 'error',
					message: i18n._(USERNAME_MUST_BE_CHARACTERS_OR_LESS_DESCRIPTOR, {maxUsernameLength: MAX_USERNAME_LENGTH}),
				};
			}
			if (!/^[a-zA-Z0-9_]+$/.test(trimmed)) {
				return {type: 'error', message: i18n._(ONLY_LETTERS_NUMBERS_AND_UNDERSCORES_DESCRIPTOR)};
			}
		}
		if (trimmed.length === 0 && suggestions.length === 1) {
			return {type: 'suggestion', username: suggestions[0]};
		}
		return null;
	}, [usernameValue, suggestions, showUsernameValidation, i18n.locale]);
	const submitDisabled =
		isLoading ||
		form.isSubmitting ||
		isPublicRegistrationClosed ||
		pendingApprovalUserId !== null ||
		shouldDisableSubmit(effectiveConsent, missingFields);
	return (
		<form className={styles.form} onSubmit={form.handleSubmit} data-flx="auth.flow.auth-register-form-core.form.submit">
			{isPublicRegistrationClosed ? (
				<div
					className={styles.registrationNotice}
					role="alert"
					data-flx="auth.flow.auth-register-form-core.closed-notice"
				>
					<Trans>Registration is currently closed. Use a registration link from an admin to create an account.</Trans>
				</div>
			) : null}
			{pendingApprovalUserId ? (
				<div
					className={styles.registrationNotice}
					role="status"
					data-flx="auth.flow.auth-register-form-core.pending-approval-notice"
				>
					<Trans>Your account request is pending approval. You can sign in after an admin approves it.</Trans>
				</div>
			) : null}
			{showEmail && (
				<FormField
					id={emailId}
					name="email"
					type="email"
					autoComplete="email"
					required
					label={i18n._(EMAIL_DESCRIPTOR)}
					value={form.getValue('email')}
					onChange={(value) => setDraftedFormValue('email', value)}
					error={form.getError('email') || fieldErrors?.email}
					data-flx="auth.flow.auth-register-form-core.form-field.set-drafted-form-value.email"
				/>
			)}
			<FormField
				id={globalNameId}
				name="global_name"
				type="text"
				label={i18n._(DISPLAY_NAME_OPTIONAL_DESCRIPTOR)}
				placeholder={i18n._(WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR)}
				value={form.getValue('global_name')}
				onChange={(value) => setDraftedFormValue('global_name', value)}
				error={form.getError('global_name') || fieldErrors?.global_name}
				data-flx="auth.flow.auth-register-form-core.form-field.set-drafted-form-value.text"
			/>
			<div data-flx="auth.flow.auth-register-form-core.div">
				<FormField
					id={usernameId}
					name="username"
					type="text"
					autoComplete="username"
					label={i18n._(USERNAME_OPTIONAL_DESCRIPTOR)}
					placeholder={i18n._(LEAVE_BLANK_FOR_A_RANDOM_USERNAME_DESCRIPTOR)}
					value={usernameValue}
					onChange={(value) => setDraftedFormValue('username', value)}
					error={form.getError('username') || fieldErrors?.username}
					data-flx="auth.flow.auth-register-form-core.form-field.set-drafted-form-value.text--2"
				/>
				<AnimatePresence mode="wait" initial={false} data-flx="auth.flow.auth-register-form-core.animate-presence">
					{helperTextState?.type === 'error' && (
						<motion.span
							key="error"
							className={styles.usernameError}
							initial={Accessibility.useReducedMotion ? {opacity: 1, y: 0} : {opacity: 0, y: -5}}
							animate={{opacity: 1, y: 0}}
							exit={Accessibility.useReducedMotion ? {opacity: 1, y: 0} : {opacity: 0, y: 5}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
							data-flx="auth.flow.auth-register-form-core.username-error"
						>
							{helperTextState.message}
						</motion.span>
					)}
					{helperTextState?.type === 'suggestion' && (
						<motion.span
							key="suggestion"
							className={styles.usernameHint}
							initial={Accessibility.useReducedMotion ? {opacity: 1, y: 0} : {opacity: 0, y: -5}}
							animate={{opacity: 1, y: 0}}
							exit={Accessibility.useReducedMotion ? {opacity: 1, y: 0} : {opacity: 0, y: 5}}
							transition={{duration: Accessibility.useReducedMotion ? 0 : 0.2}}
							data-flx="auth.flow.auth-register-form-core.username-hint"
						>
							<Trans>How about:</Trans>{' '}
							<button
								type="button"
								className={styles.suggestionLink}
								onClick={() => setDraftedFormValue('username', helperTextState.username)}
								data-flx="auth.flow.auth-register-form-core.suggestion-link.set-drafted-form-value.button"
							>
								{helperTextState.username}
							</button>
						</motion.span>
					)}
				</AnimatePresence>
			</div>
			{showPassword && (
				<FormField
					id={passwordId}
					name="password"
					type="password"
					autoComplete="new-password"
					required
					label={i18n._(PASSWORD_DESCRIPTOR)}
					value={form.getValue('password')}
					onChange={(value) => setDraftedFormValue('password', value)}
					error={form.getError('password') || fieldErrors?.password}
					data-flx="auth.flow.auth-register-form-core.form-field.set-drafted-form-value.password"
				/>
			)}
			{showPassword && showPasswordConfirmation && (
				<FormField
					id={confirmPasswordId}
					name="confirm_password"
					type="password"
					autoComplete="new-password"
					required
					label={i18n._(CONFIRM_PASSWORD_DESCRIPTOR)}
					value={form.getValue('confirm_password')}
					onChange={(value) => setDraftedFormValue('confirm_password', value)}
					error={form.getError('confirm_password')}
					data-flx="auth.flow.auth-register-form-core.form-field.set-drafted-form-value.password--2"
				/>
			)}
			{collectDateOfBirth ? (
				<DateOfBirthField
					selectedMonth={selectedMonth}
					selectedDay={selectedDay}
					selectedYear={selectedYear}
					onMonthChange={handleMonthChange}
					onDayChange={handleDayChange}
					onYearChange={handleYearChange}
					error={fieldErrors?.date_of_birth}
					data-flx="auth.flow.auth-register-form-core.date-of-birth-field"
				/>
			) : null}
			{extraContent}
			<RegistrationLegalConsent
				checked={consent}
				config={legalConsentConfig}
				onChange={handleConsentChange}
				data-flx="auth.flow.auth-register-form-core.registration-legal-consent.consent-change"
			/>
			<SubmitTooltip
				consent={effectiveConsent}
				legalConsentRequirement={legalConsentConfig.requirement ?? undefined}
				missingFields={missingFields}
				data-flx="auth.flow.auth-register-form-core.submit-tooltip"
			>
				<Button
					type="submit"
					fitContainer
					disabled={submitDisabled}
					data-flx="auth.flow.auth-register-form-core.button.submit"
				>
					{submitLabel}
				</Button>
			</SubmitTooltip>
		</form>
	);
});

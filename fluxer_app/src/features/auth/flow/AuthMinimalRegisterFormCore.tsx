// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Button} from '@app/features/ui/button/Button';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useId, useMemo, useRef, useState} from 'react';

const DATE_OF_BIRTH_DESCRIPTOR = msg({
	message: 'Date of birth',
	comment: 'Short label in the authentication auth minimal register form core. Keep the tone plain and specific.',
});
const DISPLAY_NAME_OPTIONAL_DESCRIPTOR = msg({
	message: 'Display name (optional)',
	comment: 'Short label in the authentication auth minimal register form core. Keep the tone plain and specific.',
});
const WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR = msg({
	message: 'What should people call you?',
	comment: 'Question prompt in the authentication auth minimal register form core. Keep the tone plain and specific.',
});

interface AuthMinimalRegisterFormCoreProps {
	submitLabel: React.ReactNode;
	redirectPath: string;
	onRegister?: (response: AuthenticationCommands.TokenResponse) => Promise<void>;
	inviteCode?: string;
	extraContent?: React.ReactNode;
	theme?: ThemeType;
}

export const AuthMinimalRegisterFormCore = observer(function AuthMinimalRegisterFormCore({
	submitLabel,
	redirectPath,
	onRegister,
	inviteCode,
	extraContent,
	theme,
}: AuthMinimalRegisterFormCoreProps) {
	const {i18n} = useLingui();
	const location = useLocation();
	const draftKey = `register:${location.pathname}${location.search}`;
	const registrationUrlCode = useMemo(() => {
		const value = new URLSearchParams(location.search).get('registration_url')?.trim();
		return value || undefined;
	}, [location.search]);
	const isPublicRegistrationClosed = RuntimeConfig.registration.mode === 'closed' && !registrationUrlCode;
	const collectDateOfBirth = RuntimeConfig.collectDateOfBirthOnRegistration;
	const {getRegisterFormDraft, setRegisterFormDraft, clearRegisterFormDraft} = useAuthRegisterDraftContext();
	const globalNameId = useId();
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
	const legalConsentConfig = getRegistrationLegalConsentConfig();
	const effectiveConsent = legalConsentConfig.requirement ? consent : true;
	const initialValues: Record<string, string> = {
		global_name: initialDraft.formValues.global_name ?? '',
	};
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
		firstFieldName: 'global_name',
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
	const missingFields = useMemo(() => {
		const missing: Array<MissingField> = [];
		if (collectDateOfBirth && (!selectedMonth || !selectedDay || !selectedYear)) {
			missing.push({key: 'date_of_birth', label: i18n._(DATE_OF_BIRTH_DESCRIPTOR)});
		}
		return missing;
	}, [selectedMonth, selectedDay, selectedYear, collectDateOfBirth, i18n.locale]);
	const globalNameValue = form.getValue('global_name');
	const submitDisabled =
		isLoading ||
		form.isSubmitting ||
		isPublicRegistrationClosed ||
		pendingApprovalUserId !== null ||
		shouldDisableSubmit(effectiveConsent, missingFields);
	return (
		<form
			className={styles.form}
			onSubmit={form.handleSubmit}
			data-flx="auth.flow.auth-minimal-register-form-core.form.submit"
		>
			{isPublicRegistrationClosed ? (
				<div
					className={styles.registrationNotice}
					role="alert"
					data-flx="auth.flow.auth-minimal-register-form-core.closed-notice"
				>
					<Trans>Registration is currently closed. Use a registration link from an admin to create an account.</Trans>
				</div>
			) : null}
			{pendingApprovalUserId ? (
				<div
					className={styles.registrationNotice}
					role="status"
					data-flx="auth.flow.auth-minimal-register-form-core.pending-approval-notice"
				>
					<Trans>Your account request is pending approval. You can sign in after an admin approves it.</Trans>
				</div>
			) : null}
			<FormField
				id={globalNameId}
				name="global_name"
				type="text"
				label={i18n._(DISPLAY_NAME_OPTIONAL_DESCRIPTOR)}
				placeholder={i18n._(WHAT_SHOULD_PEOPLE_CALL_YOU_DESCRIPTOR)}
				value={globalNameValue}
				onChange={(value) => setDraftedFormValue('global_name', value)}
				error={form.getError('global_name') || fieldErrors?.global_name}
				data-flx="auth.flow.auth-minimal-register-form-core.form-field.set-drafted-form-value.text"
			/>
			{collectDateOfBirth ? (
				<DateOfBirthField
					selectedMonth={selectedMonth}
					selectedDay={selectedDay}
					selectedYear={selectedYear}
					onMonthChange={handleMonthChange}
					onDayChange={handleDayChange}
					onYearChange={handleYearChange}
					error={fieldErrors?.date_of_birth}
					data-flx="auth.flow.auth-minimal-register-form-core.date-of-birth-field"
				/>
			) : null}
			{extraContent}
			<RegistrationLegalConsent
				checked={consent}
				config={legalConsentConfig}
				onChange={handleConsentChange}
				data-flx="auth.flow.auth-minimal-register-form-core.registration-legal-consent.consent-change"
			/>
			<SubmitTooltip
				consent={effectiveConsent}
				legalConsentRequirement={legalConsentConfig.requirement ?? undefined}
				missingFields={missingFields}
				data-flx="auth.flow.auth-minimal-register-form-core.submit-tooltip"
			>
				<Button
					type="submit"
					fitContainer
					disabled={submitDisabled}
					data-flx="auth.flow.auth-minimal-register-form-core.button.submit"
				>
					{submitLabel}
				</Button>
			</SubmitTooltip>
		</form>
	);
});

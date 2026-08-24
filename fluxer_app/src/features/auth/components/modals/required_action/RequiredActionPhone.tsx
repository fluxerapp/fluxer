// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MFA_CODE_DIGIT_COUNT,
	PHONE_VERIFICATION_LIMIT,
	PHONE_VERIFICATION_WINDOW_DAYS,
} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/auth/components/modals/RequiredActionModal.module.css';
import {
	COUNTRY_DESCRIPTOR,
	ENTER_PHONE_CODE_DESCRIPTION_DESCRIPTOR,
	ENTER_PHONE_CODE_TITLE_DESCRIPTOR,
	ENTER_VALID_PHONE_DESCRIPTOR,
	INBOUND_PHONE_CODE_LABEL_DESCRIPTOR,
	INBOUND_PHONE_DEFAULT_REASON_DESCRIPTOR,
	INBOUND_PHONE_DESTINATION_LABEL_DESCRIPTOR,
	INBOUND_PHONE_EXPENSIVE_REASON_DESCRIPTOR,
	INBOUND_PHONE_PREPARE_DESCRIPTION_DESCRIPTOR,
	INBOUND_PHONE_PREPARE_TITLE_DESCRIPTOR,
	INBOUND_PHONE_SEND_DESCRIPTION_DESCRIPTOR,
	INBOUND_PHONE_SEND_TITLE_DESCRIPTOR,
	INBOUND_PHONE_START_DESCRIPTION_DESCRIPTOR,
	INBOUND_PHONE_START_TITLE_DESCRIPTOR,
	INBOUND_PHONE_WAIT_DESCRIPTION_DESCRIPTOR,
	INBOUND_PHONE_WAIT_TITLE_DESCRIPTOR,
	MESSAGE_RATES_NOTICE_DESCRIPTOR,
	PHONE_NUMBER_DESCRIPTION_DESCRIPTOR,
	PHONE_NUMBER_DESCRIPTOR,
	PHONE_NUMBER_IS_REQUIRED_DESCRIPTOR,
	PHONE_NUMBER_TITLE_DESCRIPTOR,
	PHONE_PRIVACY_DESCRIPTOR,
	PHONE_VERIFIED_DESCRIPTOR,
	PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR,
	SEARCH_COUNTRIES_DESCRIPTOR,
} from '@app/features/auth/components/modals/required_action/RequiredActionDescriptors';
import {
	StepShell,
	useRequiredActionFormSubmit,
	ValueBlock,
	VerificationCodeForm,
} from '@app/features/auth/components/modals/required_action/RequiredActionShared';
import {
	type ActiveInboundChallenge,
	type CodeFormInputs,
	normalizeVerificationCode,
	type PhoneFormInputs,
	type PhoneInboundChallengeReason,
	type PhoneScreen,
	type SubmitCallback,
} from '@app/features/auth/components/modals/required_action/RequiredActionTypes';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Combobox, type ComboboxFilterOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {
	COUNTRY_CODES,
	type CountryCode,
	formatPhoneNumber,
	getCountryName,
	getDefaultCountry,
	getE164PhoneNumber,
	getPhoneNumberPlaceholder,
} from '@app/media/data/CountryCodes';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {type UseFormReturn, useForm} from 'react-hook-form';

interface CountrySelectOption {
	value: string;
	label: string;
	country: CountryCode;
}

const getCountryOptions = (locale: string): ReadonlyArray<CountrySelectOption> =>
	COUNTRY_CODES.map((country) => ({
		value: country.code,
		label: `${getCountryName(country.code, locale)} (${country.dialCode})`,
		country,
	}));
const renderCountryOption = (option: CountrySelectOption) => {
	const {country} = option;
	const locale = LocaleUtils.getCurrentLocale();
	const countryName = getCountryName(country.code, locale);
	const flagUrl = EmojiUtils.getEmojiURL(country.flag);
	return (
		<div className={styles.countryOption} data-flx="auth.required-action-modal.country-option.country-option">
			{flagUrl ? (
				<img
					src={flagUrl}
					alt={countryName}
					aria-hidden={true}
					className={styles.countryFlag}
					data-flx="auth.required-action-modal.country-option.country-flag"
				/>
			) : (
				<span
					className={styles.countryFlagText}
					role="img"
					aria-label={countryName}
					data-flx="auth.required-action-modal.country-option.country-flag-text"
				>
					{country.flag}
				</span>
			)}
			<span data-flx="auth.required-action-modal.country-option.span">{countryName}</span>
			<span className={styles.countryDialCode} data-flx="auth.required-action-modal.country-option.country-dial-code">
				({country.dialCode})
			</span>
		</div>
	);
};
const renderCountryValue = (option: CountrySelectOption | null) => {
	if (!option) return null;
	const {country} = option;
	const locale = LocaleUtils.getCurrentLocale();
	const countryName = getCountryName(country.code, locale);
	const flagUrl = EmojiUtils.getEmojiURL(country.flag);
	return (
		<div className={styles.countryValue} data-flx="auth.required-action-modal.single-value.country-value">
			{flagUrl ? (
				<img
					src={flagUrl}
					alt={countryName}
					className={styles.countryFlag}
					data-flx="auth.required-action-modal.single-value.country-flag"
				/>
			) : (
				<span
					className={styles.countryFlagText}
					role="img"
					aria-label={countryName}
					data-flx="auth.required-action-modal.single-value.country-flag-text"
				>
					{country.flag}
				</span>
			)}
			<span data-flx="auth.required-action-modal.single-value.span">{country.dialCode}</span>
		</div>
	);
};

function getInboundReasonText(i18n: I18n, reason: PhoneInboundChallengeReason | null): string {
	return reason === 'expensive_destination'
		? i18n._(INBOUND_PHONE_EXPENSIVE_REASON_DESCRIPTOR)
		: i18n._(INBOUND_PHONE_DEFAULT_REASON_DESCRIPTOR);
}

interface InboundPhoneStartStepProps {
	requiresInboundPhone: boolean;
}

export const InboundPhoneStartStep: React.FC<InboundPhoneStartStepProps> = ({requiresInboundPhone}) => {
	const {i18n} = useLingui();
	return (
		<StepShell
			title={i18n._(INBOUND_PHONE_START_TITLE_DESCRIPTOR)}
			description={i18n._(INBOUND_PHONE_START_DESCRIPTION_DESCRIPTOR)}
			notice={requiresInboundPhone ? null : i18n._(MESSAGE_RATES_NOTICE_DESCRIPTOR)}
			data-flx="auth.required-action.required-action-phone.inbound-phone-start-step.step-shell"
		/>
	);
};

interface InboundPhoneInstructionStepProps {
	challenge: ActiveInboundChallenge;
	kind: 'prepare' | 'send' | 'wait';
}

export const InboundPhoneInstructionStep: React.FC<InboundPhoneInstructionStepProps> = ({challenge, kind}) => {
	const {i18n} = useLingui();
	if (kind === 'prepare') {
		return (
			<StepShell
				title={i18n._(INBOUND_PHONE_PREPARE_TITLE_DESCRIPTOR)}
				description={i18n._(INBOUND_PHONE_PREPARE_DESCRIPTION_DESCRIPTOR)}
				notice={getInboundReasonText(i18n, challenge.reason)}
				data-flx="auth.required-action.required-action-phone.inbound-phone-instruction-step.step-shell"
			/>
		);
	}
	if (kind === 'send') {
		return (
			<StepShell
				title={i18n._(INBOUND_PHONE_SEND_TITLE_DESCRIPTOR)}
				description={i18n._(INBOUND_PHONE_SEND_DESCRIPTION_DESCRIPTOR)}
				data-flx="auth.required-action.required-action-phone.inbound-phone-instruction-step.step-shell--2"
			>
				<div className={styles.valueStack} data-flx="auth.required-action-modal.inbound-phone.value-stack">
					<ValueBlock
						label={i18n._(INBOUND_PHONE_CODE_LABEL_DESCRIPTOR)}
						value={
							<span className={styles.selectable} data-flx="auth.required-action-modal.inbound-phone.code">
								{challenge.code}
							</span>
						}
						data-flx="auth.required-action.required-action-phone.inbound-phone-instruction-step.value-block"
					/>
					<ValueBlock
						label={i18n._(INBOUND_PHONE_DESTINATION_LABEL_DESCRIPTOR)}
						value={
							<span className={styles.selectable} data-flx="auth.required-action-modal.inbound-phone.destination">
								{challenge.ourNumber}
							</span>
						}
						data-flx="auth.required-action.required-action-phone.inbound-phone-instruction-step.value-block--2"
					/>
				</div>
			</StepShell>
		);
	}
	return (
		<StepShell
			title={i18n._(INBOUND_PHONE_WAIT_TITLE_DESCRIPTOR)}
			description={i18n._(INBOUND_PHONE_WAIT_DESCRIPTION_DESCRIPTOR)}
			notice={i18n._(MESSAGE_RATES_NOTICE_DESCRIPTOR)}
			data-flx="auth.required-action.required-action-phone.inbound-phone-instruction-step.step-shell--3"
		/>
	);
};

interface PhoneNumberFormProps {
	form: UseFormReturn<PhoneFormInputs>;
	onSubmit: SubmitCallback;
	selectedCountry: CountryCode;
	formattedPhone: string;
	onCountryChange: (country: CountryCode) => void;
	onPhoneInput: (event: React.ChangeEvent<HTMLInputElement>) => void;
}

export const PhoneNumberStep: React.FC<PhoneNumberFormProps> = ({
	form,
	onSubmit,
	selectedCountry,
	formattedPhone,
	onCountryChange,
	onPhoneInput,
}) => {
	const {i18n} = useLingui();
	const locale = LocaleUtils.getCurrentLocale();
	const countryOptions = useMemo(() => getCountryOptions(locale), [locale]);
	return (
		<Form form={form} onSubmit={onSubmit} data-flx="auth.required-action-modal.phone-number-form.form.submit">
			<StepShell
				title={i18n._(PHONE_NUMBER_TITLE_DESCRIPTOR)}
				description={i18n._(PHONE_NUMBER_DESCRIPTION_DESCRIPTOR)}
				notice={i18n._(PHONE_PRIVACY_DESCRIPTOR, {
					limit: PHONE_VERIFICATION_LIMIT,
					duration: PHONE_VERIFICATION_WINDOW_DAYS,
				})}
				data-flx="auth.required-action.required-action-phone.phone-number-step.step-shell"
			>
				<div
					className={styles.phoneInputContainer}
					data-flx="auth.required-action-modal.phone-number-form.phone-input-container"
				>
					<Combobox
						label={i18n._(COUNTRY_DESCRIPTOR)}
						value={selectedCountry.code}
						onChange={(value) => {
							const country = countryOptions.find((o) => o.value === value)?.country;
							if (country) {
								onCountryChange(country);
							}
						}}
						options={countryOptions}
						renderOption={renderCountryOption}
						renderValue={renderCountryValue}
						placeholder={i18n._(SEARCH_COUNTRIES_DESCRIPTOR)}
						filterOption={(option: ComboboxFilterOption<CountrySelectOption>, inputValue: string) => {
							const searchTerm = inputValue.toLowerCase();
							const countryName = getCountryName(option.data.country.code, locale);
							return (
								countryName.toLowerCase().includes(searchTerm) ||
								option.data.country.dialCode.includes(searchTerm) ||
								option.data.country.code.toLowerCase().includes(searchTerm)
							);
						}}
						data-flx="auth.required-action-modal.phone-number-form.select"
					/>

					<Input
						data-step-focus="true"
						data-flx="auth.required-action-modal.phone-number-form.input"
						{...form.register('phoneNumber')}
						autoComplete="tel"
						autoFocus={true}
						value={formattedPhone}
						onChange={onPhoneInput}
						error={form.formState.errors.phoneNumber?.message}
						label={i18n._(PHONE_NUMBER_DESCRIPTOR)}
						placeholder={getPhoneNumberPlaceholder(selectedCountry)}
						required={true}
					/>
				</div>
			</StepShell>
		</Form>
	);
};

interface PhoneCodeStepProps {
	form: UseFormReturn<CodeFormInputs>;
	onSubmit: SubmitCallback;
	recipient: string;
}

export const PhoneCodeStep: React.FC<PhoneCodeStepProps> = ({form, onSubmit, recipient}) => {
	const {i18n} = useLingui();
	return (
		<VerificationCodeForm
			form={form}
			onSubmit={onSubmit}
			title={i18n._(ENTER_PHONE_CODE_TITLE_DESCRIPTOR)}
			description={i18n._(ENTER_PHONE_CODE_DESCRIPTION_DESCRIPTOR, {
				digitCount: MFA_CODE_DIGIT_COUNT,
				phoneNumber: recipient,
			})}
			data-flx="auth.required-action.required-action-phone.phone-code-step.verification-code-form.submit"
		/>
	);
};

interface UsePhoneVerificationParams {
	mock: boolean;
	requiresInboundPhone: boolean;
	resetKey: string | undefined;
}

export interface PhoneVerificationController {
	screen: PhoneScreen;
	phoneForm: UseFormReturn<PhoneFormInputs>;
	codeForm: UseFormReturn<CodeFormInputs>;
	isPhoneSubmitting: boolean;
	isCodeSubmitting: boolean;
	isStartingInbound: boolean;
	selectedCountry: CountryCode;
	formattedPhone: string;
	phoneCodeRecipient: string;
	onStartInbound: SubmitCallback;
	onPhoneFormSubmit: SubmitCallback;
	onCodeFormSubmit: SubmitCallback;
	onCountryChange: (country: CountryCode) => void;
	onPhoneInput: (event: React.ChangeEvent<HTMLInputElement>) => void;
	onRefreshInboundChallenge: () => Promise<'inbound-challenge' | 'phone-code' | null>;
	onBackToPhone: () => void;
}

export const usePhoneVerification = ({
	mock,
	requiresInboundPhone,
	resetKey,
}: UsePhoneVerificationParams): PhoneVerificationController => {
	const {i18n} = useLingui();
	const initialPhoneScreen = (): PhoneScreen =>
		DeveloperOptions.mockRequiredActionsPhoneStep === 'code'
			? {kind: 'phone-code', recipient: ''}
			: {kind: 'phone-number'};
	const [screen, setScreen] = useState<PhoneScreen>(initialPhoneScreen);
	const [selectedCountry, setSelectedCountry] = useState<CountryCode>(getDefaultCountry());
	const [phoneNumber, setPhoneNumber] = useState('');
	const [formattedPhone, setFormattedPhone] = useState('');
	const [isStartingInbound, setIsStartingInbound] = useState(false);
	const phoneForm = useForm<PhoneFormInputs>();
	const codeForm = useForm<CodeFormInputs>();
	const defaultScreen = useCallback(
		(): PhoneScreen => (requiresInboundPhone ? {kind: 'phone-inbound-start'} : {kind: 'phone-number'}),
		[requiresInboundPhone],
	);
	useEffect(() => {
		setFormattedPhone(formatPhoneNumber(phoneNumber, selectedCountry));
	}, [phoneNumber, selectedCountry]);
	useEffect(() => {
		if (mock) {
			setScreen(initialPhoneScreen());
		} else if (resetKey) {
			setScreen(defaultScreen());
			setPhoneNumber('');
			setFormattedPhone('');
			phoneForm.reset();
			codeForm.reset();
		}
	}, [mock, resetKey, defaultScreen, phoneForm, codeForm]);
	const getCurrentE164PhoneNumber = useCallback(
		() => getE164PhoneNumber(phoneNumber, selectedCountry),
		[phoneNumber, selectedCountry],
	);
	const setInvalidPhoneNumberError = useCallback(() => {
		phoneForm.setError('phoneNumber', {message: i18n._(PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR)});
	}, [phoneForm, i18n]);
	const onStartInbound = useCallback(async () => {
		if (requiresInboundPhone) {
			setIsStartingInbound(true);
			try {
				const challenge = await UserCommands.startInboundPhoneChallenge();
				setScreen({
					kind: 'phone-inbound-challenge',
					code: challenge.challenge_code,
					ourNumber: challenge.our_number,
					reason: null,
				});
			} finally {
				setIsStartingInbound(false);
			}
			return;
		}
		if (!phoneNumber) {
			phoneForm.setError('phoneNumber', {message: i18n._(PHONE_NUMBER_IS_REQUIRED_DESCRIPTOR)});
			return;
		}
		const e164Phone = getCurrentE164PhoneNumber();
		if (!e164Phone) {
			setInvalidPhoneNumberError();
			return;
		}
		const verification = await UserCommands.sendPhoneVerification(e164Phone);
		if (verification.channel === 'inbound_challenge') {
			setScreen({
				kind: 'phone-inbound-challenge',
				code: verification.challenge_code,
				ourNumber: verification.our_number,
				reason: verification.reason,
			});
			return;
		}
		setScreen({kind: 'phone-code', recipient: e164Phone});
	}, [getCurrentE164PhoneNumber, phoneForm, phoneNumber, requiresInboundPhone, setInvalidPhoneNumberError, i18n]);
	const returnToPhoneStepWithError = useCallback(() => {
		setScreen({kind: 'phone-number'});
		phoneForm.setError('phoneNumber', {message: i18n._(ENTER_VALID_PHONE_DESCRIPTOR)});
	}, [phoneForm, i18n]);
	const onSubmitCode = useCallback(
		async (data: CodeFormInputs) => {
			const e164Phone = getCurrentE164PhoneNumber();
			if (!e164Phone) {
				returnToPhoneStepWithError();
				return;
			}
			await UserCommands.verifyPhone(e164Phone, normalizeVerificationCode(data.code));
			ToastCommands.success(i18n._(PHONE_VERIFIED_DESCRIPTOR));
		},
		[getCurrentE164PhoneNumber, returnToPhoneStepWithError, i18n],
	);
	const onRefreshInboundChallenge = useCallback(async (): Promise<'inbound-challenge' | 'phone-code' | null> => {
		if (screen.kind !== 'phone-inbound-challenge') return null;
		const currentReason = screen.reason;
		setIsStartingInbound(true);
		try {
			if (currentReason === 'expensive_destination') {
				const e164Phone = getCurrentE164PhoneNumber();
				if (!e164Phone) {
					returnToPhoneStepWithError();
					return null;
				}
				const verification = await UserCommands.sendPhoneVerification(e164Phone);
				if (verification.channel === 'inbound_challenge') {
					setScreen({
						kind: 'phone-inbound-challenge',
						code: verification.challenge_code,
						ourNumber: verification.our_number,
						reason: verification.reason,
					});
					return 'inbound-challenge';
				}
				setScreen({kind: 'phone-code', recipient: e164Phone});
				return 'phone-code';
			}
			const challenge = await UserCommands.startInboundPhoneChallenge();
			setScreen({
				kind: 'phone-inbound-challenge',
				code: challenge.challenge_code,
				ourNumber: challenge.our_number,
				reason: null,
			});
			return 'inbound-challenge';
		} finally {
			setIsStartingInbound(false);
		}
	}, [getCurrentE164PhoneNumber, screen, returnToPhoneStepWithError]);
	const {handleSubmit: onPhoneFormSubmit, isSubmitting: isPhoneSubmitting} = useRequiredActionFormSubmit(
		phoneForm,
		onStartInbound,
		'phoneNumber',
		'phone-number',
	);
	const {handleSubmit: onCodeFormSubmit, isSubmitting: isCodeSubmitting} = useRequiredActionFormSubmit(
		codeForm,
		onSubmitCode,
		'code',
		'phone-code',
	);
	const onPhoneInput = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
		setPhoneNumber(e.target.value.replace(/\D/g, ''));
	}, []);
	const onCountryChange = useCallback((country: CountryCode) => {
		setSelectedCountry(country);
		setPhoneNumber('');
		setFormattedPhone('');
	}, []);
	const onBackToPhone = useCallback(() => {
		setScreen({kind: 'phone-number'});
		codeForm.reset();
	}, [codeForm]);
	const phoneCodeRecipient =
		screen.kind === 'phone-code' ? screen.recipient : (getCurrentE164PhoneNumber() ?? formattedPhone);
	return {
		screen,
		phoneForm,
		codeForm,
		isPhoneSubmitting,
		isCodeSubmitting,
		isStartingInbound,
		selectedCountry,
		formattedPhone,
		phoneCodeRecipient,
		onStartInbound,
		onPhoneFormSubmit,
		onCodeFormSubmit,
		onCountryChange,
		onPhoneInput,
		onRefreshInboundChallenge,
		onBackToPhone,
	};
};

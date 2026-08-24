// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	MFA_CODE_DIGIT_COUNT,
	PHONE_VERIFICATION_LIMIT,
	PHONE_VERIFICATION_WINDOW_DAYS,
	PRODUCT_NAME,
	SUPPORT_EMAIL,
	SUPPORT_EMAIL_MAILTO,
} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {VERIFICATION_CODE_DESCRIPTOR, VERIFY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Combobox, type ComboboxFilterOption} from '@app/features/ui/components/form/FormCombobox';
import {Input} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import styles from '@app/features/user/components/modals/PhoneAddModal.module.css';
import Users from '@app/features/user/state/Users';
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
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';
import {useForm} from 'react-hook-form';

const PHONE_NUMBER_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Phone number is required',
	comment: 'Label in the phone add modal.',
});
const PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR = msg({
	message: 'Enter a valid phone number',
	comment: 'Label in the phone add modal.',
});
const VERIFY_PHONE_NUMBER_FORM_DESCRIPTOR = msg({
	message: 'Verify phone number form',
	comment: 'Label in the phone add modal.',
});
const VERIFY_PHONE_NUMBER_DESCRIPTOR = msg({
	message: 'Verify phone number',
	comment: 'Short label in the phone add modal. Keep it concise.',
});
const COUNTRY_DESCRIPTOR = msg({
	message: 'Country',
	comment: 'Short label in the phone add modal. Keep it concise.',
});
const SEARCH_COUNTRIES_DESCRIPTOR = msg({
	message: 'Search countries...',
	comment: 'Button or menu action label in the phone add modal. Keep it concise.',
});
const PHONE_NUMBER_DESCRIPTOR = msg({
	message: 'Phone number',
	comment: 'Short label in the phone add modal. Keep it concise.',
});

interface PhoneFormInputs {
	phoneNumber: string;
}

interface CodeFormInputs {
	code: string;
}

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
		<div className={styles.flagOption} data-flx="user.phone-add-modal.country-option.flag-option">
			{flagUrl ? (
				<img
					src={flagUrl}
					alt={countryName}
					aria-hidden={true}
					className={styles.flagImage}
					data-flx="user.phone-add-modal.country-option.flag-image"
				/>
			) : (
				<span
					className={styles.flagImageText}
					role="img"
					aria-label={countryName}
					data-flx="user.phone-add-modal.country-option.flag-image-text"
				>
					{country.flag}
				</span>
			)}
			<span data-flx="user.phone-add-modal.country-option.span">{countryName}</span>
			<span className={styles.dialCodeText} data-flx="user.phone-add-modal.country-option.dial-code-text">
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
		<div className={styles.flagOption} data-flx="user.phone-add-modal.single-value.flag-option">
			{flagUrl ? (
				<img
					src={flagUrl}
					alt={countryName}
					className={styles.flagImage}
					data-flx="user.phone-add-modal.single-value.flag-image"
				/>
			) : (
				<span
					className={styles.flagImageText}
					role="img"
					aria-label={countryName}
					data-flx="user.phone-add-modal.single-value.flag-image-text"
				>
					{country.flag}
				</span>
			)}
			<span data-flx="user.phone-add-modal.single-value.span">{country.dialCode}</span>
		</div>
	);
};

type PhoneAddStep = 'phone' | 'code' | 'inbound_waiting';

const PHONE_ADD_STEP_ORDER: ReadonlyArray<PhoneAddStep> = ['phone', 'code', 'inbound_waiting'];
export const PhoneAddModal = observer(() => {
	const {i18n} = useLingui();
	const locale = LocaleUtils.getCurrentLocale();
	const countryOptions = getCountryOptions(locale);
	const [step, setStep] = useState<PhoneAddStep>('phone');
	const [selectedCountry, setSelectedCountry] = useState<CountryCode>(getDefaultCountry());
	const [phoneNumber, setPhoneNumber] = useState('');
	const [formattedPhone, setFormattedPhone] = useState('');
	const [inboundChallenge, setInboundChallenge] =
		useState<UserCommands.PhoneSendVerificationInboundChallengeResponse | null>(null);
	const phoneForm = useForm<PhoneFormInputs>();
	const codeForm = useForm<CodeFormInputs>();
	useEffect(() => {
		const formatted = formatPhoneNumber(phoneNumber, selectedCountry);
		setFormattedPhone(formatted);
	}, [phoneNumber, selectedCountry]);
	const handlePhoneInput = (e: React.ChangeEvent<HTMLInputElement>) => {
		const value = e.target.value;
		const digitsOnly = value.replace(/\D/g, '');
		setPhoneNumber(digitsOnly);
	};
	const completePhoneVerification = () => {
		ModalCommands.pop();
		ToastCommands.createToast({type: 'success', children: <Trans>Phone number verified</Trans>});
	};
	useEffect(() => {
		if (step !== 'inbound_waiting' || Users.currentUser?.hasVerifiedPhone !== true) return;
		ModalCommands.pop();
		ToastCommands.createToast({type: 'success', children: <Trans>Phone number verified</Trans>});
	}, [step, Users.currentUser?.hasVerifiedPhone]);
	const onSubmitPhone = async () => {
		if (!phoneNumber) {
			phoneForm.setError('phoneNumber', {message: i18n._(PHONE_NUMBER_IS_REQUIRED_DESCRIPTOR)});
			return;
		}
		const e164Phone = getE164PhoneNumber(phoneNumber, selectedCountry);
		if (!e164Phone) {
			phoneForm.setError('phoneNumber', {message: i18n._(PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR)});
			return;
		}
		const verification = await UserCommands.sendPhoneVerification(e164Phone);
		if (verification.channel === 'inbound_challenge') {
			setInboundChallenge(verification);
			setStep('inbound_waiting');
			return;
		}
		setStep('code');
	};
	const onSubmitCode = async (data: CodeFormInputs) => {
		const e164Phone = getE164PhoneNumber(phoneNumber, selectedCountry);
		if (!e164Phone) {
			setStep('phone');
			phoneForm.setError('phoneNumber', {message: i18n._(PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR)});
			return;
		}
		await UserCommands.verifyPhone(e164Phone, data.code.split(' ').join(''));
		completePhoneVerification();
	};
	const handleInboundResend = async () => {
		const e164Phone = getE164PhoneNumber(phoneNumber, selectedCountry);
		if (!e164Phone) {
			setStep('phone');
			phoneForm.setError('phoneNumber', {message: i18n._(PLEASE_ENTER_A_VALID_PHONE_NUMBER_DESCRIPTOR)});
			return;
		}
		const verification = await UserCommands.sendPhoneVerification(e164Phone);
		if (verification.channel === 'inbound_challenge') {
			setInboundChallenge(verification);
			return;
		}
		setStep('code');
	};
	const {handleSubmit: handlePhoneSubmit} = useFormSubmit({
		form: phoneForm,
		onSubmit: onSubmitPhone,
		defaultErrorField: 'phoneNumber',
	});
	const {handleSubmit: handleCodeSubmit} = useFormSubmit({
		form: codeForm,
		onSubmit: onSubmitCode,
		defaultErrorField: 'code',
	});
	const renderPhoneStep = () => (
		<Form
			form={phoneForm}
			onSubmit={handlePhoneSubmit}
			aria-label={i18n._(VERIFY_PHONE_NUMBER_FORM_DESCRIPTOR)}
			data-flx="user.phone-add-modal.form.phone-submit"
		>
			<Modal.InputGroup data-flx="user.phone-add-modal.form-content">
				<Combobox
					label={i18n._(COUNTRY_DESCRIPTOR)}
					value={selectedCountry.code}
					onChange={(value) => {
						const country = countryOptions.find((o) => o.value === value)?.country;
						if (country) {
							setSelectedCountry(country);
							setPhoneNumber('');
							setFormattedPhone('');
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
					data-flx="user.phone-add-modal.select"
				/>
				<Input
					data-flx="user.phone-add-modal.input"
					{...phoneForm.register('phoneNumber')}
					autoComplete="tel"
					autoFocus={true}
					value={formattedPhone}
					onChange={handlePhoneInput}
					error={phoneForm.formState.errors.phoneNumber?.message}
					label={i18n._(PHONE_NUMBER_DESCRIPTOR)}
					placeholder={getPhoneNumberPlaceholder(selectedCountry)}
					required={true}
					footer={
						<p className={styles.footerText} data-flx="user.phone-add-modal.footer-text">
							<Trans>
								We'll send an SMS code when available. Your number is not linked to your account. We keep only an
								encrypted marker, with no user ID, to allow at most {PHONE_VERIFICATION_LIMIT} verifications in about{' '}
								{PHONE_VERIFICATION_WINDOW_DAYS} days.
							</Trans>
						</p>
					}
				/>
			</Modal.InputGroup>
		</Form>
	);
	const renderInboundWaitingStep = () => {
		if (!inboundChallenge) return null;
		return (
			<>
				<div className={styles.notice} data-flx="user.phone-add-modal.notice">
					<p className={styles.footerText} data-flx="user.phone-add-modal.footer-text--2">
						<Trans>
							Sending an SMS to this phone number is too expensive for {PRODUCT_NAME}, so we need you to send us an SMS
							instead. We know this isn't ideal. You can also contact{' '}
							<ExternalLink href={SUPPORT_EMAIL_MAILTO} data-flx="user.phone-add-modal.external-link">
								{SUPPORT_EMAIL}
							</ExternalLink>{' '}
							to have us lift this requirement from your account.
						</Trans>
					</p>
				</div>
				<div className={styles.stepsContainer} data-flx="user.phone-add-modal.steps-container">
					<div className={styles.stepRow} data-flx="user.phone-add-modal.step-row">
						<div className={styles.stepBadge} data-flx="user.phone-add-modal.step-badge">
							1
						</div>
						<p className={styles.footerText} data-flx="user.phone-add-modal.footer-text--3">
							<Trans>Open your phone's messaging app and create a new text message.</Trans>
						</p>
					</div>
					<div className={styles.stepRow} data-flx="user.phone-add-modal.step-row--2">
						<div className={styles.stepBadge} data-flx="user.phone-add-modal.step-badge--2">
							2
						</div>
						<p className={styles.footerText} data-flx="user.phone-add-modal.footer-text--4">
							<Trans>
								Send the code{' '}
								<strong className={styles.selectable} data-flx="user.phone-add-modal.selectable">
									{inboundChallenge.challenge_code}
								</strong>{' '}
								to{' '}
								<strong className={styles.selectable} data-flx="user.phone-add-modal.selectable--2">
									{inboundChallenge.our_number}
								</strong>
							</Trans>
						</p>
					</div>
					<div className={styles.stepRow} data-flx="user.phone-add-modal.step-row--3">
						<div className={styles.stepBadge} data-flx="user.phone-add-modal.step-badge--3">
							3
						</div>
						<p className={styles.footerText} data-flx="user.phone-add-modal.footer-text--5">
							<Trans>Wait a moment. This window will close automatically once we receive your message.</Trans>
						</p>
					</div>
				</div>
			</>
		);
	};
	const renderCodeStep = () => (
		<Form
			form={codeForm}
			onSubmit={handleCodeSubmit}
			aria-label={i18n._(VERIFY_PHONE_NUMBER_FORM_DESCRIPTOR)}
			data-flx="user.phone-add-modal.form.code-submit"
		>
			<Input
				data-flx="user.phone-add-modal.input--2"
				{...codeForm.register('code')}
				autoComplete="one-time-code"
				autoFocus={true}
				error={codeForm.formState.errors.code?.message}
				label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
				required={true}
				footer={
					<p className={styles.footerText} data-flx="user.phone-add-modal.footer-text--6">
						<Trans>
							Enter the code with {MFA_CODE_DIGIT_COUNT} digits sent to{' '}
							{getE164PhoneNumber(phoneNumber, selectedCountry) ?? formattedPhone}.
						</Trans>
					</p>
				}
			/>
		</Form>
	);
	const renderStepBody = () => {
		switch (step) {
			case 'phone':
				return renderPhoneStep();
			case 'inbound_waiting':
				return renderInboundWaitingStep();
			case 'code':
				return renderCodeStep();
		}
	};
	const renderStepFooter = () => {
		switch (step) {
			case 'phone':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.phone-add-modal.button.pop">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handlePhoneSubmit}
							submitting={phoneForm.formState.isSubmitting}
							data-flx="user.phone-add-modal.button.submit"
						>
							<Trans>Verify phone</Trans>
						</Button>
					</>
				);
			case 'inbound_waiting':
				return (
					<>
						<Button
							variant="secondary"
							onClick={() => setStep('phone')}
							data-flx="user.phone-add-modal.button.set-step"
						>
							<Trans>Back</Trans>
						</Button>
						<Button
							onClick={handleInboundResend}
							submitting={phoneForm.formState.isSubmitting}
							data-flx="user.phone-add-modal.button.inbound-resend"
						>
							<Trans>Get new code</Trans>
						</Button>
					</>
				);
			case 'code':
				return (
					<>
						<Button
							onClick={() => setStep('phone')}
							variant="secondary"
							data-flx="user.phone-add-modal.button.set-step--2"
						>
							<Trans>Back</Trans>
						</Button>
						<Button
							onClick={handleCodeSubmit}
							submitting={codeForm.formState.isSubmitting}
							data-flx="user.phone-add-modal.button.submit--2"
						>
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					</>
				);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="user.phone-add-modal.modal-root">
			<Modal.Header title={i18n._(VERIFY_PHONE_NUMBER_DESCRIPTOR)} data-flx="user.phone-add-modal.modal-header" />
			<Modal.Content data-flx="user.phone-add-modal.modal-content">
				<Modal.ContentLayout data-flx="user.phone-add-modal.modal-content-layout">
					<SteppedCarousel step={step} steps={PHONE_ADD_STEP_ORDER} data-flx="user.phone-add-modal.stepped-carousel">
						{renderStepBody()}
					</SteppedCarousel>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.phone-add-modal.modal-footer">{renderStepFooter()}</Modal.Footer>
		</Modal.Root>
	);
});

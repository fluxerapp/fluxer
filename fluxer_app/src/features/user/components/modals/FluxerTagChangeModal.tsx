// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_FLUXER_TAG, PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import {
	CANCEL_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	GET_PREMIUM_DESCRIPTOR,
	USERNAME_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {UsernameValidationRules} from '@app/features/ui/components/form/UsernameValidationRules';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {PlutoniumUpsell} from '@app/features/ui/plutonium_upsell/PlutoniumUpsell';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import styles from '@app/features/user/components/modals/FluxerTagChangeModal.module.css';
import type {User} from '@app/features/user/models/User';
import {getFormattedDateTime} from '@app/features/user/utils/DateFormatting';
import {isVisionaryDiscriminator0000Blocked} from '@app/features/user/utils/FluxerTagDiscriminatorUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef} from 'react';
import {Controller, useForm} from 'react-hook-form';

const USERNAME_ALREADY_TAKEN_DESCRIPTOR = msg({
	message: 'Username already taken',
	comment: 'Short label in the FluxerTag change modal. Keep it concise.',
});
const USERNAME_UPDATED_DESCRIPTOR = msg({
	message: 'Username updated',
	comment: 'Short label in the FluxerTag change modal. Keep it concise.',
});
const CHANGE_USERNAME_FORM_DESCRIPTOR = msg({
	message: 'Change username form',
	comment: 'Short label in the FluxerTag change modal. Keep it concise.',
});
const CHANGE_YOUR_USERNAME_DESCRIPTOR = msg({
	message: 'Change your username',
	comment: 'Short label in the FluxerTag change modal. Keep it concise.',
});
const GET_TO_CUSTOMIZE_YOUR_TAG_OR_KEEP_IT_DESCRIPTOR = msg({
	message: 'Get {premiumProductName} to customize your tag or keep it when changing your username',
	comment:
		'Description text in the FluxerTag change modal. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const TAG_DESCRIPTOR = msg({
	message: 'Tag',
	comment: 'Short label in the FluxerTag change modal. Keep it concise.',
});
const CUSTOM_DISCRIMINATORS_ARE_NOT_AVAILABLE_ON_THIS_INSTANCE_DESCRIPTOR = msg({
	message: 'Custom discriminators are not available on this instance',
	comment: 'Error message in the FluxerTag change modal.',
});
const THE_0000_TAG_IS_RESERVED_FOR_VISIONARY_SUBSCRIBERS_DESCRIPTOR = msg({
	message: 'The #0000 tag is reserved for Visionary subscribers. Visionary is no longer available for purchase.',
	comment: 'Description text in the FluxerTag change modal.',
});

interface FormInputs {
	username: string;
	discriminator: string;
}

interface FluxerTagChangeModalProps {
	user: User;
}

export const FluxerTagChangeModal = observer(({user}: FluxerTagChangeModalProps) => {
	const {i18n} = useLingui();
	const usernameRef = useRef<HTMLInputElement>(null);
	const hasCustomDiscriminator = isLimitToggleEnabled(
		{feature_custom_discriminator: LimitResolver.resolve({key: 'feature_custom_discriminator', fallback: 0})},
		'feature_custom_discriminator',
	);
	const isVisionary = user.premiumType === UserPremiumTypes.LIFETIME;
	const showPremium = shouldShowPremiumFeatures();
	const skipAvailabilityCheckRef = useRef(false);
	const resubmitHandlerRef = useRef<(() => Promise<void>) | null>(null);
	const confirmedRerollRef = useRef(false);
	const form = useForm<FormInputs>({
		defaultValues: {
			username: user.username,
			discriminator: user.discriminator,
		},
	});
	useEffect(() => {
		const subscription = form.watch((_, info) => {
			if (info?.name === 'username') {
				confirmedRerollRef.current = false;
			}
		});
		return () => {
			subscription.unsubscribe();
		};
	}, [form]);
	const onSubmit = useCallback(
		async (data: FormInputs) => {
			const usernameValue = data.username.trim();
			const normalizedDiscriminator = data.discriminator;
			const currentUsername = user.username.trim();
			const currentDiscriminator = user.discriminator;
			const isSameTag = usernameValue === currentUsername && normalizedDiscriminator === currentDiscriminator;
			if (!hasCustomDiscriminator && !skipAvailabilityCheckRef.current && !confirmedRerollRef.current) {
				const tagTaken = await UserCommands.checkFluxerTagAvailability({
					username: usernameValue,
					discriminator: normalizedDiscriminator,
				});
				if (tagTaken && !isSameTag) {
					const fluxerTag = `${usernameValue}#${normalizedDiscriminator}`;
					ModalCommands.push(
						modal(() => (
							<ConfirmModal
								title={i18n._(USERNAME_ALREADY_TAKEN_DESCRIPTOR)}
								description={
									<div
										className={styles.confirmDescription}
										data-flx="user.fluxer-tag-change-modal.on-submit.confirm-description"
									>
										<p data-flx="user.fluxer-tag-change-modal.on-submit.p">
											<Trans>
												The username{' '}
												<strong data-flx="user.fluxer-tag-change-modal.on-submit.strong">{fluxerTag}</strong> is already
												taken. Continuing will reroll your tag automatically.
											</Trans>
										</p>
										<p
											className={styles.confirmSecondary}
											data-flx="user.fluxer-tag-change-modal.on-submit.confirm-secondary"
										>
											<Trans>Cancel if you want to choose a different username instead.</Trans>
										</p>
									</div>
								}
								primaryText={i18n._(CONTINUE_DESCRIPTOR)}
								secondaryText={i18n._(CANCEL_DESCRIPTOR)}
								primaryVariant="primary"
								onPrimary={async () => {
									confirmedRerollRef.current = true;
									skipAvailabilityCheckRef.current = true;
									try {
										await resubmitHandlerRef.current?.();
									} finally {
										skipAvailabilityCheckRef.current = false;
									}
								}}
								data-flx="user.fluxer-tag-change-modal.on-submit.confirm-modal"
							/>
						)),
					);
					return;
				}
			}
			await UserCommands.update({
				username: usernameValue,
				discriminator: normalizedDiscriminator,
			});
			if (skipAvailabilityCheckRef.current) {
				skipAvailabilityCheckRef.current = false;
			}
			ModalCommands.pop();
			ToastCommands.createToast({type: 'success', children: i18n._(USERNAME_UPDATED_DESCRIPTOR)});
		},
		[hasCustomDiscriminator],
	);
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'username',
	});
	resubmitHandlerRef.current = handleSubmit;
	const selectedDiscriminator = form.watch('discriminator');
	const hasNonLifetimePremium = showPremium && user.isPremium() && !isVisionary;
	const hasPendingDiscriminatorChange = hasNonLifetimePremium && selectedDiscriminator !== user.discriminator;
	const shouldShowPremiumDiscriminatorWarning =
		hasNonLifetimePremium && (user.premiumDiscriminator || hasPendingDiscriminatorChange);
	const premiumExpiresAtLabel = hasNonLifetimePremium
		? user.premiumUntil
			? getFormattedDateTime(user.premiumUntil)
			: null
		: null;
	return (
		<Modal.Root size="small" centered initialFocusRef={usernameRef} data-flx="user.fluxer-tag-change-modal.modal-root">
			<Form
				form={form}
				onSubmit={handleSubmit}
				aria-label={i18n._(CHANGE_USERNAME_FORM_DESCRIPTOR)}
				data-flx="user.fluxer-tag-change-modal.form.submit"
			>
				<Modal.Header
					title={i18n._(CHANGE_YOUR_USERNAME_DESCRIPTOR)}
					data-flx="user.fluxer-tag-change-modal.modal-header"
				/>
				<Modal.Content data-flx="user.fluxer-tag-change-modal.modal-content">
					<Modal.ContentLayout data-flx="user.fluxer-tag-change-modal.modal-content-layout">
						<Modal.Description data-flx="user.fluxer-tag-change-modal.modal-description">
							<Trans>
								Usernames can only contain letters (a-z, A-Z), numbers (0-9), and underscores. Usernames are
								case-insensitive.
							</Trans>
						</Modal.Description>
						<div className={styles.fluxerTagContainer} data-flx="user.fluxer-tag-change-modal.fluxer-tag-container">
							<span className={styles.fluxerTagLabel} data-flx="user.fluxer-tag-change-modal.fluxer-tag-label">
								<Trans>Username</Trans>
							</span>
							{(form.formState.errors.username || form.formState.errors.discriminator) && (
								<div className={styles.errorBox} role="alert" data-flx="user.fluxer-tag-change-modal.error-box">
									{form.formState.errors.username?.message || form.formState.errors.discriminator?.message}
								</div>
							)}
							<div className={styles.fluxerTagInputRow} data-flx="user.fluxer-tag-change-modal.fluxer-tag-input-row">
								<div className={styles.usernameInput} data-flx="user.fluxer-tag-change-modal.username-input">
									<Controller
										name="username"
										control={form.control}
										render={({field}) => (
											<Input
												data-flx="user.fluxer-tag-change-modal.input.text"
												{...field}
												ref={usernameRef}
												autoComplete="username"
												aria-label={i18n._(USERNAME_DESCRIPTOR)}
												placeholder={EXAMPLE_FLUXER_TAG}
												required={true}
												type="text"
											/>
										)}
										data-flx="user.fluxer-tag-change-modal.controller"
									/>
								</div>
								<span className={styles.separator} data-flx="user.fluxer-tag-change-modal.separator">
									#
								</span>
								<div className={styles.discriminatorInput} data-flx="user.fluxer-tag-change-modal.discriminator-input">
									{!hasCustomDiscriminator ? (
										showPremium ? (
											<Tooltip
												text={i18n._(GET_TO_CUSTOMIZE_YOUR_TAG_OR_KEEP_IT_DESCRIPTOR, {
													premiumProductName: PREMIUM_PRODUCT_NAME,
												})}
												data-flx="user.fluxer-tag-change-modal.tooltip"
											>
												<div
													className={styles.discriminatorInputDisabled}
													data-flx="user.fluxer-tag-change-modal.discriminator-input-disabled"
												>
													<Input
														data-flx="user.fluxer-tag-change-modal.input.set-value.text"
														{...form.register('discriminator')}
														aria-label={i18n._(TAG_DESCRIPTOR)}
														maxLength={4}
														placeholder="0000"
														required={true}
														type="text"
														disabled={true}
														onChange={(e) => {
															const value = e.target.value.replace(/\D/g, '');
															form.setValue('discriminator', value);
														}}
													/>
													<FocusRing offset={-2} data-flx="user.fluxer-tag-change-modal.focus-ring">
														<button
															type="button"
															onClick={() => {
																PremiumModalCommands.open();
															}}
															className={styles.discriminatorOverlay}
															aria-label={i18n._(GET_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
															data-flx="user.fluxer-tag-change-modal.discriminator-overlay.open.button"
														/>
													</FocusRing>
												</div>
											</Tooltip>
										) : (
											<Tooltip
												text={i18n._(CUSTOM_DISCRIMINATORS_ARE_NOT_AVAILABLE_ON_THIS_INSTANCE_DESCRIPTOR)}
												data-flx="user.fluxer-tag-change-modal.tooltip--2"
											>
												<div
													className={styles.discriminatorInputDisabled}
													data-flx="user.fluxer-tag-change-modal.discriminator-input-disabled--2"
												>
													<Input
														data-flx="user.fluxer-tag-change-modal.input.set-value.text--2"
														{...form.register('discriminator')}
														aria-label={i18n._(TAG_DESCRIPTOR)}
														maxLength={4}
														placeholder="0000"
														required={true}
														type="text"
														disabled={true}
														onChange={(e) => {
															const value = e.target.value.replace(/\D/g, '');
															form.setValue('discriminator', value);
														}}
													/>
												</div>
											</Tooltip>
										)
									) : (
										<Input
											data-flx="user.fluxer-tag-change-modal.input.set-value.text--3"
											{...form.register('discriminator', {
												validate: (value) =>
													isVisionaryDiscriminator0000Blocked({
														showPremium,
														isVisionary,
														discriminator: value,
													})
														? i18n._(THE_0000_TAG_IS_RESERVED_FOR_VISIONARY_SUBSCRIBERS_DESCRIPTOR)
														: true,
											})}
											aria-label={i18n._(TAG_DESCRIPTOR)}
											maxLength={4}
											placeholder="0000"
											required={true}
											type="text"
											disabled={false}
											onChange={(e) => {
												const value = e.target.value.replace(/\D/g, '');
												form.setValue('discriminator', value, {shouldValidate: true});
											}}
										/>
									)}
								</div>
							</div>
							<div className={styles.validationBox} data-flx="user.fluxer-tag-change-modal.validation-box">
								<UsernameValidationRules
									username={form.watch('username')}
									data-flx="user.fluxer-tag-change-modal.username-validation-rules"
								/>
							</div>
							{shouldShowPremiumDiscriminatorWarning && (
								<WarningAlert
									className={styles.premiumDiscriminatorWarning}
									data-flx="user.fluxer-tag-change-modal.premium-discriminator-warning"
								>
									{hasPendingDiscriminatorChange ? (
										premiumExpiresAtLabel ? (
											<Trans>
												If you save this username, your tag will reroll randomly when your {PREMIUM_PRODUCT_NAME}{' '}
												expires on{' '}
												<strong data-flx="user.fluxer-tag-change-modal.strong">{premiumExpiresAtLabel}</strong>, unless
												it renews.
											</Trans>
										) : (
											<Trans>
												If you save this username, your tag will reroll randomly when your {PREMIUM_PRODUCT_NAME}{' '}
												expires, unless it renews.
											</Trans>
										)
									) : premiumExpiresAtLabel ? (
										<Trans>
											You changed your tag while on {PREMIUM_PRODUCT_NAME}. It will reroll randomly when your{' '}
											{PREMIUM_PRODUCT_NAME} expires on{' '}
											<strong data-flx="user.fluxer-tag-change-modal.strong--2">{premiumExpiresAtLabel}</strong>, unless
											it renews.
										</Trans>
									) : (
										<Trans>
											You changed your tag while on {PREMIUM_PRODUCT_NAME}. It will reroll randomly when your{' '}
											{PREMIUM_PRODUCT_NAME} expires, unless it renews.
										</Trans>
									)}
								</WarningAlert>
							)}
							{!hasCustomDiscriminator && (
								<PlutoniumUpsell
									className={styles.premiumUpsell}
									data-flx="user.fluxer-tag-change-modal.premium-upsell"
								>
									<Trans>Customize your tag or keep it when changing your username</Trans>
								</PlutoniumUpsell>
							)}
						</div>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="user.fluxer-tag-change-modal.modal-footer">
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.fluxer-tag-change-modal.button.pop">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting} data-flx="user.fluxer-tag-change-modal.button.submit">
						<Trans>Continue</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '@app/actions/PremiumModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import * as UserActionCreators from '@app/actions/UserActionCreators';
import {Form} from '@app/components/form/Form';
import {Input} from '@app/components/form/Input';
import {UsernameValidationRules} from '@app/components/form/UsernameValidationRules';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import styles from '@app/components/modals/FluxerTagChangeModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {PlutoniumUpsell} from '@app/components/uikit/plutonium_upsell/PlutoniumUpsell';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import type {UserRecord} from '@app/records/UserRecord';
import {LimitResolver} from '@app/utils/limits/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/utils/limits/LimitUtils';
import {shouldShowPremiumFeatures} from '@app/utils/PremiumUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef} from 'react';
import {Controller, useForm} from 'react-hook-form';

interface FormInputs {
	username: string;
	discriminator: string;
}

interface FluxerTagChangeModalProps {
	user: UserRecord;
}

export const FluxerTagChangeModal = observer(({user}: FluxerTagChangeModalProps) => {
	const {t} = useLingui();
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
				const tagTaken = await UserActionCreators.checkFluxerTagAvailability({
					username: usernameValue,
					discriminator: normalizedDiscriminator,
				});

				if (tagTaken && !isSameTag) {
					const fluxerTag = `${usernameValue}#${normalizedDiscriminator}`;

					ModalActionCreators.push(
						modal(() => (
							<ConfirmModal
								title={t`FluxerTag already taken`}
								description={
									<div className={styles.confirmDescription}>
										<p>
											<Trans>
												The FluxerTag <strong>{fluxerTag}</strong> is already taken. Continuing will reroll your
												discriminator automatically.
											</Trans>
										</p>
										<p className={styles.confirmSecondary}>
											<Trans>Cancel if you want to choose a different username instead.</Trans>
										</p>
									</div>
								}
								primaryText={t`Continue`}
								secondaryText={t`Cancel`}
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
							/>
						)),
					);

					return;
				}
			}

			await UserActionCreators.update({
				username: usernameValue,
				discriminator: normalizedDiscriminator,
			});
			if (skipAvailabilityCheckRef.current) {
				skipAvailabilityCheckRef.current = false;
			}
			ModalActionCreators.pop();
			ToastActionCreators.createToast({type: 'success', children: t`FluxerTag updated`});
		},
		[hasCustomDiscriminator],
	);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'username',
	});
	resubmitHandlerRef.current = handleSubmit;

	return (
		<Modal.Root size="small" centered initialFocusRef={usernameRef}>
			<Form form={form} onSubmit={handleSubmit} aria-label={t`Change FluxerTag form`}>
				<Modal.Header title={t`Change Your FluxerTag`} />
				<Modal.Content>
					<Modal.ContentLayout>
						<Modal.Description>
							{hasCustomDiscriminator ? (
								isVisionary ? (
									<Trans>
										Usernames can only contain letters (a-z, A-Z), numbers (0-9), and underscores.
										Usernames are case-insensitive. You can pick any available 4-digit tag from #0000
										to #9999.
									</Trans>
								) : (
									<Trans>
										Usernames can only contain letters (a-z, A-Z), numbers (0-9), and underscores.
										Usernames are case-insensitive. You can pick any available 4-digit tag from #0001
										to #9999.
									</Trans>
								)
							) : (
								<Trans>
									Usernames can only contain letters (a-z, A-Z), numbers (0-9), and underscores. Usernames are
									case-insensitive.
								</Trans>
							)}
						</Modal.Description>
						<div className={styles.fluxerTagContainer}>
							<span className={styles.fluxerTagLabel}>{t`FluxerTag`}</span>
							<div className={styles.fluxerTagInputRow}>
								<div className={styles.usernameInput}>
									<Controller
										name="username"
										control={form.control}
										render={({field}) => (
											<Input
												{...field}
												ref={usernameRef}
												autoComplete="username"
												aria-label={t`Username`}
												placeholder={t`Marty_McFly`}
												required={true}
												type="text"
											/>
										)}
									/>
								</div>
								<span className={styles.separator}>#</span>
								<div className={styles.discriminatorInput}>
									{!hasCustomDiscriminator ? (
										showPremium ? (
											<Tooltip text={t`Get Plutonium to customize your tag or keep it when changing your username`}>
												<div className={styles.discriminatorInputDisabled}>
													<Input
														{...form.register('discriminator')}
														aria-label={t`4-digit tag`}
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
													<FocusRing offset={-2}>
														<button
															type="button"
															onClick={() => {
																PremiumModalActionCreators.open();
															}}
															className={styles.discriminatorOverlay}
															aria-label={t`Get Plutonium`}
														/>
													</FocusRing>
												</div>
											</Tooltip>
										) : (
											<Tooltip text={t`Custom discriminators are not available on this instance`}>
												<div className={styles.discriminatorInputDisabled}>
													<Input
														{...form.register('discriminator')}
														aria-label={t`4-digit tag`}
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
											{...form.register('discriminator')}
											aria-label={t`4-digit tag`}
											maxLength={4}
											placeholder="0000"
											required={true}
											type="text"
											disabled={false}
											onChange={(e) => {
												const value = e.target.value.replace(/\D/g, '');
												form.setValue('discriminator', value);
											}}
										/>
									)}
								</div>
							</div>
							{(form.formState.errors.username || form.formState.errors.discriminator) && (
								<span className={styles.errorMessage}>
									{form.formState.errors.username?.message || form.formState.errors.discriminator?.message}
								</span>
							)}
							<div className={styles.validationBox}>
								<UsernameValidationRules username={form.watch('username')} />
							</div>
							{!hasCustomDiscriminator && (
								<PlutoniumUpsell className={styles.premiumUpsell}>
									<Trans>Customize your 4-digit tag or keep it when changing your username</Trans>
								</PlutoniumUpsell>
							)}
						</div>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={ModalActionCreators.pop} variant="secondary">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting}>
						<Trans>Continue</Trans>
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

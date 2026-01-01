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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Controller, useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import {UsernameValidationRules} from '~/components/form/UsernameValidationRules';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import styles from '~/components/modals/FluxerTagChangeModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {PlutoniumUpsell} from '~/components/uikit/PlutoniumUpsell/PlutoniumUpsell';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import UserStore from '~/stores/UserStore';

interface FormInputs {
	username: string;
	discriminator: string;
}

export const FluxerTagChangeModal = observer(() => {
	const {t} = useLingui();
	const user = UserStore.getCurrentUser()!;
	const usernameRef = React.useRef<HTMLInputElement>(null);
	const hasPremium = user.isPremium();
	const skipAvailabilityCheckRef = React.useRef(false);
	const resubmitHandlerRef = React.useRef<(() => Promise<void>) | null>(null);
	const confirmedRerollRef = React.useRef(false);

	const form = useForm<FormInputs>({
		defaultValues: {
			username: user.username,
			discriminator: user.discriminator,
		},
	});

	React.useEffect(() => {
		const subscription = form.watch((_, info) => {
			if (info?.name === 'username') {
				confirmedRerollRef.current = false;
			}
		});

		return () => {
			subscription.unsubscribe();
		};
	}, [form]);

	const onSubmit = React.useCallback(
		async (data: FormInputs) => {
			const usernameValue = data.username.trim();
			const normalizedDiscriminator = data.discriminator;
			const currentUsername = user.username.trim();
			const currentDiscriminator = user.discriminator;
			const isSameTag = usernameValue === currentUsername && normalizedDiscriminator === currentDiscriminator;

			if (!hasPremium && !skipAvailabilityCheckRef.current && !confirmedRerollRef.current) {
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
		[hasPremium, user.username, user.discriminator],
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
				<Modal.Header title={t`Change your FluxerTag`} />
				<Modal.Content className={confirmStyles.content}>
					<p className={clsx(styles.description, confirmStyles.descriptionText)}>
						{hasPremium ? (
							<Trans>
								Usernames can only contain letters (a-z, A-Z), numbers (0-9), and underscores. Usernames are
								case-insensitive. You can pick your own 4-digit tag if it's available.
							</Trans>
						) : (
							<Trans>
								Usernames can only contain letters (a-z, A-Z), numbers (0-9), and underscores. Usernames are
								case-insensitive.
							</Trans>
						)}
					</p>
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
								{!hasPremium ? (
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
						{!hasPremium && (
							<PlutoniumUpsell className={styles.premiumUpsell}>
								<Trans>Customize your 4-digit tag or keep it when changing your username</Trans>
							</PlutoniumUpsell>
						)}
					</div>
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

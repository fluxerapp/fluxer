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
import {CaretDownIcon, CheckIcon, CopyIcon, GiftIcon, NetworkSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {useForm} from 'react-hook-form';
import type {GiftMetadata} from '~/actions/GiftActionCreators';
import * as GiftActionCreators from '~/actions/GiftActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {UserPremiumTypes} from '~/Constants';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import {Logger} from '~/lib/Logger';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import UserStore from '~/stores/UserStore';
import {getFormattedShortDate} from '~/utils/DateUtils';
import {getGiftDurationText} from '~/utils/giftUtils';
import styles from './GiftInventoryTab.module.css';

const logger = new Logger('GiftInventoryTab');

interface GiftCodeFormInputs {
	code: string;
}

interface GiftCardProps {
	gift: GiftMetadata;
	isExpanded: boolean;
	onToggle: () => void;
	onRedeemSuccess: () => void;
}

const GiftCard: React.FC<GiftCardProps> = observer(({gift, isExpanded, onToggle, onRedeemSuccess}) => {
	const {t, i18n} = useLingui();
	const currentUser = UserStore.currentUser;
	const [copied, setCopied] = React.useState(false);
	const [redeeming, setRedeeming] = React.useState(false);

	const giftUrl = `${RuntimeConfigStore.giftEndpoint}/${gift.code}`;
	const isLifetime = currentUser?.isPremium() && currentUser.premiumType === UserPremiumTypes.LIFETIME;
	const isRedeemed = !!gift.redeemed_at;

	const durationText = getGiftDurationText(i18n, gift);

	const handleCopy = async () => {
		try {
			await TextCopyActionCreators.copy(i18n, giftUrl, true);
			setCopied(true);
			ToastActionCreators.createToast({type: 'success', children: <Trans>Gift URL copied to clipboard!</Trans>});
			setTimeout(() => setCopied(false), 2000);
		} catch (error) {
			logger.error('Failed to copy gift URL', error);
			ToastActionCreators.createToast({type: 'error', children: <Trans>Failed to copy URL</Trans>});
		}
	};

	const handleRedeem = async () => {
		setRedeeming(true);
		try {
			await GiftActionCreators.redeem(i18n, gift.code);
			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Gift redeemed successfully! Enjoy your Plutonium!</Trans>,
			});
			onRedeemSuccess();
		} catch (error) {
			logger.error('Failed to redeem gift', error);
			ToastActionCreators.createToast({type: 'error', children: <Trans>Failed to redeem gift</Trans>});
		} finally {
			setRedeeming(false);
		}
	};

	React.useEffect(() => {
		const currentUser = UserStore.getCurrentUser();
		if (currentUser?.hasUnreadGiftInventory) {
			UserActionCreators.update({has_unread_gift_inventory: false});
		}
	}, []);

	return (
		<div className={styles.giftCard}>
			<button type="button" onClick={onToggle} className={styles.giftCardHeader}>
				<div className={clsx(styles.giftIcon, isRedeemed ? styles.giftIconRedeemed : styles.giftIconActive)}>
					<GiftIcon className={styles.giftIconImage} />
				</div>
				<div className={styles.giftInfo}>
					<h3 className={styles.giftTitle}>{durationText}</h3>
					<p className={styles.giftDate}>
						{isRedeemed ? (
							<Trans>Redeemed {getFormattedShortDate(new Date(gift.redeemed_at!))}</Trans>
						) : (
							<Trans>Purchased {getFormattedShortDate(new Date(gift.created_at))}</Trans>
						)}
					</p>
				</div>
				<CaretDownIcon weight="bold" className={clsx(styles.expandIcon, isExpanded && styles.expandIconRotated)} />
			</button>

			{isExpanded && (
				<div className={styles.giftCardContent}>
					<div className={styles.giftCardActions}>
						<div className={styles.giftUrlSection}>
							<Input
								id={`gift-url-${gift.code}`}
								label={t`Gift URL`}
								value={giftUrl}
								readOnly
								onClick={(e) => e.currentTarget.select()}
								rightElement={
									<Button
										compact
										fitContent
										onClick={handleCopy}
										leftIcon={copied ? <CheckIcon size={16} weight="bold" /> : <CopyIcon size={16} />}
									>
										{copied ? t`Copied` : t`Copy`}
									</Button>
								}
							/>
						</div>

						{isRedeemed ? (
							<div className={styles.redeemedMessage}>
								<p className={styles.redeemedMessageText}>
									{gift.redeemed_by ? (
										<Trans>
											Redeemed by {gift.redeemed_by.username}#{gift.redeemed_by.discriminator}
										</Trans>
									) : (
										<Trans>This gift has been redeemed</Trans>
									)}
								</p>
							</div>
						) : (
							!isLifetime && (
								<div className={styles.redeemButtonContainer}>
									<Button variant="primary" onClick={handleRedeem} disabled={redeeming} submitting={redeeming}>
										<Trans>Redeem for Yourself</Trans>
									</Button>
								</div>
							)
						)}
					</div>
				</div>
			)}
		</div>
	);
});

const GiftInventoryTab: React.FC = observer(() => {
	const {t, i18n} = useLingui();
	const [gifts, setGifts] = React.useState<Array<GiftMetadata>>([]);
	const [loading, setLoading] = React.useState(true);
	const [error, setError] = React.useState(false);
	const [expandedGiftId, setExpandedGiftId] = React.useState<string | null>(null);

	const giftCodeForm = useForm<GiftCodeFormInputs>({defaultValues: {code: ''}});

	const handleGiftCodeSubmit = React.useCallback(
		async (data: GiftCodeFormInputs) => {
			const trimmedCode = data.code.trim();
			if (!trimmedCode) return;

			await GiftActionCreators.redeem(i18n, trimmedCode);
			giftCodeForm.reset();
		},
		[giftCodeForm, i18n],
	);

	const {handleSubmit: handleGiftCodeSubmitForm, isSubmitting: isGiftCodeSubmitting} = useFormSubmit({
		form: giftCodeForm,
		onSubmit: handleGiftCodeSubmit,
		defaultErrorField: 'code',
	});

	const fetchGifts = React.useCallback(async () => {
		try {
			setError(false);
			const userGifts = await GiftActionCreators.fetchUserGifts();
			setGifts(userGifts);
			setLoading(false);
		} catch (error) {
			logger.error('Failed to fetch user gifts', error);
			setError(true);
			setLoading(false);
		}
	}, []);

	React.useEffect(() => {
		fetchGifts();
	}, [fetchGifts]);

	const handleToggle = (code: string) => {
		setExpandedGiftId((prev) => (prev === code ? null : code));
	};

	const handleRedeemSuccess = () => {
		fetchGifts();
	};

	return (
		<div className={styles.container}>
			<div>
				<h2 className={styles.sectionHeader}>
					<Trans>Redeem a Gift</Trans>
				</h2>
				<p className={styles.sectionDescription}>
					<Trans>Enter a gift code to redeem Plutonium for your account.</Trans>
				</p>

				<Form form={giftCodeForm} onSubmit={handleGiftCodeSubmitForm} aria-label={t`Redeem gift code form`}>
					<div className={styles.redeemForm}>
						<div className={styles.redeemInput}>
							<Input
								{...giftCodeForm.register('code', {
									required: t`Gift code is required`,
									minLength: {
										value: 1,
										message: t`Gift code cannot be empty`,
									},
									maxLength: {
										value: 100,
										message: t`Gift code is too long`,
									},
								})}
								error={giftCodeForm.formState.errors.code?.message}
								label=""
								placeholder={t`Enter gift code...`}
								autoFocus={false}
								minLength={1}
								maxLength={100}
								required={true}
							/>
						</div>
						<Button
							type="submit"
							variant="primary"
							submitting={isGiftCodeSubmitting}
							disabled={!giftCodeForm.watch('code')?.trim()}
							className={styles.redeemButton}
						>
							<Trans>Redeem</Trans>
						</Button>
					</div>
				</Form>
			</div>

			<div>
				<h2 className={styles.sectionHeader}>
					<Trans>Your Purchased Gifts</Trans>
				</h2>
				<p className={styles.sectionDescriptionNoMargin}>
					<Trans>
						Manage your purchased Plutonium gift codes. Share the gift URL with someone special or redeem it for
						yourself!
					</Trans>
				</p>
			</div>

			{loading && (
				<div className={styles.loadingContainer}>
					<Spinner />
				</div>
			)}

			{error && (
				<StatusSlate
					Icon={NetworkSlashIcon}
					title={t`Failed to load gift inventory`}
					description={t`Please try again later.`}
					actions={[
						{
							text: t`Retry`,
							onClick: fetchGifts,
							variant: 'primary',
						},
					]}
				/>
			)}

			{!loading && !error && gifts.length === 0 && (
				<StatusSlate
					Icon={GiftIcon}
					title={<Trans>No gifts yet</Trans>}
					description={<Trans>Purchase a Plutonium gift from the Plutonium tab to share with friends!</Trans>}
					actions={[
						{
							text: <Trans>Go to Plutonium</Trans>,
							onClick: () => ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'plutonium'}),
							variant: 'primary',
							fitContent: true,
						},
					]}
				/>
			)}

			{!loading && !error && gifts.length > 0 && (
				<div className={styles.giftsList}>
					{gifts.map((gift) => (
						<GiftCard
							key={gift.code}
							gift={gift}
							isExpanded={expandedGiftId === gift.code}
							onToggle={() => handleToggle(gift.code)}
							onRedeemSuccess={handleRedeemSuccess}
						/>
					))}
				</div>
			)}
		</div>
	);
});

export default GiftInventoryTab;

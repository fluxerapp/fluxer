// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {GiftSendToFriendModal} from '@app/features/expressions/components/modals/GiftSendToFriendModal';
import type {GiftMetadata} from '@app/features/gift/commands/GiftCommands';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import {getGiftDurationText} from '@app/features/gift/utils/GiftUtils';
import {
	CLAIM_ACCOUNT_DESCRIPTOR,
	COPIED_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import styles from '@app/features/user/components/modals/tabs/GiftInventoryTab.module.css';
import Users from '@app/features/user/state/Users';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {showUserErrorModal} from '@app/features/user/utils/UserErrorModalUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CaretDownIcon,
	CheckIcon,
	CopyIcon,
	GiftIcon,
	NetworkSlashIcon,
	ShareNetworkIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';
import {useForm} from 'react-hook-form';

const GIFT_URL_DESCRIPTOR = msg({
	message: 'Gift URL',
	comment: 'Short label in the gift inventory tab. Keep it concise.',
});
const COPY_DESCRIPTOR = msg({
	message: 'Copy',
	comment: 'Button or menu action label in the gift inventory tab. Keep it concise.',
});
const REDEEM_GIFT_CODE_FORM_DESCRIPTOR = msg({
	message: 'Redeem gift code form',
	comment: 'Label in the gift inventory tab.',
});
const GIFT_CODE_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'Gift code is required',
	comment: 'Label in the gift inventory tab.',
});
const GIFT_CODE_CANNOT_BE_EMPTY_DESCRIPTOR = msg({
	message: 'Gift code cannot be empty',
	comment: 'Error message in the gift inventory tab.',
});
const GIFT_CODE_IS_TOO_LONG_DESCRIPTOR = msg({
	message: 'Gift code is too long',
	comment: 'Label in the gift inventory tab.',
});
const GIFT_REDEEMED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Gift redeemed successfully. Enjoy your {premiumProductName}.',
	comment: 'Toast shown after redeeming a premium gift code.',
});
const CLAIM_ACCOUNT_TO_MANAGE_GIFTS_DESCRIPTOR = msg({
	message: 'Claim your account to redeem or manage {premiumProductName} gift codes.',
	comment: 'Gift inventory empty-state text shown to unclaimed accounts.',
});
const ENTER_GIFT_CODE_TO_REDEEM_DESCRIPTOR = msg({
	message: 'Enter a gift code to redeem {premiumProductName} for your account.',
	comment: 'Gift redemption form description.',
});
const BUY_GIFT_FROM_PREMIUM_TAB_DESCRIPTOR = msg({
	message: 'Buy a {premiumProductName} gift from the {premiumProductName} tab to share with friends.',
	comment: 'Gift inventory empty-state description. premiumProductName is the premium tab name and product name.',
});
const GO_TO_PREMIUM_DESCRIPTOR = msg({
	message: 'Go to {premiumProductName}',
	comment: 'CTA label that opens the premium settings tab from gift inventory.',
});
const ENTER_GIFT_CODE_DESCRIPTOR = msg({
	message: 'Enter gift code…',
	comment: 'Short label in the gift inventory tab. Keep it concise.',
});
const FAILED_TO_LOAD_GIFT_INVENTORY_DESCRIPTOR = msg({
	message: 'Failed to load gift inventory',
	comment: 'Error message in the gift inventory tab.',
});
const COULDN_T_COPY_GIFT_URL_DESCRIPTOR = msg({
	message: "Couldn't copy gift URL",
	comment: 'Title of the error modal shown when copying a gift URL fails.',
});
const PLEASE_TRY_AGAIN_LATER_DESCRIPTOR = msg({
	message: 'Try again later.',
	comment: 'Label in the gift inventory tab.',
});
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
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const [copied, setCopied] = useState(false);
	const [redeeming, setRedeeming] = useState(false);
	const giftUrl = `${RuntimeConfig.giftEndpoint}/${gift.code}`;
	const isLifetime = currentUser?.isPremium() && currentUser.premiumType === UserPremiumTypes.LIFETIME;
	const isRedeemed = !!gift.redeemed_at;
	const durationText = getGiftDurationText(i18n, gift);
	const redeemerTag = gift.redeemed_by ? NicknameUtils.getDisplayName(gift.redeemed_by) : null;
	const handleCopy = async () => {
		try {
			await TextCopyCommands.copy(i18n, giftUrl, true);
			setCopied(true);
			ToastCommands.createToast({type: 'success', children: <Trans>Gift URL copied to clipboard!</Trans>});
			setTimeout(() => setCopied(false), 2000);
		} catch (error) {
			logger.error('Failed to copy gift URL', error);
			showUserErrorModal(i18n._(COULDN_T_COPY_GIFT_URL_DESCRIPTOR), i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR));
		}
	};
	const handleRedeem = async () => {
		setRedeeming(true);
		try {
			await GiftCommands.redeem(i18n, gift.code);
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(GIFT_REDEEMED_SUCCESSFULLY_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
			});
			onRedeemSuccess();
		} catch (error) {
			logger.error('Failed to redeem gift', error);
		} finally {
			setRedeeming(false);
		}
	};
	const handleShare = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<GiftSendToFriendModal
					code={gift.code}
					data-flx="user.gift-inventory-tab.handle-share.gift-send-to-friend-modal"
				/>
			)),
		);
	}, [gift.code]);
	useEffect(() => {
		const currentUser = Users.getCurrentUser();
		if (currentUser?.hasUnreadGiftInventory) {
			UserCommands.update({has_unread_gift_inventory: false});
		}
	}, []);
	return (
		<div className={styles.giftCard} data-flx="user.gift-inventory-tab.gift-card.gift-card">
			<button
				type="button"
				onClick={onToggle}
				aria-expanded={isExpanded}
				className={styles.giftCardHeader}
				data-flx="user.gift-inventory-tab.gift-card.gift-card-header.toggle.button"
			>
				<div
					className={clsx(styles.giftIcon, isRedeemed ? styles.giftIconRedeemed : styles.giftIconActive)}
					data-flx="user.gift-inventory-tab.gift-card.gift-icon"
				>
					<GiftIcon className={styles.giftIconImage} data-flx="user.gift-inventory-tab.gift-card.gift-icon-image" />
				</div>
				<div className={styles.giftInfo} data-flx="user.gift-inventory-tab.gift-card.gift-info">
					<h3 className={styles.giftTitle} data-flx="user.gift-inventory-tab.gift-card.gift-title">
						{durationText}
					</h3>
					<p className={styles.giftDate} data-flx="user.gift-inventory-tab.gift-card.gift-date">
						{isRedeemed ? (
							<Trans>Redeemed {getFormattedShortDate(new Date(gift.redeemed_at!))}</Trans>
						) : (
							<Trans>Purchased {getFormattedShortDate(new Date(gift.created_at))}</Trans>
						)}
					</p>
				</div>
				<CaretDownIcon
					weight="bold"
					className={clsx(styles.expandIcon, isExpanded && styles.expandIconRotated)}
					data-flx="user.gift-inventory-tab.gift-card.expand-icon"
				/>
			</button>
			{isExpanded && (
				<div className={styles.giftCardContent} data-flx="user.gift-inventory-tab.gift-card.gift-card-content">
					<div className={styles.giftCardActions} data-flx="user.gift-inventory-tab.gift-card.gift-card-actions">
						<div className={styles.giftUrlSection} data-flx="user.gift-inventory-tab.gift-card.gift-url-section">
							<Input
								id={`gift-url-${gift.code}`}
								label={i18n._(GIFT_URL_DESCRIPTOR)}
								value={giftUrl}
								readOnly
								onClick={(e) => e.currentTarget.select()}
								rightElement={
									<Button
										compact
										fitContent
										onClick={handleCopy}
										leftIcon={
											copied ? (
												<CheckIcon
													size={remFromPx(16)}
													weight="bold"
													data-flx="user.gift-inventory-tab.gift-card.check-icon"
												/>
											) : (
												<CopyIcon size={remFromPx(16)} data-flx="user.gift-inventory-tab.gift-card.copy-icon" />
											)
										}
										data-flx="user.gift-inventory-tab.gift-card.button.copy"
									>
										{copied ? i18n._(COPIED_DESCRIPTOR) : i18n._(COPY_DESCRIPTOR)}
									</Button>
								}
								data-flx="user.gift-inventory-tab.gift-card.input.select"
							/>
						</div>
						{isRedeemed && (
							<div className={styles.redeemedMessage} data-flx="user.gift-inventory-tab.gift-card.redeemed-message">
								<p
									className={styles.redeemedMessageText}
									data-flx="user.gift-inventory-tab.gift-card.redeemed-message-text"
								>
									{redeemerTag ? <Trans>Redeemed by {redeemerTag}</Trans> : <Trans>This gift has been redeemed</Trans>}
								</p>
							</div>
						)}
						{!isRedeemed && (
							<div className={styles.giftCardFooter} data-flx="user.gift-inventory-tab.gift-card.gift-card-footer">
								{!isLifetime && (
									<Button
										variant="primary"
										onClick={handleRedeem}
										disabled={redeeming}
										submitting={redeeming}
										data-flx="user.gift-inventory-tab.gift-card.button.redeem"
									>
										<Trans>Redeem for yourself</Trans>
									</Button>
								)}
								<Button
									variant="secondary"
									onClick={handleShare}
									leftIcon={
										<ShareNetworkIcon
											size={remFromPx(16)}
											weight="bold"
											data-flx="user.gift-inventory-tab.gift-card.share-network-icon"
										/>
									}
									data-flx="user.gift-inventory-tab.gift-card.button.share"
								>
									<Trans>Share with a friend</Trans>
								</Button>
							</div>
						)}
					</div>
				</div>
			)}
		</div>
	);
});
const GiftInventoryTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const [gifts, setGifts] = useState<Array<GiftMetadata>>([]);
	const [loading, setLoading] = useState(true);
	const [error, setError] = useState(false);
	const [expandedGiftId, setExpandedGiftId] = useState<string | null>(null);
	const isUnclaimed = !(Users.currentUser?.isClaimed() ?? false);
	const giftCodeForm = useForm<GiftCodeFormInputs>({defaultValues: {code: ''}});
	const handleGiftCodeSubmit = useCallback(
		async (data: GiftCodeFormInputs) => {
			const trimmedCode = data.code.trim();
			if (!trimmedCode) return;
			await GiftCommands.redeem(i18n, trimmedCode);
			giftCodeForm.reset();
		},
		[giftCodeForm, i18n],
	);
	const {handleSubmit: handleGiftCodeSubmitForm, isSubmitting: isGiftCodeSubmitting} = useFormSubmit({
		form: giftCodeForm,
		onSubmit: handleGiftCodeSubmit,
		defaultErrorField: 'code',
	});
	const fetchGifts = useCallback(async () => {
		if (isUnclaimed) {
			setLoading(false);
			return;
		}
		try {
			setError(false);
			const userGifts = await GiftCommands.fetchUserGifts();
			setGifts(userGifts);
			setLoading(false);
		} catch (error) {
			logger.error('Failed to fetch user gifts', error);
			setError(true);
			setLoading(false);
		}
	}, [isUnclaimed]);
	useEffect(() => {
		fetchGifts();
	}, [fetchGifts]);
	const handleToggle = (code: string) => {
		setExpandedGiftId((prev) => (prev === code ? null : code));
	};
	const handleRedeemSuccess = () => {
		fetchGifts();
	};
	if (isUnclaimed) {
		return (
			<StatusSlate
				Icon={WarningCircleIcon}
				title={<Trans>Claim your account</Trans>}
				description={i18n._(CLAIM_ACCOUNT_TO_MANAGE_GIFTS_DESCRIPTOR, {
					premiumProductName: PREMIUM_PRODUCT_NAME,
				})}
				actions={[
					{
						text: i18n._(CLAIM_ACCOUNT_DESCRIPTOR),
						onClick: () => openClaimAccountModal({force: true}),
						variant: 'primary',
					},
				]}
				data-flx="user.gift-inventory-tab.status-slate"
			/>
		);
	}
	return (
		<SettingsTabContainer data-flx="user.gift-inventory-tab.settings-tab-container">
			<SettingsSection
				id="redeem-gift"
				title={<Trans>Redeem a gift</Trans>}
				description={i18n._(ENTER_GIFT_CODE_TO_REDEEM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
				data-flx="user.gift-inventory-tab.redeem-gift"
			>
				<Form
					form={giftCodeForm}
					onSubmit={handleGiftCodeSubmitForm}
					aria-label={i18n._(REDEEM_GIFT_CODE_FORM_DESCRIPTOR)}
					data-flx="user.gift-inventory-tab.form"
				>
					<div className={styles.redeemForm} data-flx="user.gift-inventory-tab.redeem-form">
						<div className={styles.redeemInput} data-flx="user.gift-inventory-tab.redeem-input">
							<Input
								data-flx="user.gift-inventory-tab.input"
								{...giftCodeForm.register('code', {
									required: i18n._(GIFT_CODE_IS_REQUIRED_DESCRIPTOR),
									minLength: {
										value: 1,
										message: i18n._(GIFT_CODE_CANNOT_BE_EMPTY_DESCRIPTOR),
									},
									maxLength: {
										value: 100,
										message: i18n._(GIFT_CODE_IS_TOO_LONG_DESCRIPTOR),
									},
								})}
								error={giftCodeForm.formState.errors.code?.message}
								label=""
								placeholder={i18n._(ENTER_GIFT_CODE_DESCRIPTOR)}
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
							data-flx="user.gift-inventory-tab.redeem-button.submit"
						>
							<Trans>Redeem</Trans>
						</Button>
					</div>
				</Form>
			</SettingsSection>
			<SettingsSection
				id="purchased-gifts"
				title={<Trans>Purchased gifts</Trans>}
				description={
					<Trans>
						Manage your purchased {PREMIUM_PRODUCT_NAME} gift codes. Share the gift URL with someone special or redeem
						it for yourself!
					</Trans>
				}
				data-flx="user.gift-inventory-tab.purchased-gifts"
			>
				{loading && (
					<div className={styles.loadingContainer} data-flx="user.gift-inventory-tab.loading-container">
						<Spinner data-flx="user.gift-inventory-tab.spinner" />
					</div>
				)}
				{error && (
					<StatusSlate
						Icon={NetworkSlashIcon}
						title={i18n._(FAILED_TO_LOAD_GIFT_INVENTORY_DESCRIPTOR)}
						description={i18n._(PLEASE_TRY_AGAIN_LATER_DESCRIPTOR)}
						actions={[
							{
								text: i18n._(TRY_AGAIN_DESCRIPTOR),
								onClick: fetchGifts,
								variant: 'primary',
							},
						]}
						data-flx="user.gift-inventory-tab.status-slate--2"
					/>
				)}
				{!loading && !error && gifts.length === 0 && (
					<StatusSlate
						Icon={GiftIcon}
						title={<Trans>No gifts yet</Trans>}
						description={i18n._(BUY_GIFT_FROM_PREMIUM_TAB_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
						actions={[
							{
								text: i18n._(GO_TO_PREMIUM_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME}),
								onClick: () => ComponentDispatch.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'plutonium'}),
								variant: 'primary',
								fitContent: true,
							},
						]}
						data-flx="user.gift-inventory-tab.status-slate--3"
					/>
				)}
				{!loading && !error && gifts.length > 0 && (
					<div className={styles.giftsList} data-flx="user.gift-inventory-tab.gifts-list">
						{gifts.map((gift) => (
							<GiftCard
								key={gift.code}
								gift={gift}
								isExpanded={expandedGiftId === gift.code}
								onToggle={() => handleToggle(gift.code)}
								onRedeemSuccess={handleRedeemSuccess}
								data-flx="user.gift-inventory-tab.gift-card"
							/>
						))}
					</div>
				)}
			</SettingsSection>
		</SettingsTabContainer>
	);
});

export default GiftInventoryTab;

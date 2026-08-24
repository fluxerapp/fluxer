// SPDX-License-Identifier: AGPL-3.0-or-later

import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import styles from '@app/features/channel/components/GiftEmbed.module.css';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {getGiftDurationText} from '@app/features/gift/utils/GiftUtils';
import {
	EmbedCard,
	EmbedSkeletonButton,
	EmbedSkeletonCircle,
	EmbedSkeletonSubtitle,
	EmbedSkeletonTitle,
} from '@app/features/messaging/components/embeds/embed_card/EmbedCard';
import cardStyles from '@app/features/messaging/components/embeds/embed_card/EmbedCard.module.css';
import {useEmbedSkeletonOverride} from '@app/features/messaging/components/embeds/embed_card/useEmbedSkeletonOverride';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {Button} from '@app/features/ui/button/Button';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GiftIcon, QuestionIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef} from 'react';

const FROM_DESCRIPTOR = msg({
	message: 'From {creatorTag}',
	comment: 'Sender label on a gift code embed. creatorTag is the FluxerTag of the gift creator.',
});
const ALREADY_REDEEMED_DESCRIPTOR = msg({
	message: 'Already redeemed',
	comment: 'Status pill on a gift code embed when the gift has already been claimed.',
});
const CLAIM_YOUR_ACCOUNT_TO_REDEEM_THIS_GIFT_DESCRIPTOR = msg({
	message: 'Claim your account to redeem this gift.',
	comment: 'Helper text on a gift code embed for guest accounts. Prompts account claim before redeeming.',
});
const CLICK_TO_CLAIM_YOUR_GIFT_DESCRIPTOR = msg({
	message: 'Click to claim your gift!',
	comment: 'Call-to-action text on a redeemable gift code embed. Tone can be friendly.',
});
const GIFT_CLAIMED_DESCRIPTOR = msg({
	message: 'Gift claimed',
	comment: 'Toast confirmation shown after successfully claiming a gift from the gift code embed.',
});
const CLAIM_ACCOUNT_TO_REDEEM_DESCRIPTOR = msg({
	message: 'Claim account to redeem',
	comment: 'Button label on a gift code embed for guest accounts. Opens the account claim flow.',
});
const CLAIM_GIFT_DESCRIPTOR = msg({
	message: 'Claim gift',
	comment: 'Primary button label on a redeemable gift code embed.',
});
const UNKNOWN_GIFT_DESCRIPTOR = msg({
	message: 'Unknown gift',
	comment: 'Title shown on a gift code embed when the gift code resolves to nothing.',
});
const THIS_GIFT_CODE_IS_INVALID_OR_ALREADY_CLAIMED_DESCRIPTOR = msg({
	message: 'This gift code is invalid or already claimed.',
	comment: 'Body shown on an unknown gift embed explaining why the code is invalid.',
});
const GIFT_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Gift unavailable',
	comment: 'Title shown on a gift code embed when the gift cannot be resolved due to a non-permanent failure.',
});
const logger = new Logger('GiftEmbed');

interface GiftEmbedProps {
	code: string;
}

export const GiftEmbed = observer(function GiftEmbed({code}: GiftEmbedProps) {
	const {i18n} = useLingui();
	const giftState = Gifts.gifts.get(code) ?? null;
	const gift = giftState?.data;
	const creator = Users.getUser(gift?.created_by?.id ?? '');
	const isUnclaimed = !(Users.currentUser?.isClaimed() ?? false);
	const shouldForceSkeleton = useEmbedSkeletonOverride();
	useEffect(() => {
		if (!giftState) {
			void GiftCommands.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, giftState]);
	const prevLoadingRef = useRef<boolean>(true);
	useEffect(() => {
		const isLoading = !!giftState?.loading;
		if (prevLoadingRef.current && !isLoading && giftState) {
			ComponentBus.dispatch('LAYOUT_RESIZED');
		}
		prevLoadingRef.current = isLoading;
	}, [giftState?.loading]);
	if (shouldForceSkeleton || !giftState || giftState.loading) {
		return <GiftLoadingState data-flx="channel.gift-embed.gift-loading-state" />;
	}
	if (giftState.invalid || giftState.error || !gift) {
		return <GiftNotFoundError data-flx="channel.gift-embed.gift-not-found-error" />;
	}
	const durationText = getGiftDurationText(i18n, gift);
	const creatorTag = creator ? NicknameUtils.getDisplayName(creator) : '';
	const handleRedeem = async () => {
		if (isUnclaimed) {
			openClaimAccountModal({force: true});
			return;
		}
		try {
			await GiftCommands.redeem(i18n, code);
		} catch (error) {
			logger.error('Failed to redeem gift', error);
		}
	};
	const subtitleNode = creator ? (
		<span className={styles.subRow} data-flx="channel.gift-embed.sub-row">
			{i18n._(FROM_DESCRIPTOR, {creatorTag})}
		</span>
	) : undefined;
	const helpText = gift.redeemed
		? i18n._(ALREADY_REDEEMED_DESCRIPTOR)
		: isUnclaimed
			? i18n._(CLAIM_YOUR_ACCOUNT_TO_REDEEM_THIS_GIFT_DESCRIPTOR)
			: i18n._(CLICK_TO_CLAIM_YOUR_GIFT_DESCRIPTOR);
	const footer =
		gift.redeemed && !isUnclaimed ? (
			<Button variant="primary" fitContainer matchSkeletonHeight disabled data-flx="channel.gift-embed.button">
				{i18n._(GIFT_CLAIMED_DESCRIPTOR)}
			</Button>
		) : (
			<Button
				variant="primary"
				fitContainer
				matchSkeletonHeight
				onClick={handleRedeem}
				disabled={gift.redeemed || isUnclaimed}
				data-flx="channel.gift-embed.button.redeem"
			>
				{(() => {
					if (gift.redeemed) return i18n._(GIFT_CLAIMED_DESCRIPTOR);
					if (isUnclaimed) return i18n._(CLAIM_ACCOUNT_TO_REDEEM_DESCRIPTOR);
					return i18n._(CLAIM_GIFT_DESCRIPTOR);
				})()}
			</Button>
		);
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div
					className={`${styles.iconCircle} ${gift.redeemed ? styles.iconCircleInactive : styles.iconCircleActive}`}
					data-flx="channel.gift-embed.icon-circle"
				>
					<GiftIcon className={styles.icon} data-flx="channel.gift-embed.icon" />
				</div>
			}
			title={
				<h3
					className={`${styles.title} ${cardStyles.title} ${gift.redeemed ? styles.titleTertiary : styles.titlePrimary}`}
					data-flx="channel.gift-embed.title"
				>
					{durationText}
				</h3>
			}
			subtitle={subtitleNode}
			body={
				<div className={styles.helpRow} data-flx="channel.gift-embed.help-row">
					{helpText}
				</div>
			}
			footer={footer}
			data-flx="channel.gift-embed.embed-card"
		/>
	);
});
const GiftLoadingState = observer(function GiftLoadingState() {
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<EmbedSkeletonCircle
					className={styles.skeletonCircle}
					data-flx="channel.gift-embed.gift-loading-state.skeleton-circle"
				/>
			}
			title={
				<EmbedSkeletonTitle
					className={styles.skeletonTitle}
					data-flx="channel.gift-embed.gift-loading-state.skeleton-title"
				/>
			}
			body={
				<EmbedSkeletonSubtitle
					className={styles.skeletonHelp}
					data-flx="channel.gift-embed.gift-loading-state.skeleton-help"
				/>
			}
			footer={
				<EmbedSkeletonButton
					className={styles.skeletonButton}
					data-flx="channel.gift-embed.gift-loading-state.skeleton-button"
				/>
			}
			data-flx="channel.gift-embed.gift-loading-state.embed-card"
		/>
	);
});
const GiftNotFoundError = observer(function GiftNotFoundError() {
	const {i18n} = useLingui();
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div
					className={`${styles.iconCircle} ${styles.iconCircleDisabled}`}
					data-flx="channel.gift-embed.gift-not-found-error.icon-circle"
				>
					<QuestionIcon
						className={`${styles.icon} ${styles.iconError}`}
						data-flx="channel.gift-embed.gift-not-found-error.icon"
					/>
				</div>
			}
			title={
				<h3
					className={`${styles.title} ${styles.titleDanger}`}
					data-flx="channel.gift-embed.gift-not-found-error.title"
				>
					{i18n._(UNKNOWN_GIFT_DESCRIPTOR)}
				</h3>
			}
			body={
				<span className={styles.helpRow} data-flx="channel.gift-embed.gift-not-found-error.help-row">
					{i18n._(THIS_GIFT_CODE_IS_INVALID_OR_ALREADY_CLAIMED_DESCRIPTOR)}
				</span>
			}
			footer={
				<Button
					variant="primary"
					fitContainer
					matchSkeletonHeight
					disabled
					data-flx="channel.gift-embed.gift-not-found-error.button"
				>
					{i18n._(GIFT_UNAVAILABLE_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.gift-embed.gift-not-found-error.embed-card"
		/>
	);
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GiftEmbed.module.css';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import Gifts from '@app/features/gift/state/Gifts';
import {getGiftDurationText} from '@app/features/gift/utils/GiftUtils';
import cardStyles from '@app/features/messaging/components/embeds/embed_card/EmbedCard.module.css';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import Users from '@app/features/user/state/Users';
import {getAccountGatePresentation} from '@app/features/user/utils/AccountGatePresentation';
import {canPerform} from '@app/features/user/utils/AccountGates';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AtomIcon, QuestionIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

const FROM_DESCRIPTOR = msg({
	message: 'From {creatorTag}',
	comment: 'Sender label on a gift code embed. creatorTag is the FluxerTag of the gift creator.',
});
const ALREADY_REDEEMED_DESCRIPTOR = msg({
	message: 'Already redeemed',
	comment: 'Status line on a gift code embed when the gift has already been claimed.',
});
const CLICK_TO_CLAIM_YOUR_GIFT_DESCRIPTOR = msg({
	message: 'Click to claim your gift!',
	comment: 'Call-to-action text on a redeemable gift code embed. Tone can be friendly.',
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
const logger = new Logger('GiftEmbed');

interface GiftEmbedProps {
	code: string;
}

export const GiftEmbed = observer(function GiftEmbed({code}: GiftEmbedProps) {
	const {i18n} = useLingui();
	const target = useMemo(currentInstanceTarget, []);
	const giftState = Gifts.getGift(code, target);
	const gift = giftState?.data;
	const creator = Users.getUser(gift?.created_by?.id ?? '') ?? gift?.created_by ?? null;
	const giftRedeemGate = canPerform(Users.currentUser, TierGatedActions.GIFT_REDEEM);
	const [redeeming, setRedeeming] = useState(false);
	useEffect(() => {
		if (!giftState) {
			void GiftCommands.fetchWithCoalescing(code, target).catch(() => {});
		}
	}, [code, giftState, target]);
	if (!giftState || giftState.loading) {
		return <GiftLoadingState data-flx="channel.gift-embed.gift-loading-state" />;
	}
	if (giftState.invalid || giftState.error || !gift) {
		return (
			<GiftCard
				muted
				icon={<QuestionIcon weight="bold" className={styles.icon} data-flx="channel.gift-embed.unknown-icon" />}
				title={i18n._(UNKNOWN_GIFT_DESCRIPTOR)}
				meta={i18n._(THIS_GIFT_CODE_IS_INVALID_OR_ALREADY_CLAIMED_DESCRIPTOR)}
				action={null}
				data-flx="channel.gift-embed.gift-card"
			/>
		);
	}
	const gatePresentation = giftRedeemGate.allowed ? null : getAccountGatePresentation(i18n, giftRedeemGate);
	const handleRedeem = async () => {
		if (gatePresentation) {
			gatePresentation.onAction();
			return;
		}
		if (redeeming) return;
		setRedeeming(true);
		try {
			await GiftCommands.redeem(i18n, code, target);
		} catch (error) {
			logger.error('Failed to redeem gift', error);
		} finally {
			setRedeeming(false);
		}
	};
	let meta: string;
	if (gift.redeemed) meta = i18n._(ALREADY_REDEEMED_DESCRIPTOR);
	else if (gatePresentation) meta = gatePresentation.description;
	else if (creator) meta = i18n._(FROM_DESCRIPTOR, {creatorTag: NicknameUtils.getDisplayName(creator)});
	else meta = i18n._(CLICK_TO_CLAIM_YOUR_GIFT_DESCRIPTOR);
	return (
		<GiftCard
			muted={gift.redeemed}
			icon={<AtomIcon weight="fill" className={styles.icon} data-flx="channel.gift-embed.plutonium-icon" />}
			title={getGiftDurationText(i18n, gift)}
			meta={meta}
			action={
				gift.redeemed ? null : (
					<Button
						variant="primary"
						small
						fitContent
						submitting={redeeming}
						onClick={handleRedeem}
						data-flx="channel.gift-embed.redeem"
					>
						{gatePresentation ? gatePresentation.actionLabel : i18n._(CLAIM_GIFT_DESCRIPTOR)}
					</Button>
				)
			}
			data-flx="channel.gift-embed.gift-card--2"
		/>
	);
});

function GiftCard({
	muted,
	icon,
	title,
	meta,
	action,
}: {
	muted: boolean;
	icon: React.ReactNode;
	title: string;
	meta: string;
	action: React.ReactNode;
}) {
	return (
		<div className={styles.card} data-flx="channel.gift-embed.card">
			<div className={clsx(styles.tile, muted && styles.tileMuted)} data-flx="channel.gift-embed.tile">
				{icon}
			</div>
			<div className={styles.text} data-flx="channel.gift-embed.text">
				<p className={clsx(styles.title, muted && styles.titleMuted)} title={title} data-flx="channel.gift-embed.title">
					{title}
				</p>
				<p className={styles.meta} title={meta} data-flx="channel.gift-embed.meta">
					{meta}
				</p>
			</div>
			{action !== null && (
				<div className={styles.action} data-flx="channel.gift-embed.action">
					{action}
				</div>
			)}
		</div>
	);
}

function GiftLoadingState() {
	return (
		<div className={styles.card} aria-busy data-flx="channel.gift-embed.loading">
			<div className={clsx(cardStyles.skeleton, styles.skeletonTile)} data-flx="channel.gift-embed.loading-tile" />
			<div className={styles.text} data-flx="channel.gift-embed.loading-text">
				<div className={styles.title} data-flx="channel.gift-embed.loading-title">
					<div
						className={clsx(cardStyles.skeleton, styles.skeletonTitle)}
						data-flx="channel.gift-embed.loading-title-bar"
					/>
				</div>
				<div className={styles.meta} data-flx="channel.gift-embed.loading-meta">
					<div
						className={clsx(cardStyles.skeleton, styles.skeletonMeta)}
						data-flx="channel.gift-embed.loading-meta-bar"
					/>
				</div>
			</div>
			<div className={styles.action} data-flx="channel.gift-embed.loading-action">
				<div
					className={clsx(cardStyles.skeleton, styles.skeletonButton)}
					data-flx="channel.gift-embed.loading-button"
				/>
			</div>
		</div>
	);
}

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
import {GiftIcon, QuestionIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GiftActionCreators from '~/actions/GiftActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import i18n from '~/i18n';
import {UserRecord} from '~/records/UserRecord';
import GiftStore from '~/stores/GiftStore';
import {getGiftDurationText} from '~/utils/giftUtils';
import styles from './GiftAcceptModal.module.css';

interface GiftAcceptModalProps {
	code: string;
}

export const GiftAcceptModal = observer(function GiftAcceptModal({code}: GiftAcceptModalProps) {
	const {t} = useLingui();
	const giftState = GiftStore.gifts.get(code) ?? null;
	const gift = giftState?.data ?? null;
	const [isRedeeming, setIsRedeeming] = React.useState(false);

	React.useEffect(() => {
		if (!giftState) {
			void GiftActionCreators.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, giftState]);

	const creator = React.useMemo(() => {
		if (!gift?.created_by) return null;
		return new UserRecord({
			id: gift.created_by.id,
			username: gift.created_by.username,
			discriminator: gift.created_by.discriminator,
			avatar: gift.created_by.avatar,
			flags: gift.created_by.flags,
		});
	}, [gift?.created_by]);

	const handleDismiss = () => {
		ModalActionCreators.pop();
	};

	const handleRedeem = async () => {
		setIsRedeeming(true);
		try {
			await GiftActionCreators.redeem(i18n, code);
			ModalActionCreators.pop();
		} catch (error) {
			console.error('[GiftAcceptModal] Failed to redeem gift:', error);
			setIsRedeeming(false);
		}
	};

	const renderLoading = () => (
		<div className={styles.loadingContent}>
			<Spinner />
		</div>
	);

	const renderError = () => (
		<>
			<div className={styles.card}>
				<div className={styles.cardGrid}>
					<div className={`${styles.iconCircle} ${styles.iconCircleDisabled}`}>
						<QuestionIcon className={`${styles.icon} ${styles.iconError}`} />
					</div>
					<div className={styles.cardContent}>
						<h3 className={`${styles.title} ${styles.titleDanger}`}>{t`Unknown Gift`}</h3>
						<span className={styles.helpText}>{t`This gift code is invalid or already claimed.`}</span>
					</div>
				</div>
			</div>
			<div className={styles.footer}>
				<Button variant="secondary" onClick={handleDismiss}>
					<Trans>Dismiss</Trans>
				</Button>
			</div>
		</>
	);

	const renderRedeemed = () => {
		const durationText = getGiftDurationText(i18n, gift!);
		return (
			<>
				<div className={styles.card}>
					<div className={styles.cardGrid}>
						<div className={`${styles.iconCircle} ${styles.iconCircleInactive}`}>
							<GiftIcon className={styles.icon} weight="fill" />
						</div>
						<div className={styles.cardContent}>
							<h3 className={`${styles.title} ${styles.titleTertiary}`}>{durationText}</h3>
							{creator && (
								<span className={styles.subtitle}>{t`From ${creator.username}#${creator.discriminator}`}</span>
							)}
							<span className={styles.helpText}>{t`This gift has already been claimed.`}</span>
						</div>
					</div>
				</div>
				<div className={styles.footer}>
					<Button variant="secondary" onClick={handleDismiss}>
						<Trans>Dismiss</Trans>
					</Button>
				</div>
			</>
		);
	};

	const renderGift = () => {
		const durationText = getGiftDurationText(i18n, gift!);
		return (
			<>
				<div className={styles.card}>
					<div className={styles.cardGrid}>
						<div className={`${styles.iconCircle} ${styles.iconCircleActive}`}>
							<GiftIcon className={styles.icon} weight="fill" />
						</div>
						<div className={styles.cardContent}>
							<h3 className={`${styles.title} ${styles.titlePrimary}`}>{durationText}</h3>
							{creator && (
								<span className={styles.subtitle}>{t`From ${creator.username}#${creator.discriminator}`}</span>
							)}
							<span className={styles.helpText}>{t`Claim your gift to activate your premium subscription!`}</span>
						</div>
					</div>
				</div>
				<div className={styles.footer}>
					<Button variant="secondary" onClick={handleDismiss} disabled={isRedeeming}>
						<Trans>Maybe later</Trans>
					</Button>
					<Button variant="primary" onClick={handleRedeem} disabled={isRedeeming} submitting={isRedeeming}>
						<Trans>Claim Gift</Trans>
					</Button>
				</div>
			</>
		);
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={<Trans>Gift</Trans>} />
			<Modal.Content padding="none" className={styles.content}>
				{!giftState || giftState.loading
					? renderLoading()
					: giftState.error || !gift
						? renderError()
						: gift.redeemed
							? renderRedeemed()
							: renderGift()}
			</Modal.Content>
		</Modal.Root>
	);
});

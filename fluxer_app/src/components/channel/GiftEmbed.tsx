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

import {useLingui} from '@lingui/react/macro';
import {GiftIcon, QuestionIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GiftActionCreators from '~/actions/GiftActionCreators';
import {
	EmbedCard,
	EmbedSkeletonButton,
	EmbedSkeletonCircle,
	EmbedSkeletonSubtitle,
	EmbedSkeletonTitle,
} from '~/components/embeds/EmbedCard/EmbedCard';
import cardStyles from '~/components/embeds/EmbedCard/EmbedCard.module.css';
import {useEmbedSkeletonOverride} from '~/components/embeds/EmbedCard/useEmbedSkeletonOverride';
import {Button} from '~/components/uikit/Button/Button';
import i18n from '~/i18n';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import GiftStore from '~/stores/GiftStore';
import UserStore from '~/stores/UserStore';
import {getGiftDurationText} from '~/utils/giftUtils';
import styles from './GiftEmbed.module.css';

interface GiftEmbedProps {
	code: string;
}

export const GiftEmbed = observer(function GiftEmbed({code}: GiftEmbedProps) {
	const {t} = useLingui();
	const giftState = GiftStore.gifts.get(code) ?? null;
	const gift = giftState?.data;
	const creator = UserStore.getUser(gift?.created_by?.id ?? '');
	const shouldForceSkeleton = useEmbedSkeletonOverride();

	React.useEffect(() => {
		if (!giftState) {
			void GiftActionCreators.fetchWithCoalescing(code).catch(() => {});
		}
	}, [code, giftState]);

	const prevLoadingRef = React.useRef<boolean>(true);
	React.useEffect(() => {
		const isLoading = !!giftState?.loading;
		if (prevLoadingRef.current && !isLoading && giftState) {
			ComponentDispatch.dispatch('LAYOUT_RESIZED');
		}
		prevLoadingRef.current = isLoading;
	}, [giftState?.loading]);

	if (shouldForceSkeleton || !giftState || giftState.loading) {
		return <GiftLoadingState />;
	}

	if (giftState.invalid || giftState.error || !gift) {
		return <GiftNotFoundError />;
	}

	const durationText = getGiftDurationText(i18n, gift);

	const handleRedeem = async () => {
		try {
			await GiftActionCreators.redeem(i18n, code);
		} catch (error) {
			console.error('Failed to redeem gift', error);
		}
	};

	const subtitleNode = creator ? (
		<span className={styles.subRow}>{t`From ${creator.username}#${creator.discriminator}`}</span>
	) : undefined;

	const helpText = gift.redeemed ? t`Already redeemed` : t`Click to claim your gift!`;

	const footer = gift.redeemed ? (
		<Button variant="primary" matchSkeletonHeight disabled>
			{t`Gift Claimed`}
		</Button>
	) : (
		<Button variant="primary" matchSkeletonHeight onClick={handleRedeem}>
			{t`Claim Gift`}
		</Button>
	);

	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div className={`${styles.iconCircle} ${gift.redeemed ? styles.iconCircleInactive : styles.iconCircleActive}`}>
					<GiftIcon className={styles.icon} />
				</div>
			}
			title={
				<h3
					className={`${styles.title} ${cardStyles.title} ${gift.redeemed ? styles.titleTertiary : styles.titlePrimary}`}
				>
					{durationText}
				</h3>
			}
			subtitle={subtitleNode}
			body={<div className={styles.helpRow}>{helpText}</div>}
			footer={footer}
		/>
	);
});

const GiftLoadingState = observer(function GiftLoadingState() {
	return (
		<EmbedCard
			splashURL={null}
			icon={<EmbedSkeletonCircle className={styles.skeletonCircle} />}
			title={<EmbedSkeletonTitle className={styles.skeletonTitle} />}
			body={<EmbedSkeletonSubtitle className={styles.skeletonHelp} />}
			footer={<EmbedSkeletonButton className={styles.skeletonButton} />}
		/>
	);
});

const GiftNotFoundError = observer(function GiftNotFoundError() {
	const {t} = useLingui();

	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div className={`${styles.iconCircle} ${styles.iconCircleDisabled}`}>
					<QuestionIcon className={`${styles.icon} ${styles.iconError}`} />
				</div>
			}
			title={<h3 className={`${styles.title} ${styles.titleDanger}`}>{t`Unknown Gift`}</h3>}
			body={<span className={styles.helpRow}>{t`This gift code is invalid or already claimed.`}</span>}
			footer={
				<Button variant="primary" matchSkeletonHeight disabled>
					{t`Gift Unavailable`}
				</Button>
			}
		/>
	);
});

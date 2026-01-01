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
import {GiftIcon} from '@phosphor-icons/react';
import type {Gift} from '~/actions/GiftActionCreators';
import {getPremiumGiftDurationText} from '~/utils/giftUtils';
import styles from './AuthPageStyles.module.css';

interface GiftHeaderProps {
	gift: Gift;
	variant: 'login' | 'register';
}

export function GiftHeader({gift, variant}: GiftHeaderProps) {
	const {i18n} = useLingui();
	const durationText = getPremiumGiftDurationText(i18n, gift);

	const sender =
		gift.created_by?.username && gift.created_by.discriminator
			? `${gift.created_by.username}#${gift.created_by.discriminator}`
			: null;

	return (
		<div className={styles.entityHeader}>
			<div className={styles.giftIconContainer}>
				<GiftIcon className={styles.giftIcon} />
			</div>
			<div className={styles.entityDetails}>
				<p className={styles.entityText}>
					{sender ? <Trans>{sender} sent you a gift!</Trans> : <Trans>You've received a gift!</Trans>}
				</p>
				<h2 className={styles.entityTitle}>{durationText}</h2>
				<p className={styles.entitySubtext}>
					{variant === 'login' ? (
						<Trans>Log in to claim your gift</Trans>
					) : (
						<Trans>Create an account to claim your gift</Trans>
					)}
				</p>
			</div>
		</div>
	);
}

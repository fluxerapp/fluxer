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

import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Button} from '~/components/uikit/Button/Button';
import * as LocaleUtils from '~/utils/LocaleUtils';
import styles from './PricingCard.module.css';

export const PricingCard = observer(
	({
		title,
		price,
		period,
		badge,
		isPopular,
		isSoldOut,
		soldOut,
		owned,
		remainingSlots,
		onSelect,
		buttonText,
		isLoading = false,
		disabled = false,
		className,
	}: {
		title: string;
		price: string;
		period?: string;
		badge?: string;
		isPopular?: boolean;
		isSoldOut?: boolean;
		soldOut?: boolean;
		owned?: boolean;
		remainingSlots?: number;
		onSelect: () => void;
		buttonText?: string;
		isLoading?: boolean;
		disabled?: boolean;
		className?: string;
	}) => {
		const locale = LocaleUtils.getCurrentLocale();
		const formatter = new Intl.NumberFormat(locale);

		const actuallySoldOut = (soldOut ?? isSoldOut ?? false) && !owned;
		const isCardDisabled = disabled || actuallySoldOut || isLoading || owned;

		const handleClick = React.useCallback(() => {
			if (isCardDisabled) return;
			onSelect();
		}, [isCardDisabled, onSelect]);

		const getButtonVariant = (): 'primary' | 'secondary' | 'inverted' => {
			if (actuallySoldOut) return 'secondary';
			if (isPopular) return 'inverted';
			return 'primary';
		};

		const renderButtonLabel = () => {
			if (owned) return <Trans>Owned</Trans>;
			if (actuallySoldOut) return <Trans>Sold Out</Trans>;
			return buttonText || <Trans>Select</Trans>;
		};

		return (
			<div
				className={clsx(
					isPopular ? styles.cardPopular : styles.cardDefault,
					isCardDisabled && styles.disabled,
					className,
				)}
				aria-busy={isLoading}
			>
				<div className={styles.popularBadgeSpace}>
					{isPopular ? (
						<div className={styles.popularBadge}>
							<Trans>Most popular</Trans>
						</div>
					) : remainingSlots !== undefined && remainingSlots >= 0 && !actuallySoldOut ? (
						<div className={styles.popularBadge}>
							<Trans>{formatter.format(remainingSlots)} remaining</Trans>
						</div>
					) : null}
				</div>

				{actuallySoldOut && (
					<div className={styles.soldOutBadge}>
						<Trans>Sold out</Trans>
					</div>
				)}

				<div className={styles.contentContainer}>
					<h3 className={isPopular ? styles.cardTitlePopular : styles.cardTitleDefault}>{title}</h3>
					<p className={isPopular ? styles.cardPricePopular : styles.cardPriceDefault}>{price}</p>
					{period && <p className={isPopular ? styles.cardPeriodPopular : styles.cardPeriodDefault}>{period}</p>}

					<div className={styles.badgeSpace}>
						{badge ? (
							<span className={clsx(styles.badge, isPopular && styles.badgeOnBrand)}>{badge}</span>
						) : (
							<span className={styles.badgePlaceholder} aria-hidden="true">
								.
							</span>
						)}
					</div>
				</div>

				<Button
					variant={getButtonVariant()}
					onClick={handleClick}
					disabled={isCardDisabled}
					submitting={isLoading}
					className={styles.selectButton}
					aria-disabled={isCardDisabled}
					aria-label={title}
				>
					{renderButtonLabel()}
				</Button>
			</div>
		);
	},
);

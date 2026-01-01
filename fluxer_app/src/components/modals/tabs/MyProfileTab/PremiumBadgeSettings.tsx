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

import {t} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {Switch} from '~/components/form/Switch';
import * as DateUtils from '~/utils/DateUtils';
import styles from './PremiumBadgeSettings.module.css';

interface PremiumBadgeSettingsProps {
	premiumBadgeHidden: boolean;
	premiumBadgeTimestampHidden: boolean;
	premiumBadgeMasked: boolean;
	premiumBadgeSequenceHidden: boolean;
	onToggle: (
		field:
			| 'premium_badge_hidden'
			| 'premium_badge_timestamp_hidden'
			| 'premium_badge_masked'
			| 'premium_badge_sequence_hidden',
		value: boolean,
	) => void;
	hasLifetimePremium: boolean;
	premiumSince?: Date | null;
	premiumLifetimeSequence?: number | null;
}

export const PremiumBadgeSettings = observer(
	({
		premiumBadgeHidden,
		premiumBadgeTimestampHidden,
		premiumBadgeMasked,
		premiumBadgeSequenceHidden,
		onToggle,
		hasLifetimePremium,
		premiumSince,
		premiumLifetimeSequence,
	}: PremiumBadgeSettingsProps) => {
		const {i18n} = useLingui();

		return (
			<div>
				<div className={styles.header}>
					<h2 className={styles.title}>
						<Trans>Plutonium Badge Privacy</Trans>
					</h2>
					<p className={styles.description}>
						<Trans>Control how your Plutonium badge is displayed to others</Trans>
					</p>
				</div>

				<div className={styles.switches}>
					<Switch
						label={t(i18n)`Hide Plutonium badge entirely`}
						description={t(i18n)`Completely hide your Plutonium badge from other users`}
						value={premiumBadgeHidden}
						onChange={(value) => onToggle('premium_badge_hidden', value)}
					/>

					<Switch
						label={
							premiumSince
								? t(i18n)`Hide Plutonium purchase date (${DateUtils.getFormattedShortDate(premiumSince)})`
								: t(i18n)`Hide Plutonium purchase date`
						}
						description={t(i18n)`Remove when you first bought Plutonium from your badge`}
						value={premiumBadgeTimestampHidden}
						onChange={(value) => onToggle('premium_badge_timestamp_hidden', value)}
						disabled={premiumBadgeHidden}
					/>

					{hasLifetimePremium && (
						<Switch
							label={t(i18n)`Mask Visionary as subscription`}
							description={t(i18n)`Show your Visionary as a regular subscription instead`}
							value={premiumBadgeMasked}
							onChange={(value) => onToggle('premium_badge_masked', value)}
							disabled={premiumBadgeHidden}
						/>
					)}

					{hasLifetimePremium && (
						<Switch
							label={
								premiumLifetimeSequence != null
									? t(i18n)`Hide Visionary ID badge (#${premiumLifetimeSequence})`
									: t(i18n)`Hide Visionary ID badge`
							}
							description={t(i18n)`Remove your Visionary ID badge`}
							value={premiumBadgeSequenceHidden}
							onChange={(value) => onToggle('premium_badge_sequence_hidden', value)}
							disabled={premiumBadgeHidden || premiumBadgeMasked}
						/>
					)}
				</div>
			</div>
		);
	},
);

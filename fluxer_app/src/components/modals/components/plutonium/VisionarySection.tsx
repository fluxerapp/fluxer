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
import {ChatsCircleIcon, CrownIcon, HashIcon, InfinityIcon, MedalIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {VisionarySlots} from '~/actions/PremiumActionCreators';
import {Button} from '~/components/uikit/Button/Button';
import {VisionaryBenefit} from '../VisionaryBenefit';
import {SectionHeader} from './SectionHeader';
import styles from './VisionarySection.module.css';

interface VisionarySectionProps {
	visionarySlots: VisionarySlots | null;
	formatter: Intl.NumberFormat;
	isVisionary: boolean;
	isPremium: boolean;
	isGiftSubscription: boolean;
	loadingCheckout: boolean;
	loadingSlots: boolean;
	handleSelectPlan: (plan: 'visionary') => void;
}

export const VisionarySection: React.FC<VisionarySectionProps> = observer(
	({
		visionarySlots,
		formatter,
		isVisionary,
		isPremium,
		isGiftSubscription,
		loadingCheckout,
		loadingSlots,
		handleSelectPlan,
	}) => {
		const {t} = useLingui();
		const currentAccessLabel = isGiftSubscription ? t`gift time` : t`subscription`;

		return (
			<section className={styles.section}>
				<SectionHeader
					title={<Trans>Visionary</Trans>}
					description={
						visionarySlots ? (
							<Trans>
								Limited to {formatter.format(visionarySlots.total)} users. One-time purchase, lifetime access.
							</Trans>
						) : (
							<Trans>One-time purchase, lifetime access.</Trans>
						)
					}
				/>

				<div className={styles.benefitsGrid}>
					<VisionaryBenefit
						icon={<InfinityIcon className={styles.benefitIcon} weight="bold" />}
						title={t`Lifetime Plutonium`}
						description={t`All current and future Plutonium benefits, forever`}
					/>

					<VisionaryBenefit
						icon={<MedalIcon className={styles.benefitIcon} weight="fill" />}
						title={t`Unique profile badge`}
						description={
							visionarySlots
								? t`Numbered badge (1-${formatter.format(visionarySlots.total)}) showing your Visionary status`
								: t`Numbered badge showing your Visionary status`
						}
					/>

					<VisionaryBenefit
						icon={<HashIcon className={styles.benefitIcon} weight="bold" />}
						title={t`Optional #0000 tag`}
						description={t`Let people add you without needing a 4-digit tag`}
					/>

					<VisionaryBenefit
						icon={<ChatsCircleIcon className={styles.benefitIcon} weight="fill" />}
						title={t`Exclusive community`}
						description={t`Visionary community with direct team access`}
					/>
				</div>

				{!isVisionary && visionarySlots && visionarySlots.remaining > 0 && (
					<div className={styles.ctaContainer}>
						<Button
							variant="primary"
							onClick={() => handleSelectPlan('visionary')}
							submitting={loadingCheckout || loadingSlots}
							className={styles.ctaButton}
						>
							<CrownIcon className={styles.ctaIcon} weight="fill" />
							<Trans>Upgrade to Visionary — {formatter.format(visionarySlots.remaining)} Left</Trans>
						</Button>

						{isPremium && (
							<p className={styles.disclaimer}>
								<Trans>
									Buying Visionary will cancel your current {currentAccessLabel} immediately. It is a one-time purchase
									with lifetime access.
								</Trans>
							</p>
						)}
					</div>
				)}
			</section>
		);
	},
);

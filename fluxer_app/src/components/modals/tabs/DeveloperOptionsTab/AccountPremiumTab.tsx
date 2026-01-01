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

import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as DeveloperOptionsActionCreators from '~/actions/DeveloperOptionsActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {UserPremiumTypes} from '~/Constants';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import {Slider} from '~/components/uikit/Slider';
import type {UserRecord} from '~/records/UserRecord';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import styles from './AccountPremiumTab.module.css';
import {
	applyPremiumScenarioOption,
	PREMIUM_SCENARIO_OPTIONS,
	type PremiumScenarioOption,
} from './premiumScenarioOptions';

interface AccountPremiumTabContentProps {
	user: UserRecord;
}

export const AccountPremiumTabContent: React.FC<AccountPremiumTabContentProps> = observer(({user}) => {
	const {t} = useLingui();
	const premiumTypeOptions = [
		{value: '', label: t`Use Actual Premium Type`},
		{value: UserPremiumTypes.NONE.toString(), label: t`None`},
		{value: UserPremiumTypes.SUBSCRIPTION.toString(), label: t`Subscription`},
		{value: UserPremiumTypes.LIFETIME.toString(), label: t`Lifetime`},
	];

	return (
		<>
			<SettingsTabSection title={<Trans>Account State Overrides</Trans>}>
				<Switch
					label={t`Email Verified Override`}
					value={DeveloperOptionsStore.emailVerifiedOverride ?? false}
					description={t`Override email verification status`}
					onChange={(value) =>
						DeveloperOptionsActionCreators.updateOption('emailVerifiedOverride', value ? true : null)
					}
				/>
				<Switch
					label={t`Unclaimed Account Override`}
					value={DeveloperOptionsStore.unclaimedAccountOverride ?? false}
					description={t`Override unclaimed account status`}
					onChange={(value) =>
						DeveloperOptionsActionCreators.updateOption('unclaimedAccountOverride', value ? true : null)
					}
				/>
				<Switch
					label={t`Unread Gift Inventory Override`}
					value={DeveloperOptionsStore.hasUnreadGiftInventoryOverride ?? false}
					description={t`Override unread gift inventory status`}
					onChange={(value) => {
						DeveloperOptionsActionCreators.updateOption('hasUnreadGiftInventoryOverride', value ? true : null);
						if (!value) DeveloperOptionsActionCreators.updateOption('unreadGiftInventoryCountOverride', null);
					}}
				/>
				{DeveloperOptionsStore.hasUnreadGiftInventoryOverride && (
					<div className={styles.sliderContainer}>
						<div className={styles.sliderLabel}>
							<span className={styles.labelText}>
								<Trans>Unread Gift Count</Trans>
							</span>
							<p className={styles.labelDescription}>
								<Trans>Set the number of unread gifts in inventory.</Trans>
							</p>
						</div>
						<Slider
							defaultValue={DeveloperOptionsStore.unreadGiftInventoryCountOverride ?? 1}
							factoryDefaultValue={1}
							minValue={0}
							maxValue={99}
							step={1}
							markers={[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99]}
							stickToMarkers={false}
							onMarkerRender={(v) => `${v}`}
							onValueRender={(v) => <Plural value={v} one="# gift" other="# gifts" />}
							onValueChange={(v) => DeveloperOptionsActionCreators.updateOption('unreadGiftInventoryCountOverride', v)}
						/>
					</div>
				)}
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Premium Type Override</Trans>}>
				<Select
					label={t`Override Premium Type`}
					value={DeveloperOptionsStore.premiumTypeOverride?.toString() ?? ''}
					options={premiumTypeOptions}
					onChange={(value) => {
						const premiumTypeOverride = value === '' ? null : Number.parseInt(value, 10);
						DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', premiumTypeOverride);
					}}
				/>
				<Switch
					label={t`Backend Premium Override`}
					value={user.premiumEnabledOverride ?? false}
					description={t`Toggle premium_enabled_override on the backend`}
					onChange={async (value) => {
						await UserActionCreators.update({premium_enabled_override: value});
					}}
				/>
				<Switch
					label={t`Has Ever Purchased Override`}
					value={DeveloperOptionsStore.hasEverPurchasedOverride ?? false}
					description={t`Simulates having a Stripe customer ID (for testing purchase history access)`}
					onChange={(value) =>
						DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', value ? true : null)
					}
				/>
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Premium Subscription Scenarios</Trans>}>
				<Select<PremiumScenarioOption>
					label={t`Test Subscription State`}
					value="none"
					options={PREMIUM_SCENARIO_OPTIONS.map(({value, label}) => ({
						value,
						label: t(label),
					}))}
					onChange={applyPremiumScenarioOption}
				/>
			</SettingsTabSection>
		</>
	);
});

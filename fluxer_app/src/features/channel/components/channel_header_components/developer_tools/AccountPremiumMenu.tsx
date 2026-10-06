// SPDX-License-Identifier: AGPL-3.0-or-later

import {showChannelErrorModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import {
	PREMIUM_TYPE_DESCRIPTOR,
	UNREAD_GIFT_COUNT_DESCRIPTOR,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperOptionLabels';
import {
	DeveloperOptionCheckbox,
	DeveloperOptionRadioItems,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsMenuComponents';
import {
	translateDescriptor,
	USE_ACTUAL_VALUE_DESCRIPTOR,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsShared';
import {getPremiumTypeOptions} from '@app/features/channel/components/channel_header_components/developer_tools/OptionPresets';
import {updateOption} from '@app/features/channel/components/channel_header_components/developer_tools/ResetOptions';
import {
	applyPremiumScenarioOption,
	PREMIUM_SCENARIO_OPTIONS,
	resetPremiumStateOverrides,
} from '@app/features/devtools/components/PremiumScenarioOptions';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import {msg, plural} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const logger = new Logger('DeveloperToolsContextMenu/AccountPremiumMenu');
const ACCOUNT_STATE_DESCRIPTOR = msg({
	message: 'Account state',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FAILED_TO_UPDATE_PREMIUM_PERK_STATE_DESCRIPTOR = msg({
	message: 'Failed to update premium perk state.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VISIONARY_BADGE_DESCRIPTOR = msg({
	message: 'Visionary badge',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VISIONARY_ID_NUMBER_DESCRIPTOR = msg({
	message: 'Visionary ID number',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SUBSCRIPTION_SCENARIOS_DESCRIPTOR = msg({
	message: 'Subscription scenarios',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const ACCOUNT_PREMIUM_OVERRIDES_DESCRIPTOR = msg({
	message: 'Account & premium overrides',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const AccountStateMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(ACCOUNT_STATE_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionCheckbox
						optionKey="emailVerifiedOverride"
						checked={DeveloperOptions.emailVerifiedOverride ?? false}
						uncheckedValue={null}
						data-flx="channel.channel-header-components.developer-tools-context-menu.account-state-menu.developer-option-checkbox"
					>
						<Trans>Email verified override</Trans>
					</DeveloperOptionCheckbox>
					<DeveloperOptionCheckbox
						optionKey="unclaimedAccountOverride"
						checked={DeveloperOptions.unclaimedAccountOverride ?? false}
						uncheckedValue={null}
						data-flx="channel.channel-header-components.developer-tools-context-menu.account-state-menu.developer-option-checkbox--2"
					>
						<Trans>Unclaimed account override</Trans>
					</DeveloperOptionCheckbox>
					<DeveloperOptionCheckbox
						optionKey="hasUnreadGiftInventoryOverride"
						checked={DeveloperOptions.hasUnreadGiftInventoryOverride ?? false}
						uncheckedValue={null}
						onAfterChange={(checked) => {
							if (!checked) updateOption('unreadGiftInventoryCountOverride', null);
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.account-state-menu.developer-option-checkbox--3"
					>
						<Trans>Unread gift inventory override</Trans>
					</DeveloperOptionCheckbox>
					{DeveloperOptions.hasUnreadGiftInventoryOverride && (
						<MenuItemSlider
							label={i18n._(UNREAD_GIFT_COUNT_DESCRIPTOR)}
							value={DeveloperOptions.unreadGiftInventoryCountOverride ?? 1}
							minValue={0}
							maxValue={99}
							onChange={(value) => updateOption('unreadGiftInventoryCountOverride', Math.round(value))}
							onFormat={(value) => plural({count: Math.round(value)}, {one: '# gift', other: '# gifts'})}
							data-flx="channel.channel-header-components.developer-tools-context-menu.account-state-menu.menu-item-slider.update-option"
						/>
					)}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.account-state-menu.menu-item-submenu"
		/>
	);
});
const PremiumTypeMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	return (
		<MenuItemSubmenu
			label={i18n._(PREMIUM_TYPE_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionRadioItems
						optionKey="premiumTypeOverride"
						options={getPremiumTypeOptions()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.premium-type-menu.developer-option-radio-items"
					/>
					{user?.isClaimed() && (
						<CheckboxItem
							checked={user.premiumEnabledOverride ?? false}
							onCheckedChange={(checked) => {
								void UserCommands.update({premium_enabled_override: checked});
							}}
							data-flx="channel.channel-header-components.developer-tools-context-menu.premium-type-menu.checkbox-item"
						>
							<Trans>Backend premium override</Trans>
						</CheckboxItem>
					)}
					{user?.isClaimed() && (
						<CheckboxItem
							checked={user.premiumPerksDisabled ?? false}
							onCheckedChange={(checked) => {
								void PremiumCommands.setPremiumPerksDisabled(checked).catch((error) => {
									logger.error('Failed to update premium perks disabled state', error);
									showChannelErrorModal({
										title: i18n._(FAILED_TO_UPDATE_PREMIUM_PERK_STATE_DESCRIPTOR),
										message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
										dataFlx:
											'channel.channel-header-components.developer-tools-context-menu.premium-perks-disabled-failed.generic-error-modal',
									});
								});
							}}
							data-flx="channel.channel-header-components.developer-tools-context-menu.premium-type-menu.checkbox-item--2"
						>
							<Trans>Disable backend premium perks</Trans>
						</CheckboxItem>
					)}
					<DeveloperOptionCheckbox
						optionKey="hasEverPurchasedOverride"
						checked={DeveloperOptions.hasEverPurchasedOverride ?? false}
						uncheckedValue={null}
						data-flx="channel.channel-header-components.developer-tools-context-menu.premium-type-menu.developer-option-checkbox"
					>
						<Trans>Has ever purchased override</Trans>
					</DeveloperOptionCheckbox>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.premium-type-menu.menu-item-submenu"
		/>
	);
});
const VisionaryBadgeMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(VISIONARY_BADGE_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionRadioItems
						optionKey="premiumLifetimeSequenceOverride"
						options={[{value: null, label: USE_ACTUAL_VALUE_DESCRIPTOR}]}
						data-flx="channel.channel-header-components.developer-tools-context-menu.visionary-badge-menu.developer-option-radio-items"
					/>
					<MenuItemSlider
						label={i18n._(VISIONARY_ID_NUMBER_DESCRIPTOR)}
						value={DeveloperOptions.premiumLifetimeSequenceOverride ?? 42}
						minValue={1}
						maxValue={999}
						onChange={(value) => updateOption('premiumLifetimeSequenceOverride', Math.round(value))}
						onFormat={(value) => `#${Math.round(value)}`}
						data-flx="channel.channel-header-components.developer-tools-context-menu.visionary-badge-menu.menu-item-slider.update-option"
					/>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.visionary-badge-menu.menu-item-submenu"
		/>
	);
});
const SubscriptionScenariosMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(SUBSCRIPTION_SCENARIOS_DESCRIPTOR)}
			render={() => (
				<>
					{PREMIUM_SCENARIO_OPTIONS.map(({value, label}) => {
						if (value === 'none') return null;
						if (value === 'reset') {
							return (
								<MenuItem
									key={value}
									onClick={resetPremiumStateOverrides}
									closeOnSelect={false}
									data-flx="channel.channel-header-components.developer-tools-context-menu.subscription-scenarios-menu.menu-item.reset-premium-state-overrides"
								>
									{translateDescriptor(i18n, label)}
								</MenuItem>
							);
						}
						const checked = DeveloperOptions.premiumScenarioOverride === value;
						return (
							<CheckboxItem
								key={value}
								checked={checked}
								onCheckedChange={(nextChecked) => {
									if (nextChecked) {
										applyPremiumScenarioOption(value);
									} else {
										resetPremiumStateOverrides();
									}
								}}
								data-flx="channel.channel-header-components.developer-tools-context-menu.subscription-scenarios-menu.checkbox-item.apply-premium-scenario-option"
							>
								{translateDescriptor(i18n, label)}
							</CheckboxItem>
						);
					})}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.subscription-scenarios-menu.menu-item-submenu"
		/>
	);
});
export const AccountPremiumMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(ACCOUNT_PREMIUM_OVERRIDES_DESCRIPTOR)}
			render={() => (
				<>
					<AccountStateMenu data-flx="channel.channel-header-components.developer-tools-context-menu.account-premium-menu.account-state-menu" />
					<PremiumTypeMenu data-flx="channel.channel-header-components.developer-tools-context-menu.account-premium-menu.premium-type-menu" />
					<VisionaryBadgeMenu data-flx="channel.channel-header-components.developer-tools-context-menu.account-premium-menu.visionary-badge-menu" />
					<SubscriptionScenariosMenu data-flx="channel.channel-header-components.developer-tools-context-menu.account-premium-menu.subscription-scenarios-menu" />
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.account-premium-menu.menu-item-submenu"
		/>
	);
});

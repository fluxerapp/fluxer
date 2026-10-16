// SPDX-License-Identifier: AGPL-3.0-or-later

import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/PremiumBadgeSettings.module.css';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const HIDE_BADGE_ENTIRELY_DESCRIPTOR = msg({
	message: 'Hide {premiumProductName} badge entirely',
	comment:
		'Label in the premium badge settings. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const COMPLETELY_HIDE_YOUR_BADGE_FROM_OTHER_USERS_DESCRIPTOR = msg({
	message: 'Completely hide your {premiumProductName} badge from other users',
	comment:
		'Label in the premium badge settings. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const HIDE_PURCHASE_DATE_DESCRIPTOR = msg({
	message: 'Hide {premiumProductName} purchase date ({dateUtilsGetFormattedShortDatePremiumSince})',
	comment:
		'Description text in the premium badge settings. Preserve {premiumProductName}, {dateUtilsGetFormattedShortDatePremiumSince}; they are inserted by code. Keep the tone plain and specific.',
});
const HIDE_PURCHASE_DATE_2_DESCRIPTOR = msg({
	message: 'Hide {premiumProductName} purchase date',
	comment:
		'Label in the premium badge settings. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const REMOVE_WHEN_YOU_FIRST_BOUGHT_FROM_YOUR_BADGE_DESCRIPTOR = msg({
	message: 'Remove when you first bought {premiumProductName} from your badge',
	comment:
		'Button or menu action label in the premium badge settings. Keep it concise. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const MASK_VISIONARY_AS_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Mask Visionary as subscription',
	comment: 'Label in the premium badge settings. Keep the tone plain and specific.',
});
const SHOW_YOUR_VISIONARY_AS_A_REGULAR_SUBSCRIPTION_INSTEAD_DESCRIPTOR = msg({
	message: 'Show your Visionary as a regular subscription instead',
	comment: 'Label in the premium badge settings. Keep the tone plain and specific.',
});
const HIDE_VISIONARY_ID_BADGE_DESCRIPTOR = msg({
	message: 'Hide Visionary ID badge ({premiumLifetimeSequenceLabel})',
	comment:
		'Label in the premium badge settings. Preserve {premiumLifetimeSequenceLabel}; it is inserted by code. Keep the tone plain and specific.',
});
const HIDE_VISIONARY_ID_BADGE_2_DESCRIPTOR = msg({
	message: 'Hide Visionary ID badge',
	comment: 'Label in the premium badge settings. Keep the tone plain and specific.',
});
const REMOVE_YOUR_VISIONARY_ID_BADGE_DESCRIPTOR = msg({
	message: 'Remove your Visionary ID badge',
	comment:
		'Button or menu action label in the premium badge settings. Keep it concise. Keep the tone plain and specific.',
});
const PREMIUM_BADGE_PRIVACY_DESCRIPTOR = msg({
	message: '{premiumProductName} badge privacy',
	comment: 'Profile settings section title for premium badge privacy controls.',
});
const PREMIUM_BADGE_DISPLAY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Control how your {premiumProductName} badge is displayed to others',
	comment: 'Profile settings section description for premium badge privacy controls.',
});

interface PremiumBadgeSettingsProps {
	premiumBadgeHidden: boolean;
	premiumBadgeTimestampHidden: boolean;
	premiumBadgeMasked: boolean;
	premiumBadgeSequenceHidden: boolean;
	disabled?: boolean;
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
		disabled,
		onToggle,
		hasLifetimePremium,
		premiumSince,
		premiumLifetimeSequence,
	}: PremiumBadgeSettingsProps) => {
		const {i18n} = useLingui();
		const premiumLifetimeSequenceLabel = premiumLifetimeSequence != null ? `#${premiumLifetimeSequence}` : null;
		return (
			<div data-flx="user.my-profile-tab.premium-badge-settings.div">
				<div className={styles.header} data-flx="user.my-profile-tab.premium-badge-settings.header">
					<h2 className={styles.title} data-flx="user.my-profile-tab.premium-badge-settings.title">
						{i18n._(PREMIUM_BADGE_PRIVACY_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
					</h2>
					<p className={styles.description} data-flx="user.my-profile-tab.premium-badge-settings.description">
						{i18n._(PREMIUM_BADGE_DISPLAY_DESCRIPTION_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
					</p>
				</div>
				<div className={styles.switches} data-flx="user.my-profile-tab.premium-badge-settings.switches">
					<Switch
						label={i18n._(HIDE_BADGE_ENTIRELY_DESCRIPTOR, {premiumProductName: getPremiumProductName()})}
						description={i18n._(COMPLETELY_HIDE_YOUR_BADGE_FROM_OTHER_USERS_DESCRIPTOR, {
							premiumProductName: getPremiumProductName(),
						})}
						value={premiumBadgeHidden}
						onChange={(value) => onToggle('premium_badge_hidden', value)}
						disabled={disabled}
						data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle"
					/>
					<Switch
						label={
							premiumSince
								? i18n._(HIDE_PURCHASE_DATE_DESCRIPTOR, {
										premiumProductName: getPremiumProductName(),
										dateUtilsGetFormattedShortDatePremiumSince: DateUtils.getFormattedShortDate(premiumSince),
									})
								: i18n._(HIDE_PURCHASE_DATE_2_DESCRIPTOR, {premiumProductName: getPremiumProductName()})
						}
						description={i18n._(REMOVE_WHEN_YOU_FIRST_BOUGHT_FROM_YOUR_BADGE_DESCRIPTOR, {
							premiumProductName: getPremiumProductName(),
						})}
						value={premiumBadgeTimestampHidden}
						onChange={(value) => onToggle('premium_badge_timestamp_hidden', value)}
						disabled={disabled || premiumBadgeHidden}
						data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle--2"
					/>
					{hasLifetimePremium && (
						<Switch
							label={i18n._(MASK_VISIONARY_AS_SUBSCRIPTION_DESCRIPTOR)}
							description={i18n._(SHOW_YOUR_VISIONARY_AS_A_REGULAR_SUBSCRIPTION_INSTEAD_DESCRIPTOR)}
							value={premiumBadgeMasked}
							onChange={(value) => onToggle('premium_badge_masked', value)}
							disabled={disabled || premiumBadgeHidden}
							data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle--3"
						/>
					)}
					{hasLifetimePremium && (
						<Switch
							label={
								premiumLifetimeSequenceLabel
									? i18n._(HIDE_VISIONARY_ID_BADGE_DESCRIPTOR, {premiumLifetimeSequenceLabel})
									: i18n._(HIDE_VISIONARY_ID_BADGE_2_DESCRIPTOR)
							}
							description={i18n._(REMOVE_YOUR_VISIONARY_ID_BADGE_DESCRIPTOR)}
							value={premiumBadgeSequenceHidden}
							onChange={(value) => onToggle('premium_badge_sequence_hidden', value)}
							disabled={disabled || premiumBadgeHidden || premiumBadgeMasked}
							data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle--4"
						/>
					)}
				</div>
			</div>
		);
	},
);

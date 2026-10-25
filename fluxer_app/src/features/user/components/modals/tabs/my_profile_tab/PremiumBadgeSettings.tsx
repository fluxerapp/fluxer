// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {formatOperatorPassSinceDate} from '@app/features/premium/utils/OperatorPassUtils';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {OperatorPassWallSettings} from '@app/features/user/components/modals/tabs/my_profile_tab/OperatorPassWallSettings';
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

const OPERATOR_PASS_SINCE_DESCRIPTOR = msg({
	message: 'Operator Pass since {since}',
	comment:
		'Line under the Operator Pass heading in profile settings. {since} is a date such as November 14, 2026. Operator Pass is a product name and stays untranslated.',
});
const SHOW_OPERATOR_PASS_BADGE_DESCRIPTOR = msg({
	message: 'Show badge on my profile',
	comment: 'Switch in the Operator Pass profile settings that shows the Operator badge on the profile.',
});
const HIDE_OPERATOR_PASS_DATE_DESCRIPTOR = msg({
	message: 'Hide when you got your {operatorPassName}',
	comment:
		'Switch label in the Operator Pass badge privacy settings, with no description under it. When on, other people no longer see the month and year the pass was first bought or received on the badge. Operator Pass is a product name and stays untranslated.',
});
const HIDE_OPERATOR_PASS_COUNT_DESCRIPTOR = msg({
	message: 'Hide how many Operator Passes you have',
	comment:
		"Switch label in the Operator Pass badge privacy settings, with no description under it. When on, other people no longer see the number of passes on the badge or on the Operators' wall. Operator Pass is a product name and stays untranslated.",
});

type BadgeSettingField =
	| 'premium_badge_hidden'
	| 'premium_badge_timestamp_hidden'
	| 'premium_badge_masked'
	| 'premium_badge_sequence_hidden'
	| 'operator_pass_badge_hidden'
	| 'operator_pass_badge_timestamp_hidden'
	| 'operator_pass_badge_count_hidden';

interface PremiumBadgeSettingsProps {
	premiumBadgeHidden: boolean;
	premiumBadgeTimestampHidden: boolean;
	premiumBadgeMasked: boolean;
	premiumBadgeSequenceHidden: boolean;
	operatorPassBadgeHidden: boolean;
	operatorPassBadgeTimestampHidden: boolean;
	operatorPassBadgeCountHidden: boolean;
	disabled?: boolean;
	onToggle: (field: BadgeSettingField, value: boolean) => void;
	hasPremium: boolean;
	hasOperatorPassBadge: boolean;
	hasLifetimePremium: boolean;
	premiumSince?: Date | null;
	premiumLifetimeSequence?: number | null;
	operatorPassSince?: Date | null;
}

export const PremiumBadgeSettings = observer(
	({
		premiumBadgeHidden,
		premiumBadgeTimestampHidden,
		premiumBadgeMasked,
		premiumBadgeSequenceHidden,
		operatorPassBadgeHidden,
		operatorPassBadgeTimestampHidden,
		operatorPassBadgeCountHidden,
		disabled,
		onToggle,
		hasPremium,
		hasOperatorPassBadge,
		hasLifetimePremium,
		premiumSince,
		premiumLifetimeSequence,
		operatorPassSince,
	}: PremiumBadgeSettingsProps) => {
		const {i18n} = useLingui();
		const premiumLifetimeSequenceLabel = premiumLifetimeSequence != null ? `#${premiumLifetimeSequence}` : null;
		return (
			<div className={styles.sections} data-flx="user.my-profile-tab.premium-badge-settings.sections">
				{hasPremium && (
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
				)}
				{hasOperatorPassBadge && (
					<div data-flx="user.my-profile-tab.premium-badge-settings.operator-pass">
						<div className={styles.header} data-flx="user.my-profile-tab.premium-badge-settings.operator-pass-header">
							<h2 className={styles.title} data-flx="user.my-profile-tab.premium-badge-settings.operator-pass-title">
								{OPERATOR_PASS_NAME}
							</h2>
							{operatorPassSince && (
								<p
									className={styles.description}
									data-flx="user.my-profile-tab.premium-badge-settings.operator-pass-description"
								>
									{i18n._(OPERATOR_PASS_SINCE_DESCRIPTOR, {since: formatOperatorPassSinceDate(operatorPassSince)})}
								</p>
							)}
						</div>
						<div
							className={styles.switches}
							data-flx="user.my-profile-tab.premium-badge-settings.operator-pass-switches"
						>
							<Switch
								label={i18n._(SHOW_OPERATOR_PASS_BADGE_DESCRIPTOR)}
								value={!operatorPassBadgeHidden}
								onChange={(value) => onToggle('operator_pass_badge_hidden', !value)}
								disabled={disabled}
								data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle--operator-pass"
							/>
							<Switch
								label={i18n._(HIDE_OPERATOR_PASS_DATE_DESCRIPTOR, {operatorPassName: OPERATOR_PASS_NAME})}
								value={operatorPassBadgeTimestampHidden}
								onChange={(value) => onToggle('operator_pass_badge_timestamp_hidden', value)}
								disabled={disabled || operatorPassBadgeHidden}
								data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle--operator-pass-date"
							/>
							<Switch
								label={i18n._(HIDE_OPERATOR_PASS_COUNT_DESCRIPTOR)}
								value={operatorPassBadgeCountHidden}
								onChange={(value) => onToggle('operator_pass_badge_count_hidden', value)}
								disabled={disabled}
								data-flx="user.my-profile-tab.premium-badge-settings.switch.toggle--operator-pass-count"
							/>
						</div>
						<OperatorPassWallSettings data-flx="user.my-profile-tab.premium-badge-settings.operator-pass-wall-settings" />
					</div>
				)}
			</div>
		);
	},
);

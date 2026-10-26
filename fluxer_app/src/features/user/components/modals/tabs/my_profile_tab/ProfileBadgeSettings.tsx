// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import {Button} from '@app/features/ui/button/Button';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Spinner} from '@app/features/ui/components/Spinner';
import {PROFILE_BADGES_SECTION_ID} from '@app/features/user/commands/UserSettingsModalCommands';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileBadgeSettings.module.css';
import type {ProfileBadgePreviewState} from '@app/features/user/hooks/useOwnProfileBadges';
import {profileBadgeImageUrl} from '@app/features/user/utils/ProfileUtils';
import {PROFILE_BADGES_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import type {OwnProfileBadgeResponse} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useId} from 'react';

const BADGES_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Choose which badges other people see on your profile.',
	comment:
		'Description under the "Badges" title in profile settings. Each badge in the list has a switch that shows or hides it for other people.',
});
const NO_BADGES_DESCRIPTOR = msg({
	message: "You don't have any badges yet.",
	comment: 'Shown in the "Badges" profile settings section when you hold no badges.',
});
const BADGES_LOAD_FAILED_DESCRIPTOR = msg({
	message: "Your badges couldn't be loaded.",
	comment:
		'Shown in the "Badges" profile settings section when the list of badges fails to load, next to a "Try again" button.',
});
const SHOW_BADGE_DESCRIPTOR = msg({
	message: 'Show {badgeLabel} on your profile',
	comment:
		'Screen reader label for the switch next to one badge in the "Badges" profile settings. {badgeLabel} is the badge name, such as "Fluxer Staff" or "Operator Pass since November 14, 2026".',
});
const VISIONARY_NUMBER_NEEDS_VISIONARY_DESCRIPTOR = msg({
	message: 'Shown only when your Visionary badge is visible and not masked.',
	comment:
		'Note under the Visionary ID row in the "Badges" profile settings, shown while its switch is disabled because the Visionary badge is hidden or shown as a regular subscription. Visionary is a product name and stays untranslated.',
});
const HIDE_PURCHASE_DATE_DESCRIPTOR = msg({
	message: 'Hide {premiumProductName} purchase date',
	comment:
		'Switch under the premium badge row in the "Badges" profile settings. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const REMOVE_WHEN_YOU_FIRST_BOUGHT_FROM_YOUR_BADGE_DESCRIPTOR = msg({
	message: 'Remove when you first bought {premiumProductName} from your badge',
	comment:
		'Description of the switch that hides the purchase date, under the premium badge row in the "Badges" profile settings. Preserve {premiumProductName}; it is inserted by code. Keep the tone plain and specific.',
});
const MASK_VISIONARY_AS_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Mask Visionary as subscription',
	comment: 'Switch under the Visionary badge row in the "Badges" profile settings. Keep the tone plain and specific.',
});
const SHOW_YOUR_VISIONARY_AS_A_REGULAR_SUBSCRIPTION_INSTEAD_DESCRIPTOR = msg({
	message: 'Show your Visionary as a regular subscription instead',
	comment:
		'Description of the masking switch under the Visionary badge row in the "Badges" profile settings. Keep the tone plain and specific.',
});
const HIDE_OPERATOR_PASS_DATE_DESCRIPTOR = msg({
	message: 'Hide when you got your {operatorPassName}',
	comment:
		'Switch under the Operator Pass badge row in the "Badges" profile settings, with no description under it. When on, other people no longer see the month and year the pass was first bought or received on the badge. Operator Pass is a product name and stays untranslated.',
});
const HIDE_OPERATOR_PASS_COUNT_DESCRIPTOR = msg({
	message: 'Hide how many Operator Passes you have',
	comment:
		'Switch under the Operator Pass badge row in the "Badges" profile settings, with no description under it. When on, other people no longer see the number of passes on the badge or on the Operators\' wall. Operator Pass is a product name and stays untranslated.',
});

export type ProfileBadgeVariantField =
	| 'premium_badge_timestamp_hidden'
	| 'premium_badge_masked'
	| 'operator_pass_badge_timestamp_hidden'
	| 'operator_pass_badge_count_hidden';

export type ProfileBadgeVariants = Record<ProfileBadgeVariantField, boolean>;

interface BadgeVariant {
	field: ProfileBadgeVariantField;
	label: MessageDescriptor;
	description?: MessageDescriptor;
}

const PURCHASE_DATE_VARIANT: BadgeVariant = {
	field: 'premium_badge_timestamp_hidden',
	label: HIDE_PURCHASE_DATE_DESCRIPTOR,
	description: REMOVE_WHEN_YOU_FIRST_BOUGHT_FROM_YOUR_BADGE_DESCRIPTOR,
};

const BADGE_VARIANTS: Readonly<Record<string, ReadonlyArray<BadgeVariant>>> = {
	visionary: [
		{
			field: 'premium_badge_masked',
			label: MASK_VISIONARY_AS_SUBSCRIPTION_DESCRIPTOR,
			description: SHOW_YOUR_VISIONARY_AS_A_REGULAR_SUBSCRIPTION_INSTEAD_DESCRIPTOR,
		},
		PURCHASE_DATE_VARIANT,
	],
	plutonium: [PURCHASE_DATE_VARIANT],
	premium: [PURCHASE_DATE_VARIANT],
	operator_pass: [
		{field: 'operator_pass_badge_timestamp_hidden', label: HIDE_OPERATOR_PASS_DATE_DESCRIPTOR},
		{field: 'operator_pass_badge_count_hidden', label: HIDE_OPERATOR_PASS_COUNT_DESCRIPTOR},
	],
};

interface ProfileBadgeSettingsProps {
	preview: ProfileBadgePreviewState;
	hiddenProfileBadges: ReadonlyArray<string>;
	variants: ProfileBadgeVariants;
	disabled: boolean;
	onToggleHidden: (key: string, hidden: boolean) => void;
	onToggleVariant: (field: ProfileBadgeVariantField, value: boolean) => void;
}

function BadgeArt({badge}: {badge: OwnProfileBadgeResponse}) {
	const imageUrl = profileBadgeImageUrl(badge);
	if (imageUrl === null) {
		return (
			<span className={styles.caption} aria-hidden="true" data-flx="user.profile-badge-settings.caption">
				{badge.caption}
			</span>
		);
	}
	return (
		<img
			src={imageUrl}
			alt=""
			className={styles.art}
			style={badge.aspect_ratio ? {width: 'auto', aspectRatio: badge.aspect_ratio} : undefined}
			data-flx="user.profile-badge-settings.art"
		/>
	);
}

function BadgeList({
	ownBadges,
	hiddenProfileBadges,
	variants,
	disabled,
	onToggleHidden,
	onToggleVariant,
}: Omit<ProfileBadgeSettingsProps, 'preview'> & {ownBadges: ReadonlyArray<OwnProfileBadgeResponse>}) {
	const {i18n} = useLingui();
	const hidden = new Set(hiddenProfileBadges);
	const switchIdPrefix = useId();
	const premiumProductName = getPremiumProductName();
	return (
		<ul className={styles.list} data-flx="user.profile-badge-settings.list">
			{ownBadges.map((badge) => {
				const shown = !hidden.has(badge.key);
				const switchId = `${switchIdPrefix}-${badge.key}`;
				const badgeProductName = badge.key === 'visionary' ? 'Visionary' : premiumProductName;
				const numberHasNoEffect =
					badge.key === 'visionary_number' && (hidden.has('visionary') || variants.premium_badge_masked);
				const badgeVariants = shown ? (BADGE_VARIANTS[badge.key] ?? []) : [];
				return (
					<li key={badge.key} className={styles.item} data-flx="user.profile-badge-settings.item">
						<div className={styles.row} data-flx="user.profile-badge-settings.row">
							<label htmlFor={switchId} className={styles.rowLabel} data-flx="user.profile-badge-settings.row-label">
								<span className={styles.artSlot} data-flx="user.profile-badge-settings.art-slot">
									<BadgeArt badge={badge} data-flx="user.profile-badge-settings.badge-art" />
								</span>
								<span className={styles.label} dir="auto" data-flx="user.profile-badge-settings.label">
									{badge.label}
								</span>
							</label>
							<Switch
								id={switchId}
								value={shown}
								onChange={(value) => onToggleHidden(badge.key, !value)}
								disabled={disabled || numberHasNoEffect}
								ariaLabel={i18n._(SHOW_BADGE_DESCRIPTOR, {badgeLabel: badge.label})}
								compact
								data-flx="user.profile-badge-settings.switch"
							/>
						</div>
						{numberHasNoEffect && (
							<p className={styles.note} data-flx="user.profile-badge-settings.note">
								{i18n._(VISIONARY_NUMBER_NEEDS_VISIONARY_DESCRIPTOR)}
							</p>
						)}
						{badgeVariants.length > 0 && (
							<div className={styles.variants} data-flx="user.profile-badge-settings.variants">
								{badgeVariants.map((variant) => (
									<Switch
										key={variant.field}
										label={i18n._(variant.label, {
											premiumProductName: badgeProductName,
											operatorPassName: OPERATOR_PASS_NAME,
										})}
										description={
											variant.description
												? i18n._(variant.description, {premiumProductName: badgeProductName})
												: undefined
										}
										value={variants[variant.field]}
										onChange={(value) => onToggleVariant(variant.field, value)}
										disabled={disabled}
										compact
										data-flx="user.profile-badge-settings.variant-switch"
									/>
								))}
							</div>
						)}
					</li>
				);
			})}
		</ul>
	);
}

export const ProfileBadgeSettings = observer(({preview, ...listProps}: ProfileBadgeSettingsProps) => {
	const {i18n} = useLingui();
	const ownBadges = preview.value?.own_badges;
	let body: ReactNode;
	if (ownBadges === undefined && preview.status === 'failed') {
		body = (
			<div className={styles.status} data-flx="user.profile-badge-settings.failed">
				<p className={styles.note} data-flx="user.profile-badge-settings.failed-text">
					{i18n._(BADGES_LOAD_FAILED_DESCRIPTOR)}
				</p>
				<Button variant="secondary" small onClick={preview.retry} data-flx="user.profile-badge-settings.retry">
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
			</div>
		);
	} else if (ownBadges === undefined) {
		body = <Spinner data-flx="user.profile-badge-settings.spinner" />;
	} else if (ownBadges.length === 0) {
		body = (
			<p className={styles.note} data-flx="user.profile-badge-settings.empty">
				{i18n._(NO_BADGES_DESCRIPTOR)}
			</p>
		);
	} else {
		body = <BadgeList ownBadges={ownBadges} {...listProps} data-flx="user.profile-badge-settings.badge-list" />;
	}
	return (
		<SettingsSection
			id={PROFILE_BADGES_SECTION_ID}
			title={i18n._(PROFILE_BADGES_DESCRIPTOR)}
			description={i18n._(BADGES_DESCRIPTION_DESCRIPTOR)}
			data-flx="user.profile-badge-settings.section"
		>
			{body}
		</SettingsSection>
	);
});

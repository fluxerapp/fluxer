// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {PREMIUM_PRODUCT_FULL_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {cdnUrl} from '@app/features/messaging/utils/MessagingUrlUtils';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {handleExternalLinkClick} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/user/components/popouts/UserProfileBadges.module.css';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {PublicUserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const BADGE_ASSET_VERSION = '2';

const badgeAssetUrl = (fileName: string) => cdnUrl(`badges/${fileName}?v=${BADGE_ASSET_VERSION}`);

const STAFF_DESCRIPTOR = msg({
	message: '{productName} Staff',
	comment:
		'Short badge title in the user profile badges popout. Preserve {productName}; it is inserted by code. English locales use Title Case for official badge titles; other locales should use natural local capitalization.',
});
const COMMUNITY_TEAM_DESCRIPTOR = msg({
	message: '{productName} Community Team',
	comment:
		'Short badge title in the user profile badges popout. Preserve {productName}; it is inserted by code. English locales use Title Case for official badge titles; other locales should use natural local capitalization.',
});
const PARTNER_DESCRIPTOR = msg({
	message: '{productName} Partner',
	comment:
		'Short badge title in the user profile badges popout. Preserve {productName}; it is inserted by code. English locales use Title Case for official badge titles; other locales should use natural local capitalization.',
});
const BUG_HUNTER_DESCRIPTOR = msg({
	message: '{productName} Bug Hunter',
	comment:
		'Short badge title in the user profile badges popout. Preserve {productName}; it is inserted by code. English locales use Title Case for official badge titles; other locales should use natural local capitalization.',
});
const VISIONARY_SINCE_DESCRIPTOR = msg({
	message: '{productName} Visionary since {premiumSinceFormatted}',
	comment:
		'Badge title with a date in the user profile badges popout. Preserve {productName}, {premiumSinceFormatted}; they are inserted by code. English locales use Title Case for the badge title part; other locales should use natural local capitalization.',
});
const VISIONARY_DESCRIPTOR = msg({
	message: '{productName} Visionary',
	comment:
		'Short badge title in the user profile badges popout. Preserve {productName}; it is inserted by code. English locales use Title Case for official badge titles; other locales should use natural local capitalization.',
});
const SUBSCRIBER_SINCE_DESCRIPTOR = msg({
	message: '{premiumProductFullName} subscriber since {premiumSinceFormatted}',
	comment:
		'Badge label with a date in the user profile badges popout. Preserve {premiumProductFullName}, {premiumSinceFormatted}; they are inserted by code. In English, keep "subscriber since" lowercase. Other locales should use natural local capitalization.',
});
const VISIONARY_ID_DESCRIPTOR = msg({
	message: 'Visionary ID {visionaryIdLabel}',
	comment:
		'Short label in the user profile badges popout. Keep it concise. Preserve {visionaryIdLabel}; it is inserted by code.',
});

interface BaseBadge {
	key: string;
	tooltip: string;
	url?: string;
}

interface IconBadge extends BaseBadge {
	type: 'icon';
	iconUrl: string;
}

interface TextBadge extends BaseBadge {
	type: 'text';
	text: string;
}

type Badge = IconBadge | TextBadge;

interface UserProfileBadgesProps {
	user: User;
	profile: Profile | null;
	isModal?: boolean;
	isMobile?: boolean;
}

export const UserProfileBadges: React.FC<UserProfileBadgesProps> = observer(
	({user, profile, isModal = false, isMobile = false}) => {
		const {i18n} = useLingui();
		const selfHosted = RuntimeConfig.isSelfHosted();
		const badges = useMemo(() => {
			const result: Array<Badge> = [];
			if (user.flags & PublicUserFlags.STAFF) {
				result.push({
					type: 'icon',
					key: 'staff',
					iconUrl: badgeAssetUrl('staff.svg'),
					tooltip: i18n._(STAFF_DESCRIPTOR, {productName: PRODUCT_NAME}),
					url: Routes.careers(),
				});
			}
			if (!selfHosted && user.flags & PublicUserFlags.CTP_MEMBER) {
				result.push({
					type: 'icon',
					key: 'ctp_member',
					iconUrl: badgeAssetUrl('ctp.svg'),
					tooltip: i18n._(COMMUNITY_TEAM_DESCRIPTOR, {productName: PRODUCT_NAME}),
				});
			}
			if (!selfHosted && user.flags & PublicUserFlags.PARTNER) {
				result.push({
					type: 'icon',
					key: 'partner',
					iconUrl: badgeAssetUrl('partner.svg'),
					tooltip: i18n._(PARTNER_DESCRIPTOR, {productName: PRODUCT_NAME}),
					url: Routes.partners(),
				});
			}
			if (!selfHosted && user.flags & PublicUserFlags.BUG_HUNTER) {
				result.push({
					type: 'icon',
					key: 'bug_hunter',
					iconUrl: badgeAssetUrl('bug-hunter.svg'),
					tooltip: i18n._(BUG_HUNTER_DESCRIPTOR, {productName: PRODUCT_NAME}),
					url: Routes.bugs(),
				});
			}
			if (!selfHosted && profile?.premiumType && profile.premiumType !== UserPremiumTypes.NONE) {
				let tooltipText = PREMIUM_PRODUCT_FULL_NAME;
				let badgeUrl = Routes.plutonium();
				if (profile.premiumType === UserPremiumTypes.LIFETIME) {
					if (profile.premiumSince) {
						const premiumSinceFormatted = DateUtils.getFormattedShortDate(profile.premiumSince);
						tooltipText = i18n._(VISIONARY_SINCE_DESCRIPTOR, {productName: PRODUCT_NAME, premiumSinceFormatted});
					} else {
						tooltipText = i18n._(VISIONARY_DESCRIPTOR, {productName: PRODUCT_NAME});
					}
					badgeUrl = Routes.helpArticle('visionary');
				} else if (profile.premiumSince) {
					const premiumSinceFormatted = DateUtils.getFormattedShortDate(profile.premiumSince);
					tooltipText = i18n._(SUBSCRIBER_SINCE_DESCRIPTOR, {
						premiumProductFullName: PREMIUM_PRODUCT_FULL_NAME,
						premiumSinceFormatted,
					});
				}
				result.push({
					type: 'icon',
					key: 'premium',
					iconUrl: badgeAssetUrl('plutonium.svg'),
					tooltip: tooltipText,
					url: badgeUrl,
				});
				if (profile.premiumType === UserPremiumTypes.LIFETIME && profile.premiumLifetimeSequence != null) {
					const visionaryIdLabel = `#${profile.premiumLifetimeSequence}`;
					result.push({
						type: 'text',
						key: 'premium_sequence',
						text: visionaryIdLabel,
						tooltip: i18n._(VISIONARY_ID_DESCRIPTOR, {visionaryIdLabel}),
						url: badgeUrl,
					});
				}
			}
			return result;
		}, [
			selfHosted,
			user.flags,
			profile?.premiumType,
			profile?.premiumSince,
			profile?.premiumLifetimeSequence,
			i18n.locale,
		]);
		if (badges.length === 0) {
			return null;
		}
		const containerClassName = isModal
			? clsx(styles.containerModal, isMobile ? styles.containerModalMobile : styles.containerModalDesktop)
			: styles.containerPopout;
		const badgeClassName = isModal && isMobile ? styles.badgeMobile : styles.badgeDesktop;
		const isDesktopInteractions = !isMobile;
		const renderInteractiveWrapper = (url: string | undefined, children: React.ReactNode) => {
			if (url && isDesktopInteractions) {
				return (
					<a
						href={url}
						target="_blank"
						rel="noopener noreferrer"
						className={styles.link}
						onClick={(event) => handleExternalLinkClick(event, url)}
						data-flx="user.user-profile-badges.render-interactive-wrapper.link"
					>
						{children}
					</a>
				);
			}
			return (
				<div className={styles.link} data-flx="user.user-profile-badges.render-interactive-wrapper.link--2">
					{children}
				</div>
			);
		};
		return (
			<div className={containerClassName} data-flx="user.user-profile-badges.div">
				{badges.map((badge) => {
					const sequenceClassName = isModal && isMobile ? styles.sequenceBadgeMobile : styles.sequenceBadgeDesktop;
					const badgeContent =
						badge.type === 'icon' ? (
							<img
								src={badge.iconUrl}
								alt={badge.tooltip}
								className={badgeClassName}
								data-flx="user.user-profile-badges.img"
							/>
						) : (
							<span
								className={clsx(styles.sequenceBadge, sequenceClassName)}
								aria-hidden="true"
								data-flx="user.user-profile-badges.sequence-badge"
							>
								{badge.text}
							</span>
						);
					return (
						<Tooltip key={badge.key} text={badge.tooltip} maxWidth="xl" data-flx="user.user-profile-badges.tooltip">
							<FocusRing offset={-2} data-flx="user.user-profile-badges.focus-ring">
								{renderInteractiveWrapper(badge.url, badgeContent)}
							</FocusRing>
						</Tooltip>
					);
				})}
			</div>
		);
	},
);

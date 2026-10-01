// SPDX-License-Identifier: AGPL-3.0-or-later

import {BadgeIcon} from '@app/features/badge/components/BadgeIcon';
import Badges from '@app/features/badge/state/Badges';
import styles from '@app/features/guild/components/GuildBadge.module.css';
import {DISCOVERABLE_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DiscoverableBadgeIcon} from '@app/features/ui/components/icons/DiscoverableBadgeIcon';
import {PartneredBadgeIcon} from '@app/features/ui/components/icons/PartneredBadgeIcon';
import {VerifiedBadgeIcon} from '@app/features/ui/components/icons/VerifiedBadgeIcon';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {handleExternalLinkClick} from '@app/features/ui/utils/NativeUtils';
import {BadgeTypes} from '@fluxer/constants/src/BadgeConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const VERIFIED_PARTNERED_COMMUNITY_DESCRIPTOR = msg({
	message: 'Verified & partnered community',
	comment: 'Label in the community badge.',
});
const PARTNERED_COMMUNITY_DESCRIPTOR = msg({
	message: 'Partnered community',
	comment: 'Short label in the community badge. Keep it concise.',
});
const VERIFIED_COMMUNITY_DESCRIPTOR = msg({
	message: 'Verified community',
	comment: 'Short label in the community badge. Keep it concise.',
});

interface GuildBadgeProps {
	readonly features: ReadonlySet<string> | ReadonlyArray<string>;
	readonly badges?: ReadonlyArray<string>;
	readonly variant?: 'default' | 'large' | 'banner';
	readonly tooltipPosition?: 'top' | 'bottom';
	readonly showTooltip?: boolean;
	readonly forceDarkTheme?: boolean;
	readonly onLightSurface?: boolean;
}

function hasFeature(features: ReadonlySet<string> | ReadonlyArray<string>, feature: string): boolean {
	if (Array.isArray(features)) {
		return features.includes(feature);
	}
	return (features as ReadonlySet<string>).has(feature);
}

export const GuildBadge = observer(function GuildBadge({
	features,
	badges,
	variant = 'default',
	tooltipPosition = 'top',
	showTooltip = true,
	forceDarkTheme = false,
	onLightSurface = false,
}: GuildBadgeProps) {
	const {i18n} = useLingui();
	const [customBadge] = Badges.resolve(BadgeTypes.GUILD, badges);
	const isVerified = hasFeature(features, GuildFeatures.VERIFIED);
	const isPartnered = hasFeature(features, GuildFeatures.PARTNERED);
	const isDiscoverable = hasFeature(features, GuildFeatures.DISCOVERABLE);
	if (!customBadge && !isVerified && !isPartnered && !isDiscoverable) {
		return null;
	}
	const badgeSize = variant === 'large' ? 24 : 20;
	const badgeClassName =
		variant === 'banner'
			? forceDarkTheme
				? styles.badgeBannerDark
				: styles.badgeBanner
			: onLightSurface
				? styles.badgeOnLightSurface
				: styles.badge;
	const {builtinIcons} = Badges;
	const renderIcon = (override: string | null, label: string, fallback: React.JSX.Element) =>
		override ? <BadgeIcon icon={override} label={label} size={badgeSize} className={badgeClassName} /> : fallback;
	let tooltipText: string;
	let icon: React.JSX.Element;
	if (customBadge) {
		tooltipText = customBadge.tooltip;
		icon = <BadgeIcon icon={customBadge.icon} label={tooltipText} size={badgeSize} className={badgeClassName} />;
	} else if (isPartnered) {
		tooltipText = isVerified ? i18n._(VERIFIED_PARTNERED_COMMUNITY_DESCRIPTOR) : i18n._(PARTNERED_COMMUNITY_DESCRIPTOR);
		icon = renderIcon(
			builtinIcons.partnered,
			tooltipText,
			<PartneredBadgeIcon
				size={badgeSize}
				className={badgeClassName}
				data-flx="guild.guild-badge.partnered-badge-icon"
			/>,
		);
	} else if (isVerified) {
		tooltipText = i18n._(VERIFIED_COMMUNITY_DESCRIPTOR);
		icon = renderIcon(
			builtinIcons.verified,
			tooltipText,
			<VerifiedBadgeIcon
				size={badgeSize}
				className={badgeClassName}
				data-flx="guild.guild-badge.verified-badge-icon"
			/>,
		);
	} else {
		tooltipText = i18n._(DISCOVERABLE_COMMUNITY_DESCRIPTOR);
		icon = renderIcon(
			builtinIcons.discoverable,
			tooltipText,
			<DiscoverableBadgeIcon
				size={badgeSize}
				className={badgeClassName}
				data-flx="guild.guild-badge.discoverable-badge-icon"
			/>,
		);
	}
	if (!showTooltip) {
		return icon;
	}
	const url = customBadge?.url;
	return (
		<Tooltip text={tooltipText} position={tooltipPosition} data-flx="guild.guild-badge.tooltip">
			{url ? (
				<a
					href={url}
					target="_blank"
					rel="noopener noreferrer"
					className={styles.badgeWrapper}
					onClick={(event) => handleExternalLinkClick(event, url)}
					data-flx="guild.guild-badge.badge-link"
				>
					{icon}
				</a>
			) : (
				<span className={styles.badgeWrapper} data-flx="guild.guild-badge.badge-wrapper">
					{icon}
				</span>
			)}
		</Tooltip>
	);
});

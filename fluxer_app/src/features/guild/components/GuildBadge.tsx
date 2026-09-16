// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/guild/components/GuildBadge.module.css';
import {DISCOVERABLE_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DiscoverableBadgeIcon} from '@app/features/ui/components/icons/DiscoverableBadgeIcon';
import {PartneredBadgeIcon} from '@app/features/ui/components/icons/PartneredBadgeIcon';
import {VerifiedBadgeIcon} from '@app/features/ui/components/icons/VerifiedBadgeIcon';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

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

export function GuildBadge({
	features,
	variant = 'default',
	tooltipPosition = 'top',
	showTooltip = true,
	forceDarkTheme = false,
	onLightSurface = false,
}: GuildBadgeProps) {
	const {i18n} = useLingui();
	const isVerified = hasFeature(features, GuildFeatures.VERIFIED);
	const isPartnered = hasFeature(features, GuildFeatures.PARTNERED);
	const isDiscoverable = hasFeature(features, GuildFeatures.DISCOVERABLE);
	if (!isVerified && !isPartnered && !isDiscoverable) {
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
	let tooltipText: string;
	let icon: React.JSX.Element;
	if (isPartnered) {
		tooltipText = isVerified ? i18n._(VERIFIED_PARTNERED_COMMUNITY_DESCRIPTOR) : i18n._(PARTNERED_COMMUNITY_DESCRIPTOR);
		icon = (
			<PartneredBadgeIcon
				size={badgeSize}
				className={badgeClassName}
				data-flx="guild.guild-badge.partnered-badge-icon"
			/>
		);
	} else if (isVerified) {
		tooltipText = i18n._(VERIFIED_COMMUNITY_DESCRIPTOR);
		icon = (
			<VerifiedBadgeIcon size={badgeSize} className={badgeClassName} data-flx="guild.guild-badge.verified-badge-icon" />
		);
	} else {
		tooltipText = i18n._(DISCOVERABLE_COMMUNITY_DESCRIPTOR);
		icon = (
			<DiscoverableBadgeIcon
				size={badgeSize}
				className={badgeClassName}
				data-flx="guild.guild-badge.discoverable-badge-icon"
			/>
		);
	}
	if (!showTooltip) {
		return icon;
	}
	return (
		<Tooltip text={tooltipText} position={tooltipPosition} data-flx="guild.guild-badge.tooltip">
			<span className={styles.badgeWrapper} data-flx="guild.guild-badge.badge-wrapper">
				{icon}
			</span>
		</Tooltip>
	);
}

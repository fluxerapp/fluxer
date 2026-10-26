// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/guild/components/GuildBadge.module.css';
import {DISCOVERABLE_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DiscoverableBadgeIcon} from '@app/features/ui/components/icons/DiscoverableBadgeIcon';
import {PartneredBadgeIcon} from '@app/features/ui/components/icons/PartneredBadgeIcon';
import {VerifiedBadgeIcon} from '@app/features/ui/components/icons/VerifiedBadgeIcon';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {handleExternalLinkClick} from '@app/features/ui/utils/NativeUtils';
import {customBadgeImageUrl} from '@app/features/user/utils/ProfileUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {InstanceGuildBadge} from '@fluxer/instance_bootstrap/src/Types';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

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

const UNKNOWN_BADGE_REVALIDATION_COOLDOWN_MS = 5 * 60 * 1000;
const unknownBadgeRevalidatedAt = new Map<string, number>();

interface GuildBadgeProps {
	readonly features: ReadonlySet<string> | ReadonlyArray<string>;
	readonly badgeIds?: ReadonlyArray<string>;
	readonly customBadgeLimit?: number;
	readonly linkable?: boolean;
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

function assignedCustomBadges(badgeIds: ReadonlyArray<string>): ReadonlyArray<InstanceGuildBadge> {
	if (badgeIds.length === 0) return [];
	const assigned = new Set(badgeIds);
	return RuntimeConfig.guildBadges.filter((badge) => assigned.has(badge.id));
}

function revalidateUnknownBadges(badgeIds: ReadonlyArray<string>): void {
	const known = new Set(RuntimeConfig.guildBadges.map((badge) => badge.id));
	const now = Date.now();
	const unknown = badgeIds.filter(
		(id) => !known.has(id) && now - (unknownBadgeRevalidatedAt.get(id) ?? 0) >= UNKNOWN_BADGE_REVALIDATION_COOLDOWN_MS,
	);
	if (unknown.length === 0) return;
	for (const id of unknown) unknownBadgeRevalidatedAt.set(id, now);
	void RuntimeConfig.revalidateDiscovery().catch(() => undefined);
}

function CustomGuildBadge({
	badge,
	size,
	linkable,
	tooltipPosition,
	showTooltip,
}: {
	readonly badge: InstanceGuildBadge;
	readonly size: number;
	readonly linkable: boolean;
	readonly tooltipPosition: 'top' | 'bottom';
	readonly showTooltip: boolean;
}) {
	const image = (
		<img
			src={customBadgeImageUrl(badge.asset_url)}
			alt={badge.name}
			width={size}
			height={size}
			className={styles.customBadge}
			data-flx="guild.guild-badge.custom-badge-image"
		/>
	);
	const href = linkable ? badge.href : null;
	const content = href ? (
		<a
			href={href}
			target="_blank"
			rel="noopener noreferrer"
			className={styles.badgeWrapper}
			onClick={(event) => handleExternalLinkClick(event, href, {stopPropagation: true})}
			onPointerDown={(event) => event.stopPropagation()}
			onKeyDown={(event) => event.stopPropagation()}
			data-flx="guild.guild-badge.custom-badge-link"
		>
			{image}
		</a>
	) : (
		<span className={styles.badgeWrapper} data-flx="guild.guild-badge.custom-badge-wrapper">
			{image}
		</span>
	);
	if (!showTooltip) {
		return content;
	}
	const description = badge.description;
	const tooltip =
		description === null
			? badge.name
			: () => (
					<span className={styles.customBadgeTooltip} data-flx="guild.guild-badge.custom-badge-tooltip-content">
						<span className={styles.customBadgeTooltipName} data-flx="guild.guild-badge.custom-badge-tooltip-name">
							{badge.name}
						</span>
						<span data-flx="guild.guild-badge.custom-badge-tooltip-description">{description}</span>
					</span>
				);
	return (
		<Tooltip text={tooltip} position={tooltipPosition} data-flx="guild.guild-badge.custom-badge-tooltip">
			{content}
		</Tooltip>
	);
}

type BuiltInGuildBadgeProps = Required<
	Omit<GuildBadgeProps, 'badgeIds' | 'customBadgeLimit' | 'linkable' | 'variant'>
> & {
	readonly variant: NonNullable<GuildBadgeProps['variant']>;
	readonly size: number;
};

const BuiltInGuildBadge = observer(function BuiltInGuildBadge({
	features,
	variant,
	size,
	tooltipPosition,
	showTooltip,
	forceDarkTheme,
	onLightSurface,
}: BuiltInGuildBadgeProps) {
	const {i18n} = useLingui();
	const isVerified = hasFeature(features, GuildFeatures.VERIFIED);
	const isPartnered = hasFeature(features, GuildFeatures.PARTNERED);
	const isDiscoverable = hasFeature(features, GuildFeatures.DISCOVERABLE) && RuntimeConfig.discoveryAvailable;
	if (!isVerified && !isPartnered && !isDiscoverable) {
		return null;
	}
	let badgeClassName: string;
	if (variant === 'banner') {
		badgeClassName = forceDarkTheme ? styles.badgeBannerDark : styles.badgeBanner;
	} else {
		badgeClassName = onLightSurface ? styles.badgeOnLightSurface : styles.badge;
	}
	let tooltipText: string;
	let icon: React.JSX.Element;
	if (isPartnered) {
		tooltipText = isVerified ? i18n._(VERIFIED_PARTNERED_COMMUNITY_DESCRIPTOR) : i18n._(PARTNERED_COMMUNITY_DESCRIPTOR);
		icon = (
			<PartneredBadgeIcon size={size} className={badgeClassName} data-flx="guild.guild-badge.partnered-badge-icon" />
		);
	} else if (isVerified) {
		tooltipText = i18n._(VERIFIED_COMMUNITY_DESCRIPTOR);
		icon = (
			<VerifiedBadgeIcon size={size} className={badgeClassName} data-flx="guild.guild-badge.verified-badge-icon" />
		);
	} else {
		tooltipText = i18n._(DISCOVERABLE_COMMUNITY_DESCRIPTOR);
		icon = (
			<DiscoverableBadgeIcon
				size={size}
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
});

export const GuildBadge = observer(function GuildBadge({
	features,
	badgeIds = [],
	customBadgeLimit,
	linkable = false,
	variant = 'default',
	tooltipPosition = 'top',
	showTooltip = true,
	forceDarkTheme = false,
	onLightSurface = false,
}: GuildBadgeProps) {
	const size = variant === 'large' ? 24 : 20;
	const builtIn = (
		<BuiltInGuildBadge
			features={features}
			variant={variant}
			size={size}
			tooltipPosition={tooltipPosition}
			showTooltip={showTooltip}
			forceDarkTheme={forceDarkTheme}
			onLightSurface={onLightSurface}
			data-flx="guild.guild-badge.built-in-guild-badge"
		/>
	);
	useEffect(() => {
		if (badgeIds.length > 0) revalidateUnknownBadges(badgeIds);
	}, [badgeIds]);
	const customBadges = assignedCustomBadges(badgeIds).slice(0, customBadgeLimit);
	if (customBadges.length === 0) {
		return builtIn;
	}
	return (
		<span className={styles.badgeRow} data-flx="guild.guild-badge.badge-row">
			{builtIn}
			{customBadges.map((badge) => (
				<CustomGuildBadge
					key={badge.id}
					badge={badge}
					size={size}
					linkable={linkable}
					tooltipPosition={tooltipPosition}
					showTooltip={showTooltip}
					data-flx="guild.guild-badge.custom-guild-badge"
				/>
			))}
		</span>
	);
});

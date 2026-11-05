// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/guild/components/GuildBadge.module.css';
import {GuildBadgesModal} from '@app/features/guild/components/modals/GuildBadgesModal';
import GuildBadgeDefinitions from '@app/features/guild/state/GuildBadgeDefinitions';
import {DISCOVERABLE_COMMUNITY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {DiscoverableBadgeIcon} from '@app/features/ui/components/icons/DiscoverableBadgeIcon';
import {PartneredBadgeIcon} from '@app/features/ui/components/icons/PartneredBadgeIcon';
import {VerifiedBadgeIcon} from '@app/features/ui/components/icons/VerifiedBadgeIcon';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {handleExternalLinkClick} from '@app/features/ui/utils/NativeUtils';
import {customBadgeImageUrl} from '@app/features/user/utils/ProfileUtils';
import {
	MORE_BADGES_DESCRIPTOR,
	SHOW_ALL_BADGES_COUNT_DESCRIPTOR,
	SHOW_ALL_BADGES_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {BuiltInGuildBadgeKind} from '@fluxer/constants/src/BadgeConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {InstanceGuildBadge} from '@fluxer/instance_bootstrap/src/Types';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

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

const DEFAULT_VISIBLE_CUSTOM_BADGES = 3;
const CUSTOM_BADGE_SETTLE_MS = 300;

interface GuildBadgeProps {
	readonly features: ReadonlySet<string> | ReadonlyArray<string>;
	readonly badgeIds?: ReadonlyArray<string>;
	readonly visibleCustomBadges?: number;
	readonly linkable?: boolean;
	readonly expandable?: boolean;
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

function useSettledOnce(settled: boolean): boolean {
	const [shown, setShown] = useState(settled);
	useEffect(() => {
		if (shown) return;
		if (settled) {
			setShown(true);
			return;
		}
		const timer = window.setTimeout(() => setShown(true), CUSTOM_BADGE_SETTLE_MS);
		return () => window.clearTimeout(timer);
	}, [settled, shown]);
	return shown || settled;
}

function MoreGuildBadges({
	badges,
	hidden,
	interactive,
	tooltipPosition,
}: {
	readonly badges: ReadonlyArray<InstanceGuildBadge>;
	readonly hidden: number;
	readonly interactive: boolean;
	readonly tooltipPosition: 'top' | 'bottom';
}) {
	const {i18n} = useLingui();
	const label = i18n._(MORE_BADGES_DESCRIPTOR, {count: hidden});
	if (!interactive) {
		return (
			<span className={styles.morePill} data-flx="guild.guild-badge.more-badges-text">
				{label}
			</span>
		);
	}
	const open = (event: React.SyntheticEvent) => {
		event.preventDefault();
		event.stopPropagation();
		ModalCommands.push(
			ModalCommands.modal(() => <GuildBadgesModal badges={badges} data-flx="guild.guild-badge.guild-badges-modal" />),
		);
	};
	return (
		<Tooltip
			text={i18n._(SHOW_ALL_BADGES_DESCRIPTOR)}
			position={tooltipPosition}
			data-flx="guild.guild-badge.more-tooltip"
		>
			<button
				type="button"
				className={styles.morePill}
				aria-label={i18n._(SHOW_ALL_BADGES_COUNT_DESCRIPTOR, {count: hidden})}
				onClick={open}
				onPointerDown={(event) => event.stopPropagation()}
				onKeyDown={(event) => event.stopPropagation()}
				data-flx="guild.guild-badge.more-badges-button"
			>
				{label}
			</button>
		</Tooltip>
	);
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
	Omit<GuildBadgeProps, 'badgeIds' | 'visibleCustomBadges' | 'linkable' | 'expandable' | 'variant'>
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
	const hidden = RuntimeConfig.hiddenBuiltInGuildBadges;
	const isVerified = hasFeature(features, GuildFeatures.VERIFIED) && !hidden.includes('verified');
	const isPartnered = hasFeature(features, GuildFeatures.PARTNERED) && !hidden.includes('partnered');
	const isDiscoverable =
		hasFeature(features, GuildFeatures.DISCOVERABLE) &&
		RuntimeConfig.discoveryAvailable &&
		!hidden.includes('discoverable');
	if (!isVerified && !isPartnered && !isDiscoverable) {
		return null;
	}
	let badgeClassName: string;
	if (variant === 'banner') {
		badgeClassName = forceDarkTheme ? styles.badgeBannerDark : styles.badgeBanner;
	} else {
		badgeClassName = onLightSurface ? styles.badgeOnLightSurface : styles.badge;
	}
	let kind: BuiltInGuildBadgeKind = 'discoverable';
	let tooltipText: string;
	let icon: React.JSX.Element;
	if (isPartnered) {
		kind = 'partnered';
		tooltipText = isVerified ? i18n._(VERIFIED_PARTNERED_COMMUNITY_DESCRIPTOR) : i18n._(PARTNERED_COMMUNITY_DESCRIPTOR);
		icon = (
			<PartneredBadgeIcon size={size} className={badgeClassName} data-flx="guild.guild-badge.partnered-badge-icon" />
		);
	} else if (isVerified) {
		kind = 'verified';
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
	const replacement = RuntimeConfig.builtInGuildBadgeImages[kind];
	if (replacement !== null) {
		icon = (
			<img
				src={customBadgeImageUrl(replacement)}
				alt={tooltipText}
				width={size}
				height={size}
				className={styles.customBadge}
				data-flx="guild.guild-badge.replaced-badge-image"
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
	visibleCustomBadges = DEFAULT_VISIBLE_CUSTOM_BADGES,
	linkable = false,
	expandable = true,
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
	const badgeKey = badgeIds.join(',');
	useEffect(() => {
		if (badgeKey === '') return;
		return GuildBadgeDefinitions.retain(badgeKey.split(','));
	}, [badgeKey]);
	const resolved = GuildBadgeDefinitions.resolve(badgeIds);
	const ready = useSettledOnce(resolved.settled);
	const allCustomBadges = ready ? resolved.badges : [];
	const customBadges = allCustomBadges.slice(0, Math.max(0, visibleCustomBadges));
	const hidden = allCustomBadges.length - customBadges.length;
	if (allCustomBadges.length === 0) {
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
			{hidden > 0 && (
				<MoreGuildBadges
					badges={allCustomBadges}
					hidden={hidden}
					interactive={showTooltip && expandable}
					tooltipPosition={tooltipPosition}
					data-flx="guild.guild-badge.more-guild-badges"
				/>
			)}
		</span>
	);
});

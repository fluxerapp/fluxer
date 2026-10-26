// SPDX-License-Identifier: AGPL-3.0-or-later

import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {CustomBadgeDefinition} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import type {VisiblePremiumBadge} from '@app/api/user/profile_badges/PremiumBadgeMasking';
import {
	operatorPassBadgeAsset,
	operatorPassSmallBadgeAsset,
	PROFILE_BADGE_ASSETS,
} from '@app/api/user/profile_badges/ProfileBadgeAssets';
import {getVisionaryNumberTag} from '@app/api/user/profile_badges/VisionaryNumberTags';
import type {ProfileBadgeKind} from '@fluxer/constants/src/BadgeConstants';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';

export interface ProfileBadgeEnvironment {
	selfHosted: boolean;
	premiumTieringActive: boolean;
	productName: string;
	premiumName: string;
	premiumInfoUrl: string | null;
	marketingUrl: string;
	mediaUrl: string;
	customBadges: ReadonlyArray<CustomBadgeDefinition>;
}

export interface VisibleOperatorPass {
	since: Date | null;
	count: number | null;
}

export interface ProfileBadgeView {
	flags: bigint;
	premium: VisiblePremiumBadge;
	operatorPass: VisibleOperatorPass | null;
	environment: ProfileBadgeEnvironment;
	locale: string;
	shown: ReadonlySet<ProfileBadgeKind>;
}

interface ProfileBadgeContent {
	label: string;
	count?: number;
	accessibleLabel?: string;
	details?: {title: string; lines: Array<string>};
	caption?: string;
	asset?: string | null;
	smallAsset?: string | null;
	aspectRatio?: number;
}

export interface ProfileBadgeDefinition {
	kind: ProfileBadgeKind;
	asset: string | null;
	availability: 'everywhere' | 'hosted' | 'self_hosted';
	href(environment: ProfileBadgeEnvironment): string | null;
	qualify(view: ProfileBadgeView): ProfileBadgeContent | null;
}

function premiumName(environment: ProfileBadgeEnvironment): string {
	return `${environment.productName} ${environment.premiumName}`;
}

function formatSinceDate(date: Date, locale: string): string {
	return getDateFormatter(locale, {dateStyle: 'long', timeZone: 'UTC'}).format(date);
}

function premiumLabel(view: ProfileBadgeView): string {
	if (view.premium.premiumType === UserPremiumTypes.LIFETIME) {
		return getContentMessage('profile_badge.premium_lifetime', view.locale, {
			premium_name: premiumName(view.environment),
		});
	}
	const since = view.premium.premiumSince;
	if (since) {
		return getContentMessage('profile_badge.premium_since', view.locale, {
			premium_name: premiumName(view.environment),
			since_date: formatSinceDate(since, view.locale),
		});
	}
	return getContentMessage('profile_badge.premium', view.locale, {premium_name: premiumName(view.environment)});
}

function flagBadge(flag: bigint, label: (view: ProfileBadgeView) => string): ProfileBadgeDefinition['qualify'] {
	return (view) => ((view.flags & flag) !== 0n ? {label: label(view)} : null);
}

const marketing = (path: string) => (environment: ProfileBadgeEnvironment) => `${environment.marketingUrl}/${path}`;

export const PROFILE_BADGE_REGISTRY: ReadonlyArray<ProfileBadgeDefinition> = [
	{
		kind: 'staff',
		asset: PROFILE_BADGE_ASSETS.staff,
		availability: 'everywhere',
		href: (environment) => (environment.selfHosted ? null : marketing('careers')(environment)),
		qualify: flagBadge(UserFlags.STAFF, (view) =>
			getContentMessage('profile_badge.staff', view.locale, {product_name: view.environment.productName}),
		),
	},
	{
		kind: 'partner',
		asset: PROFILE_BADGE_ASSETS.partner,
		availability: 'hosted',
		href: marketing('partners'),
		qualify: flagBadge(UserFlags.PARTNER, (view) =>
			getContentMessage('profile_badge.partner', view.locale, {product_name: view.environment.productName}),
		),
	},
	{
		kind: 'bug_hunter',
		asset: PROFILE_BADGE_ASSETS.bug_hunter,
		availability: 'hosted',
		href: () => 'https://www.youtube.com/watch?v=ez8m4PXksQs',
		qualify: flagBadge(UserFlags.BUG_HUNTER, (view) =>
			getContentMessage('profile_badge.bug_hunter', view.locale, {product_name: view.environment.productName}),
		),
	},
	{
		kind: 'operator_pass',
		asset: PROFILE_BADGE_ASSETS.operator_pass,
		availability: 'hosted',
		href: marketing('pass'),
		qualify: (view) => {
			if (!view.operatorPass) return null;
			const {since, count} = view.operatorPass;
			const stacked = count != null && count >= 2;
			const sinceDate = since ? formatSinceDate(since, view.locale) : null;
			const lines: Array<string> = [];
			if (sinceDate) {
				lines.push(
					getContentMessage('profile_badge.operator_pass_since_line', view.locale, {
						since_date: sinceDate,
					}),
				);
			}
			if (stacked) {
				lines.push(getContentMessage('profile_badge.operator_pass_count', view.locale, {count}));
				lines.push(getContentMessage('profile_badge.operator_pass_recognition', view.locale));
			}
			let label: string;
			if (sinceDate && stacked) {
				label = getContentMessage('profile_badge.operator_pass_since_count', view.locale, {
					since_date: sinceDate,
					count,
				});
			} else if (sinceDate) {
				label = getContentMessage('profile_badge.operator_pass_since', view.locale, {
					since_date: sinceDate,
				});
			} else {
				label = getContentMessage('profile_badge.operator_pass', view.locale);
			}
			const title = getContentMessage('profile_badge.operator_pass_title', view.locale);
			return {
				label,
				asset: operatorPassBadgeAsset(count),
				smallAsset: operatorPassSmallBadgeAsset(count),
				...(stacked ? {count} : {}),
				accessibleLabel: stacked
					? getContentMessage('profile_badge.operator_pass_accessible_count', view.locale, {count})
					: title,
				details: {title, lines},
			};
		},
	},
	{
		kind: 'visionary',
		asset: PROFILE_BADGE_ASSETS.visionary,
		availability: 'hosted',
		href: marketing('help/visionary'),
		qualify: (view) => {
			if (view.premium.premiumType !== UserPremiumTypes.LIFETIME) return null;
			const since = view.premium.premiumSince;
			const productName = view.environment.productName;
			return {
				label: since
					? getContentMessage('profile_badge.visionary_since', view.locale, {
							product_name: productName,
							since_date: formatSinceDate(since, view.locale),
						})
					: getContentMessage('profile_badge.visionary', view.locale, {product_name: productName}),
			};
		},
	},
	{
		kind: 'visionary_number',
		asset: null,
		availability: 'hosted',
		href: marketing('help/visionary'),
		qualify: (view) => {
			const sequence = view.premium.premiumLifetimeSequence;
			if (!view.shown.has('visionary') || sequence == null) return null;
			const caption = `#${String(sequence)}`;
			const label = getContentMessage('profile_badge.visionary_number', view.locale, {number: caption});
			const tag = getVisionaryNumberTag(sequence);
			if (!tag) {
				Logger.warn({sequence}, 'No Visionary number tag for this sequence');
				return {label, caption};
			}
			return {label, caption, asset: tag.asset, aspectRatio: tag.width / tag.height};
		},
	},
	{
		kind: 'plutonium',
		asset: PROFILE_BADGE_ASSETS.plutonium,
		availability: 'hosted',
		href: marketing('plutonium'),
		qualify: (view) =>
			view.premium.premiumType === UserPremiumTypes.SUBSCRIPTION ? {label: premiumLabel(view)} : null,
	},
	{
		kind: 'premium',
		asset: PROFILE_BADGE_ASSETS.premium,
		availability: 'self_hosted',
		href: (environment) => environment.premiumInfoUrl,
		qualify: (view) => {
			const premiumType = view.premium.premiumType;
			if (!view.environment.premiumTieringActive || !premiumType) return null;
			return {label: premiumLabel(view)};
		},
	},
];

// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveContentLocale} from '@app/api/content_i18n/ContentI18n';
import {customBadgeImageUrl} from '@app/api/instance/InstanceAssetReferences';
import type {User} from '@app/api/models/User';
import type {OperatorPassHolder} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	applyPremiumBadgeMasking,
	isOperatorPassCountHidden,
	isOperatorPassTimestampHidden,
} from '@app/api/user/profile_badges/PremiumBadgeMasking';
import {
	PROFILE_BADGE_REGISTRY,
	type ProfileBadgeEnvironment,
	type ProfileBadgeView,
	type VisibleOperatorPass,
} from '@app/api/user/profile_badges/ProfileBadgeRegistry';
import {customProfileBadgeKey, type ProfileBadgeKey, type ProfileBadgeKind} from '@fluxer/constants/src/BadgeConstants';
import {PremiumFlags, stripHiddenProfileBadgeFlags} from '@fluxer/constants/src/UserConstants';
import type {
	AdminProfileBadgeResponse,
	OwnProfileBadgeResponse,
	ProfileBadgeResponse,
} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';

export type ProfileBadgeSubject = Pick<
	User,
	| 'flags'
	| 'premiumFlags'
	| 'premiumType'
	| 'premiumSince'
	| 'premiumLifetimeSequence'
	| 'badgeIds'
	| 'hiddenProfileBadges'
>;

export interface ProfileBadgeInput {
	user: ProfileBadgeSubject;
	operatorPass: OperatorPassHolder | null;
	locale: string | null | undefined;
	environment: ProfileBadgeEnvironment;
	restricted?: boolean;
	profileHidden?: boolean;
	viewerIsSelf?: boolean;
}

type ViewSource = Omit<ProfileBadgeView, 'shown'> & {customBadgeIds: ReadonlySet<bigint>};

const NO_HIDDEN_BADGES: ReadonlySet<ProfileBadgeKey> = new Set();

function publicOperatorPass(
	holder: OperatorPassHolder | null,
	user: ProfileBadgeSubject,
	viewerIsSelf: boolean,
): VisibleOperatorPass | null {
	if (!holder || user.hiddenProfileBadges.has('operator_pass')) return null;
	const since = !viewerIsSelf && isOperatorPassTimestampHidden(user.premiumFlags) ? null : holder.since;
	const count = !viewerIsSelf && isOperatorPassCountHidden(user.premiumFlags) ? null : holder.count;
	return {since, count};
}

function publicViewSource(input: ProfileBadgeInput): ViewSource | null {
	if (input.profileHidden) return null;
	const {user, environment} = input;
	const hidden = user.hiddenProfileBadges;
	const restricted = input.restricted === true;
	return {
		flags: stripHiddenProfileBadgeFlags(user.flags, hidden),
		premium: applyPremiumBadgeMasking(user, {
			premiumFlags: user.premiumFlags,
			hidden,
			selfHosted: environment.selfHosted,
			restricted,
		}),
		operatorPass: restricted ? null : publicOperatorPass(input.operatorPass, user, input.viewerIsSelf === true),
		environment,
		locale: resolveContentLocale(input.locale),
		customBadgeIds: new Set([...user.badgeIds].filter((id) => !hidden.has(customProfileBadgeKey(id.toString())))),
	};
}

function heldViewSource(input: ProfileBadgeInput, premiumFlags: number): ViewSource {
	const {user, environment} = input;
	return {
		flags: user.flags,
		premium: applyPremiumBadgeMasking(user, {
			premiumFlags,
			hidden: NO_HIDDEN_BADGES,
			selfHosted: environment.selfHosted,
			restricted: false,
		}),
		operatorPass: input.operatorPass ? {since: input.operatorPass.since, count: input.operatorPass.count} : null,
		environment,
		locale: resolveContentLocale(input.locale),
		customBadgeIds: user.badgeIds,
	};
}

type KeyedProfileBadge = ProfileBadgeResponse & {key: ProfileBadgeKey};

function render(source: ViewSource | null): Array<KeyedProfileBadge> {
	if (!source) return [];
	const {selfHosted} = source.environment;
	const shown = new Set<ProfileBadgeKind>();
	const badges: Array<KeyedProfileBadge> = [];
	for (const definition of PROFILE_BADGE_REGISTRY) {
		if (definition.availability === (selfHosted ? 'hosted' : 'self_hosted')) continue;
		const content = definition.qualify({...source, shown});
		if (!content) continue;
		shown.add(definition.kind);
		badges.push({
			key: definition.kind,
			kind: definition.kind,
			label: content.label,
			asset: content.asset ?? definition.asset,
			asset_url: null,
			...(content.smallAsset ? {small_asset: content.smallAsset} : {}),
			aspect_ratio: content.aspectRatio ?? null,
			caption: content.caption ?? null,
			href: definition.href(source.environment),
			...(content.count !== undefined ? {count: content.count} : {}),
			...(content.accessibleLabel !== undefined ? {accessible_label: content.accessibleLabel} : {}),
			...(content.details !== undefined ? {details: content.details} : {}),
		});
	}
	for (const badge of source.environment.customBadges) {
		if (!source.customBadgeIds.has(BigInt(badge.id))) continue;
		badges.push({
			key: customProfileBadgeKey(badge.id),
			kind: 'custom',
			label: badge.name,
			asset: null,
			asset_url: customBadgeImageUrl(badge.asset, source.environment.mediaUrl),
			aspect_ratio: null,
			caption: null,
			href: badge.href,
			...(badge.description !== null ? {details: {title: badge.name, lines: [badge.description]}} : {}),
		});
	}
	return badges;
}

export function buildProfileBadges(input: ProfileBadgeInput): Array<ProfileBadgeResponse> {
	return render(publicViewSource(input));
}

export function buildOwnProfileBadges(input: ProfileBadgeInput): Array<OwnProfileBadgeResponse> {
	const hidden = input.user.hiddenProfileBadges;
	return render(heldViewSource(input, input.user.premiumFlags & ~PremiumFlags.BADGE_MASKED)).map((badge) => ({
		...badge,
		hidden: hidden.has(badge.key),
	}));
}

export function buildOperatorProfileBadges(input: ProfileBadgeInput): Array<AdminProfileBadgeResponse> {
	const publicKeys = new Set(render(publicViewSource({...input, restricted: false})).map((badge) => badge.key));
	return render(heldViewSource(input, 0)).map((badge) => ({...badge, suppressed: !publicKeys.has(badge.key)}));
}

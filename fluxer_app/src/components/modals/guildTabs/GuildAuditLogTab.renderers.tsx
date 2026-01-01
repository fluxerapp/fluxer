/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Plural, Trans} from '@lingui/react/macro';
import type {ReactNode} from 'react';
import type {GuildAuditLogEntry} from '~/actions/GuildActionCreators';
import {
	GuildExplicitContentFilterTypes,
	GuildMFALevel,
	GuildVerificationLevel,
	MessageNotifications,
} from '~/Constants';
import i18n from '~/i18n';
import GuildStore from '~/stores/GuildStore';
import {
	type ChangeShapeWithUnknowns,
	formatAccentColor,
	formatDateStringValue,
	getChannelTypeLabel,
	getFeatureDiff,
	getFeatureLabel,
	getOperationDiff,
	getPermissionDiff,
	getRtcRegionLabel,
	getSplashAlignmentLabel,
	getStickerFormatLabel,
	getSystemChannelFlagDiff,
	isEmptyString,
	safeScalarString,
} from '~/utils/modals/guildTabs/GuildAuditLogTabUtils';
import {formatPermissionLabel} from '~/utils/PermissionUtils';
import {ColorDot} from './GuildAuditLogTab.components';
import {AuditLogTargetType} from './GuildAuditLogTab.constants';
import {renderValueInline} from './GuildAuditLogTab.utils';

type TranslateFn = (descriptor: MessageDescriptor) => string;

export type ChangeRenderer = (
	change: ChangeShapeWithUnknowns,
	ctx: {entry: GuildAuditLogEntry; guildId: string; t: TranslateFn},
) => ReactNode | null;

const renderInline = (value: unknown, t: TranslateFn, guildId?: string): ReactNode =>
	renderValueInline(value, guildId, t);

const joinLabels = (labels: Array<string>): string => labels.join(', ');

const normalizeStringArray = (value: unknown): Array<string> =>
	Array.isArray(value) ? value.filter((item): item is string => typeof item === 'string') : [];

const getRoleNameResolver = (guildId: string): ((roleId: string) => string) => {
	const roles = GuildStore.getGuildRoles(guildId, true);
	const roleNameById = new Map(roles.map((r) => [r.id, r.name] as const));
	return (roleId) => roleNameById.get(roleId) ?? roleId;
};

const getRoleDiff = (oldValue: unknown, newValue: unknown): {added: Array<string>; removed: Array<string>} => {
	const oldRoles = normalizeStringArray(oldValue);
	const newRoles = normalizeStringArray(newValue);

	const oldSet = new Set(oldRoles);
	const newSet = new Set(newRoles);

	return {
		added: newRoles.filter((id) => !oldSet.has(id)),
		removed: oldRoles.filter((id) => !newSet.has(id)),
	};
};

const mapFeatureLabels = (features: Array<string>, t: TranslateFn): Array<string> =>
	features.map((feature) => getFeatureLabel(feature, t) ?? feature);

const formatPermissionList = (flags: Array<bigint>, i18nInstance: I18n = i18n): string =>
	flags
		.map((flag) => formatPermissionLabel(i18nInstance, flag, false))
		.filter((label): label is string => label !== null)
		.join(', ');

const whenOldValueMissing =
	(hasNoOld: ChangeRenderer, hasOld: ChangeRenderer): ChangeRenderer =>
	(change, ctx) =>
		change.oldValue == null ? hasNoOld(change, ctx) : hasOld(change, ctx);

const whenNewValueMissing =
	(hasNoNew: ChangeRenderer, hasNew: ChangeRenderer): ChangeRenderer =>
	(change, ctx) =>
		change.newValue == null ? hasNoNew(change, ctx) : hasNew(change, ctx);

const whenNewOrOldMissing =
	(
		hasBoth: ChangeRenderer,
		hasNoOld: ChangeRenderer,
		hasNoNew: ChangeRenderer,
		hasNeither: ChangeRenderer,
	): ChangeRenderer =>
	(change, ctx) => {
		if (change.newValue != null && change.oldValue != null) return hasBoth(change, ctx);
		if (change.newValue != null) return hasNoOld(change, ctx);
		if (change.oldValue != null) return hasNoNew(change, ctx);
		return hasNeither(change, ctx);
	};

const renderChannelTypeValue = (value: unknown, t: TranslateFn): ReactNode => {
	const label = getChannelTypeLabel(value, t);
	return renderInline(label ?? value, t);
};

const renderRtcRegionValue = (value: unknown, t: TranslateFn): ReactNode => {
	const label = getRtcRegionLabel(value, t);
	return renderInline(label ?? value, t);
};

const renderSplashAlignmentValue = (value: unknown, t: TranslateFn): ReactNode => {
	const label = getSplashAlignmentLabel(value, t);
	return renderInline(label ?? value, t);
};

const getNSFWLevelLabel = (value: unknown, t: TranslateFn): string | null => {
	if (typeof value !== 'number') return null;

	switch (value) {
		case 0:
			return t(msg`Default`);
		case 1:
			return t(msg`Explicit`);
		case 2:
			return t(msg`Safe`);
		case 3:
			return t(msg`Age Restricted`);
		default:
			return null;
	}
};

const getExplicitContentFilterLabel = (value: unknown, t: TranslateFn): string | null => {
	if (typeof value !== 'number') return null;

	switch (value) {
		case GuildExplicitContentFilterTypes.DISABLED:
			return t(msg`Disabled`);
		case GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES:
			return t(msg`Members without roles`);
		case GuildExplicitContentFilterTypes.ALL_MEMBERS:
			return t(msg`All members`);
		default:
			return null;
	}
};

const renderAllowOrDenyDiff = (kind: 'allow' | 'deny', change: ChangeShapeWithUnknowns, t: TranslateFn): ReactNode => {
	const {added, removed} = getPermissionDiff(change.oldValue, change.newValue);

	const addedLabels = added.length > 0 ? formatPermissionList(added) : '';
	const removedLabels = removed.length > 0 ? formatPermissionList(removed) : '';

	if (kind === 'allow') {
		if (added.length > 0 && removed.length === 0) return <Trans>Allowed {renderInline(addedLabels, t)}.</Trans>;
		if (removed.length > 0 && added.length === 0)
			return <Trans>Removed allow for {renderInline(removedLabels, t)}.</Trans>;
		if (added.length > 0 && removed.length > 0) {
			return (
				<Trans>
					Allowed {renderInline(addedLabels, t)} and removed allow for {renderInline(removedLabels, t)}.
				</Trans>
			);
		}
		return <Trans>Updated allowed permissions.</Trans>;
	}

	if (added.length > 0 && removed.length === 0) return <Trans>Denied {renderInline(addedLabels, t)}.</Trans>;
	if (removed.length > 0 && added.length === 0)
		return <Trans>Removed deny for {renderInline(removedLabels, t)}.</Trans>;
	if (added.length > 0 && removed.length > 0) {
		return (
			<Trans>
				Denied {renderInline(addedLabels, t)} and removed deny for {renderInline(removedLabels, t)}.
			</Trans>
		);
	}
	return <Trans>Updated denied permissions.</Trans>;
};

const formatMaxAge = (seconds: number): ReactNode => {
	const SECONDS_PER_MINUTE = 60;
	const SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE;
	const SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;

	if (seconds % SECONDS_PER_DAY === 0) {
		const days = seconds / SECONDS_PER_DAY;
		return <Plural value={days} one={<strong># day</strong>} other={<strong># days</strong>} />;
	}

	if (seconds % SECONDS_PER_HOUR === 0) {
		const hours = seconds / SECONDS_PER_HOUR;
		return <Plural value={hours} one={<strong># hour</strong>} other={<strong># hours</strong>} />;
	}

	if (seconds % SECONDS_PER_MINUTE === 0) {
		const minutes = seconds / SECONDS_PER_MINUTE;
		return <Plural value={minutes} one={<strong># minute</strong>} other={<strong># minutes</strong>} />;
	}

	return <Plural value={seconds} one={<strong># second</strong>} other={<strong># seconds</strong>} />;
};

const GUILD_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {t}) => <Trans>Renamed the community to {renderInline(change.newValue, t)}.</Trans>,
	icon_hash: () => <Trans>Updated the community icon.</Trans>,
	splash_hash: () => <Trans>Updated the invite splash.</Trans>,
	owner_id: (change, {guildId, t}) => (
		<Trans>Transferred ownership to {renderInline(change.newValue, t, guildId)}.</Trans>
	),
	region: (change, {t}) => <Trans>Changed the voice region to {renderRtcRegionValue(change.newValue, t)}.</Trans>,
	afk_channel_id: (change, {guildId, t}) => (
		<Trans>Set the AFK channel to {renderInline(change.newValue, t, guildId)}.</Trans>
	),
	afk_timeout: (change, {t}) => <Trans>Set the AFK timeout to {renderInline(change.newValue, t)} minutes.</Trans>,
	splash_card_alignment: (change, {t}) => (
		<Trans>Set the invite splash alignment to {renderSplashAlignmentValue(change.newValue, t)}.</Trans>
	),
	mfa_level: (change) =>
		change.newValue === GuildMFALevel.ELEVATED ? (
			<Trans>
				<strong>Enabled</strong> two-factor authentication requirement.
			</Trans>
		) : (
			<Trans>
				<strong>Disabled</strong> two-factor authentication requirement.
			</Trans>
		),
	verification_level: (change) => {
		switch (change.newValue) {
			case GuildVerificationLevel.NONE:
				return (
					<Trans>
						Set verification level to <strong>None</strong>.
					</Trans>
				);
			case GuildVerificationLevel.LOW:
				return (
					<Trans>
						Set verification level to <strong>Low</strong>.
					</Trans>
				);
			case GuildVerificationLevel.MEDIUM:
				return (
					<Trans>
						Set verification level to <strong>Medium</strong>.
					</Trans>
				);
			case GuildVerificationLevel.HIGH:
				return (
					<Trans>
						Set verification level to <strong>High</strong>.
					</Trans>
				);
			default:
				return null;
		}
	},
	default_message_notifications: (change) => {
		if (change.newValue === MessageNotifications.ALL_MESSAGES) {
			return (
				<Trans>
					Set default notifications to <strong>All Messages</strong>.
				</Trans>
			);
		}
		if (change.newValue === MessageNotifications.ONLY_MENTIONS) {
			return (
				<Trans>
					Set default notifications to <strong>Only Mentions</strong>.
				</Trans>
			);
		}
		return null;
	},
	vanity_url_code: whenNewValueMissing(
		() => (
			<Trans>
				<strong>Removed</strong> the vanity URL.
			</Trans>
		),
		(change, {t}) => <Trans>Set the vanity URL to {renderInline(change.newValue, t)}.</Trans>,
	),
	features: (change, {t}) => {
		const {added, removed} = getFeatureDiff(change.oldValue, change.newValue);

		const addedLabels = mapFeatureLabels(added, t);
		const removedLabels = mapFeatureLabels(removed, t);

		if (addedLabels.length > 0 && removedLabels.length === 0) {
			return <Trans>Enabled features: {renderInline(joinLabels(addedLabels), t)}.</Trans>;
		}
		if (removedLabels.length > 0 && addedLabels.length === 0) {
			return <Trans>Disabled features: {renderInline(joinLabels(removedLabels), t)}.</Trans>;
		}

		if (addedLabels.length === 0 && removedLabels.length === 0) return null;

		return (
			<>
				{addedLabels.length > 0 ? <Trans>Enabled {renderInline(joinLabels(addedLabels), t)}.</Trans> : null}
				{removedLabels.length > 0 ? <Trans>Disabled {renderInline(joinLabels(removedLabels), t)}.</Trans> : null}
			</>
		);
	},
	banner_hash: () => <Trans>Updated the community banner.</Trans>,
	nsfw_level: (change, {t}) => {
		const label = getNSFWLevelLabel(change.newValue, t);
		return label ? <Trans>Set NSFW level to {renderInline(label, t)}.</Trans> : null;
	},
	explicit_content_filter: (change, {t}) => {
		const label = getExplicitContentFilterLabel(change.newValue, t);
		return label ? <Trans>Set explicit content filter to {renderInline(label, t)}.</Trans> : null;
	},
	system_channel_id: whenNewValueMissing(
		() => <Trans>Removed the system channel.</Trans>,
		(change, {guildId, t}) => <Trans>Set the system channel to {renderInline(change.newValue, t, guildId)}.</Trans>,
	),
	system_channel_flags: (change, {t}) => {
		const {added, removed} = getSystemChannelFlagDiff(change.oldValue, change.newValue, t);

		if (added.length > 0) {
			return <Trans>Enabled {renderInline(joinLabels(added), t)} for the system channel.</Trans>;
		}
		if (removed.length > 0) {
			return <Trans>Disabled {renderInline(joinLabels(removed), t)} for the system channel.</Trans>;
		}
		return null;
	},
	rules_channel_id: whenNewValueMissing(
		() => <Trans>Removed the rules channel.</Trans>,
		(change, {guildId, t}) => <Trans>Set the rules channel to {renderInline(change.newValue, t, guildId)}.</Trans>,
	),
	disabled_operations: (change, {t}) => {
		const {added, removed} = getOperationDiff(change.oldValue, change.newValue, t);

		const nodes: Array<ReactNode> = [];
		if (added.length > 0) nodes.push(<Trans key="disabled">Disabled {renderInline(joinLabels(added), t)}.</Trans>);
		if (removed.length > 0)
			nodes.push(<Trans key="reenabled">Re-enabled {renderInline(joinLabels(removed), t)}.</Trans>);

		if (nodes.length === 0) return null;
		if (nodes.length === 1) return nodes[0];
		return <>{nodes}</>;
	},
	embed_splash_hash: () => <Trans>Updated the embed splash.</Trans>,
};

const CHANNEL_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	channel_id: (change, {guildId, t}) => (
		<Trans>Set the channel ID to {renderInline(change.newValue, t, guildId)}.</Trans>
	),
	type: (change, {t}) => <Trans>Set the channel type to {renderChannelTypeValue(change.newValue, t)}.</Trans>,
	name: (change, {t}) => <Trans>Renamed the channel to {renderInline(change.newValue, t)}.</Trans>,
	topic: whenNewValueMissing(
		() => <Trans>Cleared the topic.</Trans>,
		(change, {t}) =>
			isEmptyString(change.newValue) ? (
				<Trans>Cleared the topic.</Trans>
			) : (
				<Trans>Updated the topic to {renderInline(change.newValue, t)}.</Trans>
			),
	),
	parent_id: whenNewValueMissing(
		() => <Trans>Removed the channel from its category.</Trans>,
		(change, {guildId, t}) => <Trans>Moved the channel to category {renderInline(change.newValue, t, guildId)}.</Trans>,
	),
	position: (change, {t}) => <Trans>Set the channel position to {renderInline(change.newValue, t)}.</Trans>,
	nsfw: (change) => (change.newValue === true ? <Trans>Enabled NSFW.</Trans> : <Trans>Disabled NSFW.</Trans>),
	rate_limit_per_user: (change, {t}) => {
		const raw = safeScalarString(change.newValue, t);
		const seconds = raw != null ? Number(raw) : 0;
		if (!seconds || Number.isNaN(seconds)) return <Trans>Disabled slowmode.</Trans>;
		return <Trans>Set slowmode to {renderInline(seconds, t)} seconds.</Trans>;
	},
	user_limit: (change, {t}) => {
		const raw = safeScalarString(change.newValue, t);
		const limit = raw != null ? Number(raw) : 0;
		if (!limit || Number.isNaN(limit)) return <Trans>Removed the user limit.</Trans>;
		return <Trans>Set the user limit to {renderInline(limit, t)}.</Trans>;
	},
	bitrate: whenOldValueMissing(
		(change, {t}) => <Trans>Set the bitrate to {renderInline(change.newValue, t)}.</Trans>,
		(change, {t}) => <Trans>Changed the bitrate to {renderInline(change.newValue, t)}.</Trans>,
	),
	rtc_region: (change, {t}) => <Trans>Set the RTC region to {renderRtcRegionValue(change.newValue, t)}.</Trans>,
	permission_overwrite_count: (change, {t}) => (
		<Trans>Set permission overwrite count to {renderInline(change.newValue, t)}.</Trans>
	),
	allow: (change, {t}) => renderAllowOrDenyDiff('allow', change, t),
	deny: (change, {t}) => renderAllowOrDenyDiff('deny', change, t),
};

const USER_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	nick: whenNewOrOldMissing(
		(change, {t}) => (
			<Trans>
				Changed nickname from {renderInline(change.oldValue, t)} to {renderInline(change.newValue, t)}.
			</Trans>
		),
		(change, {t}) => <Trans>Set nickname to {renderInline(change.newValue, t)}.</Trans>,
		(change, {t}) => (
			<Trans>
				<strong>Removed</strong> nickname {renderInline(change.oldValue, t)}.
			</Trans>
		),
		() => null,
	),
	deaf: (change) =>
		change.newValue === true ? (
			<Trans>
				<strong>Deafened</strong> the member.
			</Trans>
		) : (
			<Trans>
				<strong>Undeafened</strong> the member.
			</Trans>
		),
	mute: (change) =>
		change.newValue === true ? (
			<Trans>
				<strong>Muted</strong> the member.
			</Trans>
		) : (
			<Trans>
				<strong>Unmuted</strong> the member.
			</Trans>
		),
	roles: (change, {guildId, t}) => {
		const {added, removed} = getRoleDiff(change.oldValue, change.newValue);
		const resolveRoleName = getRoleNameResolver(guildId);

		const addedLabels = added.map(resolveRoleName);
		const removedLabels = removed.map(resolveRoleName);

		if (addedLabels.length > 0 && removedLabels.length === 0) {
			return <Trans>Added {renderInline(joinLabels(addedLabels), t)}.</Trans>;
		}
		if (removedLabels.length > 0 && addedLabels.length === 0) {
			return <Trans>Removed {renderInline(joinLabels(removedLabels), t)}.</Trans>;
		}
		if (addedLabels.length > 0 && removedLabels.length > 0) {
			return (
				<Trans>
					Added {renderInline(joinLabels(addedLabels), t)} and removed {renderInline(joinLabels(removedLabels), t)}.
				</Trans>
			);
		}
		return null;
	},
	$remove: (change, {guildId, t}) => {
		const resolveRoleName = getRoleNameResolver(guildId);
		const roleIds = normalizeStringArray(change.oldValue);
		const labels = roleIds.map(resolveRoleName);
		return labels.length > 0 ? (
			<Trans>Removed {renderInline(joinLabels(labels), t)}.</Trans>
		) : (
			<Trans>Removed roles.</Trans>
		);
	},
	$add: (change, {guildId, t}) => {
		const resolveRoleName = getRoleNameResolver(guildId);
		const roleIds = normalizeStringArray(change.newValue);
		const labels = roleIds.map(resolveRoleName);
		return labels.length > 0 ? (
			<Trans>Added {renderInline(joinLabels(labels), t)}.</Trans>
		) : (
			<Trans>Added roles.</Trans>
		);
	},
	avatar_hash: () => <Trans>Updated the avatar.</Trans>,
	banner_hash: () => <Trans>Updated the banner.</Trans>,
	reason: (change, {t}) => <Trans>Set reason to {renderInline(change.newValue, t)}.</Trans>,
	prune_delete_days: (change, {t}) => {
		const rawDays = safeScalarString(change.newValue, t);
		const parsed = rawDays != null ? Number(rawDays) : 0;
		const dayCount = Number.isFinite(parsed) ? parsed : 0;

		return (
			<Trans>
				Pruned members inactive for{' '}
				<Plural value={dayCount} one={<strong># day</strong>} other={<strong># days</strong>} />.
			</Trans>
		);
	},
	bio: whenNewValueMissing(
		() => <Trans>Cleared the bio.</Trans>,
		(change, {t}) =>
			isEmptyString(change.newValue) ? (
				<Trans>Cleared the bio.</Trans>
			) : (
				<Trans>Updated the bio to {renderInline(change.newValue, t)}.</Trans>
			),
	),
	pronouns: whenNewValueMissing(
		() => <Trans>Cleared the pronouns.</Trans>,
		(change, {t}) => <Trans>Updated the pronouns to {renderInline(change.newValue, t)}.</Trans>,
	),
	accent_color: (change, {t}) => {
		const color = formatAccentColor(change.newValue);
		return color ? (
			<Trans>
				Set accent color to {renderInline(color, t)} <ColorDot color={color} />.
			</Trans>
		) : null;
	},
	communication_disabled_until: (change, {t}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Timed out until {renderInline(formatted, t)}.</Trans> : null;
	},
	temporary: (change) =>
		change.newValue === true ? (
			<Trans>Marked the member as temporary.</Trans>
		) : (
			<Trans>Marked the member as permanent.</Trans>
		),
	banned_at: (change, {t}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Banned at {renderInline(formatted, t)}.</Trans> : null;
	},
	expires_at: (change, {t}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Ban expires at {renderInline(formatted, t)}.</Trans> : null;
	},
};

const ROLE_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {t}) => {
		const oldLabel = change.oldValue != null ? renderInline(change.oldValue, t) : null;
		const newLabel = change.newValue != null ? renderInline(change.newValue, t) : null;

		if (oldLabel && newLabel) {
			return (
				<Trans>
					Renamed from {oldLabel} to {newLabel}.
				</Trans>
			);
		}

		if (newLabel) {
			return <Trans>Created with name {newLabel}.</Trans>;
		}

		if (oldLabel) {
			return <Trans>Removed role {oldLabel}.</Trans>;
		}

		return null;
	},
	permissions: (change, {t}) => {
		const {added, removed} = getPermissionDiff(change.oldValue, change.newValue);

		const addedLabels = added.length > 0 ? formatPermissionList(added) : '';
		const removedLabels = removed.length > 0 ? formatPermissionList(removed) : '';

		if (added.length > 0 && removed.length === 0) return <Trans>Granted {renderInline(addedLabels, t)}.</Trans>;
		if (removed.length > 0 && added.length === 0) return <Trans>Revoked {renderInline(removedLabels, t)}.</Trans>;
		if (added.length > 0 && removed.length > 0) {
			return (
				<Trans>
					Granted {renderInline(addedLabels, t)} and revoked {renderInline(removedLabels, t)}.
				</Trans>
			);
		}
		return null;
	},
	position: (change, {t}) => {
		const value = change.newValue ?? change.oldValue;
		if (value == null) return null;
		return <Trans>Moved the role to position {renderInline(value, t)}.</Trans>;
	},
	allow: (change, {t}) => renderAllowOrDenyDiff('allow', change, t),
	deny: (change, {t}) => renderAllowOrDenyDiff('deny', change, t),
	color: (change, {t}) => {
		if (change.newValue === '#000000') return <Trans>Cleared the role color.</Trans>;

		const color = safeScalarString(change.newValue, t);
		return color ? (
			<Trans>
				Set role color to {renderInline(color, t)} <ColorDot color={color} />.
			</Trans>
		) : null;
	},
	hoist: (change) =>
		change.newValue === true ? (
			<Trans>Display role members separately.</Trans>
		) : (
			<Trans>Don't display separately.</Trans>
		),
	mentionable: (change) =>
		change.newValue === true ? <Trans>Allow @mention.</Trans> : <Trans>Disallow @mention.</Trans>,
	icon_hash: () => <Trans>Updated the role icon.</Trans>,
	unicode_emoji: whenNewValueMissing(
		() => <Trans>Removed the unicode emoji.</Trans>,
		(change, {t}) => <Trans>Set the unicode emoji to {renderInline(change.newValue, t)}.</Trans>,
	),
};

const INVITE_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	code: (change, {t}) => <Trans>Invite code is {renderInline(change.newValue, t)}.</Trans>,
	max_uses: (change, {t}) =>
		change.newValue === 0 ? (
			<Trans>This invite has unlimited uses.</Trans>
		) : (
			<Trans>This invite expires after {renderInline(change.newValue, t)} uses.</Trans>
		),
	max_age: (change) => {
		if (change.newValue === 0) return <Trans>This invite never expires.</Trans>;
		if (typeof change.newValue !== 'number') return null;
		return <Trans>This invite expires in {formatMaxAge(change.newValue)}.</Trans>;
	},
	uses: (change, {t}) => <Trans>Used {renderInline(change.newValue, t)} times.</Trans>,
	temporary: (change) =>
		change.newValue === true ? (
			<Trans>
				<strong>Grants</strong> temporary membership.
			</Trans>
		) : (
			<Trans>
				<strong>Grants</strong> permanent membership.
			</Trans>
		),
	created_at: (change, {t}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Created on {renderInline(formatted, t)}.</Trans> : null;
	},
};

const EMOJI_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {t}) => <Trans>Renamed emoji to {renderInline(change.newValue, t)}.</Trans>,
	animated: (change) =>
		change.newValue === true ? <Trans>Marked emoji as animated.</Trans> : <Trans>Marked emoji as static.</Trans>,
};

const STICKER_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {t}) => <Trans>Renamed sticker to {renderInline(change.newValue, t)}.</Trans>,
	description: whenNewValueMissing(
		() => <Trans>Cleared the sticker description.</Trans>,
		(change, {t}) => <Trans>Updated the sticker description to {renderInline(change.newValue, t)}.</Trans>,
	),
	format_type: (change, {t}) => {
		const label = getStickerFormatLabel(change.newValue, t);
		return label ? <Trans>Set the sticker format to {renderInline(label, t)}.</Trans> : null;
	},
};

const WEBHOOK_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	channel_id: whenOldValueMissing(
		(change, {guildId, t}) => <Trans>Created for channel {renderInline(change.newValue, t, guildId)}.</Trans>,
		(change, {guildId, t}) => <Trans>Moved to channel {renderInline(change.newValue, t, guildId)}.</Trans>,
	),
	name: whenOldValueMissing(
		(change, {t}) => <Trans>Created with name {renderInline(change.newValue, t)}.</Trans>,
		(change, {t}) => (
			<Trans>
				Renamed from {renderInline(change.oldValue, t)} to {renderInline(change.newValue, t)}.
			</Trans>
		),
	),
	avatar_hash: () => <Trans>Updated the webhook avatar.</Trans>,
	type: (change, {t}) => <Trans>Set webhook type to {renderInline(change.newValue, t)}.</Trans>,
};

export const renderSubChanges = (
	_targetType: AuditLogTargetType,
	_entry: GuildAuditLogEntry,
	_change: ChangeShapeWithUnknowns,
	_guildId: string,
): ReactNode => {
	return null;
};

const RENDERERS_BY_TARGET: Partial<Record<AuditLogTargetType, Record<string, ChangeRenderer>>> = {
	[AuditLogTargetType.GUILD]: GUILD_CHANGE_RENDERERS,
	[AuditLogTargetType.CHANNEL]: CHANNEL_CHANGE_RENDERERS,
	[AuditLogTargetType.USER]: USER_CHANGE_RENDERERS,
	[AuditLogTargetType.ROLE]: ROLE_CHANGE_RENDERERS,
	[AuditLogTargetType.INVITE]: INVITE_CHANGE_RENDERERS,
	[AuditLogTargetType.WEBHOOK]: WEBHOOK_CHANGE_RENDERERS,
	[AuditLogTargetType.EMOJI]: EMOJI_CHANGE_RENDERERS,
	[AuditLogTargetType.STICKER]: STICKER_CHANGE_RENDERERS,
};

export const getRendererTableForTarget = (targetType: AuditLogTargetType): Record<string, ChangeRenderer> =>
	RENDERERS_BY_TARGET[targetType] ?? {};

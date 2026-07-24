// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {ColorDot} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabComponents';
import {AuditLogTargetType} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabConstants';
import {renderValueInline} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabUtils';
import Guilds from '@app/features/guild/state/Guilds';
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
	getSystemChannelFlagDiff,
	isEmptyString,
	safeScalarString,
} from '@app/features/guild/utils/guild_tabs/GuildAuditLogTabUtils';
import {
	DAYS_DURATION_PLURAL_DESCRIPTOR,
	HOURS_DURATION_PLURAL_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {
	GuildExplicitContentFilterTypes,
	GuildMFALevel,
	GuildVerificationLevel,
} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Plural, Trans} from '@lingui/react/macro';
import type {ReactNode} from 'react';

const SAFE_DESCRIPTOR = msg({
	message: 'Safe',
	comment: 'Short label in the guild audit log tab.renderers. Keep it concise.',
});
const MATURE_DESCRIPTOR = msg({
	message: 'Mature',
	comment: 'Short label in the guild audit log tab.renderers. Keep it concise.',
});
const DISABLED_DESCRIPTOR = msg({
	message: 'Disabled',
	comment: 'Button or menu action label in the guild audit log tab.renderers. Keep it concise.',
});
const MEMBERS_WITHOUT_ROLES_DESCRIPTOR = msg({
	message: 'Members without roles',
	comment: 'Short label in the guild audit log tab.renderers. Keep it concise.',
});
const ALL_MEMBERS_DESCRIPTOR = msg({
	message: 'All members',
	comment: 'Short label in the guild audit log tab.renderers. Keep it concise.',
});

export type ChangeRenderer = (
	change: ChangeShapeWithUnknowns,
	ctx: {entry: GuildAuditLogEntryResponse; guildId: string; i18n: I18n},
) => ReactNode | null;

const renderInline = (value: unknown, i18nInstance: I18n, guildId?: string, isIdField?: boolean): ReactNode =>
	renderValueInline(value, guildId, i18nInstance, isIdField);
const joinLabels = (labels: Array<string>): string => labels.join(', ');
const normalizeStringArray = (value: unknown): Array<string> =>
	Array.isArray(value) ? value.filter((item): item is string => typeof item === 'string') : [];
const getRoleNameResolver = (guildId: string): ((roleId: string) => string) => {
	const roles = Guilds.getGuildRoles(guildId, true);
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
const mapFeatureLabels = (features: Array<string>, i18nInstance: I18n): Array<string> =>
	features.map((feature) => getFeatureLabel(feature, i18nInstance) ?? feature);
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
const renderChannelTypeValue = (value: unknown, i18nInstance: I18n): ReactNode => {
	const label = getChannelTypeLabel(value, i18nInstance);
	return renderInline(label ?? value, i18nInstance);
};
const renderRtcRegionValue = (value: unknown, i18nInstance: I18n): ReactNode => {
	const label = getRtcRegionLabel(value, i18nInstance);
	return renderInline(label ?? value, i18nInstance);
};
const renderSplashAlignmentValue = (value: unknown, i18nInstance: I18n): ReactNode => {
	const label = getSplashAlignmentLabel(value, i18nInstance);
	return renderInline(label ?? value, i18nInstance);
};
const getMatureContentLevelLabel = (value: unknown, i18nInstance: I18n): string | null => {
	if (typeof value !== 'number') return null;
	switch (value) {
		case 0:
		case 2:
			return i18nInstance._(SAFE_DESCRIPTOR);
		case 1:
		case 3:
			return i18nInstance._(MATURE_DESCRIPTOR);
		default:
			return null;
	}
};
const getExplicitContentFilterLabel = (value: unknown, i18nInstance: I18n): string | null => {
	if (typeof value !== 'number') return null;
	switch (value) {
		case GuildExplicitContentFilterTypes.DISABLED:
			return i18nInstance._(DISABLED_DESCRIPTOR);
		case GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES:
			return i18nInstance._(MEMBERS_WITHOUT_ROLES_DESCRIPTOR);
		case GuildExplicitContentFilterTypes.ALL_MEMBERS:
			return i18nInstance._(ALL_MEMBERS_DESCRIPTOR);
		default:
			return null;
	}
};
const renderAllowOrDenyDiff = (
	kind: 'allow' | 'deny',
	change: ChangeShapeWithUnknowns,
	i18nInstance: I18n,
): ReactNode => {
	const {added, removed} = getPermissionDiff(change.oldValue, change.newValue);
	const addedLabels = added.length > 0 ? formatPermissionList(added) : '';
	const removedLabels = removed.length > 0 ? formatPermissionList(removed) : '';
	if (kind === 'allow') {
		if (added.length > 0 && removed.length === 0)
			return <Trans>Allowed {renderInline(addedLabels, i18nInstance)}.</Trans>;
		if (removed.length > 0 && added.length === 0)
			return <Trans>Removed allow for {renderInline(removedLabels, i18nInstance)}.</Trans>;
		if (added.length > 0 && removed.length > 0) {
			return (
				<Trans>
					Allowed {renderInline(addedLabels, i18nInstance)} and removed allow for{' '}
					{renderInline(removedLabels, i18nInstance)}.
				</Trans>
			);
		}
		return <Trans>Updated allowed permissions.</Trans>;
	}
	if (added.length > 0 && removed.length === 0) return <Trans>Denied {renderInline(addedLabels, i18nInstance)}.</Trans>;
	if (removed.length > 0 && added.length === 0)
		return <Trans>Removed deny for {renderInline(removedLabels, i18nInstance)}.</Trans>;
	if (added.length > 0 && removed.length > 0) {
		return (
			<Trans>
				Denied {renderInline(addedLabels, i18nInstance)} and removed deny for{' '}
				{renderInline(removedLabels, i18nInstance)}.
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
		return (
			<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.format-max-age.strong">
				{i18n._(DAYS_DURATION_PLURAL_DESCRIPTOR, {days})}
			</strong>
		);
	}
	if (seconds % SECONDS_PER_HOUR === 0) {
		const hours = seconds / SECONDS_PER_HOUR;
		return (
			<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.format-max-age.strong--2">
				{i18n._(HOURS_DURATION_PLURAL_DESCRIPTOR, {hours})}
			</strong>
		);
	}
	if (seconds % SECONDS_PER_MINUTE === 0) {
		const minutes = seconds / SECONDS_PER_MINUTE;
		return (
			<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.format-max-age.strong--3">
				{i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes})}
			</strong>
		);
	}
	return (
		<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.format-max-age.strong--4">
			{i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds})}
		</strong>
	);
};
const GUILD_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {i18n}) => <Trans>Renamed the community to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	icon_hash: () => <Trans>Updated the community icon.</Trans>,
	splash_hash: () => <Trans>Updated the invite splash.</Trans>,
	owner_id: (change, {guildId, i18n}) => (
		<Trans>Transferred ownership to {renderInline(change.newValue, i18n, guildId, true)}.</Trans>
	),
	region: (change, {i18n}) => <Trans>Changed the voice region to {renderRtcRegionValue(change.newValue, i18n)}.</Trans>,
	afk_channel_id: (change, {guildId, i18n}) => (
		<Trans>Set the AFK channel to {renderInline(change.newValue, i18n, guildId, true)}.</Trans>
	),
	afk_timeout: (change, {i18n}) => {
		const raw = safeScalarString(change.newValue, i18n);
		const minutes = raw != null ? Number(raw) : Number.NaN;
		if (Number.isNaN(minutes))
			return <Trans>Set the AFK timeout to {renderInline(change.newValue, i18n)} minutes.</Trans>;
		return (
			<Plural
				value={minutes}
				one="Set the AFK timeout to # minute."
				other="Set the AFK timeout to # minutes."
				data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.afk-timeout.plural"
			/>
		);
	},
	splash_card_alignment: (change, {i18n}) => (
		<Trans>Set the invite splash alignment to {renderSplashAlignmentValue(change.newValue, i18n)}.</Trans>
	),
	mfa_level: (change) =>
		change.newValue === GuildMFALevel.ELEVATED ? (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.mfa-level.strong">Enabled</strong> two-factor
				authentication requirement.
			</Trans>
		) : (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.mfa-level.strong--2">Disabled</strong>{' '}
				two-factor authentication requirement.
			</Trans>
		),
	verification_level: (change) => {
		switch (change.newValue) {
			case GuildVerificationLevel.NONE:
				return (
					<Trans>
						Set verification level to{' '}
						<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.verification-level.strong">none</strong>.
					</Trans>
				);
			case GuildVerificationLevel.LOW:
				return (
					<Trans>
						Set verification level to{' '}
						<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.verification-level.strong--2">low</strong>.
					</Trans>
				);
			case GuildVerificationLevel.MEDIUM:
				return (
					<Trans>
						Set verification level to{' '}
						<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.verification-level.strong--3">
							medium
						</strong>
						.
					</Trans>
				);
			case GuildVerificationLevel.HIGH:
				return (
					<Trans>
						Set verification level to{' '}
						<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.verification-level.strong--4">high</strong>
						.
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
					Set default notifications to{' '}
					<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.default-message-notifications.strong">
						all messages
					</strong>
					.
				</Trans>
			);
		}
		if (change.newValue === MessageNotifications.ONLY_MENTIONS) {
			return (
				<Trans>
					Set default notifications to{' '}
					<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.default-message-notifications.strong--2">
						only mentions
					</strong>
					.
				</Trans>
			);
		}
		return null;
	},
	vanity_url_code: whenNewValueMissing(
		() => (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.strong">Removed</strong> the vanity URL.
			</Trans>
		),
		(change, {i18n}) => <Trans>Set the vanity URL to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	),
	features: (change, {i18n}) => {
		const {added, removed} = getFeatureDiff(change.oldValue, change.newValue);
		const addedLabels = mapFeatureLabels(added, i18n);
		const removedLabels = mapFeatureLabels(removed, i18n);
		if (addedLabels.length > 0 && removedLabels.length === 0) {
			return <Trans>Enabled features: {renderInline(joinLabels(addedLabels), i18n)}.</Trans>;
		}
		if (removedLabels.length > 0 && addedLabels.length === 0) {
			return <Trans>Disabled features: {renderInline(joinLabels(removedLabels), i18n)}.</Trans>;
		}
		if (addedLabels.length === 0 && removedLabels.length === 0) return null;
		return (
			<>
				{addedLabels.length > 0 ? <Trans>Enabled {renderInline(joinLabels(addedLabels), i18n)}.</Trans> : null}
				{removedLabels.length > 0 ? <Trans>Disabled {renderInline(joinLabels(removedLabels), i18n)}.</Trans> : null}
			</>
		);
	},
	banner_hash: () => <Trans>Updated the community banner.</Trans>,
	nsfw_level: (change, {i18n}) => {
		const label = getMatureContentLevelLabel(change.newValue, i18n);
		return label ? <Trans>Set mature content level to {renderInline(label, i18n)}.</Trans> : null;
	},
	explicit_content_filter: (change, {i18n}) => {
		const label = getExplicitContentFilterLabel(change.newValue, i18n);
		return label ? <Trans>Set explicit content filter to {renderInline(label, i18n)}.</Trans> : null;
	},
	system_channel_id: whenNewValueMissing(
		() => <Trans>Removed the system channel.</Trans>,
		(change, {guildId, i18n}) => (
			<Trans>Set the system channel to {renderInline(change.newValue, i18n, guildId, true)}.</Trans>
		),
	),
	system_channel_flags: (change, {i18n}) => {
		const {added, removed} = getSystemChannelFlagDiff(change.oldValue, change.newValue, i18n);
		if (added.length > 0) {
			return <Trans>Enabled {renderInline(joinLabels(added), i18n)} for the system channel.</Trans>;
		}
		if (removed.length > 0) {
			return <Trans>Disabled {renderInline(joinLabels(removed), i18n)} for the system channel.</Trans>;
		}
		return null;
	},
	rules_channel_id: whenNewValueMissing(
		() => <Trans>Removed the rules channel.</Trans>,
		(change, {guildId, i18n}) => (
			<Trans>Set the rules channel to {renderInline(change.newValue, i18n, guildId, true)}.</Trans>
		),
	),
	disabled_operations: (change, {i18n}) => {
		const {added, removed} = getOperationDiff(change.oldValue, change.newValue, i18n);
		const nodes: Array<ReactNode> = [];
		if (added.length > 0) nodes.push(<Trans key="disabled">Disabled {renderInline(joinLabels(added), i18n)}.</Trans>);
		if (removed.length > 0)
			nodes.push(<Trans key="reenabled">Re-enabled {renderInline(joinLabels(removed), i18n)}.</Trans>);
		if (nodes.length === 0) return null;
		if (nodes.length === 1) return nodes[0];
		return <>{nodes}</>;
	},
	embed_splash_hash: () => <Trans>Updated the embed splash.</Trans>,
	message_history_cutoff: whenNewValueMissing(
		() => <Trans>Cleared the message history threshold.</Trans>,
		(change, {i18n}) => {
			const formatted = formatDateStringValue(change.newValue);
			return formatted ? <Trans>Set message history threshold to {renderInline(formatted, i18n)}.</Trans> : null;
		},
	),
};
const CHANNEL_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	channel_id: (change, {guildId, i18n}) => (
		<Trans>Set the channel ID to {renderInline(change.newValue, i18n, guildId, true)}.</Trans>
	),
	type: (change, {i18n}) => <Trans>Set the channel type to {renderChannelTypeValue(change.newValue, i18n)}.</Trans>,
	name: (change, {i18n}) => <Trans>Renamed the channel to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	topic: whenNewValueMissing(
		() => <Trans>Cleared the topic.</Trans>,
		(change, {i18n}) =>
			isEmptyString(change.newValue) ? (
				<Trans>Cleared the topic.</Trans>
			) : (
				<Trans>Updated the topic to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>
			),
	),
	parent_id: whenNewValueMissing(
		() => <Trans>Removed the channel from its category.</Trans>,
		(change, {guildId, i18n}) => (
			<Trans>Moved the channel to category {renderInline(change.newValue, i18n, guildId, true)}.</Trans>
		),
	),
	position: (change, {i18n}) => <Trans>Set the channel position to {renderInline(change.newValue, i18n)}.</Trans>,
	nsfw: (change) =>
		change.newValue === true ? <Trans>Enabled mature content.</Trans> : <Trans>Disabled mature content.</Trans>,
	rate_limit_per_user: (change, {i18n}) => {
		const raw = safeScalarString(change.newValue, i18n);
		const seconds = raw != null ? Number(raw) : 0;
		if (!seconds || Number.isNaN(seconds)) return <Trans>Disabled slowmode.</Trans>;
		return (
			<Trans>
				Set slowmode to {renderInline(seconds, i18n)}{' '}
				<Plural
					value={seconds}
					one="second"
					other="seconds"
					data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.rate-limit-per-user.plural"
				/>
				.
			</Trans>
		);
	},
	user_limit: (change, {i18n}) => {
		const raw = safeScalarString(change.newValue, i18n);
		const limit = raw != null ? Number(raw) : 0;
		if (!limit || Number.isNaN(limit)) return <Trans>Removed the user limit.</Trans>;
		return <Trans>Set the user limit to {renderInline(limit, i18n)}.</Trans>;
	},
	voice_connection_limit: (change, {i18n}) => {
		const raw = safeScalarString(change.newValue, i18n);
		const limit = raw != null ? Number(raw) : 0;
		if (!limit || Number.isNaN(limit)) return <Trans>Reset the connection limit.</Trans>;
		return <Trans>Set the connection limit to {renderInline(limit, i18n)}.</Trans>;
	},
	bitrate: whenOldValueMissing(
		(change, {i18n}) => <Trans>Set the bitrate to {renderInline(change.newValue, i18n)}.</Trans>,
		(change, {i18n}) => <Trans>Changed the bitrate to {renderInline(change.newValue, i18n)}.</Trans>,
	),
	rtc_region: (change, {i18n}) => <Trans>Set the RTC region to {renderRtcRegionValue(change.newValue, i18n)}.</Trans>,
	permission_overwrite_count: (change, {i18n}) => (
		<Trans>Set permission overwrite count to {renderInline(change.newValue, i18n)}.</Trans>
	),
	allow: (change, {i18n}) => renderAllowOrDenyDiff('allow', change, i18n),
	deny: (change, {i18n}) => renderAllowOrDenyDiff('deny', change, i18n),
};
const USER_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	nick: whenNewOrOldMissing(
		(change, {i18n}) => (
			<Trans>
				Changed nickname from {renderInline(change.oldValue, i18n, undefined, false)} to {renderInline(change.newValue, i18n, undefined, false)}.
			</Trans>
		),
		(change, {i18n}) => <Trans>Set nickname to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
		(change, {i18n}) => (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.strong--2">Removed</strong> nickname{' '}
				{renderInline(change.oldValue, i18n, undefined, false)}.
			</Trans>
		),
		() => null,
	),
	deaf: (change) =>
		change.newValue === true ? (
			<Trans comment="Activity-log sentence. A moderator deafened a member in voice chat, meaning the member can no longer hear others. Preserve the rich-text tags.">
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.deaf.strong">Deafened</strong> the member.
			</Trans>
		) : (
			<Trans comment="Activity-log sentence. A moderator removed voice deafen from a member, meaning the member can hear others again. Preserve the rich-text tags.">
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.deaf.strong--2">Undeafened</strong> the member.
			</Trans>
		),
	mute: (change) =>
		change.newValue === true ? (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.mute.strong">Muted</strong> the member.
			</Trans>
		) : (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.mute.strong--2">Unmuted</strong> the member.
			</Trans>
		),
	roles: (change, {guildId, i18n}) => {
		const {added, removed} = getRoleDiff(change.oldValue, change.newValue);
		const resolveRoleName = getRoleNameResolver(guildId);
		const addedLabels = added.map(resolveRoleName);
		const removedLabels = removed.map(resolveRoleName);
		if (addedLabels.length > 0 && removedLabels.length === 0) {
			return <Trans>Added {renderInline(joinLabels(addedLabels), i18n)}.</Trans>;
		}
		if (removedLabels.length > 0 && addedLabels.length === 0) {
			return <Trans>Removed {renderInline(joinLabels(removedLabels), i18n)}.</Trans>;
		}
		if (addedLabels.length > 0 && removedLabels.length > 0) {
			return (
				<Trans>
					Added {renderInline(joinLabels(addedLabels), i18n)} and removed{' '}
					{renderInline(joinLabels(removedLabels), i18n)}.
				</Trans>
			);
		}
		return null;
	},
	$remove: (change, {guildId, i18n}) => {
		const resolveRoleName = getRoleNameResolver(guildId);
		const roleIds = normalizeStringArray(change.oldValue);
		const labels = roleIds.map(resolveRoleName);
		return labels.length > 0 ? (
			<Trans>Removed {renderInline(joinLabels(labels), i18n)}.</Trans>
		) : (
			<Trans>Removed roles.</Trans>
		);
	},
	$add: (change, {guildId, i18n}) => {
		const resolveRoleName = getRoleNameResolver(guildId);
		const roleIds = normalizeStringArray(change.newValue);
		const labels = roleIds.map(resolveRoleName);
		return labels.length > 0 ? (
			<Trans>Added {renderInline(joinLabels(labels), i18n)}.</Trans>
		) : (
			<Trans>Added roles.</Trans>
		);
	},
	avatar_hash: () => <Trans>Updated the avatar.</Trans>,
	banner_hash: () => <Trans>Updated the banner.</Trans>,
	reason: (change, {i18n}) => <Trans>Set reason to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	prune_delete_days: (change, {i18n}) => {
		const rawDays = safeScalarString(change.newValue, i18n);
		const parsed = rawDays != null ? Number(rawDays) : 0;
		const dayCount = Number.isFinite(parsed) ? parsed : 0;
		return (
			<Trans>
				Pruned members inactive for{' '}
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.prune-delete-days.strong">
					<Plural
						value={dayCount}
						one="# day"
						other="# days"
						data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.prune-delete-days.plural"
					/>
				</strong>
				.
			</Trans>
		);
	},
	bio: whenNewValueMissing(
		() => <Trans>Cleared the bio.</Trans>,
		(change, {i18n}) =>
			isEmptyString(change.newValue) ? (
				<Trans>Cleared the bio.</Trans>
			) : (
				<Trans>Updated the bio to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>
			),
	),
	pronouns: whenNewValueMissing(
		() => <Trans>Cleared the pronouns.</Trans>,
		(change, {i18n}) => <Trans>Updated the pronouns to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	),
	accent_color: (change, {i18n}) => {
		const color = formatAccentColor(change.newValue);
		return color ? (
			<Trans>
				Set accent color to {renderInline(color, i18n)}{' '}
				<ColorDot color={color} data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.accent-color.color-dot" />.
			</Trans>
		) : null;
	},
	communication_disabled_until: (change, {i18n}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Timed out until {renderInline(formatted, i18n)}.</Trans> : null;
	},
	temporary: (change) =>
		change.newValue === true ? (
			<Trans>Marked the member as temporary.</Trans>
		) : (
			<Trans>Marked the member as permanent.</Trans>
		),
	banned_at: (change, {i18n}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Banned at {renderInline(formatted, i18n)}.</Trans> : null;
	},
	expires_at: (change, {i18n}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Ban expires at {renderInline(formatted, i18n)}.</Trans> : null;
	},
};
const ROLE_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {i18n}) => {
		const oldLabel = change.oldValue != null ? renderInline(change.oldValue, i18n, undefined, false) : null;
		const newLabel = change.newValue != null ? renderInline(change.newValue, i18n, undefined, false) : null;
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
	permissions: (change, {i18n}) => {
		const {added, removed} = getPermissionDiff(change.oldValue, change.newValue);
		const addedLabels = added.length > 0 ? formatPermissionList(added) : '';
		const removedLabels = removed.length > 0 ? formatPermissionList(removed) : '';
		if (added.length > 0 && removed.length === 0) return <Trans>Granted {renderInline(addedLabels, i18n)}.</Trans>;
		if (removed.length > 0 && added.length === 0) return <Trans>Revoked {renderInline(removedLabels, i18n)}.</Trans>;
		if (added.length > 0 && removed.length > 0) {
			return (
				<Trans>
					Granted {renderInline(addedLabels, i18n)} and revoked {renderInline(removedLabels, i18n)}.
				</Trans>
			);
		}
		return null;
	},
	position: (change, {i18n}) => {
		const value = change.newValue ?? change.oldValue;
		if (value == null) return null;
		return <Trans>Moved the role to position {renderInline(value, i18n)}.</Trans>;
	},
	allow: (change, {i18n}) => renderAllowOrDenyDiff('allow', change, i18n),
	deny: (change, {i18n}) => renderAllowOrDenyDiff('deny', change, i18n),
	color: (change) => {
		const hex = formatAccentColor(change.newValue);
		if (hex == null) return null;
		if (hex === '#000000') return <Trans>Cleared the role color.</Trans>;
		return (
			<Trans>
				Set role color to <strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.color.strong">{hex}</strong>{' '}
				<ColorDot color={hex} data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.color.color-dot" />.
			</Trans>
		);
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
		(change, {i18n}) => <Trans>Set the unicode emoji to {renderInline(change.newValue, i18n)}.</Trans>,
	),
};
const INVITE_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	code: (change, {i18n}) => <Trans>Invite code is {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	max_uses: (change, {i18n}) =>
		change.newValue === 0 ? (
			<Trans>This invite has unlimited uses.</Trans>
		) : (
			<Trans>This invite expires after {renderInline(change.newValue, i18n)} uses.</Trans>
		),
	max_age: (change) => {
		if (change.newValue === 0) return <Trans>This invite never expires.</Trans>;
		if (typeof change.newValue !== 'number') return null;
		return <Trans>This invite expires in {formatMaxAge(change.newValue)}.</Trans>;
	},
	uses: (change, {i18n}) => {
		const raw = safeScalarString(change.newValue, i18n);
		const count = raw != null ? Number(raw) : Number.NaN;
		if (Number.isNaN(count)) return <Trans>Used {renderInline(change.newValue, i18n)} times.</Trans>;
		return (
			<Plural
				value={count}
				one="Used # time."
				other="Used # times."
				data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.uses.plural"
			/>
		);
	},
	temporary: (change) =>
		change.newValue === true ? (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.temporary.strong">Grants</strong> temporary
				membership.
			</Trans>
		) : (
			<Trans>
				<strong data-flx="guild.guild-tabs.guild-audit-log-tab-renderers.temporary.strong--2">Grants</strong> permanent
				membership.
			</Trans>
		),
	created_at: (change, {i18n}) => {
		const formatted = formatDateStringValue(change.newValue);
		return formatted ? <Trans>Created on {renderInline(formatted, i18n)}.</Trans> : null;
	},
};
const EMOJI_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {i18n}) => <Trans>Renamed emoji to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	animated: (change) =>
		change.newValue === true ? <Trans>Marked emoji as animated.</Trans> : <Trans>Marked emoji as static.</Trans>,
};
const STICKER_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	name: (change, {i18n}) => <Trans>Renamed sticker to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	description: whenNewValueMissing(
		() => <Trans>Cleared the sticker description.</Trans>,
		(change, {i18n}) => <Trans>Updated the sticker description to {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
	),
	animated: (change) => {
		if (change.newValue === true) {
			return <Trans>The sticker was uploaded as animated.</Trans>;
		}
		if (change.newValue === false) {
			return <Trans>The sticker was uploaded as static.</Trans>;
		}
		return null;
	},
};
const WEBHOOK_CHANGE_RENDERERS: Record<string, ChangeRenderer> = {
	channel_id: whenOldValueMissing(
		(change, {guildId, i18n}) => <Trans>Created for channel {renderInline(change.newValue, i18n, guildId, true)}.</Trans>,
		(change, {guildId, i18n}) => <Trans>Moved to channel {renderInline(change.newValue, i18n, guildId, true)}.</Trans>,
	),
	name: whenOldValueMissing(
		(change, {i18n}) => <Trans>Created with name {renderInline(change.newValue, i18n, undefined, false)}.</Trans>,
		(change, {i18n}) => (
			<Trans>
				Renamed from {renderInline(change.oldValue, i18n, undefined, false)} to {renderInline(change.newValue, i18n, undefined, false)}.
			</Trans>
		),
	),
	avatar_hash: () => <Trans>Updated the webhook avatar.</Trans>,
	type: (change, {i18n}) => <Trans>Set webhook type to {renderInline(change.newValue, i18n)}.</Trans>,
};
export const renderSubChanges = (
	_targetType: AuditLogTargetType,
	_entry: GuildAuditLogEntryResponse,
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

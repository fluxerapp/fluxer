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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Plural, Trans} from '@lingui/react/macro';
import type React from 'react';
import type {AuditLogChangeEntry, GuildAuditLogEntry} from '~/actions/GuildActionCreators';
import {ChannelTypes} from '~/Constants';
import type {SelectOption} from '~/components/form/Select';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import {getFormattedDateTime} from '~/utils/DateUtils';
import {
	type ChangeShapeWithUnknowns,
	formatDateStringValue,
	getChannelTypeLabel,
	isBasicRecord,
	isEmptyString,
	looksLikeSnowflake,
	resolveIdToName,
	safeScalarString,
	shouldHideChangeKey,
	toChangeShape,
} from '~/utils/modals/guildTabs/GuildAuditLogTabUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {ClickableUser} from './GuildAuditLogTab.components';
import type {AuditLogTargetType} from './GuildAuditLogTab.constants';
import styles from './GuildAuditLogTab.module.css';

export const shouldNotRenderChangeDetail = (targetType: AuditLogTargetType, changeKey: string): boolean =>
	shouldHideChangeKey(targetType, changeKey);

const looksLikeHexColor = (s: string): boolean => /^#[0-9a-fA-F]{6}$/.test(s);

const renderEntityInline = (
	label: string,
	guildId: string | undefined,
	t: (message: MessageDescriptor) => string,
): React.ReactNode => {
	if (!label) return <strong>{t(msg`something`)}</strong>;

	if (looksLikeSnowflake(label) && guildId) {
		const name = resolveIdToName(label, guildId);
		if (name) return <strong title={label}>{name}</strong>;
	}

	if (looksLikeSnowflake(label)) return <strong>{t(msg`unknown entity`)}</strong>;
	return <strong>{label}</strong>;
};

const renderActorInline = (
	actorUser: UserRecord | null,
	actorId: string | null | undefined,
	guildId: string | undefined,
	t: (message: MessageDescriptor) => string,
): React.ReactNode => {
	if (actorUser) return <ClickableUser user={actorUser} guildId={guildId} showAvatar={false} />;
	if (actorId) return <strong>{t(msg`Someone`)}</strong>;
	return <strong>{t(msg`Someone`)}</strong>;
};

const renderMemberInline = (
	memberUser: UserRecord | null,
	memberIdOrLabel: string | null | undefined,
	guildId: string | undefined,
	t: (message: MessageDescriptor) => string,
): React.ReactNode => {
	if (memberUser) return <ClickableUser user={memberUser} guildId={guildId} showAvatar={false} />;
	if (memberIdOrLabel) return renderEntityInline(memberIdOrLabel, guildId, t);
	return <strong>{t(msg`someone`)}</strong>;
};

const renderBoldValue = (content: React.ReactNode): React.ReactNode => <strong>{content}</strong>;

export const renderValueInline = (
	value: unknown,
	guildId: string | undefined,
	t: (message: MessageDescriptor) => string,
): React.ReactNode => {
	if (isEmptyString(value)) return renderBoldValue(t(msg`nothing`));

	const scalar = safeScalarString(value, t);
	if (scalar !== null) {
		if (typeof value === 'number') return renderBoldValue(scalar);
		if (typeof value === 'boolean') return renderBoldValue(scalar);
		if (typeof value === 'string' && looksLikeSnowflake(value)) {
			if (guildId) {
				const name = resolveIdToName(value, guildId);
				if (name) return <strong title={value}>{name}</strong>;
			}

			return <strong title={value}>{value}</strong>;
		}
		if (typeof value === 'string' && looksLikeHexColor(value)) return <strong>{value}</strong>;

		return renderBoldValue(scalar);
	}

	if (Array.isArray(value)) {
		return renderBoldValue(<Plural value={value.length} one="# item" other="# items" />);
	}

	if (isBasicRecord(value)) return renderBoldValue(t(msg`details`));

	return renderBoldValue(t(msg`value`));
};

export const shouldShowFallbackChangeDetail = (_change: ChangeShapeWithUnknowns): boolean => {
	return false;
};

export const renderFallbackChangeDetail = (_change: ChangeShapeWithUnknowns): React.ReactNode => {
	return null;
};

export const renderOptionDetailSentence = (
	key: string,
	value: unknown,
	guildId: string | undefined,
	actionType: AuditLogActionType | undefined,
	t: (message: MessageDescriptor) => string,
): React.ReactNode => {
	if (
		actionType === AuditLogActionType.MESSAGE_BULK_DELETE &&
		(key === 'count' || key === 'message_count' || key === 'delete_count' || key === 'channel_id')
	) {
		return null;
	}

	if (key === 'type') {
		const label = getChannelTypeLabel(value, t);
		if (label) {
			return <Trans>Channel type: {renderValueInline(label, guildId, t)}.</Trans>;
		}
		return <Trans>Channel type: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'channel_id') {
		return <Trans>Channel: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'message_id') {
		return <Trans>Message: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'inviter_id') {
		return <Trans>Invited by {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'vanity_url_code') {
		return <Trans>Vanity URL code: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'uses') {
		return <Trans>Used {renderValueInline(value, guildId, t)} times.</Trans>;
	}

	if (key === 'created_at') {
		const formatted = formatDateStringValue(value);
		if (formatted) {
			return <Trans>Created on {renderValueInline(formatted, guildId, t)}.</Trans>;
		}
	}

	if (key === 'temporary') {
		return value === true ? (
			<Trans>
				<strong>Grants</strong> temporary membership.
			</Trans>
		) : (
			<Trans>
				<strong>Grants</strong> permanent membership.
			</Trans>
		);
	}

	if (key === 'name') {
		return <Trans>Name: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'count' || key === 'delete_count' || key === 'messages' || key === 'message_count') {
		return <Trans>Deleted {renderValueInline(value, guildId, t)} messages.</Trans>;
	}

	if (key === 'members_removed' || key === 'members_pruned') {
		return <Trans>Removed {renderValueInline(value, guildId, t)} members.</Trans>;
	}

	if (key === 'channel') {
		return <Trans>Channel: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	if (key === 'max_age') {
		const seconds = typeof value === 'number' ? value : null;
		if (seconds === 0) {
			return <Trans>This invite never expires.</Trans>;
		}
		if (seconds != null) {
			const minutes = seconds / 60;
			const hours = minutes / 60;
			const days = hours / 24;

			if (days >= 1 && days % 1 === 0) {
				return <Plural value={days} one="This invite expires in # day." other="This invite expires in # days." />;
			}
			if (hours >= 1 && hours % 1 === 0) {
				return <Plural value={hours} one="This invite expires in # hour." other="This invite expires in # hours." />;
			}
			if (minutes >= 1 && minutes % 1 === 0) {
				return (
					<Plural value={minutes} one="This invite expires in # minute." other="This invite expires in # minutes." />
				);
			}
			return (
				<Plural value={seconds} one="This invite expires in # second." other="This invite expires in # seconds." />
			);
		}
	}

	if (key === 'delete_member_days') {
		const days = typeof value === 'number' ? value : null;
		if (days != null) {
			return (
				<Plural
					value={days}
					one="Members inactive for # day will be pruned."
					other="Members inactive for # days will be pruned."
				/>
			);
		}
	}

	if (key === 'role_name') {
		return <Trans>Role: {renderValueInline(value, guildId, t)}.</Trans>;
	}

	return null;
};

export const findChangeNewNumber = (
	changes: Array<AuditLogChangeEntry> | null | undefined,
	key: string,
): number | null => {
	if (!changes) return null;
	for (const raw of changes) {
		const c = toChangeShape(raw);
		if (c.key === key && typeof c.newValue === 'number') return c.newValue;
	}
	return null;
};

export const findChangeNewScalar = (
	changes: Array<AuditLogChangeEntry> | null | undefined,
	key: string,
	t: (message: MessageDescriptor) => string,
): string | null => {
	if (!changes) return null;
	for (const raw of changes) {
		const c = toChangeShape(raw);
		if (c.key !== key) continue;
		const s = safeScalarString(c.newValue, t);
		if (s != null) return s;
	}
	return null;
};

export const findChangeScalar = (
	changes: Array<AuditLogChangeEntry> | null | undefined,
	key: string,
	t: (message: MessageDescriptor) => string,
): string | null => {
	if (!changes) return null;
	for (const raw of changes) {
		const c = toChangeShape(raw);
		if (c.key !== key) continue;
		const newScalar = safeScalarString(c.newValue, t);
		if (newScalar != null) return newScalar;
		const oldScalar = safeScalarString(c.oldValue, t);
		if (oldScalar != null) return oldScalar;
	}
	return null;
};

const getOptionScalar = (
	entry: GuildAuditLogEntry,
	keys: Array<string>,
	t: (message: MessageDescriptor) => string,
): string | null => {
	const options = entry.options as unknown;
	if (!isBasicRecord(options)) return null;
	for (const k of keys) {
		const v = options[k];
		const s = safeScalarString(v, t);
		if (s != null) return s;
	}
	return null;
};

const getOptionNumber = (
	entry: GuildAuditLogEntry,
	keys: Array<string>,
	t: (message: MessageDescriptor) => string,
): number | null => {
	const options = entry.options as unknown;
	if (!isBasicRecord(options)) return null;
	for (const k of keys) {
		const v = options[k];
		if (typeof v === 'number') return v;
		const s = safeScalarString(v, t);
		if (s != null) {
			const n = Number(s);
			if (!Number.isNaN(n)) return n;
		}
	}
	return null;
};

export const resolveTargetLabel = (entry: GuildAuditLogEntry, t: (message: MessageDescriptor) => string): string => {
	if (entry.target_id) return entry.target_id;

	const options = entry.options as unknown;
	if (isBasicRecord(options)) {
		const maybe =
			options.name ??
			options.title ??
			options.code ??
			options.channel ??
			options.channel_id ??
			options.id ??
			options.target_id ??
			null;

		const scalar = safeScalarString(maybe, t);
		if (scalar) return scalar;
	}

	return t(msg`Unknown target`);
};

export const resolveChannelLabel = (
	entry: GuildAuditLogEntry,
	guildId: string | undefined,
	t: (message: MessageDescriptor) => string,
): string | null => {
	const options = entry.options as unknown;
	if (!isBasicRecord(options)) return null;

	const channelValue = options.channel ?? options.channel_id;
	const channelId = safeScalarString(channelValue, t);
	if (!channelId) return null;

	if (guildId && looksLikeSnowflake(channelId)) {
		const channel = ChannelStore.getChannel(channelId);
		if (channel?.name) return channel.name;
	}

	return looksLikeSnowflake(channelId) ? null : channelId;
};

const resolveChannelRecord = (entry: GuildAuditLogEntry, t: (message: MessageDescriptor) => string) => {
	const options = entry.options as unknown;
	if (!isBasicRecord(options)) return null;

	const channelValue = options.channel ?? options.channel_id;
	const channelId = safeScalarString(channelValue, t);
	if (!channelId) return null;

	return ChannelStore.getChannel(channelId) ?? null;
};

export const formatTimestamp = (logId: string): string => {
	const timestamp = SnowflakeUtils.extractTimestamp(logId);
	return getFormattedDateTime(timestamp);
};

export interface AuditLogUserOption extends SelectOption<string> {
	user: UserRecord;
}

export const buildUserOptions = (members: Array<{user: UserRecord}> | undefined): Array<AuditLogUserOption> => {
	if (!members) return [];
	return members
		.slice()
		.sort((a, b) => a.user.username.localeCompare(b.user.username))
		.map((member) => {
			const label = member.user.globalName ?? `${member.user.username}#${member.user.discriminator}`;
			return {value: member.user.id, label, user: member.user};
		});
};

export const renderEntrySummary = (args: {
	entry: GuildAuditLogEntry;
	actorUser: UserRecord | null;
	targetUser: UserRecord | null;
	targetLabel: string;
	channelLabel: string | null;
	guildId: string;
	t: (message: MessageDescriptor) => string;
}): React.ReactNode => {
	const {entry, actorUser, targetUser, targetLabel, channelLabel, guildId, t} = args;

	const actor = renderActorInline(actorUser, entry.user_id, guildId, t);
	const targetMember = renderMemberInline(targetUser, entry.target_id ?? targetLabel, guildId, t);
	const targetEntity = renderEntityInline(targetLabel, guildId, t);
	const channelRecord = resolveChannelRecord(entry, t);

	const channelDisplayLabel = channelRecord
		? `${channelRecord.type === ChannelTypes.GUILD_TEXT ? '#' : ''}${channelRecord.name ?? t(msg`Unknown channel`)}`
		: (channelLabel ?? null);

	const channelNode = channelDisplayLabel ? <span className={styles.channelPlain}>{channelDisplayLabel}</span> : null;

	const actionType = entry.action_type as AuditLogActionType;

	const changedName =
		findChangeScalar(entry.changes, 'name', t) ??
		findChangeScalar(entry.changes, 'nick', t) ??
		findChangeScalar(entry.changes, 'code', t) ??
		getOptionScalar(entry, ['name', 'title', 'code'], t) ??
		null;

	const namedTarget = changedName ? renderEntityInline(changedName, guildId, t) : targetEntity;

	const pruneDaysRaw = findChangeNewScalar(entry.changes, 'prune_delete_days', t);
	const pruneDays = pruneDaysRaw ? Number(pruneDaysRaw) : null;

	const bulkCount = getOptionNumber(entry, ['count', 'delete_count', 'messages', 'message_count'], t);
	const reason = typeof entry.reason === 'string' && entry.reason.trim() ? entry.reason.trim() : null;

	const withBecause = (sentence: React.ReactNode) =>
		reason ? (
			<>
				{sentence} <Trans>Because {reason}.</Trans>
			</>
		) : (
			sentence
		);

	switch (actionType) {
		case AuditLogActionType.GUILD_UPDATE:
			return withBecause(<Trans>{actor} updated the community settings.</Trans>);

		case AuditLogActionType.CHANNEL_CREATE:
			return withBecause(
				<Trans>
					{actor} created the channel {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.CHANNEL_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated the channel {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.CHANNEL_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted the channel {namedTarget}.
				</Trans>,
			);

		case AuditLogActionType.CHANNEL_OVERWRITE_CREATE:
			return withBecause(
				<Trans>
					{actor} added channel permissions for {targetEntity}
					{channelNode ? <> in {channelNode}</> : null}.
				</Trans>,
			);
		case AuditLogActionType.CHANNEL_OVERWRITE_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated channel permissions for {targetEntity}
					{channelNode ? <> in {channelNode}</> : null}.
				</Trans>,
			);
		case AuditLogActionType.CHANNEL_OVERWRITE_DELETE:
			return withBecause(
				<Trans>
					{actor} removed channel permissions for {targetEntity}
					{channelNode ? <> in {channelNode}</> : null}.
				</Trans>,
			);

		case AuditLogActionType.MEMBER_KICK:
			return withBecause(
				<Trans>
					{actor} kicked {targetMember}.
				</Trans>,
			);
		case AuditLogActionType.MEMBER_BAN_ADD:
			return withBecause(
				<Trans>
					{actor} banned {targetMember}.
				</Trans>,
			);
		case AuditLogActionType.MEMBER_BAN_REMOVE:
			return withBecause(
				<Trans>
					{actor} unbanned {targetMember}.
				</Trans>,
			);

		case AuditLogActionType.MEMBER_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated {targetMember}.
				</Trans>,
			);
		case AuditLogActionType.MEMBER_ROLE_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated roles for {targetMember}.
				</Trans>,
			);

		case AuditLogActionType.MEMBER_PRUNE:
			return withBecause(
				pruneDays != null && !Number.isNaN(pruneDays) ? (
					<Trans>
						{actor} pruned members inactive for {renderValueInline(pruneDays, guildId, t)} days.
					</Trans>
				) : (
					<Trans>{actor} pruned inactive members.</Trans>
				),
			);

		case AuditLogActionType.MEMBER_MOVE:
			return withBecause(
				channelNode ? (
					<Trans>
						{actor} moved {targetMember} to {channelNode}.
					</Trans>
				) : (
					<Trans>
						{actor} moved {targetMember} to another voice channel.
					</Trans>
				),
			);

		case AuditLogActionType.MEMBER_DISCONNECT:
			return withBecause(
				<Trans>
					{actor} disconnected {targetMember} from voice.
				</Trans>,
			);

		case AuditLogActionType.BOT_ADD:
			return withBecause(
				<Trans>
					{actor} added the bot {targetMember}.
				</Trans>,
			);

		case AuditLogActionType.ROLE_CREATE:
			return withBecause(
				<Trans>
					{actor} created the role {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.ROLE_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated the role {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.ROLE_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted the role {namedTarget}.
				</Trans>,
			);

		case AuditLogActionType.INVITE_CREATE:
			return withBecause(
				<Trans>
					{actor} created the invite {namedTarget}
					{channelNode ? <> for {channelNode}</> : null}.
				</Trans>,
			);
		case AuditLogActionType.INVITE_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated the invite {namedTarget}
					{channelNode ? <> for {channelNode}</> : null}.
				</Trans>,
			);
		case AuditLogActionType.INVITE_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted the invite {namedTarget}
					{channelNode ? <> for {channelNode}</> : null}.
				</Trans>,
			);

		case AuditLogActionType.WEBHOOK_CREATE:
			return withBecause(
				<Trans>
					{actor} created the webhook {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.WEBHOOK_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated the webhook {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.WEBHOOK_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted the webhook {namedTarget}.
				</Trans>,
			);

		case AuditLogActionType.EMOJI_CREATE:
			return withBecause(
				<Trans>
					{actor} added the emoji {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.EMOJI_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated the emoji {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.EMOJI_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted the emoji {namedTarget}.
				</Trans>,
			);

		case AuditLogActionType.STICKER_CREATE:
			return withBecause(
				<Trans>
					{actor} added the sticker {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.STICKER_UPDATE:
			return withBecause(
				<Trans>
					{actor} updated the sticker {namedTarget}.
				</Trans>,
			);
		case AuditLogActionType.STICKER_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted the sticker {namedTarget}.
				</Trans>,
			);

		case AuditLogActionType.MESSAGE_DELETE:
			return withBecause(
				<Trans>
					{actor} deleted a message{channelNode ? <> in {channelNode}</> : null}.
				</Trans>,
			);
		case AuditLogActionType.MESSAGE_BULK_DELETE:
			return withBecause(
				bulkCount != null ? (
					<Trans>
						{actor} deleted {renderValueInline(bulkCount, guildId, t)} messages
						{channelNode ? <> in {channelNode}</> : null}.
					</Trans>
				) : (
					<Trans>
						{actor} deleted multiple messages{channelNode ? <> in {channelNode}</> : null}.
					</Trans>
				),
			);
		case AuditLogActionType.MESSAGE_PIN:
			return withBecause(
				<Trans>
					{actor} pinned a message{channelNode ? <> in {channelNode}</> : null}.
				</Trans>,
			);
		case AuditLogActionType.MESSAGE_UNPIN:
			return withBecause(
				<Trans>
					{actor} unpinned a message{channelNode ? <> in {channelNode}</> : null}.
				</Trans>,
			);

		default:
			return withBecause(
				<Trans>
					{actor} performed an audit action on {targetEntity}.
				</Trans>,
			);
	}
};

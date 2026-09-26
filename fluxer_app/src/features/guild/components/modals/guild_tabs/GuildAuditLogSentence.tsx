// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import Channels from '@app/features/channel/state/Channels';
import Emoji from '@app/features/emoji/state/Emoji';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTab.module.css';
import {
	ClickableUser,
	ColorDot,
	CopyIdInline,
	InlineCode,
} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabComponents';
import Guilds from '@app/features/guild/state/Guilds';
import type {
	AuditLogPlaceholder,
	AuditLogSentence,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	DELETED_CATEGORY_LABEL,
	DELETED_CHANNEL_LABEL,
	DELETED_ROLE_LABEL,
	MORE_PERMISSIONS_DESCRIPTOR,
	SYSTEM_ACTOR_LABEL,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogSharedMessages';
import {largestDurationUnit} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {
	DAYS_DURATION_PLURAL_DESCRIPTOR,
	HOURS_DURATION_PLURAL_DESCRIPTOR,
	MINUTES_DURATION_PLURAL_DESCRIPTOR,
	SECONDS_DURATION_PLURAL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import Users from '@app/features/user/state/Users';
import {getEmojiURL} from '@app/features/user/utils/AvatarUtils';
import {getFormattedDateTime} from '@app/features/user/utils/DateFormatting';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';
import {observer} from 'mobx-react-lite';
import type React from 'react';

type ChannelPlaceholder = Extract<AuditLogPlaceholder, {kind: 'channel'}>;
type RolePlaceholder = Extract<AuditLogPlaceholder, {kind: 'role'}>;
type EmojiPlaceholder = Extract<AuditLogPlaceholder, {kind: 'emoji'}>;

interface GuildAuditLogSentenceProps {
	sentence: AuditLogSentence;
	guildId: string;
}

const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment:
		'Activity log text shown in place of a member name inside an activity log sentence when the user is not loaded or no longer exists.',
});

const TEXT_MAX_CODE_POINTS = 200;
const TEXT_TRUNCATION_SUFFIX = '…';
const PERMISSION_LIST_LIMIT = 8;
const PERMISSION_LIST_SHOWN = 7;
const PLURAL_COUNT_VALUE = 'count';

function formatDuration(i18n: I18n, seconds: number): string {
	const {unit, value} = largestDurationUnit(seconds);
	switch (unit) {
		case 'days':
			return i18n._(DAYS_DURATION_PLURAL_DESCRIPTOR, {days: value});
		case 'hours':
			return i18n._(HOURS_DURATION_PLURAL_DESCRIPTOR, {hours: value});
		case 'minutes':
			return i18n._(MINUTES_DURATION_PLURAL_DESCRIPTOR, {minutes: value});
		case 'seconds':
			return i18n._(SECONDS_DURATION_PLURAL_DESCRIPTOR, {seconds: value});
	}
}

function formatPermissions(i18n: I18n, flags: ReadonlyArray<bigint>): string {
	const shownFlags = flags.length > PERMISSION_LIST_LIMIT ? flags.slice(0, PERMISSION_LIST_SHOWN) : flags;
	const items = shownFlags.map((flag) => formatPermissionLabel(i18n, flag));
	if (shownFlags.length < flags.length) {
		items.push(i18n._(MORE_PERMISSIONS_DESCRIPTOR, {count: flags.length - shownFlags.length}));
	}
	return formatListWithConfig(items, {locale: i18n.locale, style: 'long', type: 'conjunction'});
}

function formatColor(value: number): string {
	return `#${value.toString(16).padStart(6, '0').toUpperCase()}`;
}

function getChannelLabel(placeholder: ChannelPlaceholder, i18n: I18n): string {
	const channel = Channels.getChannel(placeholder.id);
	const name = placeholder.recordedName ?? channel?.name;
	if (name == null) {
		return placeholder.fallback === 'category' ? i18n._(DELETED_CATEGORY_LABEL) : `#${i18n._(DELETED_CHANNEL_LABEL)}`;
	}
	const hasHashPrefix = channel?.type === ChannelTypes.GUILD_TEXT || channel?.type === ChannelTypes.GUILD_LINK;
	return hasHashPrefix ? `#${name}` : name;
}

function getRoleLabel(placeholder: RolePlaceholder, guildId: string, i18n: I18n): string {
	if (placeholder.id === guildId) return EVERYONE_MENTION;
	return placeholder.recordedName ?? Guilds.getGuildRole(guildId, placeholder.id)?.name ?? i18n._(DELETED_ROLE_LABEL);
}

function renderUser(id: string, guildId: string, i18n: I18n): React.ReactNode {
	const user = Users.getUser(id);
	if (user) {
		return (
			<ClickableUser
				user={user}
				guildId={guildId}
				showAvatar={false}
				data-flx="guild.guild-tabs.guild-audit-log-sentence.render-user.clickable-user"
			/>
		);
	}
	return (
		<strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-user.strong">
			{i18n._(UNKNOWN_USER_DESCRIPTOR)}
		</strong>
	);
}

function renderSystem(i18n: I18n): React.ReactNode {
	return (
		<strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-system.strong">
			{i18n._(SYSTEM_ACTOR_LABEL)}
		</strong>
	);
}

function renderChannel(placeholder: ChannelPlaceholder, i18n: I18n): React.ReactNode {
	return (
		<CopyIdInline
			id={placeholder.id}
			data-flx="guild.guild-tabs.guild-audit-log-sentence.render-channel.copy-id-inline"
		>
			<strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-channel.strong">
				{getChannelLabel(placeholder, i18n)}
			</strong>
		</CopyIdInline>
	);
}

function renderRole(placeholder: RolePlaceholder, guildId: string, i18n: I18n): React.ReactNode {
	return (
		<CopyIdInline id={placeholder.id} data-flx="guild.guild-tabs.guild-audit-log-sentence.render-role.copy-id-inline">
			<strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-role.strong">
				{getRoleLabel(placeholder, guildId, i18n)}
			</strong>
		</CopyIdInline>
	);
}

function renderName(value: string): React.ReactNode {
	return <strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-name.strong">{value}</strong>;
}

function renderValue(value: string): React.ReactNode {
	return <strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-value.strong">{value}</strong>;
}

function renderText(value: string): React.ReactNode {
	const codePoints = Array.from(value);
	if (codePoints.length <= TEXT_MAX_CODE_POINTS) {
		return (
			<InlineCode data-flx="guild.guild-tabs.guild-audit-log-sentence.render-text.inline-code">{value}</InlineCode>
		);
	}
	const truncated = `${codePoints.slice(0, TEXT_MAX_CODE_POINTS).join('')}${TEXT_TRUNCATION_SUFFIX}`;
	return (
		<InlineCode title={value} data-flx="guild.guild-tabs.guild-audit-log-sentence.render-text.inline-code--2">
			{truncated}
		</InlineCode>
	);
}

function renderEmoji({id, name}: EmojiPlaceholder): React.ReactNode {
	const label = `:${name}:`;
	const emoji = id === null ? undefined : Emoji.getEmojiById(id);
	if (id === null || emoji === undefined) {
		return <strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-emoji.strong">{label}</strong>;
	}
	return (
		<span className={styles.inlineEmoji} data-flx="guild.guild-tabs.guild-audit-log-sentence.render-emoji.inline-emoji">
			<img
				src={getEmojiURL({id, animated: emoji.animated})}
				alt=""
				draggable={false}
				className={styles.inlineEmojiImage}
				data-flx="guild.guild-tabs.guild-audit-log-sentence.render-emoji.inline-emoji-image"
			/>
			<strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-emoji.strong--2">{label}</strong>
		</span>
	);
}

function renderColor(value: number): React.ReactNode {
	const hex = formatColor(value);
	return (
		<span className={styles.colorValue} data-flx="guild.guild-tabs.guild-audit-log-sentence.render-color.color-value">
			<strong data-flx="guild.guild-tabs.guild-audit-log-sentence.render-color.strong">{hex}</strong>
			<ColorDot color={hex} data-flx="guild.guild-tabs.guild-audit-log-sentence.render-color.color-dot" />
		</span>
	);
}

function renderPlaceholder(placeholder: AuditLogPlaceholder, guildId: string, i18n: I18n): React.ReactNode {
	switch (placeholder.kind) {
		case 'user':
			return renderUser(placeholder.id, guildId, i18n);
		case 'system':
			return renderSystem(i18n);
		case 'channel':
			return renderChannel(placeholder, i18n);
		case 'role':
			return renderRole(placeholder, guildId, i18n);
		case 'name':
			return renderName(placeholder.value);
		case 'text':
			return renderText(placeholder.value);
		case 'emoji':
			return renderEmoji(placeholder);
		case 'date':
			return renderValue(getFormattedDateTime(placeholder.timestamp));
		case 'duration':
			return renderValue(formatDuration(i18n, placeholder.seconds));
		case 'permissions':
			return renderValue(formatPermissions(i18n, placeholder.flags));
		case 'color':
			return renderColor(placeholder.value);
		case 'label':
			return renderValue(i18n._(placeholder.descriptor));
	}
}

export const GuildAuditLogSentence: React.FC<GuildAuditLogSentenceProps> = observer(({sentence, guildId}) => {
	const {i18n} = useLingui();
	const values: Record<string, React.ReactNode> = {};
	for (const [name, value] of Object.entries(sentence.values)) {
		if (typeof value !== 'number') {
			values[name] = renderPlaceholder(value, guildId, i18n);
			continue;
		}
		values[name] = name === PLURAL_COUNT_VALUE ? value : renderValue(getCachedNumberFormat(i18n.locale).format(value));
	}
	return <Trans id={sentence.descriptor.id} message={sentence.descriptor.message} values={values} />;
});

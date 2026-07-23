// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import type {Channel} from '@app/features/channel/models/Channel';
import ChannelDisplayName from '@app/features/channel/state/ChannelDisplayName';
import {UNKNOWN_CHANNEL_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {compareChannelPosition} from '@app/features/messaging/utils/ChannelShared';
import {EncryptedVoiceChannelIcon} from '@app/features/ui/components/icons/EncryptedVoiceChannelIcon';
import {LinkChannelIcon} from '@app/features/ui/components/icons/LinkChannelIcon';
import {LockedLinkChannelIcon} from '@app/features/ui/components/icons/LockedLinkChannelIcon';
import {LockedTextChannelIcon} from '@app/features/ui/components/icons/LockedTextChannelIcon';
import {LockedVoiceChannelIcon} from '@app/features/ui/components/icons/LockedVoiceChannelIcon';
import {MatureLinkChannelIcon} from '@app/features/ui/components/icons/MatureLinkChannelIcon';
import {MatureTextChannelIcon} from '@app/features/ui/components/icons/MatureTextChannelIcon';
import {MatureVoiceChannelIcon} from '@app/features/ui/components/icons/MatureVoiceChannelIcon';
import {NoConnectChannelIcon} from '@app/features/ui/components/icons/NoConnectChannelIcon';
import {TextChannelIcon} from '@app/features/ui/components/icons/TextChannelIcon';
import {VoiceChannelIcon} from '@app/features/ui/components/icons/VoiceChannelIcon';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {CaretDownIcon, type IconProps, NotePencilIcon} from '@phosphor-icons/react';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';

const VOICE_MATURE_DESCRIPTOR = msg({
	message: 'Voice (mature)',
	comment: 'Channel type chip label for a mature-content voice channel.',
});
const VOICE_DESCRIPTOR = msg({
	message: 'Voice',
	comment: 'Channel type chip label for a voice channel.',
});
const CATEGORY_DESCRIPTOR = msg({
	message: 'Category',
	comment: 'Channel type chip label for a community category container.',
});
const LINK_MATURE_DESCRIPTOR = msg({
	message: 'Link (mature)',
	comment: 'Channel type chip label for a mature-content linked external channel.',
});
const LINK_DESCRIPTOR = msg({
	message: 'Link',
	comment: 'Channel type chip label for a linked external channel.',
});
const TEXT_MATURE_DESCRIPTOR = msg({
	message: 'Text (mature)',
	comment: 'Channel type chip label for a mature-content text channel.',
});
const TEXT_DESCRIPTOR = msg({
	message: 'Text',
	comment: 'Channel type chip label for a text channel.',
});
const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback name in channel display utilities when a user record is not available.',
});
const UNNAMED_GROUP_DESCRIPTOR = msg({
	message: 'Unnamed group',
	comment: 'Fallback display name for a group DM with no name set.',
});

export function compareChannels(a: Channel, b: Channel): number {
	return compareChannelPosition(a, b);
}

function isChannelRoleRequired(channel: {
	type: number;
	guildId?: string;
	permissionOverwrites?: Readonly<Record<string, {deny: bigint}>>;
}): boolean {
	if (!channel.guildId || !channel.permissionOverwrites) return false;
	const overwrite = channel.permissionOverwrites[channel.guildId];
	if (!overwrite) return false;
	switch (channel.type) {
		case ChannelTypes.GUILD_VOICE:
			return (overwrite.deny & Permissions.CONNECT) === Permissions.CONNECT;
		case ChannelTypes.GUILD_TEXT:
		case ChannelTypes.GUILD_LINK:
			return (overwrite.deny & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL;
		default:
			return false;
	}
}

export function getIcon(
	channel: {
		type: number;
		nsfw?: boolean;
		guildId?: string;
		permissionOverwrites?: Readonly<Record<string, {deny: bigint}>>;
	},
	props: IconProps = {},
	options?: {locked?: boolean; e2eeEncrypted?: boolean},
) {
	const roleRequired = isChannelRoleRequired(channel);
	switch (channel.type) {
		case ChannelTypes.GUILD_TEXT: {
			if (channel.nsfw)
				return <MatureTextChannelIcon data-flx="channel.channel-utils.get-icon.mature-text-channel-icon" {...props} />;
			if (roleRequired)
				return <LockedTextChannelIcon data-flx="channel.channel-utils.get-icon.locked-text-channel-icon" {...props} />;
			return <TextChannelIcon data-flx="channel.channel-utils.get-icon.text-channel-icon" {...props} />;
		}
		case ChannelTypes.GUILD_VOICE: {
			if (options?.e2eeEncrypted)
				return (
					<EncryptedVoiceChannelIcon
						data-flx="channel.channel-utils.get-icon.encrypted-voice-channel-icon"
						{...props}
					/>
				);
			if (channel.nsfw)
				return (
					<MatureVoiceChannelIcon data-flx="channel.channel-utils.get-icon.mature-voice-channel-icon" {...props} />
				);
			if (options?.locked)
				return <NoConnectChannelIcon data-flx="channel.channel-utils.get-icon.no-connect-channel-icon" {...props} />;
			if (roleRequired)
				return (
					<LockedVoiceChannelIcon data-flx="channel.channel-utils.get-icon.locked-voice-channel-icon" {...props} />
				);
			return <VoiceChannelIcon data-flx="channel.channel-utils.get-icon.voice-channel-icon" {...props} />;
		}
		case ChannelTypes.GUILD_LINK: {
			if (channel.nsfw)
				return <MatureLinkChannelIcon data-flx="channel.channel-utils.get-icon.mature-link-channel-icon" {...props} />;
			if (roleRequired)
				return <LockedLinkChannelIcon data-flx="channel.channel-utils.get-icon.locked-link-channel-icon" {...props} />;
			return <LinkChannelIcon data-flx="channel.channel-utils.get-icon.link-channel-icon" {...props} />;
		}
		case ChannelTypes.GUILD_CATEGORY:
			return <CaretDownIcon weight="bold" data-flx="channel.channel-utils.get-icon.caret-down-icon" {...props} />;
		case ChannelTypes.GUILD_THREAD:
			return <ThreadIcon size={typeof props.size === 'number' ? props.size : 20} className={props.className as string | undefined} data-flx="channel.channel-utils.get-icon.thread-icon" />;
		case ChannelTypes.DM_PERSONAL_NOTES:
			return <NotePencilIcon weight="bold" data-flx="channel.channel-utils.get-icon.note-pencil-icon" {...props} />;
		default:
			return <TextChannelIcon data-flx="channel.channel-utils.get-icon.text-channel-icon--2" {...props} />;
	}
}

export function getName(channel: Channel) {
	switch (channel.type) {
		case ChannelTypes.GUILD_VOICE:
			return channel.nsfw ? i18n._(VOICE_MATURE_DESCRIPTOR) : i18n._(VOICE_DESCRIPTOR);
		case ChannelTypes.GUILD_CATEGORY:
			return i18n._(CATEGORY_DESCRIPTOR);
		case ChannelTypes.GUILD_LINK:
			return channel.nsfw ? i18n._(LINK_MATURE_DESCRIPTOR) : i18n._(LINK_DESCRIPTOR);
		default:
			return channel.nsfw ? i18n._(TEXT_MATURE_DESCRIPTOR) : i18n._(TEXT_DESCRIPTOR);
	}
}

const getDirectMessageDisplayName = (channel: Channel): string => {
	if (channel.recipientIds.length === 0) {
		return i18n._(UNKNOWN_USER_DESCRIPTOR);
	}
	const recipient = Users.getUser(channel.recipientIds[0]);
	const nickname = recipient ? NicknameUtils.getNickname(recipient) : null;
	return nickname ?? i18n._(UNKNOWN_USER_DESCRIPTOR);
};
const getGroupDMDisplayName = (channel: Channel): string => {
	const customName = channel.name?.trim() ?? '';
	if (customName.length > 0) {
		return customName;
	}
	return ChannelDisplayName.getDisplayName(channel.id) ?? i18n._(UNNAMED_GROUP_DESCRIPTOR);
};

export function getDMDisplayName(channel: Channel): string {
	switch (channel.type) {
		case ChannelTypes.DM_PERSONAL_NOTES:
			return i18n._(PERSONAL_NOTES_DESCRIPTOR);
		case ChannelTypes.DM:
			return getDirectMessageDisplayName(channel);
		case ChannelTypes.GROUP_DM:
			return getGroupDMDisplayName(channel);
		default:
			return channel.name || i18n._(UNKNOWN_CHANNEL_DESCRIPTOR);
	}
}

export function isSystemDmChannel(channel: Channel): boolean {
	return channel.type === ChannelTypes.DM && channel.recipientIds.includes(FLUXERBOT_ID);
}

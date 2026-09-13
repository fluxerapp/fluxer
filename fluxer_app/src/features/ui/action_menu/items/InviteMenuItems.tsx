// SPDX-License-Identifier: AGPL-3.0-or-later

import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {INVITE_TO_COMMUNITY_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {SendInviteToCommunityIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {INVITE_SENT_FOR_DESCRIPTOR} from '@app/features/ui/action_menu/items/dm_menu_data/shared';
import {
	beginInviteToCommunityGuard,
	getInviteToCommunityGuardKey,
	scheduleInviteToCommunityGuardRelease,
} from '@app/features/ui/action_menu/items/InviteToCommunityGuard';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const logger = new Logger('InviteMenuItems');

interface InviteCandidate {
	guild: Guild;
	channelId: string;
}

const getDefaultInviteChannelId = (guildId: string): string | null => {
	return InviteUtils.getDefaultCommunityInviteChannelId(guildId) ?? null;
};

interface InviteToCommunityMenuItemProps {
	user: User;
	onClose: () => void;
}

export const InviteToCommunityMenuItem: React.FC<InviteToCommunityMenuItemProps> = observer(({user, onClose}) => {
	const {i18n} = useLingui();
	const sendingRef = useRef(false);
	const [sendingGuildId, setSendingGuildId] = useState<string | null>(null);
	const candidates = useMemo(() => {
		return Guilds.getGuilds()
			.filter((guild) => !GuildMembers.getMember(guild.id, user.id))
			.map((guild): InviteCandidate | null => {
				const channelId = getDefaultInviteChannelId(guild.id);
				return channelId ? {guild, channelId} : null;
			})
			.filter((candidate): candidate is InviteCandidate => candidate !== null)
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	}, [user.id]);
	const handleSendInvite = useCallback(
		async (candidate: InviteCandidate) => {
			if (sendingRef.current) return;
			const guardKey = getInviteToCommunityGuardKey(user.id, candidate.guild.id, candidate.channelId);
			if (!beginInviteToCommunityGuard(guardKey)) return;
			sendingRef.current = true;
			setSendingGuildId(candidate.guild.id);
			try {
				let invite: Invite;
				let inviteUrl: string;
				const inviteCapability = InviteUtils.getInviteCapability(candidate.channelId, candidate.guild.id);
				if (inviteCapability.useVanityUrl && inviteCapability.vanityUrlCode) {
					inviteUrl = InviteUtils.getVanityInviteUrl(inviteCapability.vanityUrlCode);
				} else {
					try {
						invite = await InviteCommands.create(candidate.channelId);
					} catch {
						return;
					}
					inviteUrl = `${RuntimeConfig.inviteEndpoint}/${invite.code}`;
				}
				const dmChannelId = await PrivateChannelCommands.ensureDMChannel(user.id);
				try {
					const result = await MessageCommands.send(dmChannelId, {
						content: inviteUrl,
						nonce: fromTimestamp(Date.now()),
					});
					if (result) {
						ToastCommands.createToast({
							type: 'success',
							children: i18n._(INVITE_SENT_FOR_DESCRIPTOR, {guildName: candidate.guild.name}),
						});
					}
				} catch (error) {
					logger.error('Failed to send invite via context menu:', error);
					showDmActionErrorModal(error);
				} finally {
					onClose();
				}
			} finally {
				sendingRef.current = false;
				setSendingGuildId(null);
				scheduleInviteToCommunityGuardRelease(guardKey);
			}
		},
		[onClose, user.id, i18n],
	);
	if (user.bot || candidates.length === 0) {
		return null;
	}
	return (
		<MenuItemSubmenu
			label={i18n._(INVITE_TO_COMMUNITY_DESCRIPTOR)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.invite-menu-items.invite-to-community-menu-item.menu-group">
					{candidates.map((candidate) => (
						<MenuItem
							key={candidate.guild.id}
							icon={
								<SendInviteToCommunityIcon data-flx="ui.action-menu.items.invite-menu-items.invite-to-community-menu-item.send-invite-to-community-icon" />
							}
							onClick={() => handleSendInvite(candidate)}
							disabled={sendingGuildId !== null}
							closeOnSelect={false}
							data-flx="ui.action-menu.items.invite-menu-items.invite-to-community-menu-item.menu-item.send-invite"
						>
							{candidate.guild.name}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.invite-menu-items.invite-to-community-menu-item.menu-item-submenu"
		/>
	);
});

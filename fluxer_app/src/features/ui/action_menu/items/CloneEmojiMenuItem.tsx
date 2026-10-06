// SPDX-License-Identifier: AGPL-3.0-or-later

import {showExpressionCloneFailedModal} from '@app/features/app/components/alerts/ExpressionCloneFailedModal';
import Emoji from '@app/features/emoji/state/Emoji';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import * as GuildEmojiCommands from '@app/features/expressions/commands/GuildEmojiCommands';
import ExpressionMetadata from '@app/features/expressions/state/ExpressionMetadata';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import {createKeyedActionGuard} from '@app/lib/overlay/KeyedActionGuard';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CopyIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect} from 'react';

const CLONED_TO_DESCRIPTOR = msg({
	message: 'Cloned :{sourceName}: to {targetName}',
	comment: 'Toast confirming an emoji was cloned to a target community.',
});
const CHECKING_ACCESS_DESCRIPTOR = msg({
	message: 'Checking access...',
	comment: 'Loading label shown while permissions are being verified.',
});
const CLONE_EMOJI_TO_DESCRIPTOR = msg({
	message: 'Clone emoji to...',
	comment: 'Submenu label listing communities the emoji can be cloned to.',
});
const EMOJI_SLOTS_FULL_DESCRIPTOR = msg({
	message: 'Emoji slots full',
	comment: 'Status label shown when the target community has no available emoji slots.',
});
const logger = new Logger('CloneEmoji');
const emojiCloneGuard = createKeyedActionGuard();

interface CloneEmojiMenuItemProps {
	emoji: FlatEmoji;
	onClose: () => void;
}

export const CloneEmojiMenuItem: React.FC<CloneEmojiMenuItemProps> = observer(({emoji, onClose}) => {
	const {i18n} = useLingui();
	const sourceGuild = emoji.guildId ? Guilds.getGuild(emoji.guildId) : null;
	const metadataState = emoji.id ? ExpressionMetadata.getEmojiMetadata(emoji.id) : null;
	const shortcutEnabled = AdvancedSettings.expressionCloneShortcutsEnabled;
	const needsMetadataLookup = shortcutEnabled && Boolean(emoji.id) && !sourceGuild;
	useEffect(() => {
		if (
			needsMetadataLookup &&
			emoji.id &&
			metadataState &&
			!metadataState.data &&
			!metadataState.loading &&
			!metadataState.error
		) {
			void ExpressionMetadata.fetchEmojiMetadata(emoji.id);
		}
	}, [needsMetadataLookup, emoji.id, metadataState]);
	if (!shortcutEnabled) return null;
	if (!emoji.id) return null;
	const emojiId = emoji.id;
	const sourceName = emoji.uniqueName || emoji.name;
	const cloningAllowed = sourceGuild
		? sourceGuild.cloneEmojiAllowed
		: metadataState?.data
			? metadataState.data.allowCloning
			: null;
	const isLoadingPermission = needsMetadataLookup && (metadataState?.loading ?? false);
	if (cloningAllowed !== true && !isLoadingPermission) return null;
	const eligible = Guilds.getGuilds()
		.filter((guild) => guild.id !== emoji.guildId)
		.filter((guild) => Permission.can(Permissions.MANAGE_EXPRESSIONS, {guildId: guild.id}))
		.map((guild) => {
			const used = Emoji.getGuildEmoji(guild.id).length;
			return {guild, slotsFull: used >= guild.maxEmojis};
		})
		.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	if (eligible.length === 0) return null;
	const cloneInto = async (guildId: string) => {
		const targetGuild = Guilds.getGuild(guildId);
		const targetName = targetGuild?.name ?? '';
		const inFlightKey = `${emojiId}:${guildId}`;
		if (!emojiCloneGuard.begin(inFlightKey)) return;
		try {
			await GuildEmojiCommands.clone(guildId, {sourceEmojiId: emojiId});
			ToastCommands.success(i18n._(CLONED_TO_DESCRIPTOR, {sourceName, targetName}));
		} catch (error) {
			logger.error(`Failed to clone emoji ${emojiId} into ${guildId}`, error);
			showExpressionCloneFailedModal(error, 'emoji', targetName);
		} finally {
			emojiCloneGuard.scheduleRelease(inFlightKey);
		}
	};
	if (isLoadingPermission) {
		return (
			<MenuGroup data-flx="ui.action-menu.items.clone-emoji-menu-item.menu-group">
				<MenuItem
					icon={<CopyIcon data-flx="ui.action-menu.items.clone-emoji-menu-item.copy-icon" />}
					disabled
					hint={i18n._(CHECKING_ACCESS_DESCRIPTOR)}
					data-flx="ui.action-menu.items.clone-emoji-menu-item.menu-item"
				>
					{i18n._(CLONE_EMOJI_TO_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		);
	}
	return (
		<MenuGroup data-flx="ui.action-menu.items.clone-emoji-menu-item.menu-group--2">
			<MenuItemSubmenu
				label={i18n._(CLONE_EMOJI_TO_DESCRIPTOR)}
				render={() => (
					<MenuGroup data-flx="ui.action-menu.items.clone-emoji-menu-item.menu-group--3">
						{eligible.map(({guild, slotsFull}) => (
							<MenuItem
								key={guild.id}
								icon={
									<GuildIcon
										id={guild.id}
										name={guild.name}
										icon={guild.icon}
										sizePx={16}
										data-flx="ui.action-menu.items.clone-emoji-menu-item.guild-icon"
									/>
								}
								disabled={slotsFull}
								hint={slotsFull ? i18n._(EMOJI_SLOTS_FULL_DESCRIPTOR) : undefined}
								onClick={() => {
									void cloneInto(guild.id);
									onClose();
								}}
								data-flx="ui.action-menu.items.clone-emoji-menu-item.menu-item.close"
							>
								{guild.name}
							</MenuItem>
						))}
					</MenuGroup>
				)}
				data-flx="ui.action-menu.items.clone-emoji-menu-item.menu-item-submenu"
			/>
		</MenuGroup>
	);
});

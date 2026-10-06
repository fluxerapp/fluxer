// SPDX-License-Identifier: AGPL-3.0-or-later

import {showExpressionCloneFailedModal} from '@app/features/app/components/alerts/ExpressionCloneFailedModal';
import Sticker from '@app/features/emoji/state/EmojiSticker';
import * as GuildStickerCommands from '@app/features/expressions/commands/GuildStickerCommands';
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

const CLONED_STICKER_TO_DESCRIPTOR = msg({
	message: 'Cloned sticker {stickerName} to {targetName}',
	comment: 'Toast confirming a sticker was cloned to a target community.',
});
const CHECKING_ACCESS_DESCRIPTOR = msg({
	message: 'Checking access...',
	comment: 'Loading label shown while permissions are being verified.',
});
const CLONE_STICKER_TO_DESCRIPTOR = msg({
	message: 'Clone sticker to…',
	comment: 'Submenu label listing communities the sticker can be cloned to.',
});
const STICKER_SLOTS_FULL_DESCRIPTOR = msg({
	message: 'Sticker slots full',
	comment: 'Status label shown when the target community has no available sticker slots.',
});
const logger = new Logger('CloneSticker');
const stickerCloneGuard = createKeyedActionGuard();

export type CloneableSticker = Readonly<{
	id: string;
	guildId: string;
	name: string;
	description?: string | null;
	tags?: ReadonlyArray<string>;
	animated: boolean;
}>;

interface CloneStickerMenuItemProps {
	sticker: CloneableSticker;
	onClose: () => void;
}

export const CloneStickerMenuItem: React.FC<CloneStickerMenuItemProps> = observer(({sticker, onClose}) => {
	const {i18n} = useLingui();
	const sourceGuild = sticker.guildId ? Guilds.getGuild(sticker.guildId) : null;
	const metadataState = ExpressionMetadata.getStickerMetadata(sticker.id);
	const shortcutEnabled = AdvancedSettings.expressionCloneShortcutsEnabled;
	const needsMetadataLookup = shortcutEnabled && !sourceGuild;
	useEffect(() => {
		if (needsMetadataLookup && !metadataState.data && !metadataState.loading && !metadataState.error) {
			void ExpressionMetadata.fetchStickerMetadata(sticker.id);
		}
	}, [needsMetadataLookup, metadataState.data, metadataState.loading, metadataState.error, sticker.id]);
	if (!shortcutEnabled) return null;
	const cloningAllowed = sourceGuild
		? sourceGuild.cloneStickerAllowed
		: metadataState.data
			? metadataState.data.allowCloning
			: null;
	const isLoadingPermission = needsMetadataLookup && metadataState.loading;
	if (cloningAllowed !== true && !isLoadingPermission) return null;
	const eligible = Guilds.getGuilds()
		.filter((guild) => guild.id !== sticker.guildId)
		.filter((guild) => Permission.can(Permissions.MANAGE_EXPRESSIONS, {guildId: guild.id}))
		.map((guild) => {
			const targetStickers = Sticker.getGuildStickers(guild.id);
			const used = targetStickers.length;
			return {guild, slotsFull: used >= guild.maxStickers};
		})
		.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	if (eligible.length === 0) return null;
	const cloneInto = async (guildId: string) => {
		const targetGuild = Guilds.getGuild(guildId);
		const targetName = targetGuild?.name ?? '';
		const inFlightKey = `${sticker.id}:${guildId}`;
		if (!stickerCloneGuard.begin(inFlightKey)) return;
		try {
			await GuildStickerCommands.clone(guildId, {sourceStickerId: sticker.id});
			ToastCommands.success(i18n._(CLONED_STICKER_TO_DESCRIPTOR, {stickerName: sticker.name, targetName}));
		} catch (error) {
			logger.error(`Failed to clone sticker ${sticker.id} into ${guildId}`, error);
			showExpressionCloneFailedModal(error, 'sticker', targetName);
		} finally {
			stickerCloneGuard.scheduleRelease(inFlightKey);
		}
	};
	if (isLoadingPermission) {
		return (
			<MenuGroup data-flx="ui.action-menu.items.clone-sticker-menu-item.menu-group">
				<MenuItem
					icon={<CopyIcon data-flx="ui.action-menu.items.clone-sticker-menu-item.copy-icon" />}
					disabled
					hint={i18n._(CHECKING_ACCESS_DESCRIPTOR)}
					data-flx="ui.action-menu.items.clone-sticker-menu-item.menu-item"
				>
					{i18n._(CLONE_STICKER_TO_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		);
	}
	return (
		<MenuGroup data-flx="ui.action-menu.items.clone-sticker-menu-item.menu-group--2">
			<MenuItemSubmenu
				label={i18n._(CLONE_STICKER_TO_DESCRIPTOR)}
				render={() => (
					<MenuGroup data-flx="ui.action-menu.items.clone-sticker-menu-item.menu-group--3">
						{eligible.map(({guild, slotsFull}) => (
							<MenuItem
								key={guild.id}
								icon={
									<GuildIcon
										id={guild.id}
										name={guild.name}
										icon={guild.icon}
										sizePx={16}
										data-flx="ui.action-menu.items.clone-sticker-menu-item.guild-icon"
									/>
								}
								disabled={slotsFull}
								hint={slotsFull ? i18n._(STICKER_SLOTS_FULL_DESCRIPTOR) : undefined}
								onClick={() => {
									void cloneInto(guild.id);
									onClose();
								}}
								data-flx="ui.action-menu.items.clone-sticker-menu-item.menu-item.close"
							>
								{guild.name}
							</MenuItem>
						))}
					</MenuGroup>
				)}
				data-flx="ui.action-menu.items.clone-sticker-menu-item.menu-item-submenu"
			/>
		</MenuGroup>
	);
});

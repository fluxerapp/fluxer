// SPDX-License-Identifier: AGPL-3.0-or-later
import {showExpressionCloneFailedModal} from '@app/features/app/components/alerts/ExpressionCloneFailedModal';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {
	CHECKING_ACCESS_DESCRIPTOR,
	type ExpressionCloneConfig,
	type ExpressionItem,
} from '@app/features/ui/action_menu/items/ExpressionMenuConfig';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import {createKeyedActionGuard} from '@app/lib/overlay/KeyedActionGuard';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {CopyIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect} from 'react';

const logger = new Logger('CloneExpression');
const expressionCloneGuard = createKeyedActionGuard();
export const CloneExpressionMenuItem = observer(
	({item: emoji, config, onClose}: {item: ExpressionItem; config: ExpressionCloneConfig; onClose: () => void}) => {
		const {i18n} = useLingui();
		const sourceGuild = emoji.guildId ? Guilds.getGuild(emoji.guildId) : null;
		const metadataState = emoji.id ? config.getMetadata(emoji.id) : null;
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
				void config.fetchMetadata(emoji.id);
			}
		}, [needsMetadataLookup, emoji.id, metadataState, config]);
		if (!shortcutEnabled) return null;
		if (!emoji.id) return null;
		const emojiId = emoji.id;

		const cloningAllowed = sourceGuild
			? config.cloningAllowed(sourceGuild)
			: metadataState?.data
				? metadataState.data.allowCloning
				: null;
		const isLoadingPermission = needsMetadataLookup && (metadataState?.loading ?? false);
		if (cloningAllowed !== true && !isLoadingPermission) return null;
		const eligible = Guilds.getGuilds()
			.filter((guild) => guild.id !== emoji.guildId)
			.filter((guild) => Permission.can(Permissions.MANAGE_EXPRESSIONS, {guildId: guild.id}))
			.map((guild) => {
				return {guild, slotsFull: config.slotsFull(guild)};
			})
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
		if (eligible.length === 0) return null;
		const cloneInto = async (guildId: string) => {
			const targetGuild = Guilds.getGuild(guildId);
			const targetName = targetGuild?.name ?? '';
			const inFlightKey = `${config.kind}:${emojiId}:${guildId}`;
			if (!expressionCloneGuard.begin(inFlightKey)) return;
			try {
				await config.clone(guildId, emojiId);
				ToastCommands.success(config.toast(i18n, emoji, targetName));
			} catch (error) {
				logger.error(`Failed to clone ${config.kind} ${emojiId} into ${guildId}`, error);
				showExpressionCloneFailedModal(error, config.kind, targetName);
			} finally {
				expressionCloneGuard.scheduleRelease(inFlightKey);
			}
		};
		if (isLoadingPermission) {
			return (
				<MenuGroup data-flx="ui.action-menu.items.clone-expression-menu-item.menu-group">
					<MenuItem
						icon={<CopyIcon data-flx="ui.action-menu.items.clone-expression-menu-item.copy-icon" />}
						disabled
						hint={i18n._(CHECKING_ACCESS_DESCRIPTOR)}
						data-flx="ui.action-menu.items.clone-expression-menu-item.menu-item"
					>
						{i18n._(config.label)}
					</MenuItem>
				</MenuGroup>
			);
		}
		return (
			<MenuGroup data-flx="ui.action-menu.items.clone-expression-menu-item.menu-group--2">
				<MenuItemSubmenu
					label={i18n._(config.label)}
					render={() => (
						<MenuGroup data-flx="ui.action-menu.items.clone-expression-menu-item.menu-group--3">
							{eligible.map(({guild, slotsFull}) => (
								<MenuItem
									key={guild.id}
									icon={
										<GuildIcon
											id={guild.id}
											name={guild.name}
											icon={guild.icon}
											sizePx={16}
											data-flx="ui.action-menu.items.clone-expression-menu-item.guild-icon"
										/>
									}
									disabled={slotsFull}
									hint={slotsFull ? i18n._(config.slotsFullLabel) : undefined}
									onClick={() => {
										void cloneInto(guild.id);
										onClose();
									}}
									data-flx="ui.action-menu.items.clone-expression-menu-item.menu-item.close"
								>
									{guild.name}
								</MenuItem>
							))}
						</MenuGroup>
					)}
					data-flx="ui.action-menu.items.clone-expression-menu-item.menu-item-submenu"
				/>
			</MenuGroup>
		);
	},
);

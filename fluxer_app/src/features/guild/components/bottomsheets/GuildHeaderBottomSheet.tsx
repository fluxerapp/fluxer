// SPDX-License-Identifier: AGPL-3.0-or-later

import {MuteDurationSheet} from '@app/features/app/components/bottomsheets/MuteDurationSheet';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import headerStyles from '@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet.module.css';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildCount from '@app/features/guild/state/GuildCount';
import {
	MUTE_COMMUNITY_DESCRIPTOR,
	UNMUTE_COMMUNITY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {useMuteSheet} from '@app/features/notification/hooks/useMuteSheet';
import {useGuildMenuData} from '@app/features/ui/action_menu/items/GuildMenuData';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
	type MenuSheetItem,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {GUILD_SETTINGS_LABEL_DESCRIPTOR} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const ONLINE_DESCRIPTOR = msg({
	message: '{presenceCount} online',
	comment:
		'Short label in the guild header bottom sheet. Keep it concise. Preserve {presenceCount}; it is inserted by code.',
});

function replaceSettingsSubmenusWithTriggers(
	groups: Array<MenuGroupType>,
	settingsLabels: Set<string>,
): Array<MenuGroupType> {
	return groups.map((group) => ({
		items: group.items.flatMap((item): Array<MenuSheetItem> => {
			if ('items' in item && settingsLabels.has(item.label) && item.onTriggerSelect) {
				const triggerItem: MenuItemType = {
					id: item.id,
					icon: item.icon,
					label: item.label,
					onClick: item.onTriggerSelect,
					disabled: item.disabled,
				};
				return [triggerItem];
			}
			return [item];
		}),
	}));
}

interface GuildHeaderBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	guild: Guild;
}

export const GuildHeaderBottomSheet: React.FC<GuildHeaderBottomSheetProps> = observer(({isOpen, onClose, guild}) => {
	const {i18n} = useLingui();
	useEffect(() => {
		if (!isOpen) return;
		GatewayConnection.syncGuildIfNeeded(guild.id, 'guild-header-bottom-sheet');
		GuildCount.requestCounts(guild.id, {force: true});
	}, [guild.id, isOpen]);
	const {muteSheetOpen, muteConfig, openMuteSheet, closeMuteSheet, handleMute, handleUnmute} = useMuteSheet({
		mode: 'guild',
		guildId: guild.id,
	});
	const {groups, isMuted, mutedText} = useGuildMenuData(guild, {
		onClose,
		onOpenMuteSheet: openMuteSheet,
	});
	const mobileGroups = useMemo(
		() => replaceSettingsSubmenusWithTriggers(groups, new Set([i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR)])),
		[groups, i18n.locale],
	);
	const guildCounts = GuildCount.getCounts(guild.id);
	const presenceCount = guildCounts?.onlineCount ?? 0;
	const memberCount = guildCounts?.memberCount ?? 0;
	const formattedPresenceCount = getCachedNumberFormat(i18n.locale).format(presenceCount);
	const headerContent = (
		<div className={headerStyles.header} data-flx="guild.guild-header-bottom-sheet.div">
			<div className={headerStyles.avatarWrapper} data-flx="guild.guild-header-bottom-sheet.div--2">
				<GuildIcon
					id={guild.id}
					name={guild.name}
					icon={guild.icon}
					className={headerStyles.icon}
					sizePx={48}
					data-flx="guild.guild-header-bottom-sheet.guild-icon"
				/>
			</div>
			<div className={headerStyles.text} data-flx="guild.guild-header-bottom-sheet.div--3">
				<span className={headerStyles.title} data-flx="guild.guild-header-bottom-sheet.span">
					{guild.name}
				</span>
				{guildCounts && (
					<div className={headerStyles.stats} data-flx="guild.guild-header-bottom-sheet.div--4">
						<div className={headerStyles.stat} data-flx="guild.guild-header-bottom-sheet.div--5">
							<div
								className={`${headerStyles.statDot} ${headerStyles.statDotOnline}`}
								data-flx="guild.guild-header-bottom-sheet.div--6"
							/>
							<span className={headerStyles.statText} data-flx="guild.guild-header-bottom-sheet.span--2">
								{i18n._(ONLINE_DESCRIPTOR, {presenceCount: formattedPresenceCount})}
							</span>
						</div>
						<div className={headerStyles.stat} data-flx="guild.guild-header-bottom-sheet.div--7">
							<div
								className={`${headerStyles.statDot} ${headerStyles.statDotMembers}`}
								data-flx="guild.guild-header-bottom-sheet.div--8"
							/>
							<span className={headerStyles.statText} data-flx="guild.guild-header-bottom-sheet.span--3">
								{plural(
									{count: memberCount},
									{
										one: '# member',
										other: '# members',
									},
								)}
							</span>
						</div>
					</div>
				)}
			</div>
		</div>
	);
	return (
		<>
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={mobileGroups}
				headerContent={headerContent}
				data-flx="guild.guild-header-bottom-sheet.menu-bottom-sheet"
			/>
			<MuteDurationSheet
				isOpen={muteSheetOpen}
				onClose={closeMuteSheet}
				isMuted={isMuted}
				mutedText={mutedText ?? null}
				muteConfig={muteConfig}
				muteTitle={i18n._(MUTE_COMMUNITY_DESCRIPTOR)}
				unmuteTitle={i18n._(UNMUTE_COMMUNITY_DESCRIPTOR)}
				onMute={handleMute}
				onUnmute={handleUnmute}
				data-flx="guild.guild-header-bottom-sheet.mute-duration-sheet"
			/>
		</>
	);
});

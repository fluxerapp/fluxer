import {createMuteConfig} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import {MUTE_CATEGORY_DESCRIPTOR, UNMUTE_CATEGORY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {MuteTargetMenuItem} from '@app/features/ui/action_menu/items/MuteTargetMenuItem';

import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface CategoryMenuItemProps {
	category: Channel;
	onClose: () => void;
}
export const MuteCategoryMenuItem: React.FC<CategoryMenuItemProps> = observer(({category, onClose}) => {
	const {i18n} = useLingui();

	const guildId = category.guildId!;
	const categoryOverride = UserGuildSettings.getChannelOverride(guildId, category.id);
	const isMuted = categoryOverride?.muted ?? false;
	const muteConfig = categoryOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const handleMute = useCallback(
		(duration: number | null) => {
			const nextMuteConfig = createMuteConfig(duration);
			UserGuildSettingsCommands.updateChannelOverride(guildId, category.id, {
				muted: true,
				mute_config: nextMuteConfig,
				collapsed: true,
			});
			onClose();
		},
		[guildId, category.id, onClose],
	);
	const handleUnmute = useCallback(() => {
		UserGuildSettingsCommands.updateChannelOverride(guildId, category.id, {
			muted: false,
			mute_config: null,
		});
		onClose();
	}, [guildId, category.id, onClose]);

	return (
		<MuteTargetMenuItem
			showUnmute={Boolean(isMuted && mutedText)}
			mutedText={mutedText}
			muteLabel={i18n._(MUTE_CATEGORY_DESCRIPTOR)}
			unmuteLabel={i18n._(UNMUTE_CATEGORY_DESCRIPTOR)}
			onMute={handleMute}
			onUnmute={handleUnmute}
			dataFlx="ui.action-menu.items.category-menu-items.mute-category-menu-item"
			data-flx="ui.action-menu.items.category-menu-items.mute-target-menu-item"
		/>
	);
});

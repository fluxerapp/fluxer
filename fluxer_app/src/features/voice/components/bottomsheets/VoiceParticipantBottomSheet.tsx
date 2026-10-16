// SPDX-License-Identifier: AGPL-3.0-or-later

import {useVoiceParticipantMenuData} from '@app/features/ui/action_menu/items/VoiceParticipantMenuData';
import type {
	VoiceParticipantMenuSource,
	VoiceParticipantMenuSurface,
} from '@app/features/ui/action_menu/items/VoiceParticipantMenuTypes';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import type {User} from '@app/features/user/models/User';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface VoiceParticipantBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	user: User;
	guildId?: string;
	connectionId?: string;
	surface: VoiceParticipantMenuSurface;
	source: VoiceParticipantMenuSource;
	isConnectionItem?: boolean;
	isParentGroupedItem?: boolean;
	participant?: unknown;
}

export const VoiceParticipantBottomSheet: React.FC<VoiceParticipantBottomSheetProps> = observer(
	({
		isOpen,
		onClose,
		user,
		guildId,
		connectionId,
		surface,
		source,
		isConnectionItem = false,
		isParentGroupedItem = false,
	}) => {
		const {groups} = useVoiceParticipantMenuData({
			user,
			guildId,
			connectionId,
			surface,
			source,
			isGroupedItem: isConnectionItem,
			isParentGroupedItem,
			onClose,
		});
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={groups}
				data-flx="voice.voice-participant-bottom-sheet.menu-bottom-sheet"
			/>
		);
	},
);

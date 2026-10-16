// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildDragItem, GuildDropResult} from '@app/features/app/components/layout/types/DndTypes';
import type {Guild} from '@app/features/guild/models/Guild';
import type React from 'react';

export interface GuildListItemProps {
	readonly guild: Guild;
	readonly isSortingList: boolean;
	readonly isSelected: boolean;
	readonly guildIndex: number | null;
	readonly selectedGuildIndex: number | null;
	readonly onGuildDrop: ((item: GuildDragItem, result: GuildDropResult) => void) | null;
	readonly onDragStateChange: ((item: GuildDragItem | null) => void) | null;
	readonly disableDrag: boolean;
	readonly insideFolderId: number | null;
	readonly isLastInsideFolder: boolean;
	readonly scrollTargetRef: React.RefCallback<HTMLElement> | null;
}

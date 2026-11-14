// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindHandler} from '@app/features/app/keybindings/keybind_manager/shared';
import type {Channel} from '@app/features/channel/models/Channel';
import type {KeybindCommand} from '@app/features/input/state/InputKeybind';
import type {Logger} from '@app/features/platform/utils/AppLogger';

export interface HandlerHost {
	register(action: KeybindCommand, handler: KeybindHandler): void;
	readonly logger: Logger;
	readonly currentChannelId: string | null;
	readonly currentGuildId: string | null;
	navigateToChannel(guildId: string | null, channelId: string): void;
	navigateToDirectMessages(): void;
	navigateToLastCommunityChannel(): boolean;
	navigateToGuildLikeSlot(slotIndex: number): void;
	cycleGuildLikeSlot(direction: 1 | -1): void;
	cycleChannelInCurrentContext(direction: 1 | -1): void;
	cycleFilteredChannelInCurrentGuild(predicate: (channel: Channel) => boolean, direction: 1 | -1): void;
	getIncomingCallChannelId(): string | null;
	acceptIncomingCall(channelId: string): void;
	declineIncomingCall(channelId: string): void;
}
